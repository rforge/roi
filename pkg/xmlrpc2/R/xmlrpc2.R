#' @import base64enc
#' @import xml2
#' @import curl

#  -----------------------------------------------------------
#  xmlrpc 
#  =======
#' @title Call the Remote Procedure
#' @description Call a reomte procedure with the \code{XML-RPC} protocol.
#' @param url a character string giving the url to the server.
#' @param method a character string giving the name of the method 
#'               to be invoked.
#' @param params a list containing the parmeters which are added to 
#'               the \code{XML} file sent via the remote procedure call.
#' @param handle a object of class \code{"curl_handle"}.
#' @param opts a list of options passed to the function \code{"handle_setopt"}.
#' @param convert a logical, if convert is \code{TRUE} (default)
#'                the \code{curl} response is converted else it is 
#'                left unchanged.
#' @param useragent a character string giving the name of the \code{"User-Agent"}. 
#' @param raise_error a logical controling the behavior if the status code
#'                    of \code{curl_fetch_memory} signals an error.
#'                    If \code{raise_error} is \code{TRUE} an error is raised,
#'                    if \code{raise_error} is \code{FALSE} 
#'                    no error is raised and an object inheriting from
#'                    \code{c("fetch_error", error")} is returned.
#'                    This object is the return value from \code{curl_fetch_memory}
#'                    where just the class \code{c("fetch_error", error")}
#'                    is added.
#' @return the reponse of \code{curl} or the response converted to 
#'         \R objects.
#' @examples
#' \dontrun{
#' url <- "https://www.neos-server.org"
#' xmlrpc(url, "listAllSolvers")
#' xmlrpc(url, "listSolversInCategory", params = list(category = "socp"))
#' }
#' @export
xmlrpc <- function(url, method, params = list(), 
    handle = NULL, opts = list(), convert = TRUE, useragent = "xmlrpc",
    raise_error = TRUE) {
    stopifnot(is.character(url), is.character(method), 
              all(nchar(names(opts)) > 0), is.logical(convert))

    body <- to_xmlrpc(method, params)

    if ( is.null(handle) ) {
        handle <- new_handle()
        handle_setopt(handle, port = 3333)
    } else {
        stopifnot(inherits(handle, "curl_handle"))
    }

    handle_setopt(handle, customrequest = "POST")
    handle_setopt(handle, followlocation = TRUE)
    handle_setopt(handle, postfields = as.character(body))

    handle_setheaders(handle, "Content-Type" = "text/xml", "User-Agent" = useragent)

    if ( length(opts) ) {
        handle_setopt(handle, .list = opts)
    }

    response <- curl_fetch_memory(url, handle)
    
    if ( !is_successful_request(response$status_code) ) {
        if (raise_error) {
            stop(request_error_msg(response$content))
        } else {
            class(response) <- c("fetch_error", "error", class(response))
            return(response)
        }
    }

    if ( isTRUE(convert) ) {
        from_xmlrpc( rawToChar(response$content), raise_error = raise_error )
    } else {
        response
    }
}

## NOTE:
##   See https://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html
##   for more information about status codes.
is_successful_request <- function(x) {
    if ( !is.numeric(x) ) return(FALSE)
    (x >= 200) & (x < 300)
}

request_error_msg <- function(x) {
    tryCatch(xml_text(read_html(rawToChar(x))), 
        error = function(e) "The request was not successful!")
}

#  -----------------------------------------------------------
#  to_xmlrpc 
#  =========
#' @title Create a \code{XML-RPC} Call
#' @description abc
#' @param method a character string giving the name of the method 
#'               to be invoked.
#' @param params a list containing the parmeters which are added to 
#'               the \code{XML} file sent via the remote procedure call.
#' @return an object of class \code{"xml_node"} containing a \code{XML-RPC} call.
#' @examples
#' params <- list(1L, 1:3, rnorm(3), LETTERS[1:3], charToRaw("A"))
#' cat(as.character(to_xmlrpc("some_method", params)))
#' @export
to_xmlrpc <- function(method, params) {
    root <- read_xml("<methodCall></methodCall>")
    xml_add_child(root, "methodName", method)
    xml_add_child(root, "params")
    parameters <- xml_children(root)[[2L]]
    for (i in seq_along(params)) {
        xml_add_child(parameters, "param")
        parameter <- xml_children(parameters)[[i]]
        xml_add_child(parameter, rpc_serialize(params[[i]]))
    }
    root
}
