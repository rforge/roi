
rpc_typeof <- function(x) UseMethod("rpc_typeof", x)
rpc_typeof.logical <- function(x) "boolean"
rpc_typeof.integer <- function(x) "i4"
rpc_typeof.double <- function(x) "double"
rpc_typeof.character <- function(x) "string"
rpc_typeof.raw <- function(x) "base64"
rpc_typeof.POSIXt <- function(x) "dateTime.iso8601"
rpc_typeof.POSIXct <- function(x) "dateTime.iso8601"
rpc_typeof.Date <- function(x) "dateTime.iso8601"

to_rpc <- function(x) UseMethod("to_rpc", x)
to_rpc.default <- identity
to_rpc.Date <- function(x) format(x, "%Y%m%dT%H:%H:%S")
to_rpc.POSIXt <- function(x) format(as.POSIXct(x), "%Y%m%dT%H:%H:%S")

#  -----------------------------------------------------------
#  rpc_serialize 
#  =============
#' Convert \R Objects into the \code{XML-RPC} Format
#' @description Serialize \R Objects so they can be passed to 
#'   \code{to_xmlrpc} as parameters.
#' @param x an \R object.
#' @param ... additional optional arguments (currently ignored).
#' @return an object of class \code{"xml_node"}.
#' @examples
#' rpc_serialize(1L)
#' rpc_serialize(1:2)
#' rpc_serialize(LETTERS[1:2])
#' @export
rpc_serialize <- function(x, ...) UseMethod("rpc_serialize", x)

#' @noRd
#' @export
rpc_serialize.NULL <- function(x, ...) {
    node <- new_xml_node("array")
    xml_add_child(node, "data")
    node
}

#' @noRd
#' @export
rpc_serialize.raw <- function(x, ...) {
    node <- new_xml_node("value")
    ## xml_add_child(node, "base64", RCurl::base64Encode(x))
    xml_add_child(node, "base64", base64encode(x))
    node
}

rpc_serialize_vector <- function(x, ...) {
    type <- rpc_typeof(x)

    x <- unname(x)
    if ( length(x) == 1 ) {
        to_value(x, type)
    } else {
        vec_to_array(x, type)
    }
}

#' @noRd
#' @export
rpc_serialize.logical <- function(x, ...) rpc_serialize_vector(as.integer(x))

#' @noRd
#' @export
rpc_serialize.integer <- rpc_serialize_vector

#' @noRd
#' @export
rpc_serialize.numeric <- rpc_serialize_vector

#' @noRd
#' @export
rpc_serialize.character <- rpc_serialize_vector

#' @noRd
#' @export
rpc_serialize.Date <- rpc_serialize_vector

#' @noRd
#' @export
rpc_serialize.POSIXt <- rpc_serialize_vector

#' @noRd
#' @export
rpc_serialize.list <- function(x, ...) {
    list_to_array(unname(x))
}

to_value <- function(x, type, cdata = FALSE) {
    value <- new_xml_node("value")
    if (cdata) {
        xml_add_child(value, type)
        ty <- xml_children(value)[[1L]]
        xml_add_child(ty, xml_cdata(x))    
    } else {
        xml_add_child(value, type, to_rpc(x))    
    }
    value
}

new_xml_node <- function(key, value = NULL) {
    root <- read_xml("<root></root>")
    if ( is.null(value) ) {
        xml_add_child(root, key)
    } else {
        xml_add_child(root, key, value)
    }
    xml_children(root)[[1L]]
}

new_xml_array <- function() {
    read_xml("<root><value><array><data></data></array></value></root>")    
}

vec_to_array <- function(x, type) {
    root <- new_xml_array()
    value <- xml_children(root)[[1L]]
    data <- xml_children(xml_children(value)[[1L]])[[1L]]
    for ( i in seq_along(x) ) {
        xml_add_child(data, 
                      to_value(x[i], type, type == "string"))
    }
    value
}

## Only supports non nested lists
list_to_array <- function(x) {
    if ( any(lengths(x) > 1) ) 
        stop("nested lists are not supported!")
    root <- new_xml_array()
    value <- xml_children(root)[[1L]]
    data <- xml_children(xml_children(value)[[1L]])[[1L]]
    for ( i in seq_along(x) ) {
        type <- rpc_typeof(x[i])
        xml_add_child(data, 
                      to_value(x[i], type, type == "string"))
    }
    value
}

#  -----------------------------------------------------------
#  from_xmlrpc 
#  ===========
#' Convert from the \code{XML-RPC} Format into an \R Object.
#' @description Convert an object of class \code{"xml_code"} or
#'   a character in the \code{XML-RPC} Format into an \R Object.
#' @param xml a character string containing \code{XML} in the 
#'            remote procedure call protocol format.
#' @param raise_error a logical controling the behavior if the
#'                    \code{XML-RPC} signals a fault. If \code{TRUE}
#'                    an error is raised, if \code{FALSE} an 
#'                    object inheriting from \code{"c("xmlrpc_error", "error")"}
#'                    is returned.
#' @return an R object derived from the input.
#' @examples
#' params <- list(1L, 1:3, rnorm(3), LETTERS[1:3], charToRaw("A"))
#' xml <- to_xmlrpc("some_method", params)
#' from_xmlrpc(xml)
#' @export
from_xmlrpc <- function(xml, raise_error = TRUE) {
    stopifnot( inherits(xml, c("xml_node", "character")) )
    if ( inherits(xml, "character") )
        xml <- read_xml(xml)

    fault <- xml_children(xml_find_all(xml, "//methodResponse/fault"))
    if ( length(fault) ) {
        ans <- unlist(lapply(fault, from_rpc))
        if (raise_error) {
            stop(paste(paste(names(ans), ans, sep = ": "), collapse = "\n"))
        } else {
            return(structure(ans, class = c("xmlrpc_error", "error")))
        }
    }
    
    values <- xml_children(xml_find_all(xml, "//param/value"))
    ans <- lapply(values, from_rpc)
    if ( length(ans) == 1L ) {
        ans[[1L]]
    } else {
        ans
    }
}

from_rpc <- function(x) {
    if ( is.null(x) )
        return(NULL)

    if ( xml_name(x) == "value" ) ## do I really need this?
        x <- xml_children(x)[[1L]]

    type <- xml_name(x)
    switch(type, 
           'array' = from_rpc_array(x),
           'struct' = from_rpc_struct(x),
           'i4' = as.integer(xml_text(x)),
           'int' = as.integer(xml_text(x)),
           'boolean' = if(xml_text(x) == "1") TRUE else FALSE,
           'double' = as.numeric(xml_text(x)),
           'string' = xml_text(x),
           'dateTime.iso8601' = as.POSIXct(strptime(xml_text(x), "%Y%m%dT%H:%M:%S")),
           'base64' = base64decode(xml_text(x)),
           xml_text(x)
    )
}

from_rpc_struct <- function(x) {
    keys <- xml_text(xml_find_all(x, "//name"))
    get_values <- function(rec) {
        xml_children(rec)[xml_name(xml_children(rec)) == "value"]
    }
    values <- lapply(xml_children(x), function(rec) from_rpc(get_values(rec)))
    names(values) <- keys
    values
}

from_rpc_array <- function(x) {
    recs <- xml_children(xml_children(x)[[1L]])
    v <- lapply(recs, function(rec) from_rpc(xml_children(rec)[[1L]]))
    if ( all_same_type(v) ) {
        unlist(v, FALSE, FALSE)
    } else {
        v
    }
}

all_same_type <- function(x) {
    isTRUE(length(unique(sapply(x, typeof))) == 1L)
}
