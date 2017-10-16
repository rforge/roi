##
## NOTES:
##   - should we guess the file type?
##
InputOutputDataBase <- function() {
    env <- new.env(parent = emptyenv())
    env$reader <- list()
    env$inverted_index_reader <- list()
    env$writer <- list()
    env$inverted_index_writer <- list()

    ## NOTE: The combination of apply and paste adds sometimes unwanted spaces
    ##       therefore we need gsub to canonicalize.
    to_id <- function(x) {
        gsub(" ", "", apply(x, 1, paste, collapse = ""), fixed = TRUE)
    }

    env$append_reader <- function(type, solver, method) {
        self <- parent.env(environment())$env

        id <- length(self$reader) + 1L
        self$reader[[id]] <- list(type = type, solver = solver, method = method)
        self$inverted_index_reader[[type]] <- c(self$inverted_index_reader[[type]], id)
        invisible(NULL)
    }

    env$get_reader_info <- function(type = NULL) {
        self <- parent.env(environment())$env
        no_reader_found <- data.frame(type=character(0), solver=character(0), stringsAsFactors=FALSE)
        if ( is.null(type) ) {
            reader <- self$reader
        } else {
            k <- self$inverted_index_reader[[type]]
            if ( is.null(k) )
                return(no_reader_found)
            reader <- self$reader[k]
        }
        if ( !length(reader) )
            return(no_reader_found)
        info <- do.call(rbind, lapply(reader, function(x) x[c("type", "solver")]))
        rownames(info) <- NULL
        return( as.data.frame(info, stringsAsFactors = FALSE) )
    }

    env$get_reader <- function(type, solver = NULL) {
        self <- parent.env(environment())$env

        k <- self$inverted_index_reader[[type]]
        if ( is.null(k) ) {
            return(1L) ## no reader of type type found
        }
        if ( is.null(solver) ) {
            return(self$reader[[k[1]]]$method)
        }
        selected_reader <- self$reader[k]
        j <- which(sapply(selected_reader, "[[", "solver") == solver)
        if ( !length(j) ) {
            return(2L) ## no reader of type type and solver solver found
        }
        return(selected_reader[[j]]$method)
    }

    env$append_writer <- function(type, solver, signature, method) {
        self <- parent.env(environment())$env

        id <- length(self$writer) + 1L
        self$writer[[id]] <- list(type = type, solver = solver,
                                  signature = signature, method = method)
        signature_ids <- to_id(signature)
        for (sig in signature_ids) {
            self$inverted_index_writer[[sig]] <- c(self$inverted_index_writer[[sig]], id)
        }
        invisible(NULL)
    }

    env$get_writer_info <- function(signature = NULL) {
        self <- parent.env(environment())$env
        no_writer_found <- data.frame(type=character(0), solver=character(0), stringsAsFactors=FALSE)
        if ( is.null(signature) ) {
            writer <- self$writer
        } else {
            k <- self$inverted_index_writer[[to_id(signature)]]
            if ( is.null(k) )
                return(no_writer_found)
            writer <- self$writer[k]
        }
        if ( !length(writer) )
            return(no_writer_found)
        info <- do.call(rbind, lapply(writer, function(x) x[c("type", "solver")]))
        rownames(info) <- NULL
        return(as.data.frame(info, stringsAsFactors = FALSE))
    }

    env$get_writer <- function(signature, type, solver = NULL) {
        self <- parent.env(environment())$env

        k <- self$inverted_index_writer[[to_id(signature)]]
        if ( is.null(k) ) {
            return(1L)  ## no with correct signature found
        }

        selected_writer <- self$writer[k]

        i <- which(sapply(selected_writer, "[[", "type") == type)
        if ( !length(i) ) {
            return(2L)
        }

        if ( is.null(solver) ) {
            return(selected_writer[[i[1]]]$method)
        }
        
        selected_writer <- selected_writer[i]
        j <- which(sapply(selected_writer, "[[", "solver") == solver)
        if ( !length(j) ) {
            return(3L)
        }
        return(selected_writer[[j]]$method)
    }

    env
}

##  -----------------------------------------------------------
##  ROI_plugin_register_reader_writer
##  =================================
##' @title Register Reader / Writer Method
##'
##' @description Register a new reader / writer method to be used with 
##'        \code{read.io} / \code{write.io}.
##' @param type a character giving the type of the file 
##'             (e.g. \code{"mps_free"}, \code{"mps_fixed"}, \code{"lp_cplex"},
##'                   \code{"lp_lpsolve"}, ...).
##' @param solver a character giving the name of the plugin (e.g. \code{"lpsolve"}).
##' @param signature a data.frame giving the signature of the optimization problems
##'        which can be read or written by the registered method.
##' @param method a function registered as reader / writer method.
##' @details
##' \itemize{
##'   \item \bold{File Types}
##'   \item \bold{Method} \cr
##' }
##' @return NULL on success
##' @family input output
##' @name ROI_plugin_register_reader_writer
##' @export
ROI_plugin_register_reader <- function(type, solver, method) {
    stopifnot(is.character(type), is.character(solver), 
                  length(type) == 1L, length(solver) == 1L)
    io_db$append_reader(type, solver, method)
}

##' @name ROI_plugin_register_reader_writer
##' @export
ROI_plugin_register_writer <- function(type, solver, signature, method) {
    stopifnot(is.character(type), is.character(solver), 
                  length(type) == 1L, length(solver) == 1L)
    io_db$append_writer(type, solver, signature, method)
}

##  -----------------------------------------------------------
##  read.op
##  =======
##' @title Read Optimization Problems
##'
##' @description Reads an optimization problem from various file formats and
##'              returns an optimization problem of class \code{"OP"}.
##' @param file a character giving the name of the file the optimization problem
##'             is to be read from.
##' @param type a character giving the type of the file 
##'             (e.g. \code{"mps_free"}, \code{"mps_fixed"}, \code{"lp_cplex"},
##'                   \code{"lp_lpsolve"}, ...).
##' @param solver an optional character giving the name of the plugin 
##'               (e.g. \code{"lpsolve"}).
##' @param ... further arguments passed on to the read method.
##' @return x an optimization problem of class \code{"OP"}.
##' @family input output
##' @export
read.op <- function(file, type, solver=NULL, ...) {
    stopifnot(is_string(file), is_string(type), file.exists(file))
    read_file <- io_db$get_reader(type, solver)
    if ( !is.function(read_file) ) {
        if ( isTRUE(read_file == 2L) ) {
            stop(sprintf("no reader found for type '%s' and solver '%s'",
                         paste(type), paste(solver)))
        }
        stop(sprintf("no reader found for type '%s'", paste(type)))
    }
    read_file(file, ...)
}

##  -----------------------------------------------------------
##  write.op
##  ========
##' @title Write Optimization Problems
##'
##' @description Write an optimization problem to file.
##' @param x an optimization problem of class \code{"OP"}.
##' @param file a character giving the name of the file the optimization problem
##'             is to be written.
##' @param type a character giving the type of the file 
##'             (e.g. \code{"freemps"}, \code{"mps_fixed"}, \code{"lp_cplex"},
##'                   \code{"lp_lpsolve"}, ...).
##' @param solver an optional character giving the name of the plugin 
##'               (e.g. \code{"lpsolve"}).
##' @param ... further arguments passed on to the write method.
##' @family input output
##' @export
write.op <- function(x, file, type, solver=NULL, ...) {
    stopifnot(inherits(x, "OP"), is_string(file), is_string(type))
    signature <- OP_signature(x)
    write_file <- io_db$get_writer(signature, type, solver)
    if ( !is.function(write_file) ) {
        if ( isTRUE(write_file == 2L) ) {
            stop(sprintf("no writer found for the given signature and type '%s'", paste(type)))
        } else if ( isTRUE(write_file == 3L) ) {
            stop(sprintf("no writer found for the given signature and type '%s' and solver '%s'", 
                         paste(type), paste(solver)))
        }
        stop(sprintf("no writer found for the given signature"))
    }
    write_file(x, file, ...)
}

##  -----------------------------------------------------------
##  ROI_registered_reader
##  =====================
##' @title List Registered Reader 
##'
##' @description Retrieve meta information about the registered reader
##' @param type an optional character giving the type of the file 
##'             (e.g. \code{"mps_free"}, \code{"mps_fixed"}, \code{"lp_cplex"},
##'                   \code{"lp_lpsolve"}, ...).
##' @return x a data.frame containing information about the registered readers.
##' @examples
##' ROI_registered_reader()
##' ROI_registered_reader("mps_fixed")
##' @family input output
##' @export
ROI_registered_reader <- function(type = NULL) {
    io_db$get_reader_info(type)
}

##  -----------------------------------------------------------
##  ROI_registered_writer
##  =====================
##' @title Write Optimization Problems
##'
##' @description Write an optimization problem to file.
##' @param signature an optimization problem of class \code{"OP"}.
##' @examples
##' ROI_registered_writer()
##' op <- OP(1:2)
##' ROI_registered_writer(OP_signature(op))
##' @family input output
##' @export
ROI_registered_writer <- function(signature = NULL) {
    io_db$get_writer_info(signature)
}
