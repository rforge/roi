
emph_which <- function(x) sprintf("\033[31m\033[1m%s\033[22m\033[39m", x)
emph_call <- function(x) sprintf("\033[31m\033[1m%s\033[22m\033[39m", x)

## Error-Types:
## - DIMENSION_MISMATCH: dimension mismatch
## - MISSPECIFICATION
error <- function(which, message, domain, hint=NULL, note=NULL, call=sys.call(-1L)) {
    msg <- sprintf("%s in %s\n", emph_which(which), emph_call(domain))
    msg <- sprintf("%s\t%s\n", msg, message)
    if (!is.null(call)) {
        msg <- sprintf("%s\tCall: %s\n", msg, deparse(call))
    }
    if (!is.null(note)) {
        note <- paste(note, collapse="\n\t      ")
        msg <- sprintf("%s\tNote: %s\n", msg, note)
    }
    if (!is.null(hint)) {
        hint <- paste(hint, collapse="\n\t      ")
        msg <- sprintf("%s\tHint: %s\n", msg, hint)
    }
    stop(msg, call.=FALSE)
}


## fun("some", "other", note="")

## which <- "DIMENSION_MISMATCH"
## message <- "The dimensions of 'Q' and 'L' don't match!"
## fun <- function(x, y, hint=NULL, note=NULL) {
##     x <- 3
##     if (TRUE) e <- error("DIMENSION_MISMATCH", "The dimensions of 'Q' and 'L' don't match!", hint, note)
##     return(e)
## }

