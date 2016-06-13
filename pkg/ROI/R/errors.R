## Error-Types:
## - DIMENSION_MISMATCH: dimension mismatch
## - MISSPECIFICATION
error <- function(which, message, domain, hint=NULL, note=NULL, call=sys.call(-1L)) {
    msg <- sprintf("%s in %s\n", which, domain)
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
