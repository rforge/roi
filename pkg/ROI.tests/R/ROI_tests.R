
TESTS <- new.env(parent=emptyenv(), size=200)
SOLVER_TESTS <- new.env(parent=emptyenv(), size=200)

new_test <- function(x, value) {
    assign(x, value, envir=base::getNamespace("ROI.tests")$TESTS)
}

new_solver_test <- function(x, value) {
    assign(x, value, envir=base::getNamespace("ROI.tests")$SOLVER_TESTS)
}

## http://misc.flogisoft.com/bash/tip_colors_and_formatting
## \033 is \
## \[31m ... red
## \[0m  ... color off
## \[1m  ... bold
## \[22m ,,, bold off
## 34m blue, 32m green, 33m yellow
red <- function(x) sprintf("\033[31m\033[1m%s\033[22m\033[0m", x)
magenta <- function(x) sprintf("\033[35m\033[1m%s\033[22m\033[0m", x)
cyan <-  function(x) sprintf("\033[36m\033[1m%s\033[22m\033[0m", x)
yellow <- function(x) sprintf("\033[33m\033[1m%s\033[22m\033[39m", x)

blue <- function(x) sprintf("\033[34m\033[1m%s\033[22m\033[39m", x)
green <- function(x) sprintf("\033[32m\033[1m%s\033[22m\033[39m", x)

## for (i in 1:100) {
##     color <- function(x) sprintf("\033[%im\033[1m%02i %s\033[22m\033[0m", i, i, x)
##     cat(color("NOTE\n"))
## }

## print some info about the check
error <- function(message) {
    cat(sprintf("%s %s\n", red("ERROR"), message))
}

warn <- function(message) {
    cat(sprintf("%s %s\n", magenta("WARNING"), message))
}

note <- function(message) {
    cat(sprintf("%s %s\n", cyan("NOTE"), message))
}

info <- function(message) {
    cat(sprintf("%s %s\n", yellow("INFO"), message))
}

## level 1 ... error
## level 2 ... warning
## level 3 ... note
## level 4 ... info
check <- function(domain, condition, level=1, message="", call=sys.call(-1L)) {
    if ( condition ) return(invisible(NULL))
    msg <- sprintf("in %s", domain)
    if ( all(nchar(message) > 0) ) msg <- sprintf("%s\n\t%s", msg, message)
    if ( level == 1 ) error(msg)
    if ( level == 2 ) warn(msg)
    if ( level == 3 ) note(msg)
    if ( level == 4 ) info(msg)
    return(invisible(NULL))
}

mksig <- function(objective="L", constraints="L", types="C", bounds="V", cones="free", maximum=FALSE) {
    .ROI_plugin_make_signature(objective = objective, constraints = constraints,
        types = types, bounds = bounds, cones = cones, maximum = maximum)
}

register_solver_test <- function(signature, name, fun) {
    new_solver_test(name, list(signature=signature, test=fun))
}

register_test <- function(signature, name, fun) {
    new_test(name, list(signature=signature, test=fun))
}

test_solver <- function(solver) {
    tests <- lapply(TESTS, "[[", "signature")
    b <- sapply(tests, function(x) solver %in% unname(names(ROI:::get_solver_methods(x))))
    applicable_tests <- names(b)[b]
    for (test in applicable_tests) {
        cat(green(test), "\n")
        TESTS[[test]]$test(solver)
    }
}

get_test <- function(x) {
    TESTS[[x]]
}


## ROI:::ROI_required_signature()
##     .ROI_plugin_make_signature( objective = c("L", "Q", "F"),
##                                 constraints = c("X", "L", "Q", "F"),
##                                 types = c("C"),
##                                 bounds = c("X", "V"),
##                                 cones = c("free"),
##                                 maximum = c(TRUE, FALSE) )
## error <- function(which, message, domain, hint=NULL, note=NULL, call=sys.call(-1L)) {
##     msg <- sprintf("%s in %s\n", emph_which(which), emph_call(domain))
##     msg <- sprintf("%s\t%s\n", msg, message)
##     if (!is.null(call)) {
##         msg <- sprintf("%s\tCall: %s\n", msg, deparse(call))
##     }
##     if (!is.null(note)) {
##         note <- paste(note, collapse="\n\t      ")
##         msg <- sprintf("%s\tNote: %s\n", msg, note)
##     }
##     if (!is.null(hint)) {
##         hint <- paste(hint, collapse="\n\t      ")
##         msg <- sprintf("%s\tHint: %s\n", msg, hint)
##     }
##     stop(msg, call.=FALSE)
## }
## 
