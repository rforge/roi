##' ROI Options
##'
##' Allow the user to set and examine a variety of ROI options like the default
##' solver or the function used to compute the gradients.
##' @param option any options can be defined, using 'key, value' pairs.
##'   If 'value' is missing the current set value is returned for the given 'option'.
##'   If both are missing. all set options are returned.
##' @param value the corresponding value to set for the given option.
##'@export
ROI_options <-
local({
    options <- list()
    function(option, value) {
        if (missing(option)) return(options)
        if (missing(value))
            options[[option]]
        else
            options[[option]] <<- value
    }
})

## Plugins directly distributed with ROI
ROI_get_included_plugins <- function()
    sprintf( "%s.%s", .plugin_prefix(), c("nlminb") )

##' @noRd
##' @import registry methods
NULL

## STATUS_DB
## create registry object containing status codes
add_status_db_schema <- function( status_db ){
    status_db$set_field("solver",   type = "character", is_key = TRUE)
    status_db$set_field("code",     type = "integer",   is_key = TRUE)
    status_db$set_field("symbol",   type = "character")
    status_db$set_field("message",  type = "character")
    status_db$set_field("roi_code", type = "integer",   alternatives = 0:1)
    status_db
}
status_db <- registry( )
status_db <- add_status_db_schema( status_db )

## SOLVER_DB
add_solver_db_schema <- function( solver_db ){
    solver_db$set_field( "solver",      type = "character", is_key = TRUE )
    solver_db$set_field( "plugin",      type = "character", is_key = TRUE )
    solver_db$set_field( "objective",   type = "character", validity_FUN = function(x) x %in% names(available_objective_classes()), is_key = TRUE)
    solver_db$set_field( "constraints", type = "character", validity_FUN = function(x) x %in% names(available_constraint_classes()), is_key = TRUE)
    for( type in available_types() )
        solver_db$set_field( type,      type = "logical",   is_key = TRUE)
    solver_db$set_field( "bounds",      type = "character", validity_FUN = valid_bound, is_key = TRUE)
    solver_db$set_field( "cones",       type = "character", validity_FUN = valid_cone, is_key = TRUE)
    solver_db$set_field( "maximum",     type = "logical",   is_key = TRUE)
    solver_db$set_field( "FUN",         type = "function" )
    solver_db
}
solver_db <- registry( )
solver_db <- add_solver_db_schema( solver_db )

cross_validate_schema <- function( args, solver_db){
    stopifnot( all(args %in% c(solver_db$get_field_names(), "types")) )
    invisible( TRUE )
}

schema_valid <- cross_validate_schema( names(formals(OP)), solver_db )

## CONTROL_DB
## create registry object for (partial) solver control argument canonicalization
add_control_db_schema <- function( control_db ){
    control_db$set_field( "solver",      type = "character", is_key = TRUE )
    control_db$set_field( "control",     type = "character", is_key = TRUE )
    control_db$set_field( "roi_control", type = "character", alternatives = ROI_available_solver_controls() )
    control_db
}
control_db <- registry( )
control_db <- add_control_db_schema( control_db )

## SOLVER Signature Database
## The solver signature is now lost in solver_db.
solver_signature_db <- SolverDatabase()

## REFORMULATION_DB
## create a database for the reformulations
reformulation_db <- ReformulationDatabase()

## Input / Output Tools
##
io_db <- InputOutputDataBase()

##
## IdGenerator
##
IdGenerator <- function() {
    id_gen <- new.env(parent=emptyenv())
    id_gen$id <- 0L
    id_gen$get_ids <- function(n = 1L) {
        self <- parent.env(environment())$id_gen
        if ( (self$id + n) > 2147483647L ) {
            self$id <- 1L
        }
        ids <- self$id + seq_len(n)
        self$id <- tail(ids, 1L)
        ids
    }
    id_gen
}

id_generator <- IdGenerator()

register_reformulations <- function() {
    ##
    ## BQP to MILP
    ##
    lbqp.cite <- paste("Boros, Endre, and Peter L. Hammer.",
                       '"Pseudo-boolean optimization."',
                       "Discrete applied mathematics 123.1 (2002): 155-225.",
                       "\n",
                       "David Meyer and Kurt Hornik (2017). relations: Data Structures and",
                       "Algorithms for Relations. R package version 0.6-7.",
                       collapse = " ")

    lbqp.descr <- paste("Reformulate a binary optimization problem with",
                        "quadratic objective and linear constraints",
                        "to a mixed integer problem with linear objective and",
                        "linear constraints.", collapse = " ")
    
    ROI_plugin_register_reformulation(from = QPLC.B(), to = LPLC.BCI(), method_name = "bqp_to_lp", 
                                      method = bqp_to_lp, description = lbqp.descr, 
                                      cite = lbqp.cite, author = "")

    ##
    ## Convex QP to SOCP
    ##
    qpsoc.descr <- "Reformulate a convex QP to a SOCP."
    ROI_plugin_register_reformulation(from = QPLC.BCI(), to = LPLC.BCI.SOC(), method_name = "qp_to_socp", 
                                      method = qp_to_socp, description = qpsoc.descr, cite = "", author = "")
}

## A try to speed up the loading of the package, but since most of the loading
## time goes into loading the matrix package using this method could only
## improve the loding time at 0.04 seconds.
## get_namespace <- function(name) {
##     if ( isNamespaceLoaded(name) ) {
##         getNamespace(name)
##     } else {
##         loadNamespace(name, keep.source = FALSE, partial = TRUE)
##     }
## }

.onLoad <- function( libname, pkgname ) {
    if( ! "ROI.plugin.nlminb" %in% ROI_registered_solvers() ){
        ## Register solver methods here.
        ## One can assign several signatures a single solver method
        ## DISABLED! see R code (solution of QP from examples.R in work not same as quadprog)
         solver <- "nlminb"
         ROI_plugin_register_solver_method(
             signatures = ROI_make_NLP_FXCV_signatures(),
             solver = solver,
             method = getFunction( ".solve_NLP_nlminb", where = getNamespace(pkgname)) )
         .add_nlminb_controls()
         ## Finally, for status code canonicalization add status codes to data base
         .add_nlminb_status_codes()


        #ROI_plugin_register_solver_method( signatures = ROI_make_QP_signatures(),
        #                            solver = solver,
        #                            method =
        #                            getFunction( ".solve_QP_nlminb", where = getNamespace(pkgname)) )
    }

 
    register_reformulations()

    ## SET DEFAULTS: for the time being 'ROI_NULL' for solving empty
    ## OPs is the default solver
    ROI_options( "default_solver", "auto" )
    ## NOTE: try since numDeriv has to be installed!
    try(ROI_options("gradient", numDeriv::grad), silent=TRUE)
    try(ROI_options("jacobian", numDeriv::jacobian), silent=TRUE)

    ROI_options("solver_selection_table",
        list(default = c("glpk", "ecos", "cplex", "quadprog", "nlminb"),
             LP   = c("glpk", "ecos", "cplex"),
             QP   = c("quadprog", "cplex", "ipop"),
             CP   = c("ecos", "scs"),
             MILP = c("glpk", "ecos", "cplex"),
             MIQP = c("cplex"),
             MICP = c("ecos"),
             NLP  = c("nlminb", "nloptr")))
    return(invisible(NULL))
}

.onAttach <- function( libname, pkgname ) {
    ## Search for all solvers in same library as ROI and register found solvers
    ## implicitely be running the corresponding .onLoad() function.
    load_plugins <- function() {
        if ( !Sys.getenv("_R_ROI_NO_CHECK_SOLVERS_") == "" ) {
            return(FALSE)
        }
        isTRUE(as.logical(Sys.getenv("ROI_LOAD_PLUGINS", TRUE)))
    }

    if ( load_plugins() ) {
        solvers <- ROI_installed_solvers( lib.loc = libname )
    } else {
        solvers <- NULL
    }

    for ( pkg_name in solvers ) {
        nmspc <- tryCatch(getNamespace(pkg_name), error = identity)
        if( !inherits(nmspc, "error") ) {
            tryCatch({
                load <- methods::getFunction( ".onLoad", where = nmspc )
                load( libname = libname, pkgname = pkg_name )
            }, error = function(e) .couldnt_load_pkg(pkg_name))
        }
    }
    ## Startup messages
    packageStartupMessage( sprintf("%s: R Optimization Infrastructure", pkgname) )
    packageStartupMessage( sprintf("Registered solver plugins: %s.",
                                   paste(names(ROI_registered_solvers()), collapse = ", ")) )
    packageStartupMessage( sprintf("Default solver: %s.", ROI_options("default_solver")) )
}

.couldnt_load_pkg <- function(pkg) {
    warning(sprintf("couldn't load plugin '%s'.", pkg), call. = FALSE)
}
