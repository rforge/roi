##########################################################################
## MAIN FUNCTION TO SOLVE OPTIMIZATION PROBLEMS USING ROI


##' @export
ROI_solve <- function( x, solver, control = NULL, ... ){

    dots <- list(...)
    control[names(dots)] <- dots

    op <- as.OP( x )

    ## handle the boundary case of no variables.
    ## FIXME: should also consider other
    #if( !length(terms(objective(x))$L) ) {
    #    y <- .solve_empty_OP(x)
    #    return(y)
    #}

    if(!length(objective(x)))
        stop("Cannot compute solution of empty objective function.")

    SOLVE <- get_solver_method( solver, OP_signature(op) )
    SOLVE( x, control )
}

ROI_solver_plugins <- function(){
  ## solvers registered
  registered_solvers <- get_solver_packages_from_db()

  ## solver packages installed
  pkgs_installed <- rownames( utils::installed.packages() )

  if( !is.null(pkgs_installed) )
    names(registered_solvers[registered_solvers %in% pkgs_installed])
  else
    NA
}

## returns solver method
get_solver_method <- function( solver, signature ){
    solver_db$get_entry( solver = solver )[[ signature ]]
}

## returns available solvers from db
get_solvers_from_db <- function( ) {
  solver_db$get_entry_names()
}

## returns package names of available solvers from db
get_solver_packages_from_db <- function ( ){
  solver_db$get_field_entries( "package" )
}


##########################################################################
## NEW SOLVER METHODS

ROI_register_solver_method <- function( signatures, solver, method, package ){
    args <- lapply( signatures, function(x) method )
    names(args) <- signatures
    args <- c( list(solver = solver, package = package), args )
    do.call(solver_db$set_entry, args)
    invisible( TRUE )
}

##########################################################################
## SIGNATURES

## make only one signature
ROI_make_signature <- function(...){
    dotargs <- list(...)
    dotargs
    required <- names(formals(OP))
    if( !is.null(names(dotargs)) )
        stopifnot( all(names(dotargs) %in% required) )
    if( length(dotargs) < 2 )
        stop( sprintf("Signature for '%s' and '%s' need to be given.", required[1], required[2]) )
    length(dotargs) <- length(formals(OP))
    names(dotargs) <- names(formals(OP))
    ## types need special treatment
    if(is.null(dotargs$types))
        dotargs$types <- available_types()[1]
    types <- as.list( logical(length(available_types())) )
    names(types) <- available_types()

    ## FIXME: handle NULL case
    .sort_types(unique(dotargs$types))
    out <- dotargs[ names(dotargs)[-which(names(dotargs) == "types")] ]
    dotargs <- types_to_logical( dotargs )
    dotargs <- lapply(dotargs, function(x) ifelse(is.null(x), FALSE, x))
    dotargs
}


    #.make_signature(dotargs)

## make a set of signatures based on problem class
ROI_make_LP_signatures <- function(){
    sigs <- expand.grid( objective = "L",
                         constraints = "L",
                         types = c("C"),
                         bounds = c("TRUE", "FALSE"),
                         maximum = c("TRUE", "FALSE") )
    stopifnot( ncol(sigs) == length(formals(OP)) )
    stopifnot( identical(colnames(sigs), names(formals(OP))) )
    apply( sigs, 1, .make_signature )
}

ROI_make_QP_signatures <- function(){
    sigs <- expand.grid( objective = "Q",
                         constraints = "L",
                         types = c("C"),
                         bounds = c("TRUE", "FALSE"),
                         maximum = c("TRUE", "FALSE") )
    stopifnot( ncol(sigs) == length(formals(OP)) )
    stopifnot( identical(colnames(sigs), names(formals(OP))) )
    apply( sigs, 1, .make_signature )
}

ROI_make_MILP_signatures <- function(){
    sigs <- expand.grid( objective = "L",
                         constraints = "L",
                         types = c("C", "I", "B", "CI", "CB", "IB", "CIB"),
                         bounds = c("TRUE", "FALSE"),
                         maximum = c("TRUE", "FALSE") )
    stopifnot( ncol(sigs) == length(formals(OP)) )
    stopifnot( identical(colnames(sigs), names(formals(OP))) )
    apply( sigs, 1, .make_signature )
}


ROI_make_MIQP_signatures <- function(){
    sigs <- expand.grid( objective = "Q",
                         constraints = "L",
                         types = c("C", "I", "B", "CI", "CB", "IB", "CIB"),
                         bounds = c("TRUE", "FALSE"),
                         maximum = c("TRUE", "FALSE") )
    stopifnot( ncol(sigs) == length(formals(OP)) )
    stopifnot( identical(colnames(sigs), names(formals(OP))) )
    apply( sigs, 1, .make_signature )
}

.make_signature <- function(x){
    x <- lapply(x, function(e){ if( as.character(e) %in% as.character(c(TRUE, FALSE))){
        if( e )
            "T"
        else
            ""
    } else e})
    out <- paste(x, collapse = "_")
    structure( out, class = c("ROI_signature", class(out)) )
}

.sort_types <- function(x){
    stopifnot( all(x %in% available_types()) )
    ord <- c(C = 1, I = 2, B = 3)
    ordered <- order(ord[x])
    x[ordered]
}
