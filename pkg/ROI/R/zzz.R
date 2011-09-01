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

## STATUS_DB
## create registry object containing status codes
status_db <- registry( )
status_db$set_field("solver",   type = "character", is_key = TRUE)
status_db$set_field("code",     type = "integer",   is_key = TRUE)
status_db$set_field("symbol",   type = "character")
status_db$set_field("message",  type = "character")
status_db$set_field("roi_code", type = "integer",   alternatives = 0:1)

## SOLVER_DB
solver_db <- registry( )
solver_db$set_field( "solver",      type = "character", is_key = TRUE )
solver_db$set_field( "objective",   type = names(available_objective_classes()), is_key = TRUE)
solver_db$set_field( "constraints", type = names(available_constraint_classes()), is_key = TRUE)
for( type in available_types() )
    solver_db$set_field( type,      type = "logical", is_key = TRUE)
solver_db$set_field( "bounds",      type = logical, is_key = TRUE)
solver_db$set_field( "maximum",     type = logical, is_key = TRUE)
solver_db$set_field( "FUN",         type = function)

stopifnot( all(names(formals(OP)) %in% c(solver_db$get_field_names(), "types")) )


#solver_db$set_field( "package", type = "character" )
##for(field in apply(ROI:::all_signatures(), 1, ROI:::.make_signature))
##    solver_db$set_field( field, type = "function" )
