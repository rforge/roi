# Copyright (C) 2011 Jelmer Ypma. All Rights Reserved.
# This code is published under the L-GPL.
#
# File:   nloptr.print.options.R
# Author: Jelmer Ypma
# Date:   7 August 2011
#
# Add default options to a user supplied list of options.
#
# Input:     
#    opts.user:                list with user defined options
#    x0:                       initial value for control variables
#    num_constraints_ineq:     number of inequality constraints
#    num_constraints_eq:       number of equality constraints
#
# Output:     
#    opts.user with default options added for those options
#              that were not part of the original opts.user.

nloptr.add.default.options <- 
function( 
    opts.user, 
    x0=0, 
    num_constraints_ineq=0, 
    num_constraints_eq=0 ) 
{
    nloptr.default.options <- nloptr.get.default.options()
    
    # get names of options that define a termination condition
    termination.opts <- 
        nloptr.default.options[ nloptr.default.options$is_termination_condition==TRUE, "name" ]
        
    if ( sum(termination.opts %in% names( opts.user )) == 0 ) {
        # get default xtol_rel
        xtol_rel_default <- 
            as.numeric( nloptr.default.options[ nloptr.default.options$name=="xtol_rel", "default" ] )
        warning( paste("No termination criterium specified, using default (relative x-tolerance = ", xtol_rel_default, ")", sep='') )
        termination_conditions <- paste("relative x-tolerance = ", xtol_rel_default, " (DEFAULT)", sep='')
    } else {
        conv_options <- unlist(opts.user[names(opts.user) %in% termination.opts])
        termination_conditions <- paste( paste( names(conv_options) ), ": ", paste( conv_options ), sep='', collapse='\t' )
    }

    # determine list with names of options that contain character values.
    # we need to add quotes around these options below.
    nloptr.list.character.options <- 
        nloptr.default.options[ nloptr.default.options$type=="character", "name" ]
            
    opts.user <- sapply( 1:nrow(nloptr.default.options), 
        function(i) { 
            tmp.opts.name <- nloptr.default.options[i,"name"]
            
            # get user defined value it it's defined
            # otherwise use default value
            tmp.value <- ifelse( 
                            is.null(opts.user[[tmp.opts.name]]), 
                            nloptr.default.options[i,"default"], 
                            opts.user[[tmp.opts.name]] 
                        )
    
            # paste options together in named list
            eval( 
                parse( 
                    text=paste( 
                        "list('", 
                        tmp.opts.name, 
                        "'=", 
                        # add quotes around value if it's a character option
                        ifelse( 
                            tmp.opts.name %in% nloptr.list.character.options,
                            paste("'", tmp.value, "'", sep=''), 
                            tmp.value
                        ), 
                        ")", 
                        sep="" 
                    ) 
                ) 
            ) 
        } 
    )
    
    return( 
        list( "opts.user" = opts.user,
              "termination_conditions" = termination_conditions 
        )
    )
}

nloptr.add.default.options_1 <- 
function( 
    opts.user, 
    x0=0, 
    num_constraints_ineq=0, 
    num_constraints_eq=0 ) 
{
    nloptr.default.options <- nloptr.get.default.options()
    rownames(nloptr.default.options) <- nloptr.default.options[,"name"]
    
    # get names of options that define a termination condition
    termination.opts <- 
        nloptr.default.options[ nloptr.default.options$is_termination_condition==TRUE, "name" ]
        
    if ( sum(termination.opts %in% names( opts.user )) == 0 ) {
        # get default xtol_rel
        xtol_rel_default <- 
            as.numeric( nloptr.default.options[ nloptr.default.options$name=="xtol_rel", "default" ] )
        warning( paste("No termination criterium specified, using default (relative x-tolerance = ", xtol_rel_default, ")", sep='') )
        termination_conditions <- paste("relative x-tolerance = ", xtol_rel_default, " (DEFAULT)", sep='')
    } else {
        conv_options <- unlist(opts.user[names(opts.user) %in% termination.opts])
        termination_conditions <- paste( paste( names(conv_options) ), ": ", paste( conv_options ), sep='', collapse='\t' )
    }

    # determine list with names of options that contain character values.
    # we need to add quotes around these options below.
    nloptr.list.character.options <- 
        nloptr.default.options[ nloptr.default.options$type=="character", "name" ]

    opts <- vector(mode="list", nrow(nloptr.default.options))
    names(opts) <- nloptr.default.options[,"name"]
    is_char_opt <- names(opts) %in% nloptr.list.character.options
    i <- which(!is_char_opt)
    opts[i] <- lapply(nloptr.default.options[i, "default"], function(x) eval(parse(text=x)))
    i <- which(is_char_opt)
    opts[i] <- nloptr.default.options[i, "default"]
    i <- which(names(opts.user) %in% names(opts))
    opts[names(opts.user)] <- opts.user
    
    return( 
        list( "opts.user" = opts,
              "termination_conditions" = termination_conditions 
        )
    )
}

nloptr.add.default.options_2 <- 
function( 
    opts.user, 
    x0=0, 
    num_constraints_ineq=0, 
    num_constraints_eq=0 ) 
{
    nloptr.default.options <- nloptr.get.default.options()
    rownames(nloptr.default.options) <- nloptr.default.options[,"name"]
    
    # get names of options that define a termination condition
    termination.opts <- 
        nloptr.default.options[ nloptr.default.options$is_termination_condition==TRUE, "name" ]
        
    if ( sum(termination.opts %in% names( opts.user )) == 0 ) {
        # get default xtol_rel
        xtol_rel_default <- 
            as.numeric( nloptr.default.options[ nloptr.default.options$name=="xtol_rel", "default" ] )
        warning( paste("No termination criterium specified, using default (relative x-tolerance = ", xtol_rel_default, ")", sep='') )
        termination_conditions <- paste("relative x-tolerance = ", xtol_rel_default, " (DEFAULT)", sep='')
    } else {
        conv_options <- unlist(opts.user[names(opts.user) %in% termination.opts])
        termination_conditions <- paste( paste( names(conv_options) ), ": ", paste( conv_options ), sep='', collapse='\t' )
    }

    # determine list with names of options that contain character values.
    # we need to add quotes around these options below.
    nloptr.list.character.options <- 
        nloptr.default.options[ nloptr.default.options$type=="character", "name" ]

    opts <- vector(mode="list", nrow(nloptr.default.options))
    names(opts) <- nloptr.default.options[,"name"]

    for ( name in names(opts) ) {
        if ( !is.null(opts.user[[name]]) ) {
            opts[[name]] <- opts.user[[name]]
        } else if ( name %in% nloptr.list.character.options ) {
            opts[[name]] <- nloptr.default.options[name, "default"]
        } else {
            opts[[name]] <- eval(parse(text=nloptr.default.options[name, "default"]))
        }
    }

    return( 
        list( "opts.user" = opts,
              "termination_conditions" = termination_conditions 
        )
    )
}

nloptr.add.default.options_3 <- 
function( 
    opts.user, 
    x0=0, 
    num_constraints_ineq=0, 
    num_constraints_eq=0 ) 
{
    nloptr.default.options <- nloptr.get.default.options()
    rownames(nloptr.default.options) <- nloptr.default.options[,"name"]
    
    # get names of options that define a termination condition
    termination.opts <- 
        nloptr.default.options[ nloptr.default.options$is_termination_condition==TRUE, "name" ]
        
    if ( sum(termination.opts %in% names( opts.user )) == 0 ) {
        # get default xtol_rel
        xtol_rel_default <- 
            as.numeric( nloptr.default.options[ nloptr.default.options$name=="xtol_rel", "default" ] )
        warning( paste("No termination criterium specified, using default (relative x-tolerance = ", xtol_rel_default, ")", sep='') )
        termination_conditions <- paste("relative x-tolerance = ", xtol_rel_default, " (DEFAULT)", sep='')
    } else {
        conv_options <- unlist(opts.user[names(opts.user) %in% termination.opts])
        termination_conditions <- paste( paste( names(conv_options) ), ": ", paste( conv_options ), sep='', collapse='\t' )
    }

    # determine list with names of options that contain character values.
    # we need to add quotes around these options below.
    nloptr.list.character.options <- 
        nloptr.default.options[ nloptr.default.options$type=="character", "name" ]

    opts <- lapply( nloptr.default.options[,"name"], 
        function(name) {            
            # get user defined value it it's defined
            # otherwise use default value
            if ( !is.null(opts.user[[name]]) )
                return( opts.user[[name]] )
            if ( name %in% nloptr.list.character.options ) 
                return( nloptr.default.options[name, "default"] )
            return( eval(parse(text=nloptr.default.options[name, "default"])) )
        }
    )
    names(opts) <- nloptr.default.options[,"name"]
    
    return( 
        list( "opts.user" = opts,
              "termination_conditions" = termination_conditions 
        )
    )
}