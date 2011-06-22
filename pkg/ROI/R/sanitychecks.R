## sanitychecks.R

###############################################################
## sanity check definitions

available_row_sense <- function( )
  c('<', '<=', "==", ">", ">=")

row_sense_is_feasible <- function( x )
  all( x %in% available_row_sense() )
