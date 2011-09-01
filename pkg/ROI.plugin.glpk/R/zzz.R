.onLoad <- function( libname, pkgname ) {
    ## PLUGINS:
    ROI:::ROI_register_solver_method( solver = sub("ROI.plugin.", "", pkgname),
                                      signatures = ROI:::ROI_make_LP_signatures(),
                                      method = ROI.plugin.glpk:::.solve_LP.glpk )
}

#.onUnload <- function( libpath ){
#    ROI:::ROI_deregister_solver_methods( solver = "glpk" )
#}
