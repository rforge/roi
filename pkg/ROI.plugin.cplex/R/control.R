## SOVLER CONTROLS
.add_controls <- function(){
    solver <- ROI_plugin_get_solver_name( getPackageName() )
    ## ROI CONTROL PENDANT AVAILABLE
    ROI_plugin_register_solver_control( solver,
                                         "trace",
                                         "verbose" )
    ROI_plugin_register_solver_control( solver,
                                         "preind",
                                         "presolve" )
    ROI_plugin_register_solver_control( solver,
                                         "itlim",
                                         "max_iter" )
    ROI_plugin_register_solver_control( solver,
                                         "tilim",
                                         "max_time" )
    ROI_plugin_register_solver_control( solver,
                                         "method",
                                         "method" )

    ## NOT AVAILABLE
    ROI_plugin_register_solver_control( solver,
                                         "maxcalls",
                                         "X" )
    ROI_plugin_register_solver_control( solver,
                                         "aggind",
                                         "X" )
    ROI_plugin_register_solver_control( solver,
                                         "epagap",
                                         "X" )
    ROI_plugin_register_solver_control( solver,
                                         "epgap",
                                         "X" )
    ROI_plugin_register_solver_control( solver,
                                         "disjcuts",
                                         "X" )
    ROI_plugin_register_solver_control( solver,
                                         "mipemphasis",
                                         "X" )
    ROI_plugin_register_solver_control( solver,
                                         "cliques",
                                         "X" )
    ROI_plugin_register_solver_control( solver,
                                         "nodesel",
                                         "X" )
    ROI_plugin_register_solver_control( solver,
                                         "probe",
                                         "X" )
    ROI_plugin_register_solver_control( solver,
                                         "varsel",
                                         "X" )
    ROI_plugin_register_solver_control( solver,
                                         "flowcovers",
                                         "X" )
    ROI_plugin_register_solver_control( solver,
                                         "solnpoolagap",
                                         "X" )
    ROI_plugin_register_solver_control( solver,
                                         "solnpoolgap",
                                         "X" )
    ROI_plugin_register_solver_control( solver,
                                         "solnpoolintensity",
                                         "X" )
    ROI_plugin_register_solver_control( solver,
                                         "round",
                                         "X" )
    invisible( TRUE )
}
