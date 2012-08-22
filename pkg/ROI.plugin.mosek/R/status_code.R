## ROI.plugin.cplex: status codes
## Description: provides canonicalization of solver status codes

## STATUS CODES
.add_cplex_status_codes <- function( ) {

    ## MOSEK status codes
    ## from  C API manual: http://docs.mosek.com/6.0/capi/index.html
    ## FIXME: only a (relevant) subset from MOSEK status codes

    solver <- ROI:::get_solver_name( getPackageName() )

    ROI:::add_status_code_to_db(solver,
                                NA,
                                "SK_SOL_STA_DUAL_FEAS	",
                                "The solution is dual feasible."
                                )
    ROI:::add_status_code_to_db(solver,
                                NA,
                                "MSK_SOL_STA_DUAL_INFEAS_CER",
                                "The solution is a certificate of dual infeasibility."
                                )
    ROI:::add_status_code_to_db(solver,
                                NA,
                                "MSK_SOL_STA_INTEGER_OPTIMAL",
                                "The primal solution is integer optimal.",
                                0L
                                )
    ROI:::add_status_code_to_db(solver,
                                NA,
                                "MSK_SOL_STA_NEAR_DUAL_FEAS",
                                "The solution is nearly dual feasible.",
                                )
    ROI:::add_status_code_to_db(solver,
                                NA,
                                "MSK_SOL_STA_NEAR_DUAL_INFEAS_CER",
                                "The solution is almost a certificate of dual infeasibility."
                                )
    ROI:::add_status_code_to_db(solver,
                                NA,
                                "MSK_SOL_STA_NEAR_INTEGER_OPTIMAL",
                                "The primal solution is near integer optimal.",
                                0L
                                )
    ROI:::add_status_code_to_db(solver,
                                NA,
                                "MSK_SOL_STA_NEAR_OPTIMAL",
                                "The solution is nearly optimal.",
                                0L
                                )
    ROI:::add_status_code_to_db(solver,
                                NA,
                                "MSK_SOL_STA_NEAR_PRIM_AND_DUAL_FEAS",
                                "The solution is nearly both primal and dual feasible.",
                                )
    ROI:::add_status_code_to_db(solver,
                                NA,
                                "MSK_SOL_STA_NEAR_PRIM_FEAS",
                                "The solution is nearly primal feasible."
                                )
    ROI:::add_status_code_to_db(solver,
                                NA,
                                "MSK_SOL_STA_NEAR_PRIM_INFEAS_CER",
                                "The solution is almost a certificate of primal infeasibility."
                                )
    ROI:::add_status_code_to_db(solver,
                                NA,
                                "MSK_SOL_STA_OPTIMAL",
                                "The solution is optimal.",
                                0L
                                )
    ROI:::add_status_code_to_db(solver,
                                NA,
                                "MSK_SOL_STA_PRIM_AND_DUAL_FEAS",
                                "The solution is both primal and dual feasible.",
                                )
    ROI:::add_status_code_to_db(solver,
                                NA,
                                "MSK_SOL_STA_PRIM_FEAS",
                                "The solution is primal feasible."
                                )
    ROI:::add_status_code_to_db(solver,
                                NA,
                                "MSK_SOL_STA_PRIM_INFEAS_CER",
                                "The solution is a certificate of primal infeasibility."
                                )
    ROI:::add_status_code_to_db(solver,
                                NA,
                                "MSK_SOL_STA_UNKNOWN",
                                "Status of the solution is unknown."
                                )

    invisible(TRUE)
}

