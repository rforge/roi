## ROI.plugin.cplex: status codes
## Description: provides canonicalization of solver status codes

## STATUS CODES
.add_status_codes <- function( ) {

    ## CPLEX status codes
    ## from CPLEX 12.6 callable library manual
    ## Source: https://www.ibm.com/support/knowledgecenter/SSSA5P_12.6.0/ilog.odms.cplex.help/refcallablelibrary/macros/Solution_status_codes.html

    solver <- ROI_plugin_get_solver_name( getPackageName() )

    ## generic status codes
    ROI_plugin_add_status_code_to_db(solver,
                                1L,
                                "CPX_STAT_OPTIMAL",
                                "(Simplex or barrier) Optimal solution.",
                                0L
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                2L,
                                "CPX_STAT_UNBOUNDED",
                                "(Simplex or Barrier) Problem is unbounded."
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                3L,
                                "CPX_STAT_INFEASIBLE",
                                "(Simplex or Barrier) Problem has been proven infeasible."
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                4L,
                                "CPX_STAT_INForUNBD",
                                "(Simplex or Barrier) Problem has been proven either infeasible or unbounded."
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                5L,
                                "CPX_STAT_OPTIMAL_INFEAS",
                                "(Simplex or Barrier) Optimal solution is available, but with infeasibilities after unscaling."
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                6L,
                                "CPX_STAT_NUM_BEST",
                                "(Simplex or Barrier) Solution is available, but not proved optimal, due to numeric difficulties during optimization."
                                )

    ROI_plugin_add_status_code_to_db(solver,
                                10L,
                                "CPX_STAT_ABORT_IT_LIM",
                                "(Simplex or Barrier) Stopped due to limit on number of iterations."
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                11L,
                                "CPX_STAT_ABORT_TIME_LIM",
                                "(Simplex or Barrier) Stopped due to a time limit."
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                13L,
                                "CPX_STAT_ABORT_USER",
                                "(Simplex or Barrier) Stopped due to a request from the user."
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                14L,
                                "CPX_STAT_FEASIBLE_RELAXED_SUM",
                                "(Simplex or Barrier, only via CPXfeasopt()) A relaxation was successfully found and a feasible solution for the problem. (if relaxed according to that relaxation) was installed. The relaxation is minimal."
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                15L,
                                "CPX_STAT_OPTIMAL_RELAXED_SUM",
                                "(Simplex or Barrier, only via CPXfeasopt()) A relaxation was successfully found and a feasible solution for the problem (if relaxed according to that relaxation) was installed. The relaxation is optimal."
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                16L,
                                "CPX_STAT_FEASIBLE_RELAXED_INF",
                                "(Simplex or Barrier, only via CPXfeasopt()) A relaxation was successfully found and a feasible solution for the problem (if relaxed according to that relaxation) was installed. The relaxation is minimal."
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                17L,
                                "CPX_STAT_OPTIMAL_RELAXED_INF",
                                "(Simplex or Barrier, only via CPXfeasopt()) A relaxation was successfully found and a feasible solution for the problem (if relaxed according to that relaxation) was installed. The relaxation is optimal."
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                18L,
                                "CPX_STAT_FEASIBLE_RELAXED_QUAD",
                                "(Simplex or Barrier, only via CPXfeasopt()) A relaxation was successfully found and a feasible solution for the problem (if relaxed according to that relaxation) was installed. The relaxation is minimal."
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                19L,
                                "CPX_STAT_OPTIMAL_RELAXED_QUAD",
                                "(Simplex or Barrier, only via CPXfeasopt()) A relaxation was successfully found and a feasible solution for the problem (if relaxed according to that relaxation) was installed. The relaxation is optimal."
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                20L,
                                "CPX_STAT_OPTIMAL_FACE_UNBOUNDED",
                                "(Barrier, only) Model has an unbounded optimal face."
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                21L,
                                "CPX_STAT_ABORT_PRIM_OBJ_LIM",
                                "(Barrier, only) Stopped due to a limit on the primal objective."
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                22L,
                                "CPX_STAT_ABORT_DUAL_OBJ_LIM",
                                "(Barrier, only) Stopped due to a limit on the dual objective."
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                23L,
                                "CPX_STAT_FEASIBLE",
                                "(Simplex or Barrier, only via CPXfeasopt()) The problem under consideration was found to be feasible after phase 1 of FeasOpt. A feasible solution is available."
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                24L,
                                "CPX_STAT_FIRSTORDER",
                                "(Barrier, only) Solution satisfies first-order optimality conditions for a solution returned by the barrier optimizer for an indefinite QP when the solution target type parameter specifies a search for a solution that satisfies first-order optimality conditions, but is not necessarily globally optimal (value 2)."
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                25L,
                                "CPX_STAT_ABORT_DETTIME_LIM",
                                "(Simplex or Barrier) Stopped due to a deterministic time limit."
                                )

    ## conflict refiner status codes
    ROI_plugin_add_status_code_to_db(solver,
                                30L,
                                "CPX_STAT_CONFLICT_FEASIBLE",
                                "(conflict refiner) The problem appears to be feasible; no conflict is available."
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                31L,
                                "CPX_STAT_CONFLICT_MINIMAL",
                                "(conflict refiner) The conflict refiner found a minimal conflict."
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                32L,
                                "CPX_STAT_CONFLICT_ABORT_CONTRADICTION",
                                "(conflict refiner) The conflict refiner concluded contradictory feasibility for the same set of constraints due to numeric problems. A conflict is available, but it is not minimal."
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                33L,
                                "CPX_STAT_CONFLICT_ABORT_TIME_LIM",
                                "(conflict refiner) The conflict refiner terminated because of a time limit. A conflict is available, but it is not minimal."
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                34L,
                                "CPX_STAT_CONFLICT_ABORT_IT_LIM",
                                "(conflict refiner) The conflict refiner terminated because of an iteration limit. A conflict is available, but it is not minimal."
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                35L,
                                "CPX_STAT_CONFLICT_ABORT_NODE_LIM",
                                "(conflict refiner) The conflict refiner terminated because of a node limit. A conflict is available, but it is not minimal."
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                36L,
                                "CPX_STAT_CONFLICT_ABORT_OBJ_LIM",
                                "(conflict refiner) The conflict refiner terminated because of an objective limit. A conflict is available, but it is not minimal."
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                37L,
                                "CPX_STAT_CONFLICT_ABORT_MEM_LIM",
                                "(conflict refiner) The conflict refiner terminated because of a memory limit. A conflict is available, but it is not minimal."
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                38L,
                                "CPX_STAT_CONFLICT_ABORT_USER",
                                "(conflict refiner) The conflict refiner terminated because a user terminated the application. A conflict is available, but it is not minimal."
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                39L,
                                "CPX_STAT_CONFLICT_ABORT_DETTIME_LIM",
                                "(conflict refiner) The conflict refiner terminated because of a deterministic time limit. A conflict is available, but it is not minimal."
                                )


    ## MIP status codes
    ROI_plugin_add_status_code_to_db(solver,
                                101L,
                                "CPXMIP_OPTIMAL",
                                "(MIP only) optimal integer solution.",
                                0L
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                102L,
                                "CPXMIP_OPTIMAL_TOL",
                                "(MIP only) optimal soluton with the tolerance defined by epgap or epagap has been found.",
                                0L
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                103L,
                                "CPXMIP_INFEASIBLE",
                                "(MIP only) Solution is integer infeasible."
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                104L,
                                "CPXMIP_SOL_LIM",
                                "(MIP only) The limit on mixed integer solutions has been reached."
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                105L,
                                "CPXMIP_NODE_LIM_FEAS",
                                "(MIP only) Node limit has been exceeded but integer solution exists."
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                106L,
                                "CPXMIP_NODE_LIM_INFEAS",
                                "(MIP only) Node limit has been reached; no integer solution."
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                107L,
                                "CPXMIP_TIME_LIM_FEAS",
                                "(MIP only) Time limit exceeded, but integer solution exists."
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                108L,
                                "CPXMIP_TIME_LIM_INFEAS",
                                "(MIP only) Time limit exceeded, no integer solution exists."
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                109L,
                                "CPXMIP_FAIL_FEAS",
                                "(MIP only) Terminated because of an error, but integer solution exists."
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                110L,
                                "CPXMIP_FAIL_INFEAS",
                                "(MIP only) Terminated because of an error; no integer solution."
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                111L,
                                "CPXMIP_MEM_LIM_FEAS",
                                "(MIP only) Limit on tree memory has been reached, but an integer solution exists."
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                112L,
                                "CPXMIP_MEM_LIM_INFEAS",
                                "(MIP only) Limit on tree memory has been reached; no integer solution."
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                113L,
                                "CPXMIP_ABORT_FEAS",
                                "(MIP only) Stopped, but an integer solution exists."
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                114L,
                                "CPXMIP_ABORT_INFEAS",
                                "(MIP only) Stopped, no integer solution found."
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                115L,
                                "CPXMIP_OPTIMAL_INFEAS",
                                "(MIP only) Problem is optimal with unscaled infeasibilities."
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                116L,
                                "CPXMIP_FAIL_FEAS_NO_TREE",
                                "(MIP only) Out of memory, no tree available, integer solution exists."
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                117L,
                                "CPXMIP_FAIL_INFEAS_NO_TREE",
                                "(MIP only) Out of memory, no tree available, no integer solution."
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                118L,
                                "CPXMIP_UNBOUNDED",
                                "(MIP only) Problem is unbounded."
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                119L,
                                "CPXMIP_INForUNBD",
                                "(MIP only) Problem has been proved either infeasible or unbounded."
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                120L,
                                "CPXMIP_FEASIBLE_RELAXED_SUM",
                                "(MIP, only via CPXfeasopt()) A relaxation was successfully found and a feasible solution for the problem (if relaxed according to that relaxation) was installed. The relaxation is minimal."
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                121L,
                                "CPXMIP_OPTIMAL_RELAXED_SUM",
                                "(MIP, only via CPXfeasopt()) A relaxation was successfully found and a feasible solution for the problem (if relaxed according to that relaxation) was installed. The relaxation is optimal."
                                )
        ROI_plugin_add_status_code_to_db(solver,
                                122L,
                                "CPXMIP_FEASIBLE_RELAXED_INF",
                                "(MIP, only via CPXfeasopt()) A relaxation was successfully found and a feasible solution for the problem (if relaxed according to that relaxation) was installed. The relaxation is minimal."
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                123L,
                                "CPXMIP_OPTIMAL_RELAXED_INF",
                                "(MIP, only via CPXfeasopt()) A relaxation was successfully found and a feasible solution for the problem (if relaxed according to that relaxation) was installed. The relaxation is optimal."
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                124L,
                                "CPXMIP_FEASIBLE_RELAXED_QUAD",
                                "(MIP, only via CPXfeasopt()) A relaxation was successfully found and a feasible solution for the problem (if relaxed according to that relaxation) was installed. The relaxation is minimal."
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                125L,
                                "CPXMIP_OPTIMAL_RELAXED_QUAD",
                                "(MIP, only via CPXfeasopt()) A relaxation was successfully found and a feasible solution for the problem (if relaxed according to that relaxation) was installed. The relaxation is optimal."
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                126L,
                                "CPXMIP_ABORT_RELAXED",
                                "(MIP, only via CPXfeasopt()) This status means that a relaxed solution is available and can be queried."
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                127L,
                                "CPXMIP_FEASIBLE",
                                "(MIP, only via CPXfeasopt()) The problem under consideration was found to be feasible after phase 1 of FeasOpt. A feasible solution is available. This status is also used in the status field of solution and mipstart files for solutions from the solution pool."
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                128L,
                                "CPXMIP_POPULATESOL_LIM",
                                "(MIP, only via CPXpopulate()) The limit on optimal mixed integer solutions generated by populate has been reached.",
                                0L
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                129L,
                                "CPXMIP_OPTIMAL_POPULATED",
                                "(MIP, only via CPXpopulate()) Populate has completed the enumeration of all solutions it could enumerate.",
                                0L
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                130L,
                                "CPXMIP_OPTIMAL_POPULATED_TOL",
                                "(MIP, only via CPXpopulate()) Populate has completed the enumeration of all solutions it could enumerate whose objective value fit the tolerance specified by the parameters.",
                                0L
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                131L,
                                "CPXMIP_DETTIME_LIM_FEAS",
                                "(MIP only) Deterministic time limit exceeded, but integer solution exists."
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                132L,
                                "CPXMIP_DETTIME_LIM_INFEAS",
                                "(MIP only) Deterministic time limit exceeded; no integer solution."
                                )
    ROI_plugin_add_status_code_to_db(solver,
                                133L,
                                "CPXMIP_ABORT_RELAXATION_UNBOUNDED",
                                "(MIP only) Could not bound convex relaxation of nonconvex (MI)QP."
                                )

    invisible(TRUE)
}

