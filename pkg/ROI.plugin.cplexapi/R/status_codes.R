.add_status_codes <- function( solver ) {

    ROI_plugin_add_status_code_to_db(solver,  1L, "CPX_STAT_OPTIMAL", "Optimal solution is available.", 0L)
    ROI_plugin_add_status_code_to_db(solver,  101L, "CPX_STAT_OPTIMAL", "Optimal integer solution has been found.", 0L)
    ROI_plugin_add_status_code_to_db(solver,  102L, "CPXMIP_OPTIMAL_TOL", "Optimal solution with the tolerance defined by epgap or epagap has been found.", 0L)

    ROI_plugin_add_status_code_to_db(solver,  2L, "CPX_STAT_UNBOUNDED", "Problem has an unbounded ray; see the concept Unboundedness for more information about infeasibility and unboundedness as a solution status.", 1L)
    ROI_plugin_add_status_code_to_db(solver,  3L, "CPX_STAT_INFEASIBLE", "Problem has been proven infeasible; see the topic Interpreting Solution Quality in the CPLEX User's Manual for more details.", 1L)
    ROI_plugin_add_status_code_to_db(solver,  4L, "CPX_STAT_INForUNBD", "Problem has been proven either infeasible or unbounded; see the topics Early reports of infeasibility based on preprocessing reductions and Infeasibility or unboundedness in presolve reductions in the CPLEX User's Manual for more detail.", 1L)
    ROI_plugin_add_status_code_to_db(solver,  5L, "CPX_STAT_OPTIMAL_INFEAS", "Optimal solution is available, but with infeasibilities after unscaling.", 1L)
    ROI_plugin_add_status_code_to_db(solver,  6L, "CPX_STAT_NUM_BEST", "Solution is available, but not proved optimal, due to numeric difficulties during optimization.", 1L)
    ROI_plugin_add_status_code_to_db(solver, 10L, "CPX_STAT_ABORT_IT_LIM", "Stopped due to limit on number of iterations.", 1L)
    ROI_plugin_add_status_code_to_db(solver, 11L, "CPX_STAT_ABORT_TIME_LIM", "Stopped due to a time limit.", 1L)
    ROI_plugin_add_status_code_to_db(solver, 12L, "CPX_STAT_ABORT_OBJ_LIM", "Stopped due to an objective limit.", 1L)
    ROI_plugin_add_status_code_to_db(solver, 13L, "CPX_STAT_ABORT_USER", "Stopped due to a request from the user.", 1L)
    ROI_plugin_add_status_code_to_db(solver, 14L, "CPX_STAT_FEASIBLE_RELAXED_SUM", "This status occurs only after a call to the Callable Library routine CPXfeasopt (or the Concert Technology method feasOpt) with the parameter CPX_PARAM_FEASOPTMODE (or FeasOptMode) set to CPX_FEASOPT_MIN_SUM (or MinSum) on a continuous problem. A relaxation was successfully found and a feasible solution for the problem. (if relaxed according to that relaxation) was installed. The relaxation is minimal.", 1L)
    ROI_plugin_add_status_code_to_db(solver, 15L, "CPX_STAT_OPTIMAL_RELAXED_SUM", "This status occurs only after a call to the Callable Library routine CPXfeasopt (or the Concert Technology method feasOpt) with the parameter CPX_PARAM_FEASOPTMODE (or FeasOptMode) set to CPX_FEASOPT_OPT_SUM (or OptSum) on a continuous problem. A relaxation was successfully found and a feasible solution for the problem (if relaxed according to that relaxation) was installed. The relaxation is optimal.", 1L)
    ROI_plugin_add_status_code_to_db(solver, 16L, "CPX_STAT_FEASIBLE_RELAXED_INF", "This status occurs only after a call to the Callable Library routine CPXfeasopt (or the Concert Technology method feasOpt) with the parameter CPX_PARAM_FEASOPTMODE (or FeasOptMode) set to CPX_FEASOPT_MIN_INF (or MinInf) on a continuous problem. A relaxation was successfully found and a feasible solution for the problem (if relaxed according to that relaxation) was installed. The relaxation is minimal.", 1L)
    ROI_plugin_add_status_code_to_db(solver, 17L, "CPX_STAT_OPTIMAL_RELAXED_INF", "This status occurs only after a call to the Callable Library routine CPXfeasopt (or the Concert Technology method feasOpt) with the parameter CPX_PARAM_FEASOPTMODE (or FeasOptMode) set to CPX_FEASOPT_OPT_INF (or OptInf) on a continuous problem. A relaxation was successfully found and a feasible solution for the problem (if relaxed according to that relaxation) was installed. The relaxation is optimal.", 1L)
    ROI_plugin_add_status_code_to_db(solver, 18L, "CPX_STAT_FEASIBLE_RELAXED_QUAD", "This status occurs only after a call to the Callable Library routine CPXfeasopt (or the Concert Technology method feasOpt) with the parameter CPX_PARAM_FEASOPTMODE (or FeasOptMode) set to CPX_FEASOPT_MIN_QUAD (or MinQuad) on a continuous problem. A relaxation was successfully found and a feasible solution for the problem (if relaxed according to that relaxation) was installed. The relaxation is minimal.", 1L)
    ROI_plugin_add_status_code_to_db(solver, 19L, "CPX_STAT_OPTIMAL_RELAXED_QUAD", "This status occurs only after a call to the Callable Library routine CPXfeasopt (or the Concert Technology method feasOpt) with the parameter CPX_PARAM_FEASOPTMODE (or FeasOptMode) set to CPX_FEASOPT_OPT_QUAD (or OptQuad) on a continuous problem. A relaxation was successfully found and a feasible solution for the problem (if relaxed according to that relaxation) was installed. The relaxation is optimal.", 1L)
    ROI_plugin_add_status_code_to_db(solver, 20L, "CPX_STAT_OPTIMAL_FACE_UNBOUNDED", "Model has an unbounded optimal face.", 1L)
    ROI_plugin_add_status_code_to_db(solver, 21L, "CPX_STAT_ABORT_PRIM_OBJ_LIM", "Stopped due to a limit on the primal objective.", 1L)
    ROI_plugin_add_status_code_to_db(solver, 22L, "CPX_STAT_ABORT_DUAL_OBJ_LIM", "Stopped due to a limit on the dual objective.", 1L)
    ROI_plugin_add_status_code_to_db(solver, 23L, "CPX_STAT_FEASIBLE", "This status occurs only after a call to the Callable Library routine CPXfeasopt (or the Concert Technology method feasOpt) on a continuous problem. The problem under consideration was found to be feasible after phase 1 of FeasOpt. A feasible solution is available.", 1L)
    ROI_plugin_add_status_code_to_db(solver, 24L, "CPX_STAT_FIRSTORDER", "Solution satisfies first-order optimality conditions for a solution returned by the barrier optimizer for an indefinite QP when the solution target type parameter specifies a search for a solution that satisfies first-order optimality conditions, but is not necessarily globally optimal (value 2).", 1L)
    ROI_plugin_add_status_code_to_db(solver, 25L, "CPX_STAT_ABORT_DETTIME_LIM", "Stopped due to a deterministic time limit.", 1L)

    ROI_plugin_add_status_code_to_db(solver,  30, "CPX_STAT_CONFLICT_FEASIBLE", "The problem appears to be feasible; no conflict is available.", 1L)
    ROI_plugin_add_status_code_to_db(solver,  31, "CPX_STAT_CONFLICT_MINIMAL", "The conflict refiner found a minimal conflict.", 1L)
    ROI_plugin_add_status_code_to_db(solver,  32, "CPX_STAT_CONFLICT_ABORT_CONTRADICTION", "The conflict refiner concluded contradictory feasibility for the same set of constraints due to numeric problems. A conflict is available, but it is not minimal.", 1L)
    ROI_plugin_add_status_code_to_db(solver,  33, "CPX_STAT_CONFLICT_ABORT_TIME_LIM", "The conflict refiner terminated because of a time limit. A conflict is available, but it is not minimal.", 1L)
    ROI_plugin_add_status_code_to_db(solver,  34, "CPX_STAT_CONFLICT_ABORT_IT_LIM", "The conflict refiner terminated because of an iteration limit. A conflict is available, but it is not minimal.", 1L)
    ROI_plugin_add_status_code_to_db(solver,  35, "CPX_STAT_CONFLICT_ABORT_NODE_LIM", "The conflict refiner terminated because of a node limit. A conflict is available, but it is not minimal.", 1L)
    ROI_plugin_add_status_code_to_db(solver,  36, "CPX_STAT_CONFLICT_ABORT_OBJ_LIM", "The conflict refiner terminated because of an objective limit. A conflict is available, but it is not minimal.", 1L)
    ROI_plugin_add_status_code_to_db(solver,  37, "CPX_STAT_CONFLICT_ABORT_MEM_LIM", "The conflict refiner terminated because of a memory limit. A conflict is available, but it is not minimal.", 1L)
    ROI_plugin_add_status_code_to_db(solver,  38, "CPX_STAT_CONFLICT_ABORT_USER", "The conflict refiner terminated because a user terminated the application. A conflict is available, but it is not minimal.", 1L)
    ROI_plugin_add_status_code_to_db(solver,  39, "CPX_STAT_CONFLICT_ABORT_DETTIME_LIM", "The conflict refiner terminated because of a deterministic time limit. A conflict is available, but it is not minimal.", 1L)
    ROI_plugin_add_status_code_to_db(solver, 103, "CPXMIP_INFEASIBLE", "Solution is integer infeasible.", 1L)
    ROI_plugin_add_status_code_to_db(solver, 104, "CPXMIP_SOL_LIM", "The limit on mixed integer solutions has been reached.", 1L)
    ROI_plugin_add_status_code_to_db(solver, 105, "CPXMIP_NODE_LIM_FEAS", "Node limit has been exceeded but integer solution exists.", 1L)
    ROI_plugin_add_status_code_to_db(solver, 106, "CPXMIP_NODE_LIM_INFEAS", "Node limit has been reached; no integer solution.", 1L)
    ROI_plugin_add_status_code_to_db(solver, 107, "CPXMIP_TIME_LIM_FEAS", "Time limit exceeded, but integer solution exists.", 1L)
    ROI_plugin_add_status_code_to_db(solver, 108, "CPXMIP_TIME_LIM_INFEAS", "Time limit exceeded; no integer solution.", 1L)
    ROI_plugin_add_status_code_to_db(solver, 109, "CPXMIP_FAIL_FEAS", "Terminated because of an error, but integer solution exists.", 1L)
    ROI_plugin_add_status_code_to_db(solver, 110, "CPXMIP_FAIL_INFEAS", "Terminated because of an error; no integer solution.", 1L)
    ROI_plugin_add_status_code_to_db(solver, 111, "CPXMIP_MEM_LIM_FEAS", "Limit on tree memory has been reached, but an integer solution exists.", 1L)
    ROI_plugin_add_status_code_to_db(solver, 112, "CPXMIP_MEM_LIM_INFEAS", "Limit on tree memory has been reached; no integer solution.", 1L)
    ROI_plugin_add_status_code_to_db(solver, 113, "CPXMIP_ABORT_FEAS", "Stopped, but an integer solution exists.", 1L)
    ROI_plugin_add_status_code_to_db(solver, 114, "CPXMIP_ABORT_INFEAS", "Stopped; no integer solution.", 1L)
    ROI_plugin_add_status_code_to_db(solver, 115, "CPXMIP_OPTIMAL_INFEAS", "Problem is optimal with unscaled infeasibilities.", 1L)
    ROI_plugin_add_status_code_to_db(solver, 116, "CPXMIP_FAIL_FEAS_NO_TREE", "Out of memory, no tree available, integer solution exists.", 1L)
    ROI_plugin_add_status_code_to_db(solver, 117, "CPXMIP_FAIL_INFEAS_NO_TREE", "Out of memory, no tree available, no integer solution.", 1L)
    ROI_plugin_add_status_code_to_db(solver, 118, "CPXMIP_UNBOUNDED", "Problem has an unbounded ray.", 1L)
    ROI_plugin_add_status_code_to_db(solver, 119, "CPXMIP_INForUNBD", "Problem has been proved either infeasible or unbounded.", 1L)
    ROI_plugin_add_status_code_to_db(solver, 120, "CPXMIP_FEASIBLE_RELAXED_SUM", "This status occurs only after a call to the Callable Library routine CPXfeasopt (or the Concert Technology method feasOpt) with the parameter CPX_PARAM_FEASOPTMODE (or FeasOptMode) set to CPX_FEASOPT_MIN_SUM (or MinSum) on a mixed integer problem. A relaxation was successfully found and a feasible solution for the problem (if relaxed according to that relaxation) was installed. The relaxation is minimal.", 1L)
    ROI_plugin_add_status_code_to_db(solver, 121, "CPXMIP_OPTIMAL_RELAXED_SUM", "This status occurs only after a call to the Callable Library routine CPXfeasopt (or the Concert Technology method feasOpt) with the parameter CPX_PARAM_FEASOPTMODE (or FeasOptMode) set to CPX_FEASOPT_OPT_SUM (or OptSum) on a mixed integer problem. A relaxation was successfully found and a feasible solution for the problem (if relaxed according to that relaxation) was installed. The relaxation is optimal.", 1L)
    ROI_plugin_add_status_code_to_db(solver, 122, "CPXMIP_FEASIBLE_RELAXED_INF", "This status occurs only after a call to the Callable Library routine CPXfeasopt (or the Concert Technology method feasOpt) with the parameter CPX_PARAM_FEASOPTMODE (or FeasOptMode) set to CPX_FEASOPT_MIN_INF (or MinInf) on a mixed integer problem. A relaxation was successfully found and a feasible solution for the problem (if relaxed according to that relaxation) was installed. The relaxation is minimal.", 1L)
    ROI_plugin_add_status_code_to_db(solver, 123, "CPXMIP_OPTIMAL_RELAXED_INF", "This status occurs only after a call to the Callable Library routine CPXfeasopt (or the Concert Technology method feasOpt) with the parameter CPX_PARAM_FEASOPTMODE (or FeasOptMode) set to CPX_FEASOPT_OPT_INF (or OptInf) on a mixed integer problem. A relaxation was successfully found and a feasible solution for the problem (if relaxed according to that relaxation) was installed. The relaxation is optimal.", 1L)
    ROI_plugin_add_status_code_to_db(solver, 124, "CPXMIP_FEASIBLE_RELAXED_QUAD", "This status occurs only after a call to the Callable Library routine CPXfeasopt (or the Concert Technology method feasOpt) with the parameter CPX_PARAM_FEASOPTMODE (or FeasOptMode) set to CPX_FEASOPT_MIN_QUAD (or MinQuad) on a mixed integer problem. A relaxation was successfully found and a feasible solution for the problem (if relaxed according to that relaxation) was installed. The relaxation is minimal.", 1L)
    ROI_plugin_add_status_code_to_db(solver, 125, "CPXMIP_OPTIMAL_RELAXED_QUAD", "This status occurs only after a call to the Callable Library routine CPXfeasopt (or the Concert Technology method feasOpt) with the parameter CPX_PARAM_FEASOPTMODE (or FeasOptMode) set to CPX_FEASOPT_OPT_QUAD (or OptQuad) on a mixed integer problem. A relaxation was successfully found and a feasible solution for the problem (if relaxed according to that relaxation) was installed. The relaxation is optimal.", 1L)
    ROI_plugin_add_status_code_to_db(solver, 126, "CPXMIP_ABORT_RELAXED", "This status occurs only after a call to the Callable Library routine CPXfeasopt (or the Concert Technology method feasOpt), when the algorithm terminates prematurely, for example after reaching a limit. This status means that a relaxed solution is available and can be queried.", 1L)
    ROI_plugin_add_status_code_to_db(solver, 127, "CPXMIP_FEASIBLE", "This status occurs only after a call to the Callable Library routine CPXfeasopt (or the Concert Technology method feasOpt) on a MIP problem. The problem under consideration was found to be feasible after phase 1 of FeasOpt. A feasible solution is available. This status is also used in the status field of solution and mipstart files for solutions from the solution pool.", 1L)
    ROI_plugin_add_status_code_to_db(solver, 128, "CPXMIP_POPULATESOL_LIM", "This status occurs only after a call to the Callable Library routine CPXpopulate (or the Concert Technology method populate) on a MIP problem. The limit on mixed integer solutions generated by populate, as specified by the parameter CPX_PARAM_POPULATELIM, has been reached.", 1L)
    ROI_plugin_add_status_code_to_db(solver, 129, "CPXMIP_OPTIMAL_POPULATED", "This status occurs only after a call to the Callable Library routine CPXpopulate (or the Concert Technology method populate) on a MIP problem. Populate has completed the enumeration of all solutions it could enumerate.", 1L)
    ROI_plugin_add_status_code_to_db(solver, 130, "CPXMIP_OPTIMAL_POPULATED_TOL", "This status occurs only after a call to the Callable Library routine CPXpopulate (or the Concert Technology method populate) on a MIP problem. Populate has completed the enumeration of all solutions it could enumerate whose objective value fit the tolerance specified by the parameters CPX_PARAM_SOLNPOOLAGAP and CPX_PARAM_SOLNPOOLGAP.", 1L)
    ROI_plugin_add_status_code_to_db(solver, 131, "CPXMIP_DETTIME_LIM_FEAS", "Deterministic time limit exceeded, but integer solution exists.", 1L)
    ROI_plugin_add_status_code_to_db(solver, 132, "CPXMIP_DETTIME_LIM_INFEAS", "Deterministic time limit exceeded; no integer solution.", 1L)
}













































