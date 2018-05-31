
.add_status_codes <- function() {
    solver <- ROI_plugin_get_solver_name( getPackageName() )

    ##' ----------------------
    ##' Termination - Codes
    ##' ----------------------
    ROI_plugin_add_status_code_to_db(solver,  1L, "NORMAL_COMPLETION", "An optimal solution was obtained.", 0L)

    ## Solver Status Codes
    ROI_plugin_add_status_code_to_db(solver,   2L, "ITERATION_INTERRUPT", "", 1L)
    ROI_plugin_add_status_code_to_db(solver,   3L, "RESOURCE_INTERRUPT", "", 1L)
    ROI_plugin_add_status_code_to_db(solver,   4L, "TERMINATED_BY_SOLVER", "", 1L)
    ROI_plugin_add_status_code_to_db(solver,   5L, "EVALUATION_ERROR_LIMIT", "", 1L)
    ROI_plugin_add_status_code_to_db(solver,   6L, "CAPABILITY_PROBLEMS", "", 1L)
    ROI_plugin_add_status_code_to_db(solver,   7L, "LICENSING_PROBLEMS", "", 1L)
    ROI_plugin_add_status_code_to_db(solver,   8L, "USER_INTERRUPT", "", 1L)
    ROI_plugin_add_status_code_to_db(solver,   9L, "ERROR_SETUP_FAILURE", "", 1L)
    ROI_plugin_add_status_code_to_db(solver,  10L, "ERROR_SOLVER_FAILURE", "", 1L)
    ROI_plugin_add_status_code_to_db(solver,  11L, "ERROR_INTERNAL_SOLVER_FAILURE", "", 1L)
    ROI_plugin_add_status_code_to_db(solver,  12L, "SOLVE_PROCESSING_SKIPPED", "", 1L)
    ROI_plugin_add_status_code_to_db(solver,  13L, "ERROR_SYSTEM_FAILURE", "", 1L)

    ## Model Status Codes (I just shift them by + 20)

    ROI_plugin_add_status_code_to_db(solver,  2L + 20L, "LOCALLY_OPTIMAL", "Locally Optimal", 0L)
    ROI_plugin_add_status_code_to_db(solver,  3L + 20L, "UNBOUNDED", "Unbounded", 1L)
    ROI_plugin_add_status_code_to_db(solver,  4L + 20L, "INFEASIBLE", "Infeasible", 1L)
    ROI_plugin_add_status_code_to_db(solver,  5L + 20L, "LOCALLY_INFEASIBLE", "Locally Infeasible", 1L)
    ROI_plugin_add_status_code_to_db(solver,  6L + 20L, "INTERMEDIATE_INFEASIBLE", "Intermediate Infeasible", 1L)
    ROI_plugin_add_status_code_to_db(solver,  7L + 20L, "FEASIBLE_SOLUTION", "Feasible Solution", 0L)
    ROI_plugin_add_status_code_to_db(solver,  8L + 20L, "INTEGER_SOLUTION", "Integer Solution", 1L)
    ROI_plugin_add_status_code_to_db(solver,  9L + 20L, "INTERMEDIATE_NON_INTEGER", "Intermediate Non-Integer", 1L)
    ROI_plugin_add_status_code_to_db(solver, 10L + 20L, "INTEGER_INFEASIBLE", "Integer Infeasible", 1L)
    ROI_plugin_add_status_code_to_db(solver, 11L + 20L, "LICENSING_PROBLEMS", "Licensing Problems - No Solution", 1L)
    ROI_plugin_add_status_code_to_db(solver, 12L + 20L, "ERROR_UNKNOWN", "Error Unknown", 1L)
    ROI_plugin_add_status_code_to_db(solver, 13L + 20L, "ERROR_NO_SOLUTION", "Error No Solution", 1L)
    ROI_plugin_add_status_code_to_db(solver, 14L + 20L, "NO_SOLUTION_RETURNED", "No Solution Returned", 1L)
    ROI_plugin_add_status_code_to_db(solver, 15L + 20L, "SOLVED_UNIQUE", "Solved Unique", 0L)
    ROI_plugin_add_status_code_to_db(solver, 16L + 20L, "SOLVED", "Solved", 0L)
    ROI_plugin_add_status_code_to_db(solver, 17L + 20L, "SOLVED_SINGULAR", "Solved Singular", 0L)
    ROI_plugin_add_status_code_to_db(solver, 18L + 20L, "UNBOUNDED_NO_SOLUTION", "Unbounded - No Solution", 1L)
    ROI_plugin_add_status_code_to_db(solver, 19L + 20L, "INFEASIBLE_NO_SOLUTION", "Infeasible - No Solution", 1L)

    ROI_plugin_add_status_code_to_db(solver,  100L, "UNKNOWN_STATUS_CODE", "The status code is unkown.", 1L)
}

generate_status_code <- function(solver_status, model_status) {
    solver_status <- as.integer(solver_status)
    model_status <- as.integer(model_status)
    if ( model_status == 0L ) return(solver_status)
    if ( solver_status == 0L ) return(model_status)
    solver_status
}

## MODEL STATUS CODE   DESCRIPTION
## 1   Optimal
## 2   Locally Optimal
## 3   Unbounded
## 4   Infeasible
## 5   Locally Infeasible
## 6   Intermediate Infeasible
## 7   Intermediate Nonoptimal
## 8   Integer Solution
## 9   Intermediate Non-Integer
## 10  Integer Infeasible
## 11  Licensing Problems - No Solution
## 12  Error Unknown
## 13  Error No Solution
## 14  No Solution Returned
## 15  Solved Unique
## 16  Solved
## 17  Solved Singular
## 18  Unbounded - No Solution
## 19  Infeasible - No Solution
## SOLVER STATUS CODE  DESCRIPTION
## 1   Normal Completion
## 2   Iteration Interrupt
## 3   Resource Interrupt
## 4   Terminated by Solver
## 5   Evaluation Error Limit
## 6   Capability Problems
## 7   Licensing Problems
## 8   User Interrupt
## 9   Error Setup Failure
## 10  Error Solver Failure
## 11  Error Internal Solver Error
## 12  Solve Processing Skipped
## 13  Error System Failure