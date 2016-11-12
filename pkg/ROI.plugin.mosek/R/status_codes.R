
.add_status_codes <- function() {
    solver <- .ROI_plugin_get_solver_name( getPackageName() )

    ##' ----------------------
    ##' Termination - Codes
    ##' ----------------------
    .ROI_plugin_add_status_code_to_db( solver,
                                       0L,
                                       "MSK_RES_OK",
                                       "No error occurred.",
                                       0L )

    .ROI_plugin_add_status_code_to_db( solver, 10030L, "MSK_RES_TRM_INTERNAL", "The optimizer terminated due to some internal reason. Please contact MOSEK support.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 10031L, "MSK_RES_TRM_INTERNAL_STOP", "The optimizer terminated for internal reasons. Please contact MOSEK support.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 10000L, "MSK_RES_TRM_MAX_ITERATIONS", "The optimizer terminated at the maximum number of iterations.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 10020L, "MSK_RES_TRM_MAX_NUM_SETBACKS", "The optimizer terminated as the maximum number of set-backs was reached. This indicates %
serious numerical problems and a possibly badly formulated problem.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 10001L, "MSK_RES_TRM_MAX_TIME", "The optimizer terminated at the maximum amount of time.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 10004L, "MSK_RES_TRM_MIO_NEAR_ABS_GAP", "The mixed-integer optimizer terminated because the near optimal absolute gap tolerance was sat-
isfied.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 10003L, "MSK_RES_TRM_MIO_NEAR_REL_GAP", "The mixed-integer optimizer terminated because the near optimal relative gap tolerance was sat-
isfied.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 10009L, "MSK_RES_TRM_MIO_NUM_BRANCHES", "The mixed-integer optimizer terminated as to the maximum number of branches was reached.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 10008L, "MSK_RES_TRM_MIO_NUM_RELAXS", "The mixed-integer optimizer terminated as the maximum number of relaxations was reached.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 10015L, "MSK_RES_TRM_NUM_MAX_NUM_INT_SOLUTIONS", "The mixed-integer optimizer terminated as the maximum number of feasible solutions was reached.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 10025L, "MSK_RES_TRM_NUMERICAL_PROBLEM", "The optimizer terminated due to numerical problems.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 10002L, "MSK_RES_TRM_OBJECTIVE_RANGE", "The optimizer terminated on the bound of the objective range.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 10006L, "MSK_RES_TRM_STALL", "The optimizer is terminated due to slow progress.
Stalling means that numerical problems prevent the optimizer from making reasonable progress
and that it make no sense to continue. In many cases this happens if the problem is badly scaled
or otherwise ill-conditioned. There is no guarantee that the solution will be (near) feasible or near
optimal. However, often stalling happens near the optimum, and the returned solution may be of
good quality. Therefore, it is recommended to check the status of then solution. If the solution
near optimal the solution is most likely good enough for most practical purposes.
Please note that if a linear optimization problem is solved using the interior-point optimizer with
basis identification turned on, the returned basic solution likely to have high accuracy, even though
the optimizer stalled.
Some common causes of stalling are a) badly scaled models, b) near feasible or near infeasible
problems and c) a non-convex problems. Case c) is only relevant for general non-linear problems.
It is not possible in general for MOSEK to check if a specific problems is convex since such a
check would be NP hard in itself. This implies that care should be taken when solving problems
involving general user defined functions.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 10007L, "MSK_RES_TRM_USER_CALLBACK", "The optimizer terminated due to the return of the user-defined call-back function.", 1L)

    ##' ----------------------
    ##' Warning - Codes
    ##' ----------------------
    .ROI_plugin_add_status_code_to_db( solver, 904L, "MSK_RES_WRN_ANA_ALMOST_INT_BOUNDS", "This warning is issued by the problem analyzer if a constraint is bound nearly integral.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 901L, "MSK_RES_WRN_ANA_C_ZERO", "This warning is issued by the problem analyzer, if the coefficients in the linear part of the objective
are all zero.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 903L, "MSK_RES_WRN_ANA_CLOSE_BOUNDS", "This warning is issued by problem analyzer, if ranged constraints or variables with very close upper
and lower bounds are detected. One should consider treating such constraints as equalities and
such variables as constants.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 902L, "MSK_RES_WRN_ANA_EMPTY_COLS", "This warning is issued by the problem analyzer, if columns, in which all coefficients are zero, are
found.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 900L, "MSK_RES_WRN_ANA_LARGE_BOUNDS", "This warning is issued by the problem analyzer, if one or more constraint or variable bounds are
very large. One should consider omitting these bounds entirely by setting them to +inf or -inf.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 807L, "MSK_RES_WRN_CONSTRUCT_INVALID_SOL_ITG", "The initial value for one or more of the integer variables is not feasible.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 810L, "MSK_RES_WRN_CONSTRUCT_NO_SOL_ITG", "The construct solution requires an integer solution.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 805L, "MSK_RES_WRN_CONSTRUCT_SOLUTION_INFEAS", "After fixing the integer variables at the suggested values then the problem is infeasible.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 201L, "MSK_RES_WRN_DROPPED_NZ_QOBJ", "One or more non-zero elements were dropped in the Q matrix in the objective.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 852L, "MSK_RES_WRN_DUPLICATE_BARVARIABLE_NAMES", "Two barvariable names are identical.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 853L, "MSK_RES_WRN_DUPLICATE_CONE_NAMES", "Two cone names are identical.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 850L, "MSK_RES_WRN_DUPLICATE_CONSTRAINT_NAMES", "Two constraint names are identical.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 851L, "MSK_RES_WRN_DUPLICATE_VARIABLE_NAMES", "Two variable names are identical.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 801L, "MSK_RES_WRN_ELIMINATOR_SPACE", "The eliminator is skipped at least once due to lack of space.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 502L, "MSK_RES_WRN_EMPTY_NAME", "A variable or constraint name is empty. The output file may be invalid.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 250L, "MSK_RES_WRN_IGNORE_INTEGER", "Ignored integer constraints.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 800L, "MSK_RES_WRN_INCOMPLETE_LINEAR_DEPENDENCY_CHECK", "The linear dependency check(s) is incomplete. Normally this is not an important warning unless
the optimization problem has been formulated with linear dependencies. Linear dependencies may
prevent MOSEK from solving the problem.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 62L, "MSK_RES_WRN_LARGE_AIJ", "A numerically large value is specified for an a i,j element in A.
MSK_DPAR_DATA_TOL_AIJ_LARGE controls when an a i,j is considered large.
The parameter", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 51L, "MSK_RES_WRN_LARGE_BOUND", "A numerically large bound value is specified.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 57L, "MSK_RES_WRN_LARGE_CJ", "A numerically large value is specified for one c j .", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 54L, "MSK_RES_WRN_LARGE_CON_FX", "An equality constraint is fixed to a numerically large value. This can cause numerical problems.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 52L, "MSK_RES_WRN_LARGE_LO_BOUND", "A numerically large lower bound value is specified.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 53L, "MSK_RES_WRN_LARGE_UP_BOUND", "A numerically large upper bound value is specified.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 500L, "MSK_RES_WRN_LICENSE_EXPIRE", "The license expires.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 505L, "MSK_RES_WRN_LICENSE_FEATURE_EXPIRE", "The license expires.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 501L, "MSK_RES_WRN_LICENSE_SERVER", "The license server is not responding.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 85L, "MSK_RES_WRN_LP_DROP_VARIABLE", "Ignored a variable because the variable was not previously defined. Usually this implies that a
variable appears in the bound section but not in the objective or the constraints.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 80L, "MSK_RES_WRN_LP_OLD_QUAD_FORMAT", "Missing '/2' after quadratic expressions in bound or objective.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 270L, "MSK_RES_WRN_MIO_INFEASIBLE_FINAL", "The final mixed-integer problem with all the integer variables fixed at their optimal values is
infeasible.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 72L, "MSK_RES_WRN_MPS_SPLIT_BOU_VECTOR", "A BOUNDS vector is split into several nonadjacent parts in an MPS file.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 71L, "MSK_RES_WRN_MPS_SPLIT_RAN_VECTOR", "A RANGE vector is split into several nonadjacent parts in an MPS file.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 70L, "MSK_RES_WRN_MPS_SPLIT_RHS_VECTOR", "An RHS vector is split into several nonadjacent parts in an MPS file.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 65L, "MSK_RES_WRN_NAME_MAX_LEN", "A name is longer than the buffer that is supposed to hold it.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 950L, "MSK_RES_WRN_NO_DUALIZER", "No automatic dualizer is available for the specified problem. The primal problem is solved.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 251L, "MSK_RES_WRN_NO_GLOBAL_OPTIMIZER", "No global optimizer is available.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 450L, "MSK_RES_WRN_NO_NONLINEAR_FUNCTION_WRITE", "The problem contains a general nonlinear function in either the objective or the constraints. Such
a nonlinear function cannot be written to a disk file. Note that quadratic terms when inputted
explicitly can be written to disk.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 200L, "MSK_RES_WRN_NZ_IN_UPR_TRI", "Non-zero elements specified in the upper triangle of a matrix were ignored.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 50L, "MSK_RES_WRN_OPEN_PARAM_FILE", "The parameter file could not be opened.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 516L, "MSK_RES_WRN_PARAM_IGNORED_CMIO", "A parameter was ignored by the conic mixed integer optimizer.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 510L, "MSK_RES_WRN_PARAM_NAME_DOU", "The parameter name is not recognized as a double parameter.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 511L, "MSK_RES_WRN_PARAM_NAME_INT", "The parameter name is not recognized as a integer parameter.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 512L, "MSK_RES_WRN_PARAM_NAME_STR", "The parameter name is not recognized as a string parameter.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 515L, "MSK_RES_WRN_PARAM_STR_VALUE", "The string is not recognized as a symbolic value for the parameter.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 802L, "MSK_RES_WRN_PRESOLVE_OUTOFSPACE", "The presolve is incomplete due to lack of space.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 930L, "MSK_RES_WRN_QUAD_CONES_WITH_ROOT_FIXED_AT_ZERO", "For at least one quadratic cone the root is fixed at (nearly) zero. This may cause problems such as
a very large dual solution. Therefore, it is recommended to remove such cones before optimizing
the problems, or to fix all the variables in the cone to 0.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 931L, "MSK_RES_WRN_RQUAD_CONES_WITH_ROOT_FIXED_AT_ZERO", "For at least one rotated quadratic cone at least one of the root variables are fixed at (nearly) zero.
This may cause problems such as a very large dual solution. Therefore, it is recommended to
remove such cones before optimizing the problems, or to fix all the variables in the cone to 0.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 351L, "MSK_RES_WRN_SOL_FILE_IGNORED_CON", "One or more lines in the constraint section were ignored when reading a solution file.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 352L, "MSK_RES_WRN_SOL_FILE_IGNORED_VAR", "One or more lines in the variable section were ignored when reading a solution file.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 300L, "MSK_RES_WRN_SOL_FILTER", "Invalid solution filter is specified.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 66L, "MSK_RES_WRN_SPAR_MAX_LEN", "A value for a string parameter is longer than the buffer that is supposed to hold it.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 960L, "MSK_RES_WRN_SYM_MAT_LARGE", "A numerically large value is specified for an e i,j element in E.
MSK_DPAR_DATA_SYM_MAT_TOL_LARGE controls when an e i,j is considered large.
The parameter", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 400L, "MSK_RES_WRN_TOO_FEW_BASIS_VARS", "An incomplete basis has been specified. Too few basis variables are specified.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 405L, "MSK_RES_WRN_TOO_MANY_BASIS_VARS", "A basis with too many variables has been specified.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 350L, "MSK_RES_WRN_UNDEF_SOL_FILE_NAME", "Undefined name occurred in a solution.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 503L, "MSK_RES_WRN_USING_GENERIC_NAMES", "Generic names are used because a name is not valid. For instance when writing an LP file the
names must not contain blanks or start with a digit.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 803L, "MSK_RES_WRN_WRITE_CHANGED_NAMES", "Some names were changed because they were invalid for the output file format.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 804L, "MSK_RES_WRN_WRITE_DISCARDED_CFIX", "The fixed objective term could not be converted to a variable and was discarded in the output file.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 63L, "MSK_RES_WRN_ZERO_AIJ", "One or more zero elements are specified in A.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 710L, "MSK_RES_WRN_ZEROS_IN_SPARSE_COL", "One or more (near) zero elements are specified in a sparse column of a matrix. It is redundant to
specify zero elements. Hence, it may indicate an error.", 0L)
    .ROI_plugin_add_status_code_to_db( solver, 705L, "MSK_RES_WRN_ZEROS_IN_SPARSE_ROW", "One or more (near) zero elements are specified in a sparse row of a matrix. Since, it is redundant
to specify zero elements then it may indicate an error.", 0L)


    ##' ----------------------
    ##' Error - Codes
    ##' ----------------------
    .ROI_plugin_add_status_code_to_db( solver, 3102L, "MSK_RES_ERR_AD_INVALID_CODELIST", "The code list data was invalid.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 3001L, "MSK_RES_ERR_API_ARRAY_TOO_SMALL", "An input array was too short.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 3002L, "MSK_RES_ERR_API_CB_CONNECT", "Failed to connect a callback object.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 3005L, "MSK_RES_ERR_API_FATAL_ERROR", "An internal error occurred in the API. Please report this problem.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 3999L, "MSK_RES_ERR_API_INTERNAL", "An internal fatal error occurred in an interface function.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1227L, "MSK_RES_ERR_ARG_IS_TOO_LARGE", "The value of a argument is too small.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1226L, "MSK_RES_ERR_ARG_IS_TOO_SMALL", "The value of a argument is too small.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1201L, "MSK_RES_ERR_ARGUMENT_DIMENSION", "A function argument is of incorrect dimension.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 5005L, "MSK_RES_ERR_ARGUMENT_IS_TOO_LARGE", "The value of a function argument is too large.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1197L, "MSK_RES_ERR_ARGUMENT_LENNEQ", "Incorrect length of arguments.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1299L, "MSK_RES_ERR_ARGUMENT_PERM_ARRAY", "An invalid permutation array is specified.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1198L, "MSK_RES_ERR_ARGUMENT_TYPE", "Incorrect argument type.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 3920L, "MSK_RES_ERR_BAR_VAR_DIM", "The dimension of a symmetric matrix variable has to greater than 0.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1266L, "MSK_RES_ERR_BASIS", "An invalid basis is specified. Either too many or too few basis variables are specified.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1610L, "MSK_RES_ERR_BASIS_FACTOR", "The factorization of the basis is invalid.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1615L, "MSK_RES_ERR_BASIS_SINGULAR", "The basis is singular and hence cannot be factored.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1070L, "MSK_RES_ERR_BLANK_NAME", "An all blank name has been specified.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 2505L, "MSK_RES_ERR_CANNOT_CLONE_NL", "A task with a nonlinear function call-back cannot be cloned.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 2506L, "MSK_RES_ERR_CANNOT_HANDLE_NL", "A function cannot handle a task with nonlinear function call-backs.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 7116L, "MSK_RES_ERR_CBF_DUPLICATE_ACOORD", "Duplicate index in ACOORD.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 7115L, "MSK_RES_ERR_CBF_DUPLICATE_BCOORD", "Duplicate index in BCOORD.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 7108L, "MSK_RES_ERR_CBF_DUPLICATE_CON", "Duplicate CON keyword.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 7110L, "MSK_RES_ERR_CBF_DUPLICATE_INT", "Duplicate INT keyword.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 7107L, "MSK_RES_ERR_CBF_DUPLICATE_OBJ", "Duplicate OBJ keyword.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 7114L, "MSK_RES_ERR_CBF_DUPLICATE_OBJACOORD", "Duplicate index in OBJCOORD.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 7109L, "MSK_RES_ERR_CBF_DUPLICATE_VAR", "Duplicate VAR keyword.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 7112L, "MSK_RES_ERR_CBF_INVALID_CON_TYPE", "Invalid constraint type.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 7113L, "MSK_RES_ERR_CBF_INVALID_DOMAIN_DIMENSION", "Invalid domain dimension.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 7121L, "MSK_RES_ERR_CBF_INVALID_INT_INDEX", "Invalid INT index.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 7111L, "MSK_RES_ERR_CBF_INVALID_VAR_TYPE", "Invalid variable type.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 7102L, "MSK_RES_ERR_CBF_NO_VARIABLES", "No variables are specified.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 7105L, "MSK_RES_ERR_CBF_NO_VERSION_SPECIFIED", "No version specified.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 7101L, "MSK_RES_ERR_CBF_OBJ_SENSE", "An invalid objective sense is specified.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 7100L, "MSK_RES_ERR_CBF_PARSE", "An error occurred while parsing an CBF file.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 7106L, "MSK_RES_ERR_CBF_SYNTAX", "Invalid syntax.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 7118L, "MSK_RES_ERR_CBF_TOO_FEW_CONSTRAINTS", "Too few constraints defined.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 7119L, "MSK_RES_ERR_CBF_TOO_FEW_INTS", "Too few ints are specified.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 7117L, "MSK_RES_ERR_CBF_TOO_FEW_VARIABLES", "Too few variables defined.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 7103L, "MSK_RES_ERR_CBF_TOO_MANY_CONSTRAINTS", "Too many constraints specified.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 7120L, "MSK_RES_ERR_CBF_TOO_MANY_INTS", "Too many ints are specified.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 7104L, "MSK_RES_ERR_CBF_TOO_MANY_VARIABLES", "Too many variables specified.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 7122L, "MSK_RES_ERR_CBF_UNSUPPORTED", "Unsupported feature is present.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1294L, "MSK_RES_ERR_CON_Q_NOT_NSD", "The quadratic constraint matrix is not negative semidefinite as expected for a con-
straint with finite lower bound. This results in a nonconvex problem. The parameter
MSK_DPAR_CHECK_CONVEXITY_REL_TOL can be used to relax the convexity check.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1293L, "MSK_RES_ERR_CON_Q_NOT_PSD", "The quadratic constraint matrix is not positive semidefinite as expected for a con-
straint with finite upper bound. This results in a nonconvex problem. The parameter
MSK_DPAR_CHECK_CONVEXITY_REL_TOL can be used to relax the convexity check.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1300L, "MSK_RES_ERR_CONE_INDEX", "An index of a non-existing cone has been specified.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1302L, "MSK_RES_ERR_CONE_OVERLAP", "One or more of the variables in the cone to be added is already member of another cone. Now
assume the variable is x j then add a new variable say x k and the constraint
x j = x k
and then let x k be member of the cone to be appended.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1307L, "MSK_RES_ERR_CONE_OVERLAP_APPEND", "The cone to be appended has one variable which is already member of another cone.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1303L, "MSK_RES_ERR_CONE_REP_VAR", "A variable is included multiple times in the cone.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1301L, "MSK_RES_ERR_CONE_SIZE", "A cone with too few members is specified.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1305L, "MSK_RES_ERR_CONE_TYPE", "Invalid cone type specified.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1306L, "MSK_RES_ERR_CONE_TYPE_STR", "Invalid cone type specified.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1055L, "MSK_RES_ERR_DATA_FILE_EXT", "The data file format cannot be determined from the file name.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1071L, "MSK_RES_ERR_DUP_NAME", "The same name was used multiple times for the same problem item type.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1385L, "MSK_RES_ERR_DUPLICATE_AIJ", "An element in the A matrix is specified twice.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 4502L, "MSK_RES_ERR_DUPLICATE_BARVARIABLE_NAMES", "Two barvariable names are identical.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 4503L, "MSK_RES_ERR_DUPLICATE_CONE_NAMES", "Two cone names are identical.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 4500L, "MSK_RES_ERR_DUPLICATE_CONSTRAINT_NAMES", "Two constraint names are identical.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 4501L, "MSK_RES_ERR_DUPLICATE_VARIABLE_NAMES", "Two variable names are identical.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1059L, "MSK_RES_ERR_END_OF_FILE", "End of file reached.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1650L, "MSK_RES_ERR_FACTOR", "An error occurred while factorizing a matrix.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1700L, "MSK_RES_ERR_FEASREPAIR_CANNOT_RELAX", "An optimization problem cannot be relaxed. This is the case e.g. for general nonlinear optimization
problems.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1702L, "MSK_RES_ERR_FEASREPAIR_INCONSISTENT_BOUND", "The upper bound is less than the lower bound for a variable or a constraint. Please correct this
before running the feasibility repair.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1701L, "MSK_RES_ERR_FEASREPAIR_SOLVING_RELAXED", "The relaxed problem could not be solved to optimality. Please consult the log file for further details.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1007L, "MSK_RES_ERR_FILE_LICENSE", "Invalid license file.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1052L, "MSK_RES_ERR_FILE_OPEN", "Error while opening a file.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1053L, "MSK_RES_ERR_FILE_READ", "File read error.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1054L, "MSK_RES_ERR_FILE_WRITE", "File write error.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1261L, "MSK_RES_ERR_FIRST", "Invalid first.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1285L, "MSK_RES_ERR_FIRSTI", "Invalid firsti.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1287L, "MSK_RES_ERR_FIRSTJ", "Invalid firstj.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1425L, "MSK_RES_ERR_FIXED_BOUND_VALUES", "A fixed constraint/variable has been specified using the bound keys but the numerical value of the
lower and upper bound is different.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1014L, "MSK_RES_ERR_FLEXLM", "The FLEXlm license manager reported an error.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1503L, "MSK_RES_ERR_GLOBAL_INV_CONIC_PROBLEM", "The global optimizer can only be applied to problems without semidefinite variables.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1380L, "MSK_RES_ERR_HUGE_AIJ", "A numerically huge value is specified for an a i,j element in A.
MSK_DPAR_DATA_TOL_AIJ_HUGE controls when an a i,j is considered huge.
The parameter", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1375L, "MSK_RES_ERR_HUGE_C", "A huge value in absolute size is specified for one c j .", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 3101L, "MSK_RES_ERR_IDENTICAL_TASKS", "Some tasks related to this function call were identical. Unique tasks were expected.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1200L, "MSK_RES_ERR_IN_ARGUMENT", "A function argument is incorrect.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1235L, "MSK_RES_ERR_INDEX", "An index is out of range.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1222L, "MSK_RES_ERR_INDEX_ARR_IS_TOO_LARGE", "An index in an array argument is too large.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1221L, "MSK_RES_ERR_INDEX_ARR_IS_TOO_SMALL", "An index in an array argument is too small.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1204L, "MSK_RES_ERR_INDEX_IS_TOO_LARGE", "An index in an argument is too large.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1203L, "MSK_RES_ERR_INDEX_IS_TOO_SMALL", "An index in an argument is too small.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1219L, "MSK_RES_ERR_INF_DOU_INDEX", "A double information index is out of range for the specified type.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1230L, "MSK_RES_ERR_INF_DOU_NAME", "A double information name is invalid.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1220L, "MSK_RES_ERR_INF_INT_INDEX", "An integer information index is out of range for the specified type.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1231L, "MSK_RES_ERR_INF_INT_NAME", "An integer information name is invalid.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1225L, "MSK_RES_ERR_INF_LINT_INDEX", "A long integer information index is out of range for the specified type.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1234L, "MSK_RES_ERR_INF_LINT_NAME", "A long integer information name is invalid.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1232L, "MSK_RES_ERR_INF_TYPE", "The information type is invalid.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 3910L, "MSK_RES_ERR_INFEAS_UNDEFINED", "The requested value is not defined for this solution type.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1400L, "MSK_RES_ERR_INFINITE_BOUND", "A numerically huge bound value is specified.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 3800L, "MSK_RES_ERR_INT64_TO_INT32_CAST", "An 32 bit integer could not cast to a 64 bit integer.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 3000L, "MSK_RES_ERR_INTERNAL", "An internal error occurred. Please report this problem.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 3500L, "MSK_RES_ERR_INTERNAL_TEST_FAILED", "An internal unit test function failed.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1253L, "MSK_RES_ERR_INV_APTRE", "aptre[j] is strictly smaller than aptrb[j] for some j.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1255L, "MSK_RES_ERR_INV_BK", "Invalid bound key.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1256L, "MSK_RES_ERR_INV_BKC", "Invalid bound key is specified for a constraint.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1257L, "MSK_RES_ERR_INV_BKX", "An invalid bound key is specified for a variable.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1272L, "MSK_RES_ERR_INV_CONE_TYPE", "Invalid cone type code is encountered.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1271L, "MSK_RES_ERR_INV_CONE_TYPE_STR", "Invalid cone type string encountered.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 2501L, "MSK_RES_ERR_INV_MARKI", "Invalid value in marki.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 2502L, "MSK_RES_ERR_INV_MARKJ", "Invalid value in markj.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1280L, "MSK_RES_ERR_INV_NAME_ITEM", "An invalid name item code is used.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 2503L, "MSK_RES_ERR_INV_NUMI", "Invalid numi.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 2504L, "MSK_RES_ERR_INV_NUMJ", "Invalid numj.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1550L, "MSK_RES_ERR_INV_OPTIMIZER", "An invalid optimizer has been chosen for the problem. This means that the simplex or the conic
optimizer is chosen to optimize a nonlinear problem.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1500L, "MSK_RES_ERR_INV_PROBLEM", "Invalid problem type. Probably a nonconvex problem has been specified.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1405L, "MSK_RES_ERR_INV_QCON_SUBI", "Invalid value in qcsubi.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1406L, "MSK_RES_ERR_INV_QCON_SUBJ", "Invalid value in qcsubj.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1404L, "MSK_RES_ERR_INV_QCON_SUBK", "Invalid value in qcsubk.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1407L, "MSK_RES_ERR_INV_QCON_VAL", "Invalid value in qcval.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1401L, "MSK_RES_ERR_INV_QOBJ_SUBI", "Invalid value in qosubi.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1402L, "MSK_RES_ERR_INV_QOBJ_SUBJ", "Invalid value in qosubj.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1403L, "MSK_RES_ERR_INV_QOBJ_VAL", "Invalid value in qoval.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1270L, "MSK_RES_ERR_INV_SK", "Invalid status key code.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1269L, "MSK_RES_ERR_INV_SK_STR", "Invalid status key string encountered.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1267L, "MSK_RES_ERR_INV_SKC", "Invalid value in skc.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1274L, "MSK_RES_ERR_INV_SKN", "Invalid value in skn.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1268L, "MSK_RES_ERR_INV_SKX", "Invalid value in skx.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1258L, "MSK_RES_ERR_INV_VAR_TYPE", "An invalid variable type is specified for a variable.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 2520L, "MSK_RES_ERR_INVALID_ACCMODE", "An invalid access mode is specified.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1473L, "MSK_RES_ERR_INVALID_AIJ", "a i,j contains an invalid floating point value, i.e. a NaN or an infinite value.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 3700L, "MSK_RES_ERR_INVALID_AMPL_STUB", "Invalid AMPL stub.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1079L, "MSK_RES_ERR_INVALID_BARVAR_NAME", "An invalid symmetric matrix variable name is used.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1800L, "MSK_RES_ERR_INVALID_COMPRESSION", "Invalid compression type.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1076L, "MSK_RES_ERR_INVALID_CON_NAME", "An invalid constraint name is used.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1078L, "MSK_RES_ERR_INVALID_CONE_NAME", "An invalid cone name is used.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 4005L, "MSK_RES_ERR_INVALID_FILE_FORMAT_FOR_CONES", "The file format does not support a problem with conic constraints.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 4010L, "MSK_RES_ERR_INVALID_FILE_FORMAT_FOR_GENERAL_NL", "The file format does not support a problem with general nonlinear terms.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 4000L, "MSK_RES_ERR_INVALID_FILE_FORMAT_FOR_SYM_MAT", "The file format does not support a problem with symmetric matrix variables.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1056L, "MSK_RES_ERR_INVALID_FILE_NAME", "An invalid file name has been specified.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1283L, "MSK_RES_ERR_INVALID_FORMAT_TYPE", "Invalid format type.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1246L, "MSK_RES_ERR_INVALID_IDX", "A specified index is invalid.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1801L, "MSK_RES_ERR_INVALID_IOMODE", "Invalid io mode.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1247L, "MSK_RES_ERR_INVALID_MAX_NUM", "A specified index is invalid.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1170L, "MSK_RES_ERR_INVALID_NAME_IN_SOL_FILE", "An invalid name occurred in a solution file.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1075L, "MSK_RES_ERR_INVALID_OBJ_NAME", "An invalid objective name is specified.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1445L, "MSK_RES_ERR_INVALID_OBJECTIVE_SENSE", "An invalid objective sense is specified.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 6000L, "MSK_RES_ERR_INVALID_PROBLEM_TYPE", "An invalid problem type.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1057L, "MSK_RES_ERR_INVALID_SOL_FILE_NAME", "An invalid file name has been specified.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1062L, "MSK_RES_ERR_INVALID_STREAM", "An invalid stream is referenced.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1275L, "MSK_RES_ERR_INVALID_SURPLUS", "Invalid surplus.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 3950L, "MSK_RES_ERR_INVALID_SYM_MAT_DIM", "A sparse symmetric matrix of invalid dimension is specified.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1064L, "MSK_RES_ERR_INVALID_TASK", "The task is invalid.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 2900L, "MSK_RES_ERR_INVALID_UTF8", "An invalid UTF8 string is encountered.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1077L, "MSK_RES_ERR_INVALID_VAR_NAME", "An invalid variable name is used.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 2901L, "MSK_RES_ERR_INVALID_WCHAR", "An invalid wchar string is encountered.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1228L, "MSK_RES_ERR_INVALID_WHICHSOL", "whichsol is invalid.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1179L, "MSK_RES_ERR_JSON_DATA", "Inconsistent data in JSON Task file", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1178L, "MSK_RES_ERR_JSON_FORMAT", "Error in an JSON Task file", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1180L, "MSK_RES_ERR_JSON_MISSING_DATA", "Missing data section in JSON task file.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1177L, "MSK_RES_ERR_JSON_NUMBER_OVERFLOW", "Invalid number entry - wrong type or value overflow.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1176L, "MSK_RES_ERR_JSON_STRING", "Error in JSON string.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1175L, "MSK_RES_ERR_JSON_SYNTAX", "Syntax error in an JSON data", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1262L, "MSK_RES_ERR_LAST", "Invalid index last. A given index was out of expected range.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1286L, "MSK_RES_ERR_LASTI", "Invalid lasti.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1288L, "MSK_RES_ERR_LASTJ", "Invalid lastj.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 7012L, "MSK_RES_ERR_LAU_ARG_K", "Invalid argument k.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 7010L, "MSK_RES_ERR_LAU_ARG_M", "Invalid argument m.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 7011L, "MSK_RES_ERR_LAU_ARG_N", "Invalid argument n.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 7018L, "MSK_RES_ERR_LAU_ARG_TRANS", "Invalid argument trans.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 7015L, "MSK_RES_ERR_LAU_ARG_TRANSA", "Invalid argument transa.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 7016L, "MSK_RES_ERR_LAU_ARG_TRANSB", "Invalid argument transb.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 7017L, "MSK_RES_ERR_LAU_ARG_UPLO", "Invalid argument uplo.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 7002L, "MSK_RES_ERR_LAU_INVALID_LOWER_TRIANGULAR_MATRIX", "An invalid lower triangular matrix.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 7019L, "MSK_RES_ERR_LAU_INVALID_SPARSE_SYMMETRIC_MATRIX", "An invalid sparse symmetric matrix is specfified. Note only the lower triangular part with no
duplicates is specifed.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 7001L, "MSK_RES_ERR_LAU_NOT_POSITIVE_DEFINITE", "A matrix is not positive definite.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 7000L, "MSK_RES_ERR_LAU_SINGULAR_MATRIX", "A matrix is singular.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 7005L, "MSK_RES_ERR_LAU_UNKNOWN", "An unknown error.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1000L, "MSK_RES_ERR_LICENSE", "Invalid license.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1020L, "MSK_RES_ERR_LICENSE_CANNOT_ALLOCATE", "The license system cannot allocate the memory required.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1021L, "MSK_RES_ERR_LICENSE_CANNOT_CONNECT", "MOSEK cannot connect to the license server. Most likely the license server is not up and running.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1001L, "MSK_RES_ERR_LICENSE_EXPIRED", "The license has expired.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1018L, "MSK_RES_ERR_LICENSE_FEATURE", "A requested feature is not available in the license file(s). Most likely due to an incorrect license
system setup.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1025L, "MSK_RES_ERR_LICENSE_INVALID_HOSTID", "The host ID specified in the license file does not match the host ID of the computer.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1016L, "MSK_RES_ERR_LICENSE_MAX", "Maximum number of licenses is reached.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1017L, "MSK_RES_ERR_LICENSE_MOSEKLM_DAEMON", "The MOSEKLM license manager daemon is not up and running.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1028L, "MSK_RES_ERR_LICENSE_NO_SERVER_LINE", "There is no SERVER line in the license file. All non-zero license count features need at least one
SERVER line.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1027L, "MSK_RES_ERR_LICENSE_NO_SERVER_SUPPORT", "The license server does not support the requested feature. Possible reasons for this error include:
- The feature has expired.
- The feature's start date is later than today's date.
- The version requested is higher than feature's the highest supported version.
- A corrupted license file.
Try restarting the license and inspect the license server debug file, usually called lmgrd.log.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1015L, "MSK_RES_ERR_LICENSE_SERVER", "The license server is not responding.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1026L, "MSK_RES_ERR_LICENSE_SERVER_VERSION", "The version specified in the checkout request is greater than the highest version number the daemon
supports.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1002L, "MSK_RES_ERR_LICENSE_VERSION", "The license is valid for another version of MOSEK.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1040L, "MSK_RES_ERR_LINK_FILE_DLL", "A file cannot be linked to a stream in the DLL version.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1066L, "MSK_RES_ERR_LIVING_TASKS", "All tasks associated with an enviroment must be deleted before the environment is deleted. There
are still some undeleted tasks.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1390L, "MSK_RES_ERR_LOWER_BOUND_IS_A_NAN", "The lower bound specified is not a number (nan).", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1152L, "MSK_RES_ERR_LP_DUP_SLACK_NAME", "The name of the slack variable added to a ranged constraint already exists.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1151L, "MSK_RES_ERR_LP_EMPTY", "The problem cannot be written to an LP formatted file.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1157L, "MSK_RES_ERR_LP_FILE_FORMAT", "Syntax error in an LP file.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1160L, "MSK_RES_ERR_LP_FORMAT", "Syntax error in an LP file.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1155L, "MSK_RES_ERR_LP_FREE_CONSTRAINT", "Free constraints cannot be written in LP file format.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1150L, "MSK_RES_ERR_LP_INCOMPATIBLE", "The problem cannot be written to an LP formatted file.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1171L, "MSK_RES_ERR_LP_INVALID_CON_NAME", "A constraint name is invalid when used in an LP formatted file.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1154L, "MSK_RES_ERR_LP_INVALID_VAR_NAME", "A variable name is invalid when used in an LP formatted file.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1163L, "MSK_RES_ERR_LP_WRITE_CONIC_PROBLEM", "The problem contains cones that cannot be written to an LP formatted file.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1164L, "MSK_RES_ERR_LP_WRITE_GECO_PROBLEM", "The problem contains general convex terms that cannot be written to an LP formatted file.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 2800L, "MSK_RES_ERR_LU_MAX_NUM_TRIES", "Could not compute the LU factors of the matrix within the maximum number of allowed tries.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1289L, "MSK_RES_ERR_MAX_LEN_IS_TOO_SMALL", "An maximum length that is too small has been specified.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1242L, "MSK_RES_ERR_MAXNUMBARVAR", "The maximum number of semidefinite variables specified is smaller than the number of semidefinite
variables in the task.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1240L, "MSK_RES_ERR_MAXNUMCON", "The maximum number of constraints specified is smaller than the number of constraints in the
task.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1304L, "MSK_RES_ERR_MAXNUMCONE", "The value specified for maxnumcone is too small.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1243L, "MSK_RES_ERR_MAXNUMQNZ", "The maximum number of non-zeros specified for the Q matrices is smaller than the number of
non-zeros in the current Q matrices.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1241L, "MSK_RES_ERR_MAXNUMVAR", "The maximum number of variables specified is smaller than the number of variables in the task.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 5010L, "MSK_RES_ERR_MIO_INTERNAL", "A fatal error occurred in the mixed integer optimizer. Please contact MOSEK support.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 7131L, "MSK_RES_ERR_MIO_INVALID_NODE_OPTIMIZER", "An invalid node optimizer was selected for the problem type.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 7130L, "MSK_RES_ERR_MIO_INVALID_ROOT_OPTIMIZER", "An invalid root optimizer was selected for the problem type.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1551L, "MSK_RES_ERR_MIO_NO_OPTIMIZER", "No optimizer is available for the current class of integer optimization problems.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1553L, "MSK_RES_ERR_MIO_NOT_LOADED", "The mixed-integer optimizer is not loaded.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1008L, "MSK_RES_ERR_MISSING_LICENSE_FILE", "MOSEK cannot license file or a token server. See the MOSEK installation manual for details.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1501L, "MSK_RES_ERR_MIXED_CONIC_AND_NL", "The problem contains nonlinear terms conic constraints. The requested operation cannot be applied
to this type of problem.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1118L, "MSK_RES_ERR_MPS_CONE_OVERLAP", "A variable is specified to be a member of several cones.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1119L, "MSK_RES_ERR_MPS_CONE_REPEAT", "A variable is repeated within the CSECTION.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1117L, "MSK_RES_ERR_MPS_CONE_TYPE", "Invalid cone type specified in a CSECTION.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1121L, "MSK_RES_ERR_MPS_DUPLICATE_Q_ELEMENT", "Duplicate elements is specfied in a Q matrix.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1100L, "MSK_RES_ERR_MPS_FILE", "An error occurred while reading an MPS file.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1108L, "MSK_RES_ERR_MPS_INV_BOUND_KEY", "An invalid bound key occurred in an MPS file.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1107L, "MSK_RES_ERR_MPS_INV_CON_KEY", "An invalid constraint key occurred in an MPS file.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1101L, "MSK_RES_ERR_MPS_INV_FIELD", "A field in the MPS file is invalid. Probably it is too wide.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1102L, "MSK_RES_ERR_MPS_INV_MARKER", "An invalid marker has been specified in the MPS file.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1109L, "MSK_RES_ERR_MPS_INV_SEC_NAME", "An invalid section name occurred in an MPS file.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1115L, "MSK_RES_ERR_MPS_INV_SEC_ORDER", "The sections in the MPS data file are not in the correct order.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1128L, "MSK_RES_ERR_MPS_INVALID_OBJ_NAME", "An invalid objective name is specified.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1122L, "MSK_RES_ERR_MPS_INVALID_OBJSENSE", "An invalid objective sense is specified.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1112L, "MSK_RES_ERR_MPS_MUL_CON_NAME", "A constraint name was specified multiple times in the ROWS section.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1116L, "MSK_RES_ERR_MPS_MUL_CSEC", "Multiple CSECTIONs are given the same name.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1114L, "MSK_RES_ERR_MPS_MUL_QOBJ", "The Q term in the objective is specified multiple times in the MPS data file.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1113L, "MSK_RES_ERR_MPS_MUL_QSEC", "Multiple QSECTIONs are specified for a constraint in the MPS data file.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1110L, "MSK_RES_ERR_MPS_NO_OBJECTIVE", "No objective is defined in an MPS file.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1120L, "MSK_RES_ERR_MPS_NON_SYMMETRIC_Q", "A non symmetric matrice has been speciefied.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1103L, "MSK_RES_ERR_MPS_NULL_CON_NAME", "An empty constraint name is used in an MPS file.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1104L, "MSK_RES_ERR_MPS_NULL_VAR_NAME", "An empty variable name is used in an MPS file.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1111L, "MSK_RES_ERR_MPS_SPLITTED_VAR", "All elements in a column of the A matrix must be specified consecutively. Hence, it is illegal to
specify non-zero elements in A for variable 1, then for variable 2 and then variable 1 again.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1125L, "MSK_RES_ERR_MPS_TAB_IN_FIELD2", "A tab char occurred in field 2.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1126L, "MSK_RES_ERR_MPS_TAB_IN_FIELD3", "A tab char occurred in field 3.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1127L, "MSK_RES_ERR_MPS_TAB_IN_FIELD5", "A tab char occurred in field 5.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1105L, "MSK_RES_ERR_MPS_UNDEF_CON_NAME", "An undefined constraint name occurred in an MPS file.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1106L, "MSK_RES_ERR_MPS_UNDEF_VAR_NAME", "An undefined variable name occurred in an MPS file.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1254L, "MSK_RES_ERR_MUL_A_ELEMENT", "An element in A is defined multiple times.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1760L, "MSK_RES_ERR_NAME_IS_NULL", "The name buffer is a NULL pointer.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1750L, "MSK_RES_ERR_NAME_MAX_LEN", "A name is longer than the buffer that is supposed to hold it.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1461L, "MSK_RES_ERR_NAN_IN_BLC", "l c contains an invalid floating point value, i.e. a NaN.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1471L, "MSK_RES_ERR_NAN_IN_BLX", "l x contains an invalid floating point value, i.e. a NaN.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1462L, "MSK_RES_ERR_NAN_IN_BUC", "u c contains an invalid floating point value, i.e. a NaN.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1472L, "MSK_RES_ERR_NAN_IN_BUX", "u x contains an invalid floating point value, i.e. a NaN.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1470L, "MSK_RES_ERR_NAN_IN_C", "c contains an invalid floating point value, i.e. a NaN.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1450L, "MSK_RES_ERR_NAN_IN_DOUBLE_DATA", "An invalid floating point value was used in some double data.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1264L, "MSK_RES_ERR_NEGATIVE_APPEND", "Cannot append a negative number.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1263L, "MSK_RES_ERR_NEGATIVE_SURPLUS", "Negative surplus.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1036L, "MSK_RES_ERR_NEWER_DLL", "The dynamic link library is newer than the specified version.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 3916L, "MSK_RES_ERR_NO_BARS_FOR_SOLUTION", "There is no s available for the solution specified. In particular note there are no s defined for the
basic and integer solutions.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 3915L, "MSK_RES_ERR_NO_BARX_FOR_SOLUTION", "There is no X available for the solution specified. In particular note there are no X defined for the
basic and integer solutions.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1600L, "MSK_RES_ERR_NO_BASIS_SOL", "No basic solution is defined.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 2950L, "MSK_RES_ERR_NO_DUAL_FOR_ITG_SOL", "No dual information is available for the integer solution.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 2001L, "MSK_RES_ERR_NO_DUAL_INFEAS_CER", "A certificate of infeasibility is not available.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1063L, "MSK_RES_ERR_NO_INIT_ENV", "env is not initialized.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1552L, "MSK_RES_ERR_NO_OPTIMIZER_VAR_TYPE", "No optimizer is available for this class of optimization problems.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 2000L, "MSK_RES_ERR_NO_PRIMAL_INFEAS_CER", "A certificate of primal infeasibility is not available.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 2953L, "MSK_RES_ERR_NO_SNX_FOR_BAS_SOL", "s xn is not available for the basis solution.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 2500L, "MSK_RES_ERR_NO_SOLUTION_IN_CALLBACK", "The required solution is not available.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 5000L, "MSK_RES_ERR_NON_UNIQUE_ARRAY", "An array does not contain unique elements.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1291L, "MSK_RES_ERR_NONCONVEX", "The optimization problem is nonconvex.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1290L, "MSK_RES_ERR_NONLINEAR_EQUALITY", "The model contains a nonlinear equality which defines a nonconvex set.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1428L, "MSK_RES_ERR_NONLINEAR_FUNCTIONS_NOT_ALLOWED", "An operation that is invalid for problems with nonlinear functions defined has been attempted.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1292L, "MSK_RES_ERR_NONLINEAR_RANGED", "Nonlinear constraints with finite lower and upper bound always define a nonconvex feasible set.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1199L, "MSK_RES_ERR_NR_ARGUMENTS", "Incorrect number of function arguments.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1060L, "MSK_RES_ERR_NULL_ENV", "env is a NULL pointer.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1065L, "MSK_RES_ERR_NULL_POINTER", "An argument to a function is unexpectedly a NULL pointer.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1061L, "MSK_RES_ERR_NULL_TASK", "task is a NULL pointer.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1250L, "MSK_RES_ERR_NUMCONLIM", "Maximum number of constraints limit is exceeded.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1251L, "MSK_RES_ERR_NUMVARLIM", "Maximum number of variables limit is exceeded.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1296L, "MSK_RES_ERR_OBJ_Q_NOT_NSD", "The quadratic coefficient matrix in the objective is not negative semidefinite as expected for a
maximization problem. The parameter MSK_DPAR_CHECK_CONVEXITY_REL_TOL can be used to relax
the convexity check.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1295L, "MSK_RES_ERR_OBJ_Q_NOT_PSD", "The quadratic coefficient matrix in the objective is not positive semidefinite as expected for a
minimization problem. The parameter MSK_DPAR_CHECK_CONVEXITY_REL_TOL can be used to relax
the convexity check.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1260L, "MSK_RES_ERR_OBJECTIVE_RANGE", "Empty objective range.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1035L, "MSK_RES_ERR_OLDER_DLL", "The dynamic link library is older than the specified version.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1030L, "MSK_RES_ERR_OPEN_DL", "A dynamic link library could not be opened.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1168L, "MSK_RES_ERR_OPF_FORMAT", "Syntax error in an OPF file", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1169L, "MSK_RES_ERR_OPF_NEW_VARIABLE", "Introducing new variables is now allowed. When a [variables] section is present, it is not allowed
to introduce new variables later in the problem.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1172L, "MSK_RES_ERR_OPF_PREMATURE_EOF", "Premature end of file in an OPF file.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1013L, "MSK_RES_ERR_OPTIMIZER_LICENSE", "The optimizer required is not licensed.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1590L, "MSK_RES_ERR_OVERFLOW", "A computation produced an overflow i.e. a very large number.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1210L, "MSK_RES_ERR_PARAM_INDEX", "Parameter index is out of range.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1215L, "MSK_RES_ERR_PARAM_IS_TOO_LARGE", "The parameter value is too large.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1216L, "MSK_RES_ERR_PARAM_IS_TOO_SMALL", "The parameter value is too small.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1205L, "MSK_RES_ERR_PARAM_NAME", "The parameter name is not correct.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1206L, "MSK_RES_ERR_PARAM_NAME_DOU", "The parameter name is not correct for a double parameter.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1207L, "MSK_RES_ERR_PARAM_NAME_INT", "The parameter name is not correct for an integer parameter.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1208L, "MSK_RES_ERR_PARAM_NAME_STR", "The parameter name is not correct for a string parameter.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1218L, "MSK_RES_ERR_PARAM_TYPE", "The parameter type is invalid.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1217L, "MSK_RES_ERR_PARAM_VALUE_STR", "The parameter value string is incorrect.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1019L, "MSK_RES_ERR_PLATFORM_NOT_LICENSED", "A requested license feature is not available for the required platform.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1580L, "MSK_RES_ERR_POSTSOLVE", "An error occurred during the postsolve. Please contact MOSEK support.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1281L, "MSK_RES_ERR_PRO_ITEM", "An invalid problem is used.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1006L, "MSK_RES_ERR_PROB_LICENSE", "The software is not licensed to solve the problem.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1409L, "MSK_RES_ERR_QCON_SUBI_TOO_LARGE", "Invalid value in qcsubi.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1408L, "MSK_RES_ERR_QCON_SUBI_TOO_SMALL", "Invalid value in qcsubi.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1417L, "MSK_RES_ERR_QCON_UPPER_TRIANGLE", "An element in the upper triangle of a Q k is specified. Only elements in the lower triangle should
be specified.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1415L, "MSK_RES_ERR_QOBJ_UPPER_TRIANGLE", "An element in the upper triangle of Q o is specified. Only elements in the lower triangle should be
specified.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1090L, "MSK_RES_ERR_READ_FORMAT", "The specified format cannot be read.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1159L, "MSK_RES_ERR_READ_LP_MISSING_END_TAG", "Syntax error in LP file. Possibly missing End tag.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1162L, "MSK_RES_ERR_READ_LP_NONEXISTING_NAME", "A variable never occurred in objective or constraints.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1310L, "MSK_RES_ERR_REMOVE_CONE_VARIABLE", "A variable cannot be removed because it will make a cone invalid.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1710L, "MSK_RES_ERR_REPAIR_INVALID_PROBLEM", "The feasibility repair does not support the specified problem type.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1711L, "MSK_RES_ERR_REPAIR_OPTIMIZATION_FAILED", "Computation the optimal relaxation failed. The cause may have been numerical problems.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 3054L, "MSK_RES_ERR_SEN_BOUND_INVALID_LO", "Analysis of lower bound requested for an index, where no lower bound exists.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 3053L, "MSK_RES_ERR_SEN_BOUND_INVALID_UP", "Analysis of upper bound requested for an index, where no upper bound exists.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 3050L, "MSK_RES_ERR_SEN_FORMAT", "Syntax error in sensitivity analysis file.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 3055L, "MSK_RES_ERR_SEN_INDEX_INVALID", "Invalid range given in the sensitivity file.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 3052L, "MSK_RES_ERR_SEN_INDEX_RANGE", "Index out of range in the sensitivity analysis file.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 3056L, "MSK_RES_ERR_SEN_INVALID_REGEXP", "Syntax error in regexp or regexp longer than 1024.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 3058L, "MSK_RES_ERR_SEN_NUMERICAL", "Numerical difficulties encountered performing the sensitivity analysis.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 3057L, "MSK_RES_ERR_SEN_SOLUTION_STATUS", "No optimal solution found to the original problem given for sensitivity analysis.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 3051L, "MSK_RES_ERR_SEN_UNDEF_NAME", "An undefined name was encountered in the sensitivity analysis file.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 3080L, "MSK_RES_ERR_SEN_UNHANDLED_PROBLEM_TYPE", "Sensitivity analysis cannot be performed for the specified problem. Sensitivity analysis is only
possible for linear problems.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 8000L, "MSK_RES_ERR_SERVER_CONNECT", "Failed to connect to remote solver server. The server string or the port string were invalid, or the
server did not accept connection.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 8001L, "MSK_RES_ERR_SERVER_PROTOCOL", "Unexpected message or data from solver server.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 8002L, "MSK_RES_ERR_SERVER_STATUS", "Server returned non-ok HTTP status code", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 8003L, "MSK_RES_ERR_SERVER_TOKEN", "The job ID specified is incorrect or invalid", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1005L, "MSK_RES_ERR_SIZE_LICENSE", "The problem is bigger than the license.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1010L, "MSK_RES_ERR_SIZE_LICENSE_CON", "The problem has too many constraints to be solved with the available license.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1012L, "MSK_RES_ERR_SIZE_LICENSE_INTVAR", "The problem contains too many integer variables to be solved with the available license.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 3900L, "MSK_RES_ERR_SIZE_LICENSE_NUMCORES", "The computer contains more cpu cores than the license allows for.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1011L, "MSK_RES_ERR_SIZE_LICENSE_VAR", "The problem has too many variables to be solved with the available license.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1350L, "MSK_RES_ERR_SOL_FILE_INVALID_NUMBER", "An invalid number is specified in a solution file.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1237L, "MSK_RES_ERR_SOLITEM", "The solution item number solitem is invalid. Please note that MSK_SOL_ITEM_SNX is invalid for
the basic solution.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1259L, "MSK_RES_ERR_SOLVER_PROBTYPE", "Problem type does not match the chosen optimizer.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1051L, "MSK_RES_ERR_SPACE", "Out of space.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1080L, "MSK_RES_ERR_SPACE_LEAKING", "MOSEK is leaking memory. This can be due to either an incorrect use of MOSEK or a bug.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1081L, "MSK_RES_ERR_SPACE_NO_INFO", "No available information about the space usage.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 3944L, "MSK_RES_ERR_SYM_MAT_DUPLICATE", "A value in a symmetric matric as been specified more than once.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1482L, "MSK_RES_ERR_SYM_MAT_HUGE", "A symmetric matrix contains a huge value in absolute size.
MSK_DPAR_DATA_SYM_MAT_TOL_HUGE controls when an e i,j is considered huge.
The
parameter", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1480L, "MSK_RES_ERR_SYM_MAT_INVALID", "A symmetric matrix contains an invalid floating point value, i.e. a NaN or an infinite value.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 3941L, "MSK_RES_ERR_SYM_MAT_INVALID_COL_INDEX", "A column index specified for sparse symmetric matrix is invalid.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 3940L, "MSK_RES_ERR_SYM_MAT_INVALID_ROW_INDEX", "A row index specified for sparse symmetric matrix is invalid.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 3943L, "MSK_RES_ERR_SYM_MAT_INVALID_VALUE", "The numerical value specified in a sparse symmetric matrix is not a value floating value.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 3942L, "MSK_RES_ERR_SYM_MAT_NOT_LOWER_TRINGULAR", "Only the lower triangular part of sparse symmetric matrix should be specified.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 2560L, "MSK_RES_ERR_TASK_INCOMPATIBLE", "The Task file is incompatible with this platform. This results from reading a file on a 32 bit
platform generated on a 64 bit platform.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 2561L, "MSK_RES_ERR_TASK_INVALID", "The Task file is invalid.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 2562L, "MSK_RES_ERR_TASK_WRITE", "Failed to write the task file.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1049L, "MSK_RES_ERR_THREAD_COND_INIT", "Could not initialize a condition.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1048L, "MSK_RES_ERR_THREAD_CREATE", "Could not create a thread. This error may occur if a large number of environments are created
and not deleted again. In any case it is a good practice to minimize the number of environments
created.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1045L, "MSK_RES_ERR_THREAD_MUTEX_INIT", "Could not initialize a mutex.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1046L, "MSK_RES_ERR_THREAD_MUTEX_LOCK", "Could not lock a mutex.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1047L, "MSK_RES_ERR_THREAD_MUTEX_UNLOCK", "Could not unlock a mutex.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 7153L, "MSK_RES_ERR_TOCONIC_CONSTR_NOT_CONIC", "The constraint is not conic representable.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 7150L, "MSK_RES_ERR_TOCONIC_CONSTR_Q_NOT_PSD", "The matrix defining the quadratric part of constraint is not positive semidefinite.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 7151L, "MSK_RES_ERR_TOCONIC_CONSTRAINT_FX", "The quadratic constraint is an equality, thus not convex.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 7152L, "MSK_RES_ERR_TOCONIC_CONSTRAINT_RA", "The quadratic constraint has finite lower and upper bound, and therefore it is not convex.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 7155L, "MSK_RES_ERR_TOCONIC_OBJECTIVE_NOT_PSD", "The matrix defining the quadratric part of the objective function is not positive semidefinite.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1245L, "MSK_RES_ERR_TOO_SMALL_MAX_NUM_NZ", "The maximum number of non-zeros specified is too small.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1252L, "MSK_RES_ERR_TOO_SMALL_MAXNUMANZ", "The maximum number of non-zeros specified for A is smaller than the number of non-zeros in the
current A.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 3100L, "MSK_RES_ERR_UNB_STEP_SIZE", "A step size in an optimizer was unexpectedly unbounded. For instance, if the step-size becomes
unbounded in phase 1 of the simplex algorithm then an error occurs. Normally this will happen
only if the problem is badly formulated. Please contact MOSEK support if this error occurs.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1265L, "MSK_RES_ERR_UNDEF_SOLUTION", "MOSEK has the following solution types:
- an interior-point solution,
- an basic solution,
- and an integer solution.
Each optimizer may set one or more of these solutions; e.g by default a successful optimization
with the interior-point optimizer defines the interior-point solution, and, for linear problems, also
the basic solution. This error occurs when asking for a solution or for information about a solution
that is not defined.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1446L, "MSK_RES_ERR_UNDEFINED_OBJECTIVE_SENSE", "The objective sense has not been specified before the optimization.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 6010L, "MSK_RES_ERR_UNHANDLED_SOLUTION_STATUS", "Unhandled solution status.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1050L, "MSK_RES_ERR_UNKNOWN", "Unknown error.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1391L, "MSK_RES_ERR_UPPER_BOUND_IS_A_NAN", "The upper bound specified is not a number (nan).", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 6020L, "MSK_RES_ERR_UPPER_TRIANGLE", "An element in the upper triangle of a lower triangular matrix is specified.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1430L, "MSK_RES_ERR_USER_FUNC_RET", "An user function reported an error.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1431L, "MSK_RES_ERR_USER_FUNC_RET_DATA", "An user function returned invalid data.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1433L, "MSK_RES_ERR_USER_NLO_EVAL", "The user-defined nonlinear function reported an error.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1440L, "MSK_RES_ERR_USER_NLO_EVAL_HESSUBI", "The user-defined nonlinear function reported an invalid subscript in the Hessian.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1441L, "MSK_RES_ERR_USER_NLO_EVAL_HESSUBJ", "The user-defined nonlinear function reported an invalid subscript in the Hessian.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1432L, "MSK_RES_ERR_USER_NLO_FUNC", "The user-defined nonlinear function reported an error.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1238L, "MSK_RES_ERR_WHICHITEM_NOT_ALLOWED", "whichitem is unacceptable.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1236L, "MSK_RES_ERR_WHICHSOL", "The solution defined by whichsol does not exists.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1158L, "MSK_RES_ERR_WRITE_LP_FORMAT", "Problem cannot be written as an LP file.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1161L, "MSK_RES_ERR_WRITE_LP_NON_UNIQUE_NAME", "An auto-generated name is not unique.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1153L, "MSK_RES_ERR_WRITE_MPS_INVALID_NAME", "An invalid name is created while writing an MPS file. Usually this will make the MPS file unread-
able.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1156L, "MSK_RES_ERR_WRITE_OPF_INVALID_VAR_NAME", "Empty variable names cannot be written to OPF files.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1166L, "MSK_RES_ERR_WRITING_FILE", "An error occurred while writing file", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 3600L, "MSK_RES_ERR_XML_INVALID_PROBLEM_TYPE", "The problem type is not supported by the XML format.", 1L)
    .ROI_plugin_add_status_code_to_db( solver, 1449L, "MSK_RES_ERR_Y_IS_UNDEFINED", "The solution item y is undefined.", 1L)


}