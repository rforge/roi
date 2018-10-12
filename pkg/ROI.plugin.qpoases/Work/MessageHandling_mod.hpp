TERMINAL_LIST_ELEMENT = -1,						/**< Terminal list element, internal usage only! */
SUCCESSFUL_RETURN = 0,							/**< Successful return. */
RET_DIV_BY_ZERO,		   						/**< Division by zero. */
RET_INDEX_OUT_OF_BOUNDS,						/**< Index out of bounds. */
RET_INVALID_ARGUMENTS,							/**< At least one of the arguments is invalid. */
RET_ERROR_UNDEFINED,							/**< Error number undefined. */
RET_WARNING_UNDEFINED,							/**< Warning number undefined. */
RET_INFO_UNDEFINED,								/**< Info number undefined. */
RET_EWI_UNDEFINED,								/**< Error/warning/info number undefined. */
RET_AVAILABLE_WITH_LINUX_ONLY,					/**< This function is available under Linux only. */
RET_UNKNOWN_BUG,								/**< The error occured is not yet known. */
RET_PRINTLEVEL_CHANGED,							/**< Print level changed. */
RET_NOT_YET_IMPLEMENTED,						/**< Requested function is not yet implemented in this version of qpOASES. */
RET_INDEXLIST_MUST_BE_REORDERD,					/**< Index list has to be reordered. (12) */
RET_INDEXLIST_EXCEEDS_MAX_LENGTH,				/**< Index list exceeds its maximal physical length. */
RET_INDEXLIST_CORRUPTED,						/**< Index list corrupted. */
RET_INDEXLIST_OUTOFBOUNDS,						/**< Physical index is out of bounds. */
RET_INDEXLIST_ADD_FAILED,						/**< Adding indices from another index set failed. */
RET_INDEXLIST_INTERSECT_FAILED,					/**< Intersection with another index set failed. */
RET_INDEX_ALREADY_OF_DESIRED_STATUS,			/**< Index is already of desired status. (18) */
RET_ADDINDEX_FAILED,							/**< Adding index to index set failed. */
RET_REMOVEINDEX_FAILED,							/**< Removing index from index set failed. */
RET_SWAPINDEX_FAILED,							/**< Cannot swap between different indexsets. */
RET_NOTHING_TO_DO,								/**< Nothing to do. */
RET_SETUP_BOUND_FAILED,							/**< Setting up bound index failed. */
RET_SETUP_CONSTRAINT_FAILED,					/**< Setting up constraint index failed. */
RET_MOVING_BOUND_FAILED,						/**< Moving bound between index sets failed. */
RET_MOVING_CONSTRAINT_FAILED,					/**< Moving constraint between index sets failed. */
RET_SHIFTING_FAILED,							/**< Shifting of bounds/constraints failed. */
RET_ROTATING_FAILED,							/**< Rotating of bounds/constraints failed. */
RET_QPOBJECT_NOT_SETUP,							/**< The QP object has not been setup correctly, use another constructor. (29) */
RET_QP_ALREADY_INITIALISED,						/**< QProblem has already been initialised. */
RET_NO_INIT_WITH_STANDARD_SOLVER,				/**< Initialisation via extern QP solver is not yet implemented. */
RET_RESET_FAILED,								/**< Reset failed. */
RET_INIT_FAILED,								/**< Initialisation failed. */
RET_INIT_FAILED_TQ,								/**< Initialisation failed due to TQ factorisation. */
RET_INIT_FAILED_CHOLESKY,						/**< Initialisation failed due to Cholesky decomposition. */
RET_INIT_FAILED_HOTSTART,						/**< Initialisation failed! QP could not be solved! */
RET_INIT_FAILED_INFEASIBILITY,					/**< Initial QP could not be solved due to infeasibility! */
RET_INIT_FAILED_UNBOUNDEDNESS,					/**< Initial QP could not be solved due to unboundedness! */
RET_INIT_FAILED_REGULARISATION,					/**< Initialisation failed as Hessian matrix could not be regularised. */
RET_INIT_SUCCESSFUL,							/**< Initialisation done. */
RET_OBTAINING_WORKINGSET_FAILED,				/**< Failed to obtain working set for auxiliary QP. (40) */
RET_SETUP_WORKINGSET_FAILED,					/**< Failed to setup working set for auxiliary QP. */
RET_SETUP_AUXILIARYQP_FAILED,					/**< Failed to setup auxiliary QP for initialised homotopy. */
RET_NO_EXTERN_SOLVER,							/**< No extern QP solver available. */
RET_QP_UNBOUNDED,								/**< QP is unbounded. */
RET_QP_INFEASIBLE,								/**< QP is infeasible. */
RET_QP_NOT_SOLVED,								/**< Problems occured while solving QP with standard solver. */
RET_QP_SOLVED,									/**< QP successfully solved. */
RET_UNABLE_TO_SOLVE_QP,							/**< Problems occured while solving QP. */
RET_INITIALISATION_STARTED,						/**< Starting problem initialisation. */
RET_HOTSTART_FAILED,							/**< Unable to perform homotopy due to internal error. (50) */
RET_HOTSTART_FAILED_TO_INIT,					/**< Unable to initialise problem. */
RET_HOTSTART_FAILED_AS_QP_NOT_INITIALISED,		/**< Unable to perform homotopy as previous QP is not solved. */
RET_ITERATION_STARTED,							/**< Iteration... */
RET_SHIFT_DETERMINATION_FAILED,					/**< Determination of shift of the QP data failed. */
RET_STEPDIRECTION_DETERMINATION_FAILED,			/**< Determination of step direction failed. */
RET_STEPLENGTH_DETERMINATION_FAILED,			/**< Determination of step direction failed. */
RET_OPTIMAL_SOLUTION_FOUND,						/**< Optimal solution of neighbouring QP found. */
RET_HOMOTOPY_STEP_FAILED,						/**< Unable to perform homotopy step. */
RET_HOTSTART_STOPPED_INFEASIBILITY,				/**< Premature homotopy termination because QP is infeasible. */
RET_HOTSTART_STOPPED_UNBOUNDEDNESS,				/**< Premature homotopy termination because QP is unbounded. (60) */
RET_WORKINGSET_UPDATE_FAILED,					/**< Unable to update working sets according to initial guesses. */
RET_MAX_NWSR_REACHED,							/**< Maximum number of working set recalculations performed. */
RET_CONSTRAINTS_NOT_SPECIFIED,					/**< Problem does comprise constraints! You also have to specify new constraints' bounds. */
RET_INVALID_FACTORISATION_FLAG,					/**< Invalid factorisation flag. */
RET_UNABLE_TO_SAVE_QPDATA,						/**< Unable to save QP data. */
RET_STEPDIRECTION_FAILED_TQ,					/**< Abnormal termination due to TQ factorisation. */
RET_STEPDIRECTION_FAILED_CHOLESKY,				/**< Abnormal termination due to Cholesky factorisation. */
RET_CYCLING_DETECTED,							/**< Cycling detected. */
RET_CYCLING_NOT_RESOLVED,						/**< Cycling cannot be resolved, QP probably infeasible. */
RET_CYCLING_RESOLVED,							/**< Cycling probably resolved. (70) */
RET_STEPSIZE,									/**< For displaying performed stepsize. */
RET_STEPSIZE_NONPOSITIVE,						/**< For displaying non-positive stepsize. */
RET_SETUPSUBJECTTOTYPE_FAILED,					/**< Setup of SubjectToTypes failed. */
RET_ADDCONSTRAINT_FAILED,						/**< Addition of constraint to working set failed. */
RET_ADDCONSTRAINT_FAILED_INFEASIBILITY,			/**< Addition of constraint to working set failed (due to QP infeasibility). */
RET_ADDBOUND_FAILED,							/**< Addition of bound to working set failed. */
RET_ADDBOUND_FAILED_INFEASIBILITY,				/**< Addition of bound to working set failed (due to QP infeasibility). */
RET_REMOVECONSTRAINT_FAILED,					/**< Removal of constraint from working set failed. */
RET_REMOVEBOUND_FAILED,							/**< Removal of bound from working set failed. */
RET_REMOVE_FROM_ACTIVESET,						/**< Removing from active set... (80) */
RET_ADD_TO_ACTIVESET,							/**< Adding to active set... */
RET_REMOVE_FROM_ACTIVESET_FAILED,				/**< Removing from active set failed. */
RET_ADD_TO_ACTIVESET_FAILED,					/**< Adding to active set failed. */
RET_CONSTRAINT_ALREADY_ACTIVE,					/**< Constraint is already active. */
RET_ALL_CONSTRAINTS_ACTIVE,						/**< All constraints are active, no further constraint can be added. */
RET_LINEARLY_DEPENDENT,							/**< New bound/constraint is linearly dependent. */
RET_LINEARLY_INDEPENDENT,						/**< New bound/constraint is linearly independent. */
RET_LI_RESOLVED,								/**< Linear indepence of active contraint matrix successfully resolved. */
RET_ENSURELI_FAILED,							/**< Failed to ensure linear indepence of active contraint matrix. */
RET_ENSURELI_FAILED_TQ,							/**< Abnormal termination due to TQ factorisation. (90) */
RET_ENSURELI_FAILED_NOINDEX,					/**< QP is infeasible. */
RET_ENSURELI_FAILED_CYCLING,					/**< QP is infeasible. */
RET_BOUND_ALREADY_ACTIVE,						/**< Bound is already active. */
RET_ALL_BOUNDS_ACTIVE,							/**< All bounds are active, no further bound can be added. */
RET_CONSTRAINT_NOT_ACTIVE,						/**< Constraint is not active. */
RET_BOUND_NOT_ACTIVE,							/**< Bound is not active. */
RET_HESSIAN_NOT_SPD,							/**< Projected Hessian matrix not positive definite. */
RET_HESSIAN_INDEFINITE,							/**< Hessian matrix is indefinite. */
RET_MATRIX_SHIFT_FAILED,						/**< Unable to update matrices or to transform vectors. */
RET_MATRIX_FACTORISATION_FAILED,				/**< Unable to calculate new matrix factorisations. (100) */
RET_PRINT_ITERATION_FAILED,						/**< Unable to print information on current iteration. */
RET_NO_GLOBAL_MESSAGE_OUTPUTFILE,				/**< No global message output file initialised. */
RET_DISABLECONSTRAINTS_FAILED,					/**< Unable to disbable constraints. */
RET_ENABLECONSTRAINTS_FAILED,					/**< Unable to enbable constraints. */
RET_ALREADY_ENABLED,							/**< Bound or constraint is already enabled. */
RET_ALREADY_DISABLED,							/**< Bound or constraint is already disabled. */
RET_NO_HESSIAN_SPECIFIED, 						/**< No Hessian matrix has been specified. */
RET_USING_REGULARISATION,						/**< Using regularisation as Hessian matrix is not positive definite. */
RET_EPS_MUST_BE_POSITVE,						/**< Eps for regularisation must be sufficiently positive. */
RET_REGSTEPS_MUST_BE_POSITVE, 					/**< Maximum number of regularisation steps must be non-negative. (110) */
RET_HESSIAN_ALREADY_REGULARISED,				/**< Hessian has been already regularised. */
RET_CANNOT_REGULARISE_IDENTITY,					/**< Identity Hessian matrix cannot be regularised. */
RET_CANNOT_REGULARISE_SPARSE,					/**< Sparse matrix cannot be regularised as diagonal entry is missing. */
RET_NO_REGSTEP_NWSR,							/**< No additional regularisation step could be performed due to limits. */
RET_FEWER_REGSTEPS_NWSR,						/**< Fewer additional regularisation steps have been performed due to limits. */
RET_CHOLESKY_OF_ZERO_HESSIAN, 					/**< Cholesky decomposition of (unregularised) zero Hessian matrix. */
RET_CONSTRAINTS_ARE_NOT_SCALED, 				/**< When defining __MANY_CONSTRAINTS__, l1 norm of each constraint must be not greater than one. */
RET_ERROR_IN_CONSTRAINTPRODUCT,					/**< Error in user-defined constraint product function. */
RET_UPDATEMATRICES_FAILED,						/**< Unable to update QP matrices. */
RET_UPDATEMATRICES_FAILED_AS_QP_NOT_SOLVED,		/**< Unable to update matrices as previous QP is not solved. */
RET_UNABLE_TO_OPEN_FILE,						/**< Unable to open file. (120) */
RET_UNABLE_TO_WRITE_FILE,						/**< Unable to write into file. */
RET_UNABLE_TO_READ_FILE,						/**< Unable to read from file. */
RET_FILEDATA_INCONSISTENT,						/**< File contains inconsistent data. */
RET_UNABLE_TO_ANALYSE_QPROBLEM, 				/**< Unable to analyse (S)QProblem(B) object. */
RET_NWSR_SET_TO_ONE,							/**< Maximum number of working set changes was set to 1. */
RET_UNABLE_TO_READ_BENCHMARK,					/**< Unable to read benchmark data. */
RET_BENCHMARK_ABORTED,							/**< Benchmark aborted. */
RET_INITIAL_QP_SOLVED,							/**< Initial QP solved. */
RET_QP_SOLUTION_STARTED,						/**< Solving QP... */
RET_BENCHMARK_SUCCESSFUL,						/**< Benchmark terminated successfully. (130) */
RET_NO_DIAGONAL_AVAILABLE,						/**< Sparse matrix does not have entries on full diagonal. */
RET_ENSURELI_DROPPED