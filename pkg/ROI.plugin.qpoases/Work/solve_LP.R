
q("no")
Rdevel

library(qpoases)

model <- qp_new_model(3, 4, 0)

A <- 

	real_t A[1*2] = { 1.0, 1.0 };
	real_t g[2] = { 1.5, 1.0 };
	real_t lb[2] = { 0.5, -2.0 };
	real_t ub[2] = { 5.0, 2.0 };
	real_t lbA[1] = { -1.0 };
	real_t ubA[1] = { 2.0 };

	/* Setup data of second LP. */
	real_t g_new[2] = { 1.0, 1.5 };
	real_t lb_new[2] = { 0.0, -1.0 };
	real_t ub_new[2] = { 5.0, -0.5 };
	real_t lbA_new[1] = { -2.0 };
	real_t ubA_new[1] = { 1.0 };


	/* Setting up QProblem object with zero Hessian matrix. */
	QProblem example( 2,1,HST_ZERO );

	Options options;
 	//options.setToMPC();
	example.setOptions( options );

	/* Solve first LP. */
	int_t nWSR = 10;
	example.init( 0,g,A,lb,ub,lbA,ubA, nWSR,0 );

	/* Solve second LP. */
	nWSR = 10;
	example.hotstart( g_new,lb_new,ub_new,lbA_new,ubA_new, nWSR,0 );


	/* Get and print solution of second LP. */
	real_t xOpt[2];
	example.getPrimalSolution( xOpt );
	printf( "\nxOpt = [ %e, %e ];  objVal = %e\n\n", xOpt[0],xOpt[1],example.getObjVal() );

	return 0;


