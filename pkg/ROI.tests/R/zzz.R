.onLoad <- function( libname, pkgname ) {
    ## LO-LC
    register_test(mksig("L", "L", "C", "V", "free", TRUE), "LP-01", test_lp_01)

    ## LO-LC-MI
    register_test(mksig("L", "L", "CI", "V", "free", TRUE), "MILP-01", test_milp_01)
    register_test(mksig("L", "L", "CI", "V", "free", TRUE), "MILP-02", test_milp_02)

    ## QO-LC
    register_test(mksig("Q", "L", "C", "V", "free", TRUE), "QP-01", test_qp_01)
}
