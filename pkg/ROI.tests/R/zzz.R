.onLoad <- function( libname, pkgname ) {
    ## LO-LC
    register_test(mksig("L", "L", "C", "V", "free", TRUE), "LP-01", test_lp_01)

    ## LO-LC-MI
    register_test(mksig("L", "L", "CI", "V", "free", TRUE), "MILP-01", test_milp_01)
    register_test(mksig("L", "L", "CI", "V", "free", TRUE), "MILP-02", test_milp_02)

    ## LO-LC-CB
    register_test(mksig("L", "L", "C", "V", c("free", "soc"), FALSE), "CP-01", test_cp_01)
    register_test(mksig("L", "L", "C", "V", c("free", "soc"), FALSE), "CP-02", test_cp_02)
    register_test(mksig("L", "L", "C", "V", c("free", "expp"), FALSE), "CP-03", test_cp_03)
    register_test(mksig("L", "L", "C", "V", c("free", "expp"), TRUE), "CP-04", test_cp_04)
    register_test(mksig("L", "L", "C", "V", c("free", "expp"), TRUE), "CP-05", test_cp_05)
    register_test(mksig("L", "L", "C", "V", c("free", "expd"), FALSE), "CP-06", test_cp_06)
    register_test(mksig("L", "L", "C", "V", c("free", "powp"), TRUE), "CP-07", test_cp_07)
    register_test(mksig("L", "L", "C", "V", c("free", "powp"), TRUE), "CP-08", test_cp_08)
    register_test(mksig("L", "L", "C", "V", c("free", "psd"), FALSE), "CP-09", test_cp_09)

    ## QO-LC
    register_test(mksig("Q", "L", "C", "V", "free", FALSE), "QP-01", test_qp_01)

    ## QO-QC
    register_test(mksig("Q", "Q", "C", "V", "free", FALSE), "QCQP-01", test_qcqp_01)


}
