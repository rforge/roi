.onLoad <- function( libname, pkgname ) {
    ## LO-LC
    register_test(mksig("L", "L", "C", "V", "X", TRUE), "LP-01", test_lp_01)
    register_test(mksig("L", "X", "C", "V", "X", TRUE), "LP-02", test_lp_02)
    register_test(mksig("L", "X", "C", "V", "X", TRUE), "LP-03", test_lp_03)

    ## LO-LC-MI
    register_test(mksig("L", "L", "CI", "V", "X", TRUE), "MILP-01", test_milp_01)
    register_test(mksig("L", "L", "CI", "V", "X", TRUE), "MILP-02", test_milp_02)

    ## LO-LC-CB
    register_test(mksig("L", "L", "C", "V", c("zero", "soc"), FALSE), "CP-01", test_cp_01)
    register_test(mksig("L", "L", "C", "V", c("zero", "soc"), FALSE), "CP-02", test_cp_02)
    register_test(mksig("L", "L", "C", "V", c("zero", "expp"), FALSE), "CP-03", test_cp_03)
    register_test(mksig("L", "L", "C", "V", c("zero", "expp"), TRUE), "CP-04", test_cp_04)
    register_test(mksig("L", "L", "C", "V", c("zero", "expp"), TRUE), "CP-05", test_cp_05)
    register_test(mksig("L", "L", "C", "V", c("zero", "expd"), FALSE), "CP-06", test_cp_06)
    register_test(mksig("L", "L", "C", "V", c("zero", "powp"), TRUE), "CP-07", test_cp_07)
    register_test(mksig("L", "L", "C", "V", c("zero", "powp"), TRUE), "CP-08", test_cp_08)
    register_test(mksig("L", "L", "C", "V", c("zero", "psd"), FALSE), "CP-09", test_cp_09)

    ## QO-LC
    register_test(mksig("Q", "L", "C", "V", "X", FALSE), "QP-01", test_qp_01)
    register_test(mksig("Q", "L", "C", "V", "X", FALSE), "QP-02", test_qp_02)
    
    ## QO-QC
    register_test(mksig("Q", "Q", "C", "V", "X", FALSE), "QCQP-01", test_qcqp_01)

    ##                  OBJ  CON  TYP  BOU  CONE
    register_test(mksig("F", "X", "C", "V", "X", FALSE), "NLP-01", test_nlp_01)
    register_test(mksig("F", "F", "C", "V", "X", TRUE), "NLP-02", test_nlp_02)
    register_test(mksig("F", "F", "C", "V", "X", FALSE), "NLP-03", test_nlp_03)
    register_test(mksig("F", "F", "C", "V", "X", FALSE), "NLP-04", test_nlp_04)
    register_test(mksig("F", "X", "C", "V", "X", FALSE), "NLP-05", test_nlp_05)
    register_test(mksig("F", "X", "C", "V", "X", FALSE), "NLP-06", test_nlp_06)
}
