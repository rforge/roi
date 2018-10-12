#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include "scs/include/scs.h"
#include "scs/include/util.h"
#include "scs/linsys/amatrix.h"

SEXP getListElement(SEXP list, const char *str) {
  SEXP elmt = R_NilValue, names = getAttrib(list, R_NamesSymbol);
  for (int i = 0; i < length(list); i++) {
    if(strcmp(CHAR(STRING_ELT(names, i)), str) == 0) {
      elmt = VECTOR_ELT(list, i);
      break;
    }
  }
  return elmt;
}

SEXP floatVec2R(scs_int n, scs_float * in) {
    SEXP ret;
    scs_int i;
    scs_float * vec;

    PROTECT(ret = allocVector(REALSXP, n));
    vec = REAL(ret);
    for (i = 0; i < n; i++) {
        vec[i] = in[i];
    }
    return ret;
}

scs_float getFloatFromListWithDefault(SEXP list, const char *str, scs_float def) {
    SEXP val = getListElement(list, str);
    if (val == R_NilValue) {
        return def;
    }
    val = coerceVector(val, REALSXP);
    return REAL(val)[0];
}

scs_int getIntFromListWithDefault(SEXP list, const char *str, scs_int def) {
    SEXP val = getListElement(list, str);
    if (val == R_NilValue) {
        return def;
    }
    val = coerceVector(val, INTSXP);
    return INTEGER(val)[0];
}

scs_float * getFloatVectorFromList(SEXP list, const char *str, scs_int * len) {
    SEXP vec = getListElement(list, str);
    *len = length(vec);
    vec = coerceVector(vec, REALSXP);
    return REAL(vec);
}

scs_int * getIntVectorFromList(SEXP list, const char *str, scs_int * len) {
    SEXP vec = getListElement(list, str);
    *len = length(vec);
    vec = coerceVector(vec, INTSXP);
    return INTEGER(vec);
}


SEXP populateInfoR(Info * info, scs_int * num_protectaddr) {
    scs_int num_protected = 0;
    SEXP infor, info_names, iter_r, status_r, statusVal_r, pobj_r, dobj_r, resPri_r,
         resDual_r, resInfeas_r, resUnbdd_r, relGap_r, setupTime_r, solveTime_r;

    PROTECT(infor = NEW_LIST(12)); num_protected++;
    PROTECT(info_names = NEW_CHARACTER(12)); num_protected++;
    SET_NAMES(infor, info_names);

    PROTECT(iter_r = allocVector(INTSXP, 1)); num_protected++;
    INTEGER(iter_r)[0] = info->iter;
    SET_STRING_ELT(info_names, 0, mkChar("iter"));
    SET_VECTOR_ELT(infor, 0, iter_r); 

    PROTECT(status_r = NEW_CHARACTER(1)); num_protected++;
    SET_STRING_ELT(status_r, 0, mkChar(info->status));
    SET_STRING_ELT(info_names, 1, mkChar("status"));
    SET_VECTOR_ELT(infor, 1, status_r); 

    PROTECT(statusVal_r = allocVector(INTSXP, 1)); num_protected++;
    INTEGER(statusVal_r)[0] = info->statusVal;
    SET_STRING_ELT(info_names, 2, mkChar("statusVal"));
    SET_VECTOR_ELT(infor, 2, statusVal_r); 

    PROTECT(pobj_r = allocVector(REALSXP, 1)); num_protected++;
    REAL(pobj_r)[0] = info->pobj;
    SET_STRING_ELT(info_names, 3, mkChar("pobj"));
    SET_VECTOR_ELT(infor, 3, pobj_r); 

    PROTECT(dobj_r = allocVector(REALSXP, 1)); num_protected++;
    REAL(dobj_r)[0] = info->dobj;
    SET_STRING_ELT(info_names, 4, mkChar("dobj"));
    SET_VECTOR_ELT(infor, 4, dobj_r); 

    PROTECT(resPri_r = allocVector(REALSXP, 1)); num_protected++;
    REAL(resPri_r)[0] = info->resPri;
    SET_STRING_ELT(info_names, 5, mkChar("resPri"));
    SET_VECTOR_ELT(infor, 5, resPri_r); 

    PROTECT(resDual_r = allocVector(REALSXP, 1)); num_protected++;
    REAL(resDual_r)[0] = info->resDual;
    SET_STRING_ELT(info_names, 6, mkChar("resDual"));
    SET_VECTOR_ELT(infor, 6, resDual_r); 

    PROTECT(resInfeas_r = allocVector(REALSXP, 1)); num_protected++;
    REAL(resInfeas_r)[0] = info->resInfeas;
    SET_STRING_ELT(info_names, 7, mkChar("resInfeas"));
    SET_VECTOR_ELT(infor, 7, resInfeas_r); 

    PROTECT(resUnbdd_r = allocVector(REALSXP, 1)); num_protected++;
    REAL(resUnbdd_r)[0] = info->resUnbdd;
    SET_STRING_ELT(info_names, 8, mkChar("resUnbdd"));
    SET_VECTOR_ELT(infor, 8, resUnbdd_r); 

    PROTECT(relGap_r = allocVector(REALSXP, 1)); num_protected++;
    REAL(relGap_r)[0] = info->relGap;
    SET_STRING_ELT(info_names, 9, mkChar("relGap"));
    SET_VECTOR_ELT(infor, 9, relGap_r); 

    PROTECT(setupTime_r = allocVector(REALSXP, 1)); num_protected++;
    REAL(setupTime_r)[0] = info->setupTime;
    SET_STRING_ELT(info_names, 10, mkChar("setupTime"));
    SET_VECTOR_ELT(infor, 10, setupTime_r); 

    PROTECT(solveTime_r = allocVector(REALSXP, 1)); num_protected++;
    REAL(solveTime_r)[0] = info->solveTime;
    SET_STRING_ELT(info_names, 11, mkChar("solveTime"));
    SET_VECTOR_ELT(infor, 11, solveTime_r); 

    *num_protectaddr += num_protected;
    return infor;
}

SEXP scsr(SEXP data, SEXP cone, SEXP params) {
    scs_int len, num_protected = 0;
    SEXP ret, retnames, infor, xr, yr, sr;

    /* allocate memory */
    Data * d = scs_malloc(sizeof(Data));
    Cone * k = scs_malloc(sizeof(Cone));
    Settings * stgs = scs_malloc(sizeof(Settings));
    AMatrix * A = scs_malloc(sizeof(AMatrix));
    Info * info = scs_calloc(1, sizeof(Info));
    Sol * sol = scs_calloc(1, sizeof(Sol));

    d->b = getFloatVectorFromList(data, "b", &len);
    d->c = getFloatVectorFromList(data, "c", &len);
    d->n = getIntFromListWithDefault(data, "n", 0);
    d->m = getIntFromListWithDefault(data, "m", 0);

    A->m = d->m;
    A->n = d->n;
    A->x = getFloatVectorFromList(data, "Ax", &len);
    A->i = getIntVectorFromList(data, "Ai", &len);
    A->p = getIntVectorFromList(data, "Ap", &len);
    d->A = A;
  
    stgs->max_iters  = getIntFromListWithDefault(params, "max_iters", MAX_ITERS);
    stgs->normalize  = getIntFromListWithDefault(params, "normalize", NORMALIZE);
    stgs->verbose    = getIntFromListWithDefault(params, "verbose", VERBOSE);
    stgs->cg_rate    = getFloatFromListWithDefault(params, "cg_rate", CG_RATE);
    stgs->scale      = getFloatFromListWithDefault(params, "scale", SCALE);
    stgs->rho_x      = getFloatFromListWithDefault(params, "rho_x", RHO_X);
    stgs->alpha      = getFloatFromListWithDefault(params, "alpha", ALPHA);
    stgs->eps        = getFloatFromListWithDefault(params, "eps", EPS);
    /* TODO add warm starting */
    stgs->warm_start = getIntFromListWithDefault(params, "warm_start", WARM_START);
    d->stgs = stgs;

    k->f = getIntFromListWithDefault(cone, "f", 0);
    k->l = getIntFromListWithDefault(cone, "l", 0);
    k->ep = getIntFromListWithDefault(cone, "ep", 0);
    k->ed = getIntFromListWithDefault(cone, "ed", 0);
    k->q = getIntVectorFromList(cone, "q", &(k->qsize));
    k->s = getIntVectorFromList(cone, "s", &(k->ssize));
    k->p = getFloatVectorFromList(cone, "p", &(k->psize));
    
    /* solve! */
    scs(d, k, sol, info);

    infor = populateInfoR(info, &num_protected);

    PROTECT(ret = NEW_LIST(4)); num_protected++;
    PROTECT(retnames = NEW_CHARACTER(4)); num_protected++;
    SET_NAMES(ret, retnames);
    
    xr = floatVec2R(d->n, sol->x); num_protected++;
    yr = floatVec2R(d->m, sol->y); num_protected++;
    sr = floatVec2R(d->m, sol->s); num_protected++;
    
    SET_STRING_ELT(retnames, 0, mkChar("x"));
    SET_VECTOR_ELT(ret, 0, xr);
    SET_STRING_ELT(retnames, 1, mkChar("y"));
    SET_VECTOR_ELT(ret, 1, yr);
    SET_STRING_ELT(retnames, 2, mkChar("s"));
    SET_VECTOR_ELT(ret, 2, sr);
    SET_STRING_ELT(retnames, 3, mkChar("info"));
    SET_VECTOR_ELT(ret, 3, infor);
    
    /* free memory */
    scs_free(info);
    scs_free(d);
    scs_free(k);
    scs_free(stgs);
    scs_free(A);
    freeSol(sol);
    UNPROTECT(num_protected);
    return ret;
}

SEXP c_to_r_integer(int c_int){
    SEXP r_val;
    PROTECT(r_val = allocVector(INTSXP, 1));
    INTEGER(r_val)[0] = c_int;
    UNPROTECT(1);
    return r_val;
}

SEXP c_to_r_double(double c_double){
    SEXP r_val;
    PROTECT(r_val = allocVector(REALSXP, 1));
    REAL(r_val)[0] = c_double;
    UNPROTECT(1);
    return r_val;
}

void Rscs_finalize_data(Data *d) {
	scs_free(d);
}

void Rscs_finalize_cone(Cone *k) {
	scs_free(k);
}

void Rscs_finalize_settings(Settings *s) {
	scs_free(s);
}

void Rscs_finalize_matrix(AMatrix *mat) {
	scs_free(mat);
}

void Rscs_finalize_info(Info *in) {
	scs_free(in);
}

void Rscs_finalize_solution(Sol *solution) {
	freeSol(solution);
}

// Data
SEXP Rscs_new_data(void) {
	SEXP x, class_info;
	Data * d = scs_malloc(sizeof(Data));
	d->m = 0;
	d->n = 0;
	d->c = NULL;
	d->A = NULL;
	d->b = NULL;
	d->stgs = NULL;
	x = PROTECT(R_MakeExternalPtr(d, R_NilValue, R_NilValue));
	R_RegisterCFinalizerEx(x, Rscs_finalize_data, TRUE);
	
	PROTECT(class_info = allocVector(STRSXP, 1));
    SET_STRING_ELT(class_info, 0, mkChar("scs_data"));
    setAttrib(x, install("class"), class_info);
    UNPROTECT(2);
    return x;
}

// Cone
SEXP Rscs_new_cone(void) {
	SEXP x, class_info;
	Cone * d = scs_malloc(sizeof(Cone));
	x = PROTECT(R_MakeExternalPtr(d, R_NilValue, R_NilValue));
	R_RegisterCFinalizerEx(x, Rscs_finalize_cone, TRUE);
	
	PROTECT(class_info = allocVector(STRSXP, 1));
    SET_STRING_ELT(class_info, 0, mkChar("scs_cone"));
    setAttrib(x, install("class"), class_info);
    UNPROTECT(2);
    return x;
}

// Settings
SEXP Rscs_new_settings(void) {
	SEXP x, class_info;
	Settings * d = scs_malloc(sizeof(Settings));
	x = PROTECT(R_MakeExternalPtr(d, R_NilValue, R_NilValue));
	R_RegisterCFinalizerEx(x, Rscs_finalize_settings, TRUE);
	
	PROTECT(class_info = allocVector(STRSXP, 1));
    SET_STRING_ELT(class_info, 0, mkChar("scs_settings"));
    setAttrib(x, install("class"), class_info);
    UNPROTECT(2);
    return x;
}

// AMatrix
SEXP Rscs_new_matrix(void) {
	SEXP x, class_info;
	AMatrix * d = scs_malloc(sizeof(AMatrix));
	x = PROTECT(R_MakeExternalPtr(d, R_NilValue, R_NilValue));
	R_RegisterCFinalizerEx(x, Rscs_finalize_data, TRUE);
	
	PROTECT(class_info = allocVector(STRSXP, 1));
    SET_STRING_ELT(class_info, 0, mkChar("scs_matrix"));
    setAttrib(x, install("class"), class_info);
    UNPROTECT(2);
    return x;
}

// Info
SEXP Rscs_new_info(void) {
	SEXP x, class_info;
	Info * d = scs_malloc(sizeof(Info));
	x = PROTECT(R_MakeExternalPtr(d, R_NilValue, R_NilValue));
	R_RegisterCFinalizerEx(x, Rscs_finalize_data, TRUE);
	
	PROTECT(class_info = allocVector(STRSXP, 1));
    SET_STRING_ELT(class_info, 0, mkChar("scs_info"));
    setAttrib(x, install("class"), class_info);
    UNPROTECT(2);
    return x;
}

// Sol
SEXP Rscs_new_solution(void) {
	SEXP x, class_info;
	Sol * d = scs_malloc(sizeof(Sol));
	x = PROTECT(R_MakeExternalPtr(d, R_NilValue, R_NilValue));
	R_RegisterCFinalizerEx(x, Rscs_finalize_solution, TRUE);
	
	PROTECT(class_info = allocVector(STRSXP, 1));
    SET_STRING_ELT(class_info, 0, mkChar("scs_data"));
    setAttrib(x, install("class"), class_info);
    UNPROTECT(2);
    return x;
}

SEXP Rscs_get_params(SEXP r_params) {
	SEXP r_list;
	Settings *params = R_ExternalPtrAddr(r_params);
	
	PROTECT(r_list = allocVector(VECSXP, 8));
	
	SET_VECTOR_ELT(r_list, 0, c_to_r_integer(params->max_iters));
	SET_VECTOR_ELT(r_list, 1, c_to_r_integer(params->normalize));
	SET_VECTOR_ELT(r_list, 2, c_to_r_integer(params->verbose));
	
	SET_VECTOR_ELT(r_list, 3, c_to_r_double(params->cg_rate));
	SET_VECTOR_ELT(r_list, 4, c_to_r_double(params->scale));
	SET_VECTOR_ELT(r_list, 5, c_to_r_double(params->rho_x));
	SET_VECTOR_ELT(r_list, 6, c_to_r_double(params->alpha));
	SET_VECTOR_ELT(r_list, 7, c_to_r_double(params->eps));
	
	UNPROTECT(1);
	
	return r_list;
}

SEXP Rscs_set_params(SEXP r_params, SEXP max_iters, SEXP normalize, SEXP verbose,
                     SEXP cg_rate, SEXP scale, SEXP rho_x, SEXP alpha, SEXP eps) {
	Settings *params = R_ExternalPtrAddr(r_params);
	
	params->max_iters  = INTEGER(max_iters)[0];
    params->normalize  = INTEGER(normalize)[0];
    params->verbose    = INTEGER(verbose)[0];
    params->cg_rate    = REAL(cg_rate)[0];
    params->scale      = REAL(scale)[0];
    params->rho_x      = REAL(rho_x)[0];
    params->alpha      = REAL(alpha)[0];
    params->eps        = REAL(eps)[0];
	
	return R_NilValue;
}

SEXP Rscs_data_get_matrix(SEXP r_dat) {
	Data *dat = R_ExternalPtrAddr(r_dat);
	if ( dat->A == NULL )
		return R_NilValue;
	
	AMatrix *A = dat->A;
	SEXP r_A;
	PROTECT(r_A = allocVector(VECSXP, 5));
	SET_VECTOR_ELT(r_A, 0, c_to_r_integer(A->m));
	SET_VECTOR_ELT(r_A, 1, c_to_r_integer(A->n));
	
	// TODO:
    
    return A;    
}

SEXP Rscs_data_set_matrix(SEXP r_dat, SEXP A_nrow, SEXP A_ncol, 
                          SEXP A_x, SEXP A_i, SEXP A_p) {
	Data *dat = R_ExternalPtrAddr(r_dat);
	if ( dat->A != NULL )
		free(A);
		
	AMatrix * A = scs_malloc(sizeof(AMatrix));
	A->m = INTEGER(A_nrow)[0];
	A->n = INTEGER(A_ncol)[0];
	A->x = REAL(A_x);
	A->i = INTEGER(A_i);
	A->p = INTEGER(A_p);
	
	return R_NilValue;
}

// returns null since anhow the values in the pointers changed
SEXP Rscs_solve(SEXP r_dat, SEXP r_cones, SEXP r_solution, SEXP r_info) {
	Data      *dat = R_ExternalPtrAddr(r_dat);
	Cone    *cones = R_ExternalPtrAddr(r_cones);
	Sol  *solution = R_ExternalPtrAddr(r_solution);
	Settings *info = R_ExternalPtrAddr(r_info);
	  
	scs(dat, cones, solution, info);
	
	return R_NilValue;
}
