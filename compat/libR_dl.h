#ifndef _LIBR_DL_H
#define _LIBR_DL_H

typedef SEXP (*fn_CAR)(SEXP e);
typedef SEXP (*fn_CDR)(SEXP e);
typedef int *(*fn_INTEGER)(SEXP e);
typedef int (*fn_LENGTH)(SEXP e);
typedef int *(*fn_LOGICAL)(SEXP e);
typedef const char *(*fn_R_CHAR)(SEXP e);
typedef double *(*fn_REAL)(SEXP e);
typedef SEXP (*fn_Rf_allocVector)(SEXPTYPE t, R_xlen_t i);
typedef void (*fn_Rf_endEmbeddedR)(int i);
typedef SEXP (*fn_Rf_eval)(SEXP e, SEXP f);
typedef int (*fn_Rf_initialize_R)(int argc, char *argv[]);
typedef SEXP (*fn_Rf_install)(const char *str);
typedef SEXP (*fn_Rf_lang1)(SEXP e);
typedef SEXP (*fn_Rf_lang2)(SEXP e, SEXP f);
typedef R_len_t (*fn_Rf_length)(SEXP e);
typedef SEXP (*fn_Rf_mkChar)(const char *str);
typedef SEXP (*fn_Rf_mkString)(const char *str);
typedef void (*fn_Rf_PrintValue)(SEXP e);
typedef SEXP (*fn_Rf_protect)(SEXP e);
typedef void (*fn_Rf_unprotect)(int i);
/* extern uintptr_t: R_CStackLimit */
/* extern SEXP: R_GlobalEnv */
/* extern SEXP: R_NilValue */
typedef SEXP (*fn_R_ParseVector)(SEXP e, int i, ParseStatus *s, SEXP f);
/* extern int: R_SignalHandlers */
typedef SEXP (*fn_R_tryEval)(SEXP e, SEXP f, int *i);
typedef SEXP (*fn_SET_STRING_ELT)(SEXP e, R_xlen_t i, SEXP f);
typedef void (*fn_setup_Rmainloop)(void);
typedef SEXP (*fn_STRING_ELT)(SEXP e, R_xlen_t i);
typedef SEXP *(*fn_STRING_PTR)(SEXP e);
typedef int (*fn_TYPEOF)(SEXP e);
typedef SEXP (*fn_VECTOR_ELT)(SEXP e, R_xlen_t i);

struct libR_dl {
    fn_CAR m_CAR;
    fn_CDR m_CDR;
    fn_INTEGER m_INTEGER;
    fn_LENGTH m_LENGTH;
    fn_LOGICAL m_LOGICAL;
    fn_R_CHAR m_R_CHAR;
    fn_REAL m_REAL;
    fn_Rf_allocVector m_Rf_allocVector;
    fn_Rf_endEmbeddedR m_Rf_endEmbeddedR;
    fn_Rf_eval m_Rf_eval;
    fn_Rf_initialize_R m_Rf_initialize_R;
    fn_Rf_install m_Rf_install;
    fn_Rf_lang1 m_Rf_lang1;
    fn_Rf_lang2 m_Rf_lang2;
    fn_Rf_length m_Rf_length;
    fn_Rf_mkChar m_Rf_mkChar;
    fn_Rf_mkString m_Rf_mkString;
    fn_Rf_PrintValue m_Rf_PrintValue;
    fn_Rf_protect m_Rf_protect;
    fn_Rf_unprotect m_Rf_unprotect;
    uintptr_t *m_R_CStackLimit;
    SEXP *m_R_GlobalEnv;
    SEXP *m_R_NilValue;
    fn_R_ParseVector m_R_ParseVector;
    int *m_R_SignalHandlers;
    fn_R_tryEval m_R_tryEval;
    fn_SET_STRING_ELT m_SET_STRING_ELT;
    fn_setup_Rmainloop m_setup_Rmainloop;
    fn_STRING_ELT m_STRING_ELT;
    fn_STRING_PTR m_STRING_PTR;
    fn_TYPEOF m_TYPEOF;
    fn_VECTOR_ELT m_VECTOR_ELT;
};

extern struct libR_dl libR_dl;

#define CAR libR_dl.m_CAR
#define CDR libR_dl.m_CDR
#define INTEGER libR_dl.m_INTEGER
#define LENGTH libR_dl.m_LENGTH
#define LOGICAL libR_dl.m_LOGICAL
#define R_CHAR libR_dl.m_R_CHAR
#define REAL libR_dl.m_REAL
#define Rf_allocVector libR_dl.m_Rf_allocVector
#define Rf_endEmbeddedR libR_dl.m_Rf_endEmbeddedR
#define Rf_eval libR_dl.m_Rf_eval
#define Rf_initialize_R libR_dl.m_Rf_initialize_R
#define Rf_install libR_dl.m_Rf_install
#define Rf_lang1 libR_dl.m_Rf_lang1
#define Rf_lang2 libR_dl.m_Rf_lang2
#define Rf_length libR_dl.m_Rf_length
#define Rf_mkChar libR_dl.m_Rf_mkChar
#define Rf_mkString libR_dl.m_Rf_mkString
#define Rf_PrintValue libR_dl.m_Rf_PrintValue
#define Rf_protect libR_dl.m_Rf_protect
#define Rf_unprotect libR_dl.m_Rf_unprotect
#define R_CStackLimit *libR_dl.m_R_CStackLimit
#define R_GlobalEnv *libR_dl.m_R_GlobalEnv
#define R_NilValue *libR_dl.m_R_NilValue
#define R_ParseVector libR_dl.m_R_ParseVector
#define R_SignalHandlers *libR_dl.m_R_SignalHandlers
#define R_tryEval libR_dl.m_R_tryEval
#define SET_STRING_ELT libR_dl.m_SET_STRING_ELT
#define setup_Rmainloop libR_dl.m_setup_Rmainloop
#define STRING_ELT libR_dl.m_STRING_ELT
#define STRING_PTR libR_dl.m_STRING_PTR
#define TYPEOF libR_dl.m_TYPEOF
#define VECTOR_ELT libR_dl.m_VECTOR_ELT

#endif /* _LIBR_DL_H */
