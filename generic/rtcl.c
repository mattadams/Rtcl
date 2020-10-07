/*
 * rtcl.c -- A minimal Tcl C extension for embedded R
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include <tcl.h>

#include <Rembedded.h>
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Parse.h>

#ifdef _WIN32
#include <windows.h>
#else
#if HAVE_UINTPTR_T
#include <stdint.h>
#endif
#if !defined(HAVE_UINTPTR_T) && !defined(uintptr_t)
typedef unsigned long uintptr_t;
#endif
#endif

#include <libR_dl.h>

/*
 * Mutex used during init of this module.
 */

TCL_DECLARE_MUTEX(rMutex)

/*
 * Handle of libR which is runtime linked during init.
 */

static Tcl_LoadHandle libR = NULL;

/*
 * Entry points into libvlc, runtime linked.
 */

struct libR_dl libR_dl = { 0 };

/*
 * For cleaning up errors returned from R
 */
void err_cleanup (char *dst, const char *src) {
  for(; *src; ++dst, ++src) {
    if (isspace(*src)) {
      *dst = ' ';
      
      while (isspace(*(src + 1))) {
        ++src;
      }
    } else {
      *dst = *src;
    }
  }
  
  *dst = '\0';
}

/*
 *----------------------------------------------------------------------
 *
 * RType --
 *
 *	Return the type of an R expression
 *
 * Results:
 *	RType takes an R SEXP as input, and returns a Tcl_Obj
 *	indicating the SEXP's type.
 *
 * Side effects:
 *	None
 *
 *----------------------------------------------------------------------
 */
static int
RType(
  Tcl_Interp *interp,
  Tcl_Obj *retObjPtr,
  SEXP s)
{
  switch(TYPEOF(s)) {

  case NILSXP:
    Tcl_SetStringObj(retObjPtr, "nil", -1);
    break;

  case SYMSXP:
    Tcl_SetStringObj(retObjPtr, "symbol", -1);
    break;

  case LISTSXP:
    Tcl_SetStringObj(retObjPtr, "list", -1);
    break;

  case CLOSXP:
    Tcl_SetStringObj(retObjPtr, "closure", -1);
    break;

  case ENVSXP:
    Tcl_SetStringObj(retObjPtr, "environment", -1);
    break;

  case PROMSXP:
    Tcl_SetStringObj(retObjPtr, "promise", -1);
    break;

  case LANGSXP:
    Tcl_SetStringObj(retObjPtr, "language", -1);
    break;

  case SPECIALSXP:
    Tcl_SetStringObj(retObjPtr, "special", -1);
    break;

  case BUILTINSXP:
    Tcl_SetStringObj(retObjPtr, "builtin", -1);
    break;

  case CHARSXP:
    Tcl_SetStringObj(retObjPtr, "char", -1);
    break;

  case LGLSXP:
    Tcl_SetStringObj(retObjPtr, "logical", -1);
    break;

  case INTSXP:
    Tcl_SetStringObj(retObjPtr, "int", -1);
    break;

  case REALSXP:
    Tcl_SetStringObj(retObjPtr, "real", -1);
    break;

  case CPLXSXP:
    Tcl_SetStringObj(retObjPtr, "complex", -1);
    break;

  case STRSXP:
    Tcl_SetStringObj(retObjPtr, "string", -1);
    break;

  case DOTSXP:
    Tcl_SetStringObj(retObjPtr, "...", -1);
    break;

  case ANYSXP:
    Tcl_SetStringObj(retObjPtr, "any", -1);
    break;

  case VECSXP:
    Tcl_SetStringObj(retObjPtr, "vector", -1);
    break;

  case EXPRSXP:
    Tcl_SetStringObj(retObjPtr, "expression", -1);
    break;

  case BCODESXP:
    Tcl_SetStringObj(retObjPtr, "bytecode", -1);
    break;

  case EXTPTRSXP:
    Tcl_SetStringObj(retObjPtr, "extpointer", -1);
    break;

  case WEAKREFSXP:
    Tcl_SetStringObj(retObjPtr, "weakref", -1);
    break;

  default:
    Tcl_SetStringObj(retObjPtr, "UNKNOWN", -1);
    break;
  }

  return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * RtoTcl --
 *
 *	Convert an R SEXP to something Tcl can deal with.
 *
 * Results:
 *	RtoTcl takes an R SEXP as input, and returns a roughly
 *	equivalent value as a Tcl_Obj. It returns TCL_OK if the
 *	conversion goes well, TCL_ERROR otherwise.
 *
 * Side effects:
 *	None
 *
 *----------------------------------------------------------------------
 */
static int
RtoTcl(
  Tcl_Interp *interp,		/* Tcl interp for error messages */
  Tcl_Obj **retObjPtrPtr,	/* Tcl object returned here */
  SEXP s)			/* R SEXP to translate */
{
  int i, l, retcode;
  Tcl_Obj **objv;
  char *str;
  Tcl_DString ds;

  retcode = TCL_OK;

  switch(TYPEOF(s)) {

  case NILSXP:
    *retObjPtrPtr = Tcl_NewStringObj("", -1);
    break;

  case SYMSXP:
    *retObjPtrPtr =
      Tcl_NewStringObj("No Tcl equivalent for R symbol expression", -1);
    retcode = TCL_ERROR;
    break;

  case LISTSXP:
    l = Rf_length(s);

    if (l == 0) {
      *retObjPtrPtr = Tcl_NewStringObj("", -1);
    } else if (l == 1) {
      return(RtoTcl(interp, retObjPtrPtr, CAR(s)));
    } else {
      objv = (Tcl_Obj **) Tcl_Alloc(l * sizeof(Tcl_Obj *));

      for (i = 0; s != NULL && s != R_NilValue; i++, s = CDR(s)) {
	RtoTcl(interp, &(objv[i]), CAR(s));
      }

      *retObjPtrPtr = Tcl_NewListObj(l, objv);

      Tcl_Free((ClientData) objv);
    }
    break;

  case CLOSXP:
    *retObjPtrPtr = Tcl_NewStringObj("No Tcl equivalent for R closures", -1);
    retcode = TCL_ERROR;
    break;

  case ENVSXP:
    *retObjPtrPtr = Tcl_NewStringObj("No Tcl equivalent for R environment", -1);
    retcode = TCL_ERROR;
    break;

  case PROMSXP:
    *retObjPtrPtr = Tcl_NewStringObj("No Tcl equivalent for R promises", -1);
    retcode = TCL_ERROR;
    break;

  case LANGSXP:
    *retObjPtrPtr =
      Tcl_NewStringObj("No Tcl equivalent for R language objects", -1);
    retcode = TCL_ERROR;
    break;

  case SPECIALSXP:
    *retObjPtrPtr =
      Tcl_NewStringObj("No Tcl equivalent for R special expressions", -1);
    retcode = TCL_ERROR;
    break;

  case BUILTINSXP:
    *retObjPtrPtr = Tcl_NewStringObj("No Tcl equivalent for R builtins", -1);
    retcode = TCL_ERROR;
    break;

  case CHARSXP:
    str = Tcl_ExternalToUtfDString(NULL, CHAR(s), -1, &ds);
    *retObjPtrPtr = Tcl_NewStringObj(str, Tcl_DStringLength(&ds));
    Tcl_DStringFree(&ds);
    break;

  case LGLSXP:
    if (LENGTH(s) == 1) {
      *retObjPtrPtr = Tcl_NewIntObj((int) (*LOGICAL(s)));
    } else {
      objv = (Tcl_Obj **) Tcl_Alloc(Rf_length(s) * sizeof(Tcl_Obj *));

      for (i = 0; i < LENGTH(s); i++) {
	objv[i] = Tcl_NewIntObj((int) (LOGICAL(s)[i]));
      }

      *retObjPtrPtr = Tcl_NewListObj(LENGTH(s), objv);
      Tcl_Free((ClientData) objv);
    }
    break;

  case INTSXP:
    if (LENGTH(s) == 1) {
      *retObjPtrPtr = Tcl_NewIntObj(*INTEGER(s));
    } else {
      objv = (Tcl_Obj **) Tcl_Alloc(LENGTH(s) * sizeof(Tcl_Obj *));

      for (i = 0; i < LENGTH(s); i++) {
	objv[i] = Tcl_NewIntObj(INTEGER(s)[i]);
      }

      *retObjPtrPtr = Tcl_NewListObj(LENGTH(s), objv);
      Tcl_Free((ClientData) objv);
    }
    break;

  case REALSXP:
    if (LENGTH(s) == 1) {
      *retObjPtrPtr = Tcl_NewDoubleObj(*REAL(s));
    } else {
      objv = (Tcl_Obj **) Tcl_Alloc(LENGTH(s) * sizeof(Tcl_Obj *));

      for (i = 0; i < LENGTH(s); i++) {
	objv[i] = Tcl_NewDoubleObj(REAL(s)[i]);
      }

      *retObjPtrPtr = Tcl_NewListObj(LENGTH(s), objv);
      Tcl_Free((ClientData) objv);
    }
    break;

  case CPLXSXP:
    *retObjPtrPtr =
      Tcl_NewStringObj("No Tcl equivalent for R complex numbers", -1);
    retcode = TCL_ERROR;
    break;

  case STRSXP:
    if (LENGTH(s) == 1) {
      str = Tcl_ExternalToUtfDString(NULL, CHAR(*STRING_PTR(s)), -1, &ds);
      *retObjPtrPtr = Tcl_NewStringObj(str, Tcl_DStringLength(&ds));
      Tcl_DStringFree(&ds);
    } else {
      objv = (Tcl_Obj **) Tcl_Alloc(Rf_length(s) * sizeof(Tcl_Obj *));

      for (i = 0; i < LENGTH(s); i++) {
	str = Tcl_ExternalToUtfDString(NULL, CHAR(STRING_PTR(s)[i]), -1, &ds);
	objv[i] = Tcl_NewStringObj(str, Tcl_DStringLength(&ds));
	Tcl_DStringFree(&ds);
      }

      *retObjPtrPtr = Tcl_NewListObj(LENGTH(s), objv);
      Tcl_Free((ClientData) objv);
    }
    break;

  case DOTSXP:
    *retObjPtrPtr =
      Tcl_NewStringObj("No Tcl equivalent for R dotted pairs", -1);
    retcode = TCL_ERROR;
    break;

  case ANYSXP:
    *retObjPtrPtr =
      Tcl_NewStringObj("No Tcl equivalent for R \"any\" type", -1);
    retcode = TCL_ERROR;
    break;

  case VECSXP:
    if (LENGTH(s) == 1) {
      i = RtoTcl(interp, retObjPtrPtr, VECTOR_ELT(s, 0));
      return i;
    } else {
      objv = (Tcl_Obj **) Tcl_Alloc(Rf_length(s) * sizeof(Tcl_Obj *));

      for (i = 0; i < LENGTH(s); i++) {
	/*@@@ Need to check return result @@@*/
	RtoTcl(interp, &(objv[i]), VECTOR_ELT(s, i));
      }

      *retObjPtrPtr = Tcl_NewListObj(LENGTH(s), objv);
      Tcl_Free((ClientData) objv);
    }

    break;

  case EXPRSXP:
    *retObjPtrPtr =
      Tcl_NewStringObj("No Tcl equivalent for R expressions", -1);
    retcode = TCL_ERROR;
    break;

  case BCODESXP:
    *retObjPtrPtr = Tcl_NewStringObj("No Tcl equivalent for R bytecode", -1);
    retcode = TCL_ERROR;
    break;

  case EXTPTRSXP:
    *retObjPtrPtr =
      Tcl_NewStringObj("No Tcl equivalent for R external pointers", -1);
    retcode = TCL_ERROR;
    break;

  case WEAKREFSXP:
    *retObjPtrPtr =
      Tcl_NewStringObj("No Tcl equivalent for R weak references", -1);
    retcode = TCL_ERROR;
    break;

  default:
    *retObjPtrPtr = Tcl_NewStringObj("Unknown S-expression type", -1);
    retcode = TCL_ERROR;
    break;
  }

  return retcode;
}

/*
 *----------------------------------------------------------------------
 *
 * REval --
 *
 *	Evaluate an R expression
 *
 * Results:
 *	REval takes an array of Tcl_Obj's, and evaluates them as R
 *	expressions. It returns a standard Tcl result.
 *
 * Side effects:
 *	May change the state of the R interpreter
 *
 *----------------------------------------------------------------------
 */
static int
REval(
  Tcl_Interp *interp,		/* For error messages */
  SEXP *s,		        /* S-Expression to put result in */
  int objc,		        /* Number of arguments */
  Tcl_Obj *CONST objv[])        /* Arguments */
{
  int i, l, evalError;
  ParseStatus status;
  SEXP text;
  SEXP exp, val;
  SEXP geterr, err;
  char *errmsg, *str;
  int offset, verbose;
  Tcl_DString ds;

  /* Check for verbose option */
  if (strcmp(Tcl_GetString(objv[1]), "-verbose") == 0) {
    if (objc < 3) {
      Tcl_WrongNumArgs(interp, 1, objv, "-verbose {R Expression}");
      return TCL_ERROR;
    }

    offset = 2;
    verbose = 1;
    objc -= 2;
  } else {
    offset = 1;
    verbose = 0;
    objc--;
  }

  /* Get the exp to evaluate */
  text = Rf_protect(Rf_allocVector(STRSXP, objc));

  for (i = 0; i < objc; i++ ) {
    str = Tcl_GetStringFromObj(objv[i+offset], &l);
    str = Tcl_UtfToExternalDString(NULL, str, l, &ds);
    SET_STRING_ELT(text, i, Rf_mkChar(str));
    Tcl_DStringFree(&ds);
  }

  /* Parse it */
  exp = Rf_protect(R_ParseVector(text, -1, &status, R_NilValue));

  if (status != PARSE_OK) {
    Rf_unprotect(2);
    Tcl_SetResult(interp, "R parse error", TCL_STATIC);
    return TCL_ERROR;
  }

  /* Evaluate it */
  l = Rf_length(exp);
  *s = R_NilValue;

  for (i = 0; i < l; i++) {
    if (verbose) {
      val = R_tryEval(VECTOR_ELT(exp, i), R_GlobalEnv, &evalError);
    } else {
      val = R_tryEvalSilent(VECTOR_ELT(exp, i), R_GlobalEnv, &evalError);
    }
    
    if (evalError) {
      Rf_unprotect(2);

      /* Retrieve error message from R */
      geterr = Rf_protect(Rf_lang1(Rf_install("geterrmessage")));
      err = R_tryEval(geterr, R_GlobalEnv, &evalError);
      Rf_unprotect(1);

      errmsg = (char *) CHAR(STRING_ELT(err, 0));
      errmsg = Tcl_ExternalToUtfDString(NULL, errmsg, -1, &ds);
      str = errmsg + Tcl_DStringLength(&ds);
      while (str > errmsg) {
	--str;
	if (*str == '\n' || *str == '\t') {
	  *str = ' ';
	}
      }
      errmsg[strlen(errmsg) - 1] = '\0';
      err_cleanup(errmsg, errmsg);
      Tcl_SetResult(interp, errmsg, TCL_VOLATILE);
      Tcl_DStringFree(&ds);
      
      return TCL_ERROR;
    } else if (verbose) {
      Rf_PrintValue(val);
    }
    
    *s = val;
  }

  Rf_unprotect(2);
  return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * Rtcl_eval --
 *
 *	Evaluate R expression(s)
 *
 * Results:
 *	A standard Tcl result
 *
 * Side effects:
 *	May change the state of the R interpreter, depending on what
 *	expression is evaluated.
 *
 *----------------------------------------------------------------------
 */
static int
Rtcl_eval(
  ClientData clientData,	/* Not used */
  Tcl_Interp *interp,		/* Current interpreter */
  int objc,			/* Number of arguments */
  Tcl_Obj *CONST objv[])	/* Arguments */
{
  SEXP ans;

  if (objc < 2) {
      Tcl_WrongNumArgs(interp, 1, objv, "?-verbose? {R Expression}");
      return TCL_ERROR;
  }

  /* Evaluate the args */
  if (REval(interp, &ans, objc, objv) != TCL_OK) {
    return TCL_ERROR;
  }

  /* Clean up and return */
  return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * Rtcl_rsource --
 *
 *	Source a file into the R environment.
 *
 * Results:
 *	A standard Tcl result
 *
 * Side effects:
 *	May change the state of the R interpreter, depending on what
 *	expression is evaluated.
 *
 *----------------------------------------------------------------------
 */
static int
Rtcl_source(
  ClientData clientData,	/* Not used */
  Tcl_Interp *interp,		/* Current interpreter */
  int objc,			/* Number of arguments */
  Tcl_Obj *CONST objv[])	/* Arguments */
{
  int evalError;
  SEXP exp, val;
  SEXP geterr, err;
  char *errmsg, *str;
  int offset, verbose;
  Tcl_Obj *pathObj;
  Tcl_DString ds;

  if (objc < 2) {
    Tcl_WrongNumArgs(interp, 1, objv, "?-verbose? {source_file.R}");
    return TCL_ERROR;
  }

  /* Check for verbose option */
  if (strcmp(Tcl_GetString(objv[1]), "-verbose") == 0) {
    if (objc < 3) {
      Tcl_WrongNumArgs(interp, 1, objv, "-verbose {source_file.R}");
      return TCL_ERROR;
    }

    offset = 2;
    verbose = 1;
    objc -= 2;
  } else {
    offset = 1;
    verbose = 0;
    objc--;
  }

  pathObj = Tcl_FSGetNormalizedPath(interp, objv[offset]);
  if (pathObj == NULL) {
    return TCL_ERROR;
  }
  Tcl_IncrRefCount(pathObj);
  str = Tcl_GetStringFromObj(pathObj, &offset);
  str = Tcl_UtfToExternalDString(NULL, str, offset, &ds);
  Tcl_DecrRefCount(pathObj);
  exp = Rf_protect(Rf_lang2(Rf_install("source"), Rf_mkString(str)));
  Tcl_DStringFree(&ds);

  if (verbose) {
    val = R_tryEval(exp, R_GlobalEnv, &evalError);
  } else {
    val = R_tryEvalSilent(exp, R_GlobalEnv, &evalError);
  }

  if (evalError) {
    Rf_unprotect(1);

    /* Retrieve error message from R */
    geterr = Rf_protect(Rf_lang1(Rf_install("geterrmessage")));
    err = R_tryEval(geterr, R_GlobalEnv, &evalError);
    Rf_unprotect(1);

    errmsg = (char *) CHAR(STRING_ELT(err, 0));
    errmsg = Tcl_ExternalToUtfDString(NULL, errmsg, -1, &ds);
    str = errmsg + Tcl_DStringLength(&ds);
    while (str > errmsg) {
      --str;
      if (*str == '\n' || *str == '\t') {
	*str = ' ';
      }
    }
    errmsg[strlen(errmsg) - 1] = '\0';
    err_cleanup(errmsg, errmsg);
    Tcl_SetResult(interp, errmsg, TCL_VOLATILE);
    Tcl_DStringFree(&ds);

    return TCL_ERROR;
  } else if (verbose) {
    Rf_PrintValue(val);
  }

  Rf_unprotect(1);
  return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * Rtcl_gettype --
 *
 *	Evaluate an R expression and return its type.
 *
 * Results:
 *	A standard Tcl result
 *
 * Side effects:
 *	May change the state of the R interpreter, depending on what
 *	expression is evaluated.
 *
 *----------------------------------------------------------------------
 */
static int
Rtcl_gettype(
  ClientData clientData,	/* Not used */
  Tcl_Interp *interp,		/* Current interpreter */
  int objc,			/* Number of arguments */
  Tcl_Obj *CONST objv[])	/* Arguments */
{
  SEXP ans;
  Tcl_Obj *r;

  if (objc < 2) {
    Tcl_WrongNumArgs(interp, 1, objv, "?-verbose? {R Expression}");
    return TCL_ERROR;
  }

  /* Evaluate the args */
  if (REval(interp, &ans, objc, objv) != TCL_OK) {
    return TCL_ERROR;
  }

  /* Return the result in an appropriate form */
  r = Tcl_NewObj();

  Tcl_IncrRefCount(r);

  if (RType(interp, r, ans) != TCL_OK) {
    Tcl_DecrRefCount(r);
    return TCL_ERROR;
  }

  /* Clean up and return */
  Tcl_SetObjResult(interp, r);
  Tcl_DecrRefCount(r);
  return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * Rtcl_getvalue --
 *
 *	Evaluate R expression and return its value.
 *
 * Results:
 *	A standard Tcl result
 *
 * Side effects:
 *	May change the state of the R interpreter, depending on what
 *	expression is evaluated.
 *
 *----------------------------------------------------------------------
 */
static int
Rtcl_getvalue(
  ClientData clientData,	/* Not used */
  Tcl_Interp *interp,		/* Current interpreter */
  int objc,			/* Number of arguments */
  Tcl_Obj *CONST objv[])	/* Arguments */
{
  SEXP ans;
  Tcl_Obj *r;

  if (objc < 2) {
    Tcl_WrongNumArgs(interp, 1, objv, "?-verbose? {R Expression}");
    return TCL_ERROR;
  }

  /* Evaluate the args */
  if (REval(interp, &ans, objc, objv) != TCL_OK) {
    return TCL_ERROR;
  }

  /* Return the result in an appropriate form */
  if (RtoTcl(interp, &r, ans) != TCL_OK) {
    return TCL_ERROR;
  }

  /* Clean up and return */
  Tcl_SetObjResult(interp, r);
  return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * Rtcl_teardown --
 *
 *	Exit handler: eventually tear down R environment.
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */
static void
Rtcl_teardown(ClientData clientData)
{
  int *initialized = (int *) clientData;

  Tcl_MutexLock(&rMutex);
  if (*initialized <= 0) {
    goto done;
  }
  *initialized = 0;
  if (Rf_endEmbeddedR != NULL) {
    Rf_endEmbeddedR(0);
  }
done:
  Tcl_MutexUnlock(&rMutex);
}

/*
 *----------------------------------------------------------------------
 *
 * Rtcl_Init --
 *
 *	Called when Tcl loads the extension to initialize the new
 *	package. The string "Rtcl" in the function name must match
 *	the PACKAGE declaration at the top of configure.ac.
 *
 * Results:
 *	A standard Tcl result
 *
 * Side effects:
 *	The Rtcl package is created.
 *      New namespace ::Rtcl is added.
 *      New commands "eval", "value", "source", "type" are added under ::Rtcl
 *
 *----------------------------------------------------------------------
 */
int DLLEXPORT Rtcl_Init(Tcl_Interp *interp)
{
  /* Init flag */
  static int initialized = 0;

  /* Rargv is the fake argument list we use to initialize R */
  char *Rargv[] = {"R", "--no-restore", "--slave"};

  /* Pointer to hold our own new namespace */
  Tcl_Namespace *nsPtr;

#ifdef USE_TCL_STUBS
  if (Tcl_InitStubs(interp, "8.5", 0) == NULL) {
    return TCL_ERROR;
  }
#else
  if (Tcl_PkgRequire(interp, "Tcl", "8.5", 0) == NULL) {
    return TCL_ERROR;
  }
#endif

  if (!initialized) {
    int errors;
    Tcl_Obj *nameobj;
#ifndef _WIN32
    int i, doenv;
    char *p;
    Tcl_DString ds;
    static const char *libnames[] = {
#ifdef __APPLE__
      "/usr/local/lib/R/lib/libR.dylib",
#else
      "/usr/local/lib/R/lib/libR.so",
      "/usr/lib64/R/lib/libR.so",
      "/usr/lib/R/lib/libR.so",
#endif
      NULL
    };
#endif

    Tcl_MutexLock(&rMutex);
    if (initialized) {
      goto initDone;
    }

#ifdef _WIN32
    /*
     * Tcl variable "r_dll" must have fully qualified path of
     * "R.dll", "env(R_HOME)" and "env(PATH)" must be already setup.
     */
    errors = 0;
    nameobj = Tcl_GetVar2Ex(interp, "r_dll", NULL, 0);
    if (nameobj == NULL) {
      initialized = -1;
      goto initDone;
    }
    Tcl_IncrRefCount(nameobj);
    if (Tcl_LoadFile(interp, nameobj, NULL, 0, NULL, &libR) != TCL_OK) {
      Tcl_DecrRefCount(nameobj);
      initialized = -1;
      goto initDone;
    }
    Tcl_DecrRefCount(nameobj);
#else
    i = errors = 0;
    doenv = (getenv("R_HOME") == NULL);
    if (!doenv) {
      p = Tcl_GetVar2(interp, "env", "R_HOME", 0);
      if (p != NULL) {
	Tcl_DStringInit(&ds);
	Tcl_DStringAppend(&ds, p, -1);
#ifdef __APPLE__
	Tcl_DStringAppend(&ds, "/lib/libR.dylib", -1);
#else
	Tcl_DStringAppend(&ds, "/lib/libR.so", -1);
#endif
	nameobj =
	  Tcl_NewStringObj(Tcl_DStringValue(&ds), Tcl_DStringLength(&ds));
	Tcl_DStringFree(&ds);
	Tcl_IncrRefCount(nameobj);
	if (Tcl_LoadFile(interp, nameobj, NULL, 0, NULL, &libR) == TCL_OK) {
	  Tcl_DecrRefCount(nameobj);
	  goto libLoaded;
	}
	Tcl_DecrRefCount(nameobj);
	doenv = 1;
      }
    }
    while (libnames[i] != NULL) {
      Tcl_ResetResult(interp);
      if (doenv) {
	Tcl_DStringInit(&ds);
	Tcl_DStringAppend(&ds, libnames[i], -1);
	p = strrchr(Tcl_DStringValue(&ds), '/');
	if (p != NULL) {
	  *p = '\0';
	  p = strrchr(Tcl_DStringValue(&ds), '/');
	  if (p != NULL) {
	    *p = '\0';
	  }
	  setenv("R_HOME", Tcl_DStringValue(&ds), 1);
	}
	Tcl_DStringFree(&ds);
      }
      nameobj = Tcl_NewStringObj(libnames[i], -1);
      Tcl_IncrRefCount(nameobj);
      if (Tcl_LoadFile(interp, nameobj, NULL, 0, NULL, &libR) == TCL_OK) {
	Tcl_DecrRefCount(nameobj);
	break;
      }
      if (doenv) {
	unsetenv("R_HOME");
      }
      Tcl_DecrRefCount(nameobj);
      ++i;
    }
    if (libnames[i] == NULL) {
      initialized = -1;
      goto initDone;
    }
libLoaded:
#endif

#define LIBR_SYM_FN(name)				\
    if (!errors) {					\
      libR_dl.m_ ## name = (fn_ ## name)		\
	Tcl_FindSymbol(NULL, libR, #name);		\
      if (libR_dl.m_ ## name == NULL) errors++;		\
    }

#define LIBR_SYM_TYPE(name, type)			\
    if (!errors) {					\
      libR_dl.m_ ## name = (type)			\
	Tcl_FindSymbol(NULL, libR, #name);		\
      if (libR_dl.m_ ## name == NULL) errors++;		\
    }

    LIBR_SYM_FN(CAR);
    LIBR_SYM_FN(CDR);
    LIBR_SYM_FN(INTEGER);
    LIBR_SYM_FN(LENGTH);
    LIBR_SYM_FN(LOGICAL);
    LIBR_SYM_FN(R_CHAR);
    LIBR_SYM_FN(REAL);
    LIBR_SYM_FN(Rf_allocVector);
    LIBR_SYM_FN(Rf_endEmbeddedR);
    LIBR_SYM_FN(Rf_eval);
    LIBR_SYM_FN(Rf_initialize_R);
    LIBR_SYM_FN(Rf_install);
    LIBR_SYM_FN(Rf_lang1);
    LIBR_SYM_FN(Rf_lang2);
    LIBR_SYM_FN(Rf_length);
    LIBR_SYM_FN(Rf_mkChar);
    LIBR_SYM_FN(Rf_mkString);
    LIBR_SYM_FN(Rf_PrintValue);
    LIBR_SYM_FN(Rf_protect);
    LIBR_SYM_FN(Rf_unprotect);
    LIBR_SYM_TYPE(R_CStackLimit, uintptr_t *);
    LIBR_SYM_TYPE(R_GlobalEnv, SEXP *);
    LIBR_SYM_TYPE(R_NilValue, SEXP *);
    LIBR_SYM_FN(R_ParseVector);
    LIBR_SYM_TYPE(R_SignalHandlers, int *);
    LIBR_SYM_FN(R_tryEval);
    LIBR_SYM_FN(SET_STRING_ELT);
    LIBR_SYM_FN(setup_Rmainloop);
    LIBR_SYM_FN(STRING_ELT);
    LIBR_SYM_FN(STRING_PTR);
    LIBR_SYM_FN(TYPEOF);
    LIBR_SYM_FN(VECTOR_ELT);

#undef LIBR_SYM_FN
#undef LIBR_SYM_TYPE

    if (errors) {
      if (libR != NULL) {
	Tcl_FSUnloadFile(interp, libR);
	Tcl_ResetResult(interp);
	libR = NULL;
      }
      initialized = -1;
    } else {
      /* Initialize R */
      Rf_initialize_R(sizeof(Rargv)/sizeof(Rargv[0]), Rargv);
      R_SignalHandlers = 0;
      R_CStackLimit = (uintptr_t) -1;
      setup_Rmainloop();
      initialized = 1;
      Tcl_CreateExitHandler(Rtcl_teardown, (ClientData) &initialized);
    }
initDone:
    Tcl_MutexUnlock(&rMutex);
  }
  if (initialized < 0) {
    Tcl_SetResult(interp, "cannot load libR", TCL_STATIC);
    return TCL_ERROR;
  }

  /* Create namespace */
  nsPtr = Tcl_CreateNamespace(interp, "::rtcl", NULL, NULL);

  if (nsPtr == NULL) {
    return TCL_ERROR;
  }

  /* Changed this to check for an error */
  if (Tcl_PkgProvide(interp, "Rtcl", "1.2") == TCL_ERROR) {
    return TCL_ERROR;
  }

  Tcl_CreateObjCommand(interp, "::rtcl::eval",
		       (Tcl_ObjCmdProc *) Rtcl_eval,
		       (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);

  Tcl_CreateObjCommand(interp, "::rtcl::source",
		       (Tcl_ObjCmdProc *) Rtcl_source,
		       (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);

  Tcl_CreateObjCommand(interp, "::rtcl::gettype",
		       (Tcl_ObjCmdProc *) Rtcl_gettype,
		       (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);

  Tcl_CreateObjCommand(interp, "::rtcl::getvalue",
		       (Tcl_ObjCmdProc *) Rtcl_getvalue,
		       (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);

  return TCL_OK;
}
