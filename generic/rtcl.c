/*
 * rtcl.c -- A minimal Tcl C extension for embedded R
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <tcl.h>

#include <Rembedded.h>
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Parse.h>

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
RType(interp, retObjPtr, s)
     Tcl_Interp *interp;
     Tcl_Obj *retObjPtr;
     SEXP s;
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
RtoTcl(interp, retObjPtrPtr, s)
     Tcl_Interp *interp;		/* Tcl interp. for error messages */
     Tcl_Obj **retObjPtrPtr;	        /* Tcl object returned here	  */
     SEXP s;			        /* R SEXP to translate		  */
{
  int i, l, retcode;
  Tcl_Obj **objv;

  retcode = TCL_OK;

  switch(TYPEOF(s)) {
    
  case NILSXP:
    *retObjPtrPtr = Tcl_NewStringObj("", -1);
    break;

  case SYMSXP:
    *retObjPtrPtr = Tcl_NewStringObj("No Tcl equivalent for R symbol expression", -1);
    retcode = TCL_ERROR;
    break;

  case LISTSXP:
    l = length(s);
    
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
    *retObjPtrPtr = Tcl_NewStringObj("No Tcl equivalent for R language objects", -1);
    retcode = TCL_ERROR;
    break;

  case SPECIALSXP:
    *retObjPtrPtr = Tcl_NewStringObj("No Tcl equivalent for R special expressions", -1);
    retcode = TCL_ERROR;
    break;

  case BUILTINSXP:
    *retObjPtrPtr = Tcl_NewStringObj("No Tcl equivalent for R builtins", -1);
    retcode = TCL_ERROR;
    break;

  case CHARSXP:
    *retObjPtrPtr = Tcl_NewStringObj(CHAR(s), -1);
    break;

  case LGLSXP:
    if (LENGTH(s) == 1) {
      *retObjPtrPtr = Tcl_NewIntObj((int) (*LOGICAL(s)));
    } else {
      objv = (Tcl_Obj **) Tcl_Alloc(length(s) * sizeof(Tcl_Obj *));
      
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
    *retObjPtrPtr = Tcl_NewStringObj("No Tcl equivalent for R complex numbers", -1);
    retcode = TCL_ERROR;
    break;

  case STRSXP:
    if (LENGTH(s) == 1) {
      *retObjPtrPtr = Tcl_NewStringObj(CHAR(*STRING_PTR(s)), -1);
    } else {
      objv = (Tcl_Obj **) Tcl_Alloc(length(s) * sizeof(Tcl_Obj *));
      
      for (i = 0; i < LENGTH(s); i++) {
	objv[i] = Tcl_NewStringObj(CHAR(STRING_PTR(s)[i]), -1);
      }
      
      *retObjPtrPtr = Tcl_NewListObj(LENGTH(s), objv);
      Tcl_Free((ClientData) objv);
    }
    break;

  case DOTSXP:
    *retObjPtrPtr = Tcl_NewStringObj("No Tcl equivalent for R dotted pairs", -1);
    retcode = TCL_ERROR;
    break;

  case ANYSXP:
    *retObjPtrPtr = Tcl_NewStringObj("No Tcl equivalent for R \"any\" type", -1);
    retcode = TCL_ERROR;
    break;

  case VECSXP:
    if (LENGTH(s) == 1) {
      i = RtoTcl(interp, retObjPtrPtr, VECTOR_ELT(s, 0));
      return i;
    } else {
      objv = (Tcl_Obj **) Tcl_Alloc(length(s) * sizeof(Tcl_Obj *));
      
      for (i = 0; i < LENGTH(s); i++) {
	/*@@@ Need to check return result. @@@*/
	RtoTcl(interp, &(objv[i]), VECTOR_ELT(s,i));
      }
      
      *retObjPtrPtr = Tcl_NewListObj(LENGTH(s), objv);
      Tcl_Free((ClientData) objv);
    }
    
    break;

  case EXPRSXP:
    *retObjPtrPtr = Tcl_NewStringObj("No Tcl equivalent for R expressions", -1);
    retcode = TCL_ERROR;
    break;

  case BCODESXP:
    *retObjPtrPtr = Tcl_NewStringObj("No Tcl equivalent for R bytecode", -1);
    retcode = TCL_ERROR;
    break;

  case EXTPTRSXP:
    *retObjPtrPtr = Tcl_NewStringObj("No Tcl equivalent for R external pointers", -1);
    retcode = TCL_ERROR;
    break;

  case WEAKREFSXP:
    *retObjPtrPtr = Tcl_NewStringObj("No Tcl equivalent for R weak references", -1);
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
REval(interp, s, objc, objv)
     Tcl_Interp *interp;		/* For error messages */
     SEXP *s;			        /* S-Expression to put result in */
     int objc;			        /* Number of arguments */
     Tcl_Obj *CONST objv[];	        /* Arguments */
{
  int i, l, evalError;
  ParseStatus status;
  SEXP text;
  SEXP exp, val;
  SEXP geterr, err;
  char *errmsg;
  int offset, verbose;

  /* Check for verbose option */
  if (strcmp(Tcl_GetString(objv[1]),"-verbose") == 0) {
    offset = 2;
    verbose = 1;
    objc -= 2;
  } else {
    offset = 1;
    verbose = 0;
    objc--;
  }

  /* Get the exp to evaluate */
  text = PROTECT(allocVector(STRSXP, objc));
  
  for (i = 0; i < objc; i++ ) {
    SET_STRING_ELT(text, i, mkChar(Tcl_GetString(objv[i+offset])));
  }

  /* Parse it */
  PROTECT(exp = R_ParseVector(text, -1, &status, R_NilValue));
  
  if (status != PARSE_OK) {
    UNPROTECT(2);
    Tcl_SetResult(interp, "R parse error", TCL_STATIC);
    return TCL_ERROR;
  }

  /* Evaluate it */
  val = R_tryEval(VECTOR_ELT(exp,0), R_GlobalEnv, &evalError);

  if (evalError) {
    UNPROTECT(2);

    /* Retrieve error message from R */
    PROTECT(geterr = lang1(install("geterrmessage")));
    err = R_tryEval(geterr, R_GlobalEnv, &evalError);
    UNPROTECT(1);

    errmsg = strdup(CHAR(STRING_ELT(err,0)));
    errmsg[strlen(errmsg) - 1] = '\0';
    Tcl_SetResult(interp, errmsg, TCL_STATIC);
    
    return TCL_ERROR;
  } else {
    if (verbose) {
      PrintValue(val);
    }
  }
  
  l = length(exp);

  for (i = 0; i < l; i++) {
    *s = eval(VECTOR_ELT(exp, i), R_GlobalEnv);
  }

  UNPROTECT(2);
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
Rtcl_eval(clientData, interp, objc, objv)
     ClientData clientData;	/* Not used. */
     Tcl_Interp *interp;	/* Current interpreter */
     int objc;			/* Number of arguments */
     Tcl_Obj *CONST objv[];	/* Arguments */
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
Rtcl_source(clientData, interp, objc, objv)
    ClientData clientData;	/* Not used. */
    Tcl_Interp *interp;		/* Current interpreter */
    int objc;			/* Number of arguments */
    Tcl_Obj *CONST objv[];	/* Arguments */
{
  int evalError;
  SEXP exp, val;
  SEXP geterr, err;
  char *errmsg;
  int offset, verbose;

  if (objc > 2) {
    Tcl_WrongNumArgs(interp, 1, objv, "?-verbose? {source_file.R}");
    return TCL_ERROR;
  }

  /* Check for verbose option */
  if (strcmp(Tcl_GetString(objv[1]),"-verbose") == 0) {
    offset = 2;
    verbose = 1;
    objc -= 2;
  } else {
    offset = 1;
    verbose = 0;
    objc--;
  }

  PROTECT(exp = lang2(install("source"), mkString(Tcl_GetString(objv[offset]))));
  val = R_tryEval(exp, R_GlobalEnv, &evalError);

  if (evalError) {
    UNPROTECT(1);

    /* Retrieve error message from R */
    PROTECT(geterr = lang1(install("geterrmessage")));
    err = R_tryEval(geterr, R_GlobalEnv, &evalError);
    UNPROTECT(1);

    errmsg = strdup(CHAR(STRING_ELT(err,0)));
    errmsg[strlen(errmsg) - 1] = '\0';
    Tcl_SetResult(interp, errmsg, TCL_STATIC);

    return TCL_ERROR;
  } else {
    if (verbose) {
      PrintValue(val);
    }
  }

  UNPROTECT(1);
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
Rtcl_gettype(clientData, interp, objc, objv)
    ClientData clientData;	/* Not used. */
    Tcl_Interp *interp;		/* Current interpreter */
    int objc;			/* Number of arguments */
    Tcl_Obj *CONST objv[];	/* Arguments */
{
    SEXP ans;
    Tcl_Obj *r;

    if (objc < 2) {
	Tcl_WrongNumArgs(interp, 1, objv, "?-verbose? {R_expression ...}");
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
Rtcl_getvalue(clientData, interp, objc, objv)
     ClientData clientData;	/* Not used. */
     Tcl_Interp *interp;	/* Current interpreter */
     int objc;			/* Number of arguments */
     Tcl_Obj *CONST objv[];	/* Arguments */
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
  /* Rargv is the fake argument list we use to initialize R */
  char *Rargv[] = {"R", "--no-restore", "--slave"};

  /* Pointer to hold our own new namespace */
  Tcl_Namespace *nsPtr;
  
  if (Tcl_InitStubs(interp, TCL_VERSION, 0) == NULL) {
    return TCL_ERROR;
  }

  /* Make sure R_HOME environment variable is set */
  setenv("R_HOME", "/usr/local/lib/R", 0);

  /* Initialize R */
  Rf_initEmbeddedR(sizeof(Rargv)/sizeof(Rargv[0]), Rargv);
  /* Rf_endEmbeddedR(0); */
  
  /* Create namespace */
  nsPtr = Tcl_CreateNamespace(interp, "::rtcl", NULL, NULL);

  if (nsPtr == NULL) {
    return TCL_ERROR;
  }
  
  /* Changed this to check for an error */
  if (Tcl_PkgProvide(interp, "Rtcl", "1.1") == TCL_ERROR) {
    return TCL_ERROR;
  }
  
  Tcl_CreateObjCommand(interp, "::rtcl::eval", (Tcl_ObjCmdProc *) Rtcl_eval,
		       (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
  
  Tcl_CreateObjCommand(interp, "::rtcl::source", (Tcl_ObjCmdProc *) Rtcl_source,
		       (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
  
  Tcl_CreateObjCommand(interp, "::rtcl::gettype", (Tcl_ObjCmdProc *) Rtcl_gettype,
		       (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);

  Tcl_CreateObjCommand(interp, "::rtcl::getvalue", (Tcl_ObjCmdProc *) Rtcl_getvalue,
		       (ClientData) NULL, (Tcl_CmdDeleteProc *) NULL);
    
  return TCL_OK;
}
