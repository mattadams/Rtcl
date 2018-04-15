/*
 * Rtcl.c --
 *
 *	This file implements a Tcl interface to the R statistical
 *	computing language
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */

#include <tcl.h>
#include <stdio.h>
#include <stdlib.h>

/*@@@ NOTE: ideally, we shouldn't need to incude Defn.h. @@@*/

#include "config.h"
#include "Defn.h"
#include "R.h"

/*
 * Borrowed from R's Parse.h file
 */

#define PARSE_NULL              0
#define PARSE_OK                1
#define PARSE_INCOMPLETE        2
#define PARSE_ERROR             3
#define PARSE_EOF               4

/*
 * We need access to R's "current expression".
 */

extern SEXP R_GlobalEnv;

/*
 *----------------------------------------------------------------------
 *
 * RType --
 *
 *	Return the type of an R expression
 *
 * Results:
 *	RType takes an R SEXP as input, and returns a
 *	Tcl_Obj indicating the SEXP's type.
 *
 * Side effects:
 *	None
 *
 *----------------------------------------------------------------------
 */

static int RType(interp, retObjPtr, s)
    Tcl_Interp *interp;
    Tcl_Obj *retObjPtr;
    SEXP s;
{
    Tcl_Obj **objv;
    int i;

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

static int RtoTcl(interp, retObjPtrPtr, s)
    Tcl_Interp *interp;		/* Tcl interp. for error messages */
    Tcl_Obj **retObjPtrPtr;	/* Tcl object returned here	  */
    SEXP s;			/* R SEXP to translate		  */
{
    Tcl_Obj **objv;
    int i, l, retcode;

    retcode = TCL_OK;
    switch(TYPEOF(s)) {
	case NILSXP:
		*retObjPtrPtr = Tcl_NewStringObj("", -1);
		break;

	case SYMSXP:
		*retObjPtrPtr = Tcl_NewStringObj(
			"No Tcl equivalent for R symbol expression", -1);
		retcode = TCL_ERROR;
		break;

	case LISTSXP:
		l = length(s);
		if(l == 0) {
		    *retObjPtrPtr = Tcl_NewStringObj("", -1);
		} else if(l == 1) {
		    return(RtoTcl(interp, retObjPtrPtr, CAR(s)));
		} else {
		    objv = (Tcl_Obj **) Tcl_Alloc(l*sizeof(Tcl_Obj *));
		    for(i = 0; s != NULL && s != R_NilValue; i++, s = CDR(s)) {
			RtoTcl(interp, &(objv[i]), CAR(s));
		    }
		    *retObjPtrPtr = Tcl_NewListObj(l, objv);
		    Tcl_Free((ClientData) objv);
		}
		break;

	case CLOSXP:
		*retObjPtrPtr = Tcl_NewStringObj(
			"No Tcl equivalent for R closures", -1);
		retcode = TCL_ERROR;
		break;

	case ENVSXP:
		*retObjPtrPtr = Tcl_NewStringObj(
			"No Tcl equivalent for R environment", -1);
		retcode = TCL_ERROR;
		break;

	case PROMSXP:
		*retObjPtrPtr = Tcl_NewStringObj(
			"No Tcl equivalent for R promises", -1);
		retcode = TCL_ERROR;
		break;

	case LANGSXP:
		*retObjPtrPtr = Tcl_NewStringObj(
			"No Tcl equivalent for R language objects", -1);
		retcode = TCL_ERROR;
		break;

	case SPECIALSXP:
		*retObjPtrPtr = Tcl_NewStringObj(
			"No Tcl equivalent for R special expressions", -1);
		retcode = TCL_ERROR;
		break;

	case BUILTINSXP:
		*retObjPtrPtr = Tcl_NewStringObj(
			"No Tcl equivalent for R builtins", -1);
		retcode = TCL_ERROR;
		break;

	case CHARSXP:
		*retObjPtrPtr = Tcl_NewStringObj(CHAR(s), -1);
		break;

	case LGLSXP:
		if (LENGTH(s) == 1) {
		    *retObjPtrPtr = Tcl_NewIntObj((int) (*LOGICAL(s)));
		} else {
		    objv = (Tcl_Obj **) Tcl_Alloc(length(s)*sizeof(Tcl_Obj *));
		    for(i = 0; i < LENGTH(s); i++) {
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
		    objv = (Tcl_Obj **) Tcl_Alloc(LENGTH(s)*sizeof(Tcl_Obj *));
		    for(i = 0; i < LENGTH(s); i++) {
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
		    objv = (Tcl_Obj **) Tcl_Alloc(LENGTH(s)*sizeof(Tcl_Obj *));
		    for(i = 0; i < LENGTH(s); i++) {
			objv[i] = Tcl_NewDoubleObj(REAL(s)[i]);
		    }
		    *retObjPtrPtr = Tcl_NewListObj(LENGTH(s), objv);
		    Tcl_Free((ClientData) objv);
		}
		break;

	case CPLXSXP:
		*retObjPtrPtr = Tcl_NewStringObj(
			"No Tcl equivalent for R complex numbers", -1);
		retcode = TCL_ERROR;
		break;

	case STRSXP:
		if (LENGTH(s) == 1) {
		    *retObjPtrPtr = Tcl_NewStringObj(CHAR(*STRING_PTR(s)), -1);
		} else {
		    objv = (Tcl_Obj **) Tcl_Alloc(length(s)*sizeof(Tcl_Obj *));
		    for(i = 0; i < LENGTH(s); i++) {
			objv[i] = Tcl_NewStringObj(CHAR(STRING_PTR(s)[i]), -1);
		    }
		    *retObjPtrPtr = Tcl_NewListObj(LENGTH(s), objv);
		    Tcl_Free((ClientData) objv);
		}
		break;

	case DOTSXP:
		*retObjPtrPtr = Tcl_NewStringObj(
			"No Tcl equivalent for R dotted pairs", -1);
		retcode = TCL_ERROR;
		break;

	case ANYSXP:
		*retObjPtrPtr = Tcl_NewStringObj(
			"No Tcl equivalent for R \"any\" type", -1);
		retcode = TCL_ERROR;
		break;

	case VECSXP:
		if (LENGTH(s) == 1) {
		    i = RtoTcl(interp, retObjPtrPtr, VECTOR_ELT(s, 0));
		    return i;
		} else {
		    objv = (Tcl_Obj **) Tcl_Alloc(length(s)*sizeof(Tcl_Obj *));
		    for(i = 0; i < LENGTH(s); i++) {
			/*@@@ Need to check return result. @@@*/
			RtoTcl(interp, &(objv[i]), VECTOR_ELT(s,i));
		    }
		    *retObjPtrPtr = Tcl_NewListObj(LENGTH(s), objv);
		    Tcl_Free((ClientData) objv);
		}
		break;

	case EXPRSXP:
		*retObjPtrPtr = Tcl_NewStringObj(
			"No Tcl equivalent for R expressions", -1);
		retcode = TCL_ERROR;
		break;

	case BCODESXP:
		*retObjPtrPtr = Tcl_NewStringObj(
			"No Tcl equivalent for R bytecode", -1);
		retcode = TCL_ERROR;
		break;

	case EXTPTRSXP:
		*retObjPtrPtr = Tcl_NewStringObj(
			"No Tcl equivalent for R external pointers", -1);
		retcode = TCL_ERROR;
		break;

	case WEAKREFSXP:
		*retObjPtrPtr = Tcl_NewStringObj(
			"No Tcl equivalent for R weak references", -1);
		retcode = TCL_ERROR;
		break;

	default:
		*retObjPtrPtr = Tcl_NewStringObj(
			"Unknown S-expression type", -1);
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
 *	REval takes an array of Tcl_Obj's, and evaluates them
 *	as R expressions. It returns a standard Tcl result.
 *
 * Side effects:
 *	May change the state of the R interpreter
 *
 *----------------------------------------------------------------------
 */

static int REval(interp, s, objc, objv)
    Tcl_Interp *interp;		/* For error messages */
    SEXP *s;			/* S-Expression to put result in */
    int objc;			/* Number of arguments */
    Tcl_Obj *CONST objv[];	/* Arguments */
{
    int i, l, rStatus;
    SEXP text, ans, rExpr;
    char *str;
    int retcode;

    /* Get the expression to evaluate. */

    text = PROTECT(allocVector(STRSXP, objc));
    for(i = 0; i < objc; i++ ) {
	SET_STRING_ELT(text, i, mkChar(Tcl_GetString(objv[i])));
    }

    /* Parse it. */

    rExpr = PROTECT(R_ParseVector(text, -1, &rStatus));
    if (rStatus != PARSE_OK) {
	UNPROTECT(2);
	Tcl_SetResult(interp, "R parse error", TCL_STATIC);
	return TCL_ERROR;
    }

    /* Evaluate it. */

    /*@@@ Should this be R_ToplevelContext or R_GlobalContext? @@@*/
    if(SETJMP(R_ToplevelContext->cjmpbuf) != 0) {
	return TCL_ERROR;
    }

    l = length(rExpr);
    for(i = 0; i < l; i++) {
	*s = eval(VECTOR_ELT(rExpr, i), R_GlobalEnv);
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
 *	May change the state of the R interpreter, depending on
 *	whatexpression is evaluated.
 *
 *----------------------------------------------------------------------
 */

static int
Rtcl_eval(clientData, interp, objc, objv)
    ClientData clientData;	/* Not used. */
    Tcl_Interp *interp;		/* Current interpreter */
    int objc;			/* Number of arguments */
    Tcl_Obj *CONST objv[];	/* Arguments */
{
    int rStatus;
    SEXP ans;
    Tcl_Obj *r;

    if (objc < 2) {
	Tcl_WrongNumArgs(interp, 1, objv, "R_expression ...");
	return TCL_ERROR;
    }

    /* Evaluate the args. */

    if(REval(interp, &ans, objc - 1, &(objv[1])) != TCL_OK) {
	return TCL_ERROR;
    }

    /* Clean up and return. */

    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * Rtcl_getval --
 *
 *	Evaluate R expression and return its value.
 *
 * Results:
 *	A standard Tcl result
 *
 * Side effects:
 *	May change the state of the R interpreter, depending on
 *	whatexpression is evaluated.
 *
 *----------------------------------------------------------------------
 */

static int
Rtcl_getval(clientData, interp, objc, objv)
    ClientData clientData;	/* Not used. */
    Tcl_Interp *interp;		/* Current interpreter */
    int objc;			/* Number of arguments */
    Tcl_Obj *CONST objv[];	/* Arguments */
{
    int rStatus;
    SEXP ans;
    Tcl_Obj *r;

    if (objc < 2) {
	Tcl_WrongNumArgs(interp, 1, objv, "R_expression ...");
	return TCL_ERROR;
    }

    /* Evaluate the args. */

    if(REval(interp, &ans, objc - 1, &(objv[1])) != TCL_OK) {
	return TCL_ERROR;
    }

    /* Return the result in an appropriate form. */

    if(RtoTcl(interp, &r, ans) != TCL_OK) {
	return TCL_ERROR;
    }

    /* Clean up and return. */

    Tcl_SetObjResult(interp, r);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * Rtcl_type --
 *
 *	Evaluate an R expression and return its type.
 *
 * Results:
 *	A standard Tcl result
 *
 * Side effects:
 *	May change the state of the R interpreter, depending on
 *	whatexpression is evaluated.
 *
 *----------------------------------------------------------------------
 */

static int
Rtcl_type(clientData, interp, objc, objv)
    ClientData clientData;	/* Not used. */
    Tcl_Interp *interp;		/* Current interpreter */
    int objc;			/* Number of arguments */
    Tcl_Obj *CONST objv[];	/* Arguments */
{
    int rStatus;
    SEXP ans;
    Tcl_Obj *r;

    if (objc < 2) {
	Tcl_WrongNumArgs(interp, 1, objv, "R_expression ...");
	return TCL_ERROR;
    }

    /* Evaluate the args. */

    if(REval(interp, &ans, objc - 1, &(objv[1])) != TCL_OK) {
	return TCL_ERROR;
    }

    /* Return the result in an appropriate form. */

    r = Tcl_NewObj();
    Tcl_IncrRefCount(r);
    if(RType(interp, r, ans) != TCL_OK) {
	Tcl_DecrRefCount(r);
	return TCL_ERROR;
    }

    /* Clean up and return. */

    Tcl_SetObjResult(interp, r);
    Tcl_DecrRefCount(r);
    return TCL_OK;
}


/*
 * Rargv is the fake argument list we use to initialize R.
 */

static char *Rargv[] = {"Rtcl", "--no-save", "--no-restore",
			"--slave", NULL};
#define N_R_ARGS ((sizeof(Rargv)/sizeof(char *)) - 1)

/*
 * This ugliness short-circuits R's error handling. errInterp
 * is the interpreter we'll use for returning errors. Since
 * this is a static variable, you better only load Rtcl ONCE,
 * and only use it in one thread. It'd be nice if the R
 * error handling function accepted something akin to Tcl's
 * clientData pointers; then we could pass the interpreter
 * to R's error handler as an argument.
 */

static Tcl_Interp *errInterp;
static void HandleRWarning(SEXP *s, char *msg);

/*
 *----------------------------------------------------------------------
 *
 * HandleRError --
 *
 *	Handle R errors so that we can get the messages
 *	in Tcl.
 *
 * Results:
 *	None
 *
 * Side effects:
 *	Takes the R interpreter back to its "toplevel" state
 *
 *----------------------------------------------------------------------
 */

static void HandleRError(SEXP *s, char *msg)
{
    Tcl_SetResult(errInterp, msg, TCL_VOLATILE);

    /*
     * The R error handler saves and restores the ErrorHook
     * routine, if you use it in the normal way. Since we're
     * cheating by doing a longjmp out of the ErrorHook routine,
     * it doesn't get reset, so we have to do it ourselves.
     */

    R_SetErrorHook(HandleRError);
    /*R_CollectWarnings = 0;*/
    jump_to_toplevel();
}

/*
 *----------------------------------------------------------------------
 *
 * HandleRWarning --
 *
 *	Handle R warnings. (We just ignore them for now.)
 *	in Tcl.
 *
 * Results:
 *	None
 *
 * Side effects:
 *	None
 *
 *----------------------------------------------------------------------
 */

static void HandleRWarning(SEXP *s, char *msg)
{
    /* Ignore warnings: do nothing. */
}

/*
 *----------------------------------------------------------------------
 *
 * Rtcl_Init --
 *
 *	Initialize the Rtcl package.
 *
 * Results:
 *	A standard Tcl result
 *
 * Side effects:
 *	The Rtcl package and namespace are created. The R interpreter
 *	is initialized.
 *
 *----------------------------------------------------------------------
 */

int
Rtcl_Init(Tcl_Interp *interp)
{
    /* Make sure we're being loaded into an appropriate Tcl version. */

    if (Tcl_InitStubs(interp, "8.4", 0) == NULL) {
	return TCL_ERROR;
    }

    if (Tcl_PkgRequire(interp, "Tcl", TCL_VERSION, 0) == NULL) {
	if (TCL_VERSION[0] == '7') {
	    if (Tcl_PkgRequire(interp, "Tcl", "8.0", 0) == NULL) {
		return TCL_ERROR;
	    }
	}
    }

    /* Make sure that the R_HOME environment variable is set. */

    setenv("R_HOME", R_HOME, 0);

    /* Initialize R. */

    Rf_initialize_R(N_R_ARGS, Rargv);
    setup_Rmainloop();

    /* Fudge the error handling. */

    errInterp = interp;
    R_SetErrorHook(HandleRError);
    R_SetWarningHook(HandleRWarning);
    /*R_CollectWarnings = 0;*/

    /*
     * Make R think it's operating interactively. If you
     * don't do this, sometimes R exits when it gets a
     * statement with an error in it.
     */

    R_Interactive = 1;

    /* Create the Rtcl namespace and package. */

    if (Tcl_PkgProvide(interp, "Rtcl", VERSION) != TCL_OK) {
	return TCL_ERROR;
    }

    if (Tcl_CreateNamespace(interp, "::Rtcl", (ClientData) NULL,
			(Tcl_NamespaceDeleteProc *) NULL) == NULL) {
	return TCL_ERROR;
    }

    /* Create all the Rtcl commands. */

    Tcl_CreateObjCommand(interp, "::Rtcl::eval", Rtcl_eval,
	    (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL);
    Tcl_CreateObjCommand(interp, "::Rtcl::getval", Rtcl_getval,
	    (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL);
    Tcl_CreateObjCommand(interp, "::Rtcl::type", Rtcl_type,
	    (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL);

    return TCL_OK;
}

