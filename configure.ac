#!/bin/bash -norc
dnl	This file is an input file used by the GNU "autoconf" program to
dnl	generate the file "configure", which is run during Tcl installation
dnl	to configure the system for the local environment.

#-----------------------------------------------------------------------
# configure.ac for Tcl Extensions.  The only places you should need to
# modify this file are marked by the string __CHANGE__
# -----------------------------------------------------------------------

#-----------------------------------------------------------------------
# __CHANGE__
# Set your package name and version numbers here.
#
# This initializes the environment with PACKAGE_NAME and PACKAGE_VERSION
# set as provided.  These will also be added as -D defs in your Makefile
# so you can encode the package version directly into the source files.
# This will also define a special symbol for Windows (BUILD_<PACKAGE_NAME>
# so that we create the export library with the dll.
#-----------------------------------------------------------------------

AC_INIT([Rtcl], [1.2.1])

#-----------------------------------------------------------------------
# Find all the R-related stuff: include files, libraries, ...
#-----------------------------------------------------------------------

AC_MSG_CHECKING([for R (Project for Statistical Computing)])

AC_ARG_WITH(R, [AS_HELP_STRING([--with-R=PATH], [full path to main R binary])], R_BINARY=$withval, R_BINARY='')

# If --with-R not specified, try to determine R_HOME automatically
if test -z "$R_BINARY"; then
  R_HOME=`echo 'cat(R.home(component = "home"))' | R --slave`;

  if test -z "$R_HOME"; then
    # R must not be in path
    AC_MSG_NOTICE([could not automatically find R])
  else
    if ! test -d "$R_HOME"; then
      # R_HOME points to something it probably shouldn't -- symlinks will fail here
      AC_MSG_ERROR([R_HOME does not point to directory: $R_HOME])
    fi
  fi
fi

# Couldn't determine R_HOME automatically
if test -z "$R_HOME"; then
  # If --with-R not specified try a reasonable default
  if test -z "$R_BINARY"; then
    AC_MSG_NOTICE([trying /usr/local/bin/R])
    R_BINARY=/usr/local/bin/R
  fi

  if test -x "$R_BINARY"; then
    R_HOME=`echo 'cat(R.home(component = "home"))' | $R_BINARY --slave`;

    if ! test -d "$R_HOME"; then
      # --with-R wasn't the droid we were looking for      
      AC_MSG_ERROR([--with-R=$R_BINARY is not the main R command])
    fi
  else
    # We need an executable file
    AC_MSG_ERROR([--with-R=$R_BINARY not executable]);
  fi
fi

AC_MSG_RESULT([install found at $R_HOME])

R_LD_LIBRARY_PATH=`echo 'cat(Sys.getenv("LD_LIBRARY_PATH"))' | ${R_HOME}/bin/R --slave`
R_CC=`"${R_HOME}/bin/R" CMD config CC`
R_CFLAGS=`"${R_HOME}/bin/R" CMD config CFLAGS`
R_CPPFLAGS=`"${R_HOME}/bin/R" CMD config --cppflags`
R_LDFLAGS=`"${R_HOME}/bin/R" CMD config --ldflags`

AC_SUBST(R_HOME)
AC_SUBST(R_LD_LIBRARY_PATH)
AC_SUBST(R_CC)
AC_SUBST(R_CFLAGS)
AC_SUBST(R_CPPFLAGS)
AC_SUBST(R_LDFLAGS)

#--------------------------------------------------------------------
# Call TEA_INIT as the first TEA_ macro to set up initial vars.
# This will define a ${TEA_PLATFORM} variable == "unix" or "windows"
# as well as PKG_LIB_FILE and PKG_STUB_LIB_FILE.
#--------------------------------------------------------------------

# Pass version of TEA expected or TEA_INIT as defined in tclconfig/tcl.m4 will fail
TEA_INIT(3.9)

AC_CONFIG_AUX_DIR(tclconfig)

#--------------------------------------------------------------------
# Load the tclConfig.sh file
#--------------------------------------------------------------------

TEA_PATH_TCLCONFIG
TEA_LOAD_TCLCONFIG

#-----------------------------------------------------------------------
# Handle the --prefix=... option by defaulting to what Tcl gave.
# Must be called after TEA_LOAD_TCLCONFIG and before TEA_SETUP_COMPILER.
#-----------------------------------------------------------------------

TEA_PREFIX

#-----------------------------------------------------------------------
# Standard compiler checks.
# This sets up CC by using the CC env var, or looks for gcc otherwise.
# This also calls AC_PROG_CC and a few others to create the basic setup
# necessary to compile executables.
#-----------------------------------------------------------------------

TEA_SETUP_COMPILER

#-----------------------------------------------------------------------
# Type checks.
#-----------------------------------------------------------------------

AC_CHECK_TYPE([uintptr_t], CFLAGS="$CFLAGS -DHAVE_UINTPTR_T=1")

#-----------------------------------------------------------------------
# __CHANGE__
# Specify the C source files to compile in TEA_ADD_SOURCES,
# public headers that need to be installed in TEA_ADD_HEADERS,
# stub library C source files to compile in TEA_ADD_STUB_SOURCES,
# and runtime Tcl library files in TEA_ADD_TCL_SOURCES.
# This defines PKG(_STUB)_SOURCES, PKG(_STUB)_OBJECTS, PKG_HEADERS
# and PKG_TCL_SOURCES.
#-----------------------------------------------------------------------

TEA_ADD_SOURCES([rtcl.c])
TEA_ADD_HEADERS([])
TEA_ADD_INCLUDES([])
TEA_ADD_LIBS([])
# Include files for R disabled for FreeBSD build
# TEA_ADD_CFLAGS([-Icompat -Icompat/R])
TEA_ADD_CFLAGS([-Icompat])
TEA_ADD_STUB_SOURCES([])
TEA_ADD_TCL_SOURCES([])

#--------------------------------------------------------------------
# __CHANGE__
# Choose which headers you need.  Extension authors should try very
# hard to only rely on the Tcl public header files.  Internal headers
# contain private data structures and are subject to change without
# notice.
# This MUST be called after TEA_LOAD_TCLCONFIG / TEA_LOAD_TKCONFIG
#--------------------------------------------------------------------

TEA_PUBLIC_TCL_HEADERS
#TEA_PRIVATE_TCL_HEADERS

#--------------------------------------------------------------------
# __CHANGE__
# A few miscellaneous platform-specific items:
#
# Define a special symbol for Windows (BUILD_Tktable in this case) so
# that we create the export library with the dll.
#
# Windows creates a few extra files that need to be cleaned up.
# You can add more files to clean if your extension creates any extra
# files.
#
# TEA_ADD any extra compiler/build info here.
#--------------------------------------------------------------------

TEA_ADD_CLEANFILES([pkgIndex.tcl])

#--------------------------------------------------------------------
# Check whether --enable-threads or --disable-threads was given.
# This auto-enables if Tcl was compiled threaded.
#--------------------------------------------------------------------

TEA_ENABLE_THREADS

#--------------------------------------------------------------------
# The statement below defines a collection of symbols related to
# building as a shared library instead of a static library.
#--------------------------------------------------------------------

TEA_ENABLE_SHARED

#--------------------------------------------------------------------
# This macro figures out what flags to use with the compiler/linker
# when building shared/static debug/optimized objects.  This information
# can be taken from the tclConfig.sh file, but this figures it all out.
#--------------------------------------------------------------------

TEA_CONFIG_CFLAGS

#--------------------------------------------------------------------
# Set the default compiler switches based on the --enable-symbols option.
#--------------------------------------------------------------------

TEA_ENABLE_SYMBOLS

#--------------------------------------------------------------------
# Everyone should be linking against the Tcl stub library.  If you
# can't for some reason, remove this definition.  If you aren't using
# stubs, you also need to modify the SHLIB_LD_LIBS setting below to
# link against the non-stubbed Tcl library.
#--------------------------------------------------------------------

AC_DEFINE(USE_TCL_STUBS)

#--------------------------------------------------------------------
# This macro generates a line to use when building a library.  It
# depends on values set by the TEA_ENABLE_SHARED, TEA_ENABLE_SYMBOLS,
# and TEA_LOAD_TCLCONFIG macros above.
#--------------------------------------------------------------------

TEA_MAKE_LIB

#--------------------------------------------------------------------
# Determine the name of the tclsh and/or wish executables in the
# Tcl and Tk build directories or the location they were installed
# into. These paths are used to support running test cases only,
# the Makefile should not be making use of these paths to generate
# a pkgIndex.tcl file or anything else at extension build time.
#--------------------------------------------------------------------

TEA_PROG_TCLSH

#--------------------------------------------------------------------
# Finally, substitute all of the various values into the Makefile.
# You may alternatively have a special pkgIndex.tcl.in or other files
# which require substituting th AC variables in.  Include these here.
#--------------------------------------------------------------------

AC_OUTPUT([Makefile pkgIndex.tcl])
