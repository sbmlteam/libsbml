AC_DEFUN(CONFIG_PROG_MATLAB,
[
  AC_ARG_WITH([matlab],
    AC_HELP_STRING([--with-matlab=PREFIX],
                   [Generate MATLAB interface library [[default=no]]]),
    [with_matlab=$withval],
    [with_matlab=no])

  if test $with_matlab != no; then




    AC_DEFINE([USE_MATLAB], 1, [Define to 1 to use Matlab])
    AC_SUBST(USE_MATLAB, 1)

    AC_SUBST(MATLAB_CPPFLAGS)
    AC_SUBST(MATLAB_LDFLAGS)
    AC_SUBST(MATLAB_LIBS)

  fi

])
