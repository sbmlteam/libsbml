dnl
dnl Filename    : lisp.m4
dnl Description : Autoconf macro to check for lisp and lisp-stuff
dnl Author(s)   : Martin Ginkel <martin.ginkel@epost.de>
dnl Organization: Max-Planck-Institute Magdeburg
dnl Created     : 2004-08-26 
dnl Revision    : $Id$
dnl Source      : $Source$
dnl
dnl Copyright 2004 Max-Planck-Institute Magdeburg
dnl
dnl This is free software; you can redistribute it and/or modify it
dnl under the terms of the GNU Lesser General Public License as published
dnl by the Free Software Foundation; either version 2.1 of the License, or
dnl any later version.
dnl 
dnl You should have received a copy of the GNU Lesser General Public License
dnl along with this library; if not, write to the Free Software Foundation,
dnl Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
dnl 
dnl THIS SOFTWARE IS PROVIDED BY THE AUTHORS AND CONTRIBUTORS ``AS IS'' AND
dnl ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
dnl IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
dnl ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE
dnl FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
dnl DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
dnl OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
dnl HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
dnl LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
dnl OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
dnl SUCH DAMAGE.


dnl CONFIG_PROG_LISP
dnl Wrapper combining the most common configuration macros into a single call.
AC_DEFUN(CONFIG_PROG_LISP,
[
  CONFIG_WITH_LISP
  CONFIG_LISP_EXIT
  CONFIG_ASDF_CHECK
  CONFIG_UFFI_CHECK
  CONFIG_CPARSE_CHECK
  CONFIG_FASL
])

dnl CONFIG_WITH_LISP
dnl find a common lisp, sets $LISP
dnl searches $PATH and common places
dnl introduces --with-lisp
AC_DEFUN(CONFIG_WITH_LISP,
[
  AC_SUBST(LISP)
  AC_ARG_WITH(lisp,
    AC_HELP_STRING([--with-lisp=<binary>],
                   [Use this lisp program (default: autodetect)]),
    [with_lisp=$withval]
    LISP_PATH="`dirname $with_lisp`" 
    LISP_PROGS="`basename $with_lisp`"
    ,
    [with_lisp=no]
    LISP_PATH="$PATH"
    LISP_PROGS='alisp lisp cl clisp'
  )

  AC_PATH_PROGS(LISP,$LISP_PROGS,no-lisp,${LISP_PATH})

  dnl Test whether the batch flag is accepted without error
  dnl CMU CL needs this to exit on EOF

  if test $with_lisp != no; then
    AC_DEFINE([USE_LISP], 1, [Define to 1 to use Lisp])
    AC_SUBST(USE_LISP)

    dnl Test lisp run
    USE_LISP=1
    if test "`basename $LISP`" = 'sbcl'; then
      LISP="$LISP --disable-debugger"
      if $LISP 2>&5 1>&5 </dev/null ; then
        true
      else
        AC_MSG_WARN([Lisp $LISP does not run properly -- see config.log for details])
        LISP=no-lisp
        USE_LISP=""
      fi
    elif $LISP -batch </dev/null 2>&5 1>&5 ; then
      LISP="$LISP -batch"
    fi
  fi

  dnl We record the USE_XXX flag, for later testing in Makefiles.

  LIBSBML_OPTIONS="$LIBSBML_OPTIONS USE_LISP"

])


dnl AC_LISP_EXIT
dnl determine the exit function of the lisp implementation
dnl sets $LISPEXIT to the function name 
AC_DEFUN(CONFIG_LISP_EXIT,
[
  AC_SUBST(LISPEXIT)
  if test -n $USE_LISP ; then
    AC_CACHE_CHECK(lisp exit, ac_cv_FUNC_LISPEXIT,
    ac_cv_FUNC_LISPEXIT=no-exit
dnl for now, leave the sbcl test first, since it seems to ignore --batch
    for ex in "sb-ext:quit :unix-status" "lisp:exit" "excl:exit" "sys:exit" "ext:quit" "unix:unix-exit" "no-exit" ; do
      echo "configure:__oline__ trying $ex ..." >&5
      echo "(ignore-errors ($ex 127))"|$LISP 2>&5 1>&5
      if test "$?" = "127" ; then
        ac_cv_FUNC_LISPEXIT=$ex 
        break
      fi
    done
    )
    LISPEXIT="$ac_cv_FUNC_LISPEXIT"
    if test "$LISPEXIT" = "no-exit"; then
      AC_MSG_ERROR([Lisp exit function cannot be determined -- see config.log for details] )
    fi
  fi
])

dnl AC_LISP_RUN(message,variable,code)
dnl prints checking message
dnl sets variable to 'ok' or 'failed' depending on 
dnl whether code exits lisp with 0 or >0
dnl cares for caching
dnl uses program $LISP
AC_DEFUN(CONFIG_LISP_RUN,
[
  AC_CACHE_CHECK($1, ac_cv_$2,
    ac_cv_$2="ok"
    echo "$3"|$LISP 1>&5 2>&5|| ac_cv_$2="failed")
  $2="$ac_cv_$2"
]
)

dnl AC_LISP_OUTPUT(message,variable,code,prefix)
dnl prints checking message
dnl sets variable to output of code or 'failed' depending on 
dnl whether code exits lisp with 0 or 1
dnl prepare is lisp code, done out of all error handlers
dnl cares for caching
dnl uses program $LISP and $LISPEXIT
AC_DEFUN(CONFIG_LISP_OUTPUT,
[
  ac_prepare="$4"
  ac_code="$3"
  ac_outfile=ac_out$$		
  ac_ltest="$ac_prepare(handler-bind ((serious-condition #'(lambda (ex) (ignore-errors (apply #'format (list t (simple-condition-format-control ex) (simple-condition-format-arguments ex)))) ($LISPEXIT 1)))) (with-open-file (cl:*standard-output* \"$ac_outfile\" :direction :output :if-exists :new-version) $ac_code ) )($LISPEXIT 0)"
  AC_CACHE_CHECK($1, ac_cv_$2,
    if echo "$ac_ltest"|$LISP 1>&5 2>&5 && test -f $ac_outfile ; then
      ac_cv_$2="`cat $ac_outfile`"
    else
      ac_cv_$2="failed"
    fi
    test -f $ac_outfile && rm $ac_outfile
  )
  if test "$ac_cv_$2" = failed; then
    echo "Failed lisp code was: " >&5
    echo "$ac_code"|sed 's/^/\|/' >&5
    test -f $ac_outfile && rm $ac_outfile
  fi
  $2="$ac_cv_$2"
]
)



dnl AC_LISP_CHECK(message,variable,code,prepare)
dnl uses AC_LISP_RUN
dnl checks whether the code produces an condition in $LISP
dnl or runs through
dnl uses program $LISP and lisp function $LISPEXIT
dnl prepare is done before code to initialize
AC_DEFUN(CONFIG_LISP_CHECK,
[
  ac_prepare="$4"
  ac_code="$3"
  ac_ltest="$ac_prepare(handler-bind ((serious-condition #'(lambda (ex) (ignore-errors (apply #'format (list t (simple-condition-format-control ex) (simple-condition-format-arguments ex)))) ($LISPEXIT 1)))) ${ac_code} )($LISPEXIT 0)"
  CONFIG_LISP_RUN($1,$2,$ac_ltest)
  if test "[$]$2" = failed; then
    echo "Failed lisp code was: " >&5
    echo "$ac_code"|sed 's/^/\|/' >&5
  fi
])


dnl find out the suffix of binary lisp files (fasl, x86f, fas, ...)
dnl This sets FASLEXT 
AC_DEFUN(CONFIG_FASL,
[ AC_SUBST(FASLEXT)
  if test -n "$USE_LISP" ; then
    CONFIG_LISP_OUTPUT(lisp binary-extension,FASLEXT,(format t \"~a\" (pathname-type (compile-file-pathname \"bla.lisp\"))),t)
  fi
])


dnl check for compilation tool asdf
dnl sets ASDF and EXT_ASDF to the directory where asdf is
dnl installed
AC_DEFUN(CONFIG_ASDF_CHECK,
[
  AC_SUBST(EXT_ASDF)
  AC_SUBST(ASDF)
  AC_SUBST(ASDF_RUN)
  if test -n "$USE_LISP" ; then
    AC_ARG_WITH(asdf,
      AC_HELP_STRING([--with-asdf=<pathname>],
	             [Use asdf at <pathname> (default:bindings/lisp/tps/asdf)]),
      ASDFPATH="$with_asdf:`dirname ${with_asdf}`"
     ,
      ASDFPATH="${srcdir}/bindings/lisp/tps/asdf"
    )
    AC_PATH_PROG(ASDF,asdf.lisp,failed,$ASDFPATH)
    if test $ASDF = failed; then
      AC_MSG_ERROR([Asdf not found in one of $ASDFPATH])
    fi
    CONFIG_LISP_CHECK(asdf load,RUN_ASDF,(load \"$ASDF\"),t)
    if test $RUN_ASDF = failed; then
      AC_MSG_ERROR([Asdf cannot be loaded -- see config.log for details] )
    fi
    ASDF=`dirname $ASDF`/
    if test "$ASDF" = "${srcdir}/bindings/lisp/tps/asdf/"; then
      ASDF_RUN="@final_libdir@/lisp/asdf/"
    else
      EXT_ASDF=1
    fi
    ac_old_dir="`pwd`"
    cd $ASDF
    ASDF="`pwd`/"
    cd $ac_old_dir
    if test -z "$ASDF_RUN"; then
      ASDF_RUN="$ASDF"
    fi
  fi
])

dnl check for uffi 
dnl sets UFFI to the directory of uffi
dnl also links the uffi.asd to the bindings/lisp
AC_DEFUN(CONFIG_UFFI_CHECK,
[
  AC_SUBST(UFFI)
  AC_SUBST(UFFI_RUN)
  AC_SUBST(EXT_UFFI)
  if test -n "$USE_LISP" ; then
    AC_ARG_WITH(uffi,
      AC_HELP_STRING([--with-uffi=<pathname>],
                     [Use uffi at <pathname> (default:bindings/lisp/tps/uffi)]), 
        UFFIPATH="${with_uffi}:`dirname ${with_uffi}`"
	,
        UFFIPATH="${srcdir}/bindings/lisp/tps/uffi"
    )
    AC_PATH_PROG(UFFI,uffi.asd,failed,$UFFIPATH)
    if test $UFFI = failed; then
      AC_MSG_ERROR([Uffi not found in one of $UFFIPATH])
    fi

    CONFIG_LISP_CHECK(uffi load,RUN_UFFI,(progn (load \"${UFFI}\") (asdf:operate 'asdf:load-op :uffi)),(require :asdf \"${ASDF}/asdf.lisp\"))

    if test $RUN_UFFI = failed; then
      AC_MSG_ERROR([uffi cannot be loaded -- see config.log for details] )
    fi

    UFFI="`dirname $UFFI`/"
    if test "$UFFI" = "${srcdir}/bindings/lisp/tps/uffi/"; then
      UFFI_RUN="@final_libdir@/lisp/uffi/"
    else
      EXT_UFFI=1
    fi
    ac_old_dir="`pwd`"
    cd $UFFI
    UFFI="`pwd`/"
    cd $ac_old_dir
    if test -z "$UFFI_RUN"; then
      UFFI_RUN="$UFFI"
    fi
    if test -h ${srcdir}/bindings/lisp/uffi.asd ; then
      rm ${srcdir}/bindings/lisp/uffi.asd
    fi
    ln -s ${UFFI}uffi.asd ${srcdir}/bindings/lisp/
  fi
])

dnl check for cparse
dnl sets CPARSE to the directory of cparse
dnl also links the cparse.asd to the bindings/lisp
AC_DEFUN(CONFIG_CPARSE_CHECK,
[
  AC_SUBST(CPARSE)
  AC_SUBST(EXT_CPARSE)
  if test -n "$USE_LISP" ; then
    AC_ARG_WITH(cparse,
      AC_HELP_STRING([--with-cparse=<pathname>],
        [Use cparse at <pathname> (default:bindings/lisp/tps/cparse, don't change)]), 
        CPARSEPATH="$with_cparse"
	,
        CPARSEPATH="${srcdir}/bindings/lisp/tps/cparse"
    )
    AC_PATH_PROG(CPARSE,cparse.asd,failed,$CPARSEPATH)
    if test $CPARSE = failed; then
      AC_MSG_ERROR([Cparse not found in one of $CPARSEPATH])
    fi

    CONFIG_LISP_CHECK(cparse load,RUN_CPARSE,(progn (load \"${CPARSE}\")(asdf:operate 'asdf:load-op :cparse)),(require :asdf \"${ASDF}/asdf.lisp\"))
    if test $RUN_CPARSE = failed; then
      AC_MSG_ERROR([cparse cannot be loaded -- see config.log for details] )
    fi
    CPARSE="`dirname $CPARSE`/"
    if test "$CPARSE" != "${srcdir}/bindings/lisp/tps/cparse/" ; then
      EXT_CPARSE=1
    fi
    ac_old_dir="`pwd`"
    cd $CPARSE
    CPARSE="`pwd`/"
    cd $ac_old_dir
    if test -h ${srcdir}/bindings/lisp/cparse.asd ; then
      rm ${srcdir}/bindings/lisp/cparse.asd
    fi
    ln -s ${CPARSE}cparse.asd ${srcdir}/bindings/lisp/
  fi
])
