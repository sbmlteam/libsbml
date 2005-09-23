##
## Filename    : mexopts-libsbml.sh
## Description : mexopts file for libsbml.
## Author(s)   : Michael Hucka <mhucka@caltech.edu>
## Organization: California Institute of Technology
## Created     : 2004-09-27
## Revision    : $Id$
## Source      : $Source$
##
## This file is based on mexopts.sh as provided by The MathWorks, Inc.,
## in MATLAB R14.  The original copyright notice from that file is:
##
## Copyright 1984-2000 The MathWorks, Inc.
## $ Revision: 1.1 $  $Date$
##
## The modifications in this file are for use with libSBML, in order to
## cope with the following issues:
##
## 1) There's a known problem with Matlab on MacOS X.  See the following:
##    http://www.mathworks.com/support/solutions/data/1-1BEIS.html?solution=1-1BEIS
##    The MacOS definitions in this file provide a work-around.
##
## 2) There's a known problem with XA64-based Linuxes (e.g., SuSE on AMD
##    opteron systems): MATLAB does not ship with 64-bit libraries.  This
##    mexopts file works around this by following the workaround procedure 
##    recommended by The MathWorks, namely to tell mex and matlab to use
##    the 32-bit libraries instead.
##

#----------------------------------------------------------------------------
#
    TMW_ROOT="$MATLAB"
    MFLAGS=''
    if [ "$ENTRYPOINT" = "mexLibrary" ]; then
        MLIBS="-L$TMW_ROOT/bin/$Arch -lmx -lmex -lmatlb -lmat -lmwservices -lut -lm"
    else  
        MLIBS="-L$TMW_ROOT/bin/$Arch -lmx -lmex -lmat -lm"
    fi
    case "$Arch" in
        Undetermined)
#----------------------------------------------------------------------------
# Change this line if you need to specify the location of the MATLAB
# root directory.  The script needs to know where to find utility
# routines so that it can determine the architecture; therefore, this
# assignment needs to be done while the architecture is still
# undetermined.
#----------------------------------------------------------------------------
            MATLAB="$MATLAB"
            ;;
        alpha)
#----------------------------------------------------------------------------
#           cc -V | grep UNIX
#           DEC C V5.9-008 on Digital UNIX V4.0 (Rev. 1229)
#           Digital UNIX Compiler Driver 3.11
            CC='cc'
            CFLAGS='-shared -ieee -pthread -std1'
            CLIBS="$MLIBS -lm"
            COPTIMFLAGS='-O -DNDEBUG'
            CDEBUGFLAGS='-g'
#
#           cxx -V
#           Compaq C++ V6.2-024 for Digital UNIX V4.0F  (Rev. 1229)
            CXX='cxx'
            CXXFLAGS='-shared -ieee -pthread'
            CXXLIBS="$MLIBS -lm"
            CXXOPTIMFLAGS='-O -DNDEBUG'
            CXXDEBUGFLAGS='-g'
#
#           f77 -what
#           Compaq Fortran 77 Driver V5.3-11
#           Compaq Fortran 77 V5.3-189-449BB
            FC='f77'
            FFLAGS='-shared -fpe3 -pthread'
            FLIBS="$MLIBS -lm"
            FOPTIMFLAGS='-O'
            FDEBUGFLAGS='-g'
#
            LD="$COMPILER"
            LDFLAGS="-pthread -shared -Wl,-expect_unresolved,'*',-hidden,-exported_symbol,$ENTRYPOINT,-exported_symbol,mexVersion,-exported_symbol,'__*'"
            LDOPTIMFLAGS='-O'
            LDDEBUGFLAGS='-g'
#
            POSTLINK_CMDS=':'
#----------------------------------------------------------------------------
            ;;
        hpux)
#----------------------------------------------------------------------------
#           what `which cc`
#           HP92453-01 B.11.11.02 HP C Compiler
#            $   Sep  8 2000 23:13:51 $
            CC='cc'
            CFLAGS='+Z +DA2.0 -D_POSIX_C_SOURCE=199506L -Wp,-H65535 -Ae'
            CLIBS="$MLIBS -lm -lc"
            COPTIMFLAGS='-O -DNDEBUG'
            CDEBUGFLAGS='-g'
#
#           what `which aCC`
#           HP aC++ B3910B A.03.30
#           HP aC++ B3910B X.03.27 Language Support Library
            CXX='aCC'
            CXXFLAGS='-AA +Z +DA2.0 -D_POSIX_C_SOURCE=199506L -D_HPUX_SOURCE'
            CXXLIBS="$MLIBS -lm -lstd_v2 -lCsup_v2"
            CXXOPTIMFLAGS='-O -DNDEBUG +Oconservative'
            CXXDEBUGFLAGS='-g'
#
#           what `which f90`
#          HP-UX f90 20001114 (140952)  B3907DB/B3909DB B.11.01.27
#           HP F90 v2.4.10
#            $ PATCH/11.00:PHCO_95167  Oct  1 1998 13:46:32 $
            F90LIBDIR='/opt/fortran90/lib/pa2.0'
            FC='f90'
            FFLAGS='+Z +DA2.0'
            FLIBS="$MLIBS -lm -L$F90LIBDIR -lF90 -lcl -lc -lisamstub"
            FOPTIMFLAGS='-O +Oconservative'
            FDEBUGFLAGS='-g'
#
            LDCXX="$COMPILER"
            LDCXXFLAGS="-b -Wl,+e,$ENTRYPOINT,+e,mexVersion,+e,_shlInit"
            LDCXXOPTIMFLAGS='-O'
            LDCXXDEBUGFLAGS='-g'
#
            LD='ld'
            LDFLAGS="-b +e $ENTRYPOINT +e mexVersion"
            LDOPTIMFLAGS=''
            LDDEBUGFLAGS=''
#
            POSTLINK_CMDS=':'
#----------------------------------------------------------------------------
            ;;
        hp700)
#----------------------------------------------------------------------------
#           what `which cc`
#           HP92453-01 A.10.32.30 HP C Compiler
            CC='cc'
#           Remove +DAportable from CFLAGS if you wish to optimize
#           for target machine
            CFLAGS='+Z -Ae +DAportable -Wp,-H65535'
            CLIBS="$MLIBS -lc"
            COPTIMFLAGS='-O -DNDEBUG'
            CDEBUGFLAGS='-g'
#
#           what `which aCC`
#           HP aC++ B3910B A.01.27
#           HP aC++ B3910B A.01.19.02 Language Support Library
            CXX='aCC'
#           Remove +DAportable from CXXFLAGS if you wish to optimize
#           for target machine
            CXXFLAGS='-AA +Z -D_HPUX_SOURCE +DAportable'
            CXXLIBS="$MLIBS -lstd_v2 -lCsup_v2"
            CXXOPTIMFLAGS='-O -DNDEBUG'
            CXXDEBUGFLAGS='-g'
#
#           what `which f90`
#            HP-UX f90 20010618 (003353)  B3907DB/B3909DB PHSS_23952 also  B.10.20.40
#            HP F90 v2.5.1
            F90LIBDIR='/opt/fortran90/lib'
            FC='f90'
            FFLAGS='+Z +DAportable'
            FLIBS="$MLIBS -L$F90LIBDIR -lF90 -lcl -lc -lisamstub"
            FOPTIMFLAGS='-O'
            FDEBUGFLAGS='-g'
#
            LDCXX="$COMPILER"
            LDCXXFLAGS="-b -Wl,+e,$ENTRYPOINT,+e,mexVersion,+e,_shlInit,+e,errno"
            LDCXXOPTIMFLAGS='-O'
            LDCXXDEBUGFLAGS='-g'
#
            LD='ld'
            LDFLAGS="-b +e $ENTRYPOINT +e mexVersion +e errno"
            LDOPTIMFLAGS=''
            LDDEBUGFLAGS=''
#
            POSTLINK_CMDS=':'
#----------------------------------------------------------------------------
            ;;
        ibm_rs)
#----------------------------------------------------------------------------
#           lslpp -l | vacpp.cmp.core
#           5.0.0.0  COMMITTED  IBM C and C++ Compilers
            CC='cc'
            CFLAGS='-D_THREAD_SAFE -D_ALL_SOURCE -qchars=signed -qlanglvl=ansi'
            CLIBS="$MLIBS -lm "
            COPTIMFLAGS='-O -DNDEBUG'
            CDEBUGFLAGS='-g'
#           lslpp -l vacpp.cmp.core
#           5.0.0.0  COMMITTED  IBM C and C++ Compilers
            CXX='/usr/vacpp/bin/xlC'
            CXXFLAGS='-D_THREAD_SAFE -D_ALL_SOURCE -qrtti=all'
            CXXLIBS="$MLIBS -lm"
            CXXOPTIMFLAGS='-O -DNDEBUG'
            CXXDEBUGFLAGS='-g'
#
#           lslpp -l xlfcmp
#           7.1.0.0  COMMITTED  I XL Fortran Compiler
            FC='f77'
            FFLAGS=''
            FLIBS="$MLIBS -lmat -lm"
            FOPTIMFLAGS='-O'
            FDEBUGFLAGS='-g'
#
            LDCXX='/usr/vacpp/bin/makeC++SharedLib'
            LDCXXFLAGS='-p 0'
            LDCXXOPTIMFLAGS=''
            LDCXXDEBUGFLAGS=''
#
            LD="$COMPILER"
            LDFLAGS="-bE:$TMW_ROOT/extern/lib/$Arch/$MAPFILE -bM:SRE -bnoentry"
            LDOPTIMFLAGS='-O -Wl,-s'
            LDDEBUGFLAGS='-g'
#
            POSTLINK_CMDS=':'
#----------------------------------------------------------------------------
            ;;
        glnx86)
#----------------------------------------------------------------------------
            RPATH="-Wl,-rpath-link,$TMW_ROOT/extern/lib/glnx86,-rpath-link,$TMW_ROOT/bin/glnx86"
#           gcc -v
#           gcc version 2.95.2 19991024 (release)
            CC='gcc'
            CFLAGS='-fPIC -ansi -D_GNU_SOURCE -pthread'
            CLIBS="$RPATH $MLIBS -lm"
            COPTIMFLAGS='-O -DNDEBUG'
            CDEBUGFLAGS='-g'
#           
#           g++ -v
#           gcc version 2.95.2 19991024 (release)
            CXX='g++'
#           Add -fhandle-exceptions to CXXFLAGS to support exception handling
            CXXFLAGS='-fPIC -ansi -D_GNU_SOURCE -pthread'
            CXXLIBS="$RPATH $MLIBS -lm"
            CXXOPTIMFLAGS='-O -DNDEBUG'
            CXXDEBUGFLAGS='-g'
#
#           g77 -v -xf77-version 
#           g77 version 2.95.2 19991024 (release) 
#           (from FSF-g77 version 0.5.25 19991024 (release))
#           NOTE: g77 is not thread safe
            FC='g77'
            FFLAGS='-fPIC'
            FLIBS="$RPATH $MLIBS -lm"
            FOPTIMFLAGS='-O'
            FDEBUGFLAGS='-g'
#
            LD="$COMPILER"
            LDEXTENSION='.mexglx'
            LDFLAGS="-pthread -shared -Wl,--version-script,$TMW_ROOT/extern/lib/glnx86/$MAPFILE"
            LDOPTIMFLAGS='-O'
            LDDEBUGFLAGS='-g'
#
            POSTLINK_CMDS=':'
#----------------------------------------------------------------------------
            ;;
        glnxa64)
#----------------------------------------------------------------------------
	    if [ "$ENTRYPOINT" = "mexLibrary" ]; then
	        MLIBS="-L$TMW_ROOT/bin/glnx86 -lmx -lmex -lmatlb -lmat -lmwservices -lut -lm"
	    else  
	        MLIBS="-L$TMW_ROOT/bin/glnx86 -lmx -lmex -lmat -lm"
	    fi

            RPATH="-Wl,-rpath-link,$TMW_ROOT/extern/lib/glnx86,-rpath-link,$TMW_ROOT/bin/glnx86"
#           gcc -v
#           gcc version 2.95.2 19991024 (release)
            CC='gcc'
            CFLAGS='-m32 -fPIC -ansi -D_GNU_SOURCE -pthread'
            CLIBS="$RPATH $MLIBS -lm"
            COPTIMFLAGS='-O -DNDEBUG'
            CDEBUGFLAGS='-g'
#           
#           g++ -v
#           gcc version 2.95.2 19991024 (release)
            CXX='g++'
#           Add -fhandle-exceptions to CXXFLAGS to support exception handling
            CXXFLAGS='-fPIC -ansi -D_GNU_SOURCE -pthread'
            CXXLIBS="$RPATH $MLIBS -lm"
            CXXOPTIMFLAGS='-O -DNDEBUG'
            CXXDEBUGFLAGS='-g'
#
#           g77 -v -xf77-version 
#           g77 version 2.95.2 19991024 (release) 
#           (from FSF-g77 version 0.5.25 19991024 (release))
#           NOTE: g77 is not thread safe
            FC='g77'
            FFLAGS='-fPIC'
            FLIBS="$RPATH $MLIBS -lm"
            FOPTIMFLAGS='-O'
            FDEBUGFLAGS='-g'
#
            LD="$COMPILER"
            LDEXTENSION='.mexa64'
            LDFLAGS="-m32 -pthread -shared -Wl,--version-script,$TMW_ROOT/extern/lib/glnx86/$MAPFILE"
            LDOPTIMFLAGS='-O'
            LDDEBUGFLAGS='-g'
#
            POSTLINK_CMDS=':'
#----------------------------------------------------------------------------
            ;;
        sgi)
#----------------------------------------------------------------------------
#           cc -version
#           MIPSpro Compilers: Version 7.3.1.2m
            CC='cc'
            CFLAGS='-n32 -signed -OPT:IEEE_NaN_inf=ON -D_POSIX_C_SOURCE=199506L -D__EXTENSIONS__ -D_XOPEN_SOURCE -D_XOPEN_SOURCE_EXTENDED'
            CLIBS="-dont_warn_unused $MLIBS -lm -lc"
            COPTIMFLAGS='-O -DNDEBUG'
            CDEBUGFLAGS='-g'
#           
#           CC -version
#           MIPSpro Compilers: Version 7.3.1.2m
            CXX='CC'
#           Add -exceptions to CXXFLAGS to support exception handling
            CXXFLAGS='-n32 -OPT:IEEE_NaN_inf=ON -D_POSIX_C_SOURCE=199506L -D__EXTENSIONS__ -D_XOPEN_SOURCE -D_XOPEN_SOURCE_EXTENDED  -LANG:STD -ptused'
            CXXLIBS="-dont_warn_unused $MLIBS -lm -lC -lCio"
            CXXOPTIMFLAGS='-O -DNDEBUG'
            CXXDEBUGFLAGS='-g'
#
#           f77 -version
#           MIPSpro Compilers: Version 7.3.1.2m
            FC='f77'
            FFLAGS='-n32 -OPT:IEEE_NaN_inf=ON'
            FLIBS="-dont_warn_unused $MLIBS -lm"
            FOPTIMFLAGS='-O'
            FDEBUGFLAGS='-g'
#
            LD="$COMPILER"
            LDFLAGS="-n32 -shared -exported_symbol $ENTRYPOINT -exported_symbol mexVersion"
            LDOPTIMFLAGS='-O'
            LDDEBUGFLAGS='-g'
#
            POSTLINK_CMDS=':'
#----------------------------------------------------------------------------
            ;;
        sol2)
#----------------------------------------------------------------------------
#           cc -V
#           WorkShop Compilers 5.0 98/12/15 C 5.0
            CC='cc'
            CFLAGS='-KPIC -dalign -xlibmieee -D__EXTENSIONS__ -D_POSIX_C_SOURCE=199506L -mt'
            CLIBS="$MLIBS -lm -lc"
            COPTIMFLAGS='-xO3 -xlibmil -DNDEBUG'
            CDEBUGFLAGS='-g'
#           
#           CC -V
#           WorkShop Compilers 5.0 98/12/15 C++ 5.0
            CXX='CC -compat=5'
            CCV=`CC -V 2>&1`
            version=`expr "$CCV" : '.*\([0-9][0-9]*\)\.'`
            if [ "$version" = "4" ]; then
                    echo "SC5.0 or later C++ compiler is required"
            fi
            CXXFLAGS='-KPIC -dalign -xlibmieee -D__EXTENSIONS__ -D_POSIX_C_SOURCE=199506L -mt'
            CXXLIBS="$MLIBS -lm -lCstd -lCrun"
            CXXOPTIMFLAGS='-xO3 -xlibmil -DNDEBUG'
            CXXDEBUGFLAGS='-g'
#
#           f77 -V
#           WorkShop Compilers 5.0 99/09/16 FORTRAN 77 5.0 patch 107596-03
            FC='f77'
            FFLAGS='-KPIC -dalign -mt'
            FLIBS="$MLIBS -lF77 -lM77 -lsunmath -lm -lcx -lc"
            FOPTIMFLAGS='-O'
            FDEBUGFLAGS='-g'
#
            LD="$COMPILER"
            LDEXTENSION='.mexsol'
            LDFLAGS="-G -mt -M$TMW_ROOT/extern/lib/$Arch/$MAPFILE"
            LDOPTIMFLAGS='-O'
            LDDEBUGFLAGS='-g'
#
            POSTLINK_CMDS=':'
#----------------------------------------------------------------------------
            ;;
        mac)
#----------------------------------------------------------------------------
            CC='cc'
            CFLAGS='-fno-common -no-cpp-precomp'

            CLIBS="$MLIBS"

            COPTIMFLAGS='-O3 -DNDEBUG'
            CDEBUGFLAGS='-g'

            if [ -f /usr/bin/g++2 ]; then
            CXX=g++2
            else
            CXX=c++
            fi
            CXXFLAGS='-fno-common -no-cpp-precomp'
            CXXLIBS="$MLIBS -lstdc++"
            CXXOPTIMFLAGS='-O3 -DNDEBUG'
            CXXDEBUGFLAGS='-g'
#
            FC='f77'
            FFLAGS='-f -N15 -N11 -s -Q51 -W'
            ABSOFTLIBDIR=`which $FC | sed -n -e '1s|bin/'$FC'|lib|p'`
            FLIBS="-L$ABSOFTLIBDIR -lfio -lf77math"
            FOPTIMFLAGS='-O'
            FDEBUGFLAGS='-g'
#
            LD="$CC"
            LDEXTENSION='.mexmac'
            LDFLAGS="-bundle -Wl,-flat_namespace -undefined suppress"
            LDOPTIMFLAGS='-O'
            LDDEBUGFLAGS='-g'
#
            POSTLINK_CMDS='nmedit -s $TMW_ROOT/extern/lib/$Arch/$MAPFILE $mex_file'
#----------------------------------------------------------------------------
            ;;
    esac
#############################################################################
#
# Architecture independent lines:
#
#     Set and uncomment any lines which will apply to all architectures.
#
#----------------------------------------------------------------------------
#           CC="$CC"
#           CFLAGS="$CFLAGS"
#           COPTIMFLAGS="$COPTIMFLAGS"
#           CDEBUGFLAGS="$CDEBUGFLAGS"
#           CLIBS="$CLIBS"
#
#           FC="$FC"
#           FFLAGS="$FFLAGS"
#           FOPTIMFLAGS="$FOPTIMFLAGS"
#           FDEBUGFLAGS="$FDEBUGFLAGS"
#           FLIBS="$FLIBS"
#
#           LD="$LD"
#           LDFLAGS="$LDFLAGS"
#           LDOPTIMFLAGS="$LDOPTIMFLAGS"
#           LDDEBUGFLAGS="$LDDEBUGFLAGS"
#----------------------------------------------------------------------------
#############################################################################
