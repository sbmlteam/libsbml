@rem
@rem Filename    : makedist.bat
@rem Description : Builds a native Windows distribution of libsbml using MSVC++
@rem Author(s)   : The SBML Team <sbml-team@caltech.edu>
@rem Created     : 2003-07-30
@rem Revision    : $Id$
@rem Source      : $Source$
@rem
@rem Copyright 2003 California Institute of Technology and
@rem Japan Science and Technology Corporation.
@rem
@rem This library is free software@rem you can redistribute it and/or modify it
@rem under the terms of the GNU Lesser General Public License as published
@rem by the Free Software Foundation@rem either version 2.1 of the License, or
@rem any later version.
@rem
@rem This library is distributed in the hope that it will be useful, but
@rem WITHOUT ANY WARRANTY, WITHOUT EVEN THE IMPLIED WARRANTY OF
@rem MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  The software and
@rem documentation provided hereunder is on an "as is" basis, and the
@rem California Institute of Technology and Japan Science and Technology
@rem Corporation have no obligations to provide maintenance, support,
@rem updates, enhancements or modifications.  In no event shall the
@rem California Institute of Technology or the Japan Science and Technology
@rem Corporation be liable to any party for direct, indirect, special,
@rem incidental or consequential damages, including lost profits, arising
@rem out of the use of this software and its documentation, even if the
@rem California Institute of Technology and/or Japan Science and Technology
@rem Corporation have been advised of the possibility of such damage.  See
@rem the GNU Lesser General Public License for more details.
@rem
@rem You should have received a copy of the GNU Lesser General Public License
@rem along with this library; if not, write to the Free Software Foundation,
@rem Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
@rem
@rem The original code contained here was initially developed by:
@rem
@rem     Andrew Finney, Ben Bornstein
@rem
@rem     The SBML Team
@rem     Control and Dynamical Systems, MC 107-81
@rem     California Institute of Technology
@rem     Pasadena, CA, 91125, USA
@rem
@rem     http://sbml.org
@rem     mailto:sbml-team@caltech.edu
@rem
@rem Contributor(s):
@rem


PATH=%PATH%;c:\Program Files\Microsoft Visual Studio\VC98\Bin
PATH=%PATH%;c:\Program Files\Microsoft Visual Studio\Common\MSDev98\Bin
PATH=%PATH%;c:\Program Files\Microsoft Visual Studio\VC98\Lib

set INCLUDE=%INCLUDE%;c:\Program Files\Microsoft Visual Studio\VC98\INCLUDE
set INCLUDE=%INCLUDE%;c:\Program Files\Microsoft Visual Studio\VC98\MFC\INCLUDE
set INCLUDE=%INCLUDE%;c:\Program Files\Microsoft Visual Studio\VC98\ATL\INCLUDE
set INCLUDE=%INCLUDE%;include

set LIB=%LIB%;c:\Program Files\Microsoft Visual Studio\VC98\LIB
set LIB=%LIB%;c:\Program Files\Microsoft Visual Studio\VC98\MFC\LIB
set LIB=%LIB%;bin

call make libsbml.mak "libsbml - Win32 Debug"   clean
call make libsbml.mak "libsbml - Win32 Release" clean

call make convertsbml.mak "convertSBML - Win32 Debug"   clean
call make convertsbml.mak "convertSBML - Win32 Release" clean

call make printsbml.mak "printSBML - Win32 Debug"   clean
call make printsbml.mak "printSBML - Win32 Release" clean

call make readsbml.mak "readSBML - Win32 Debug"   clean
call make readsbml.mak "readSBML - Win32 Release" clean

call make translatemath.mak "translateMath - Win32 Debug"   clean
call make translatemath.mak "translateMath - Win32 Release" clean

call make validatesbml.mak  "validateSBML - Win32 Debug"   clean
call make validatesbml.mak  "validateSBML - Win32 Release" clean

call make libsbml.mak "libsbml - Win32 Debug"
call make libsbml.mak "libsbml - Win32 Release"

call make convertsbml.mak   "convertSBML - Win32 Release"
call make printsbml.mak     "printSBML - Win32 Release"
call make readsbml.mak      "readSBML - Win32 Release"
call make translatemath.mak "translateMath - Win32 Release"
call make validatesbml.mak  "validateSBML - Win32 Release"

call deltree "Debug"
call deltree "Release"
call deltree "convertSBML___Win32_Debug"
call deltree "convertSBML___Win32_Release"
call deltree "printSBML___Win32_Debug"
call deltree "printSBML___Win32_Release"
call deltree "readSBML___Win32_Debug"
call deltree "readSBML___Win32_Release"
call deltree "translateMath___Win32_Debug"
call deltree "translateMath___Win32_Release"
call deltree "validateSBML___Win32_Debug"
call deltree "validateSBML___Win32_Release"

del *.ncb
del *.opt
del *.plg

mkdir include\sbml
echo a | xcopy ..\src\sbml include\sbml
