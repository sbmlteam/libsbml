rmdir /S libsbml-matlab

mkdir libsbml-matlab
cd libsbml-matlab

rem copy files

copy ..\..\..\..\..\AUTHORS.txt AUTHORS.txt
copy ..\..\..\..\..\COPYING.txt COPYING.txt
copy ..\..\..\..\..\FUNDING.txt FUNDING.txt
copy ..\..\..\..\..\VERSION.txt VERSION.txt
copy ..\..\..\..\..\COPYING.html COPYING.html
copy ..\Uninstall_libSBML_matlab.bat Uninstall_libSBML_matlab.bat

rem make new directories

mkdir matlab
mkdir install

rem matlab directory

cd matlab
copy ..\..\..\..\..\..\src\bindings\matlab\TranslateSBML.m TranslateSBML.m
copy ..\..\..\..\..\..\src\bindings\matlab\TranslateSBML.c TranslateSBML.c
copy ..\..\..\..\..\..\src\bindings\matlab\OutputSBML.m OutputSBML.m
copy ..\..\..\..\..\..\src\bindings\matlab\OutputSBML.c OutputSBML.c
copy ..\..\..\..\..\..\src\bindings\matlab\Contents.m Contents.m
copy ..\..\..\..\..\..\src\bindings\matlab\CheckAndConvert.m  CheckAndConvert.m
copy ..\..\..\..\..\..\src\bindings\matlab\isoctave.m  isoctave.m
copy ..\..\..\..\..\..\src\bindings\matlab\ConvertFormulaToMathML.m  ConvertFormulaToMathML.m
copy ..\..\..\..\..\..\src\bindings\matlab\isSBML_Model.m  isSBML_Model.m
copy ..\..\..\..\..\..\src\bindings\matlab\buildSBML.m  buildSBML.m
copy ..\..\..\..\..\..\src\bindings\matlab\installSBML.m  installSBML.m
copy ..\..\..\..\..\..\src\bindings\matlab\test.xml test.xml

REM these need to find the static matlab builds

copy ..\..\..\friday-25\libSBML-4.3.0-win32\libSBML-4.3.0-win32\bindings\matlab\TranslateSBML.mexw32 TranslateSBML.mexw32
copy ..\..\..\friday-25\libSBML-4.3.0-win32\libSBML-4.3.0-win32\bindings\matlab\OutputSBML.mexw32 OutputSBML.mexw32

cd ..

cd install
copy ..\..\install\install.bat install.bat
cd ..


cd C:\libsbml_trunk\dev\utilities\win_installer\matlab












