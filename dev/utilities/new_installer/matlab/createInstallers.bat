@echo off
REM 
REM  Simple build script to create the matlab installers it just 
REM  calls createInstallFiles with the defaults used by the one
REM  click installer. 
REM 

SET MATLAB_INST_DIR=%~dp0
cd /d %MATLAB_INST_DIR%

SET COMPILER=C:\Program Files (x86)\Inno Setup 5\ISCC.exe

call createInstallFiles.bat 32 c:\32bit -silent
"%COMPILER%" install_matlab_libsbml_script_xml_win32.iss

call createInstallFiles.bat 64 c:\64bit -silent
"%COMPILER%" install_matlab_libsbml_script_xml_win64.iss


cd /d %MATLAB_INST_DIR%

