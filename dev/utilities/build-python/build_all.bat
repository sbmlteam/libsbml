@echo off
SET BASE_DIR=%~dp0
Setlocal EnableDelayedExpansion

:: build libsbml
call build_libsbml.bat

:: create all python binaries
call build_python.bat

:: create conda binaries in dist\conda
call build_conda.bat 

:: you can limit what will be deleted by providing some 
:: options here. They are: 
::
::          ALL     - delete all files created
::          BUILD   - delete build dirs
::          INSTALL - delete install dirs
::          TEMP    - delete python temp files and dirs
::
call remove_files.bat
