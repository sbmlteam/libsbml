@echo off
SET BASE_DIR=%~dp0
Setlocal EnableDelayedExpansion

call build_libsbml.bat
call build_python.bat
call build_conda.bat 
call remove_files.bat