@echo off
REM 
REM This script is necessary to use mkoctfile on windows with octave 3.2.4
REM 
SET PATH=C:\Octave\3.2.4_gcc-4.4.0\bin;C:\Octave\3.2.4_gcc-4.4.0\mingw32\bin;%PATH%;
C:\Octave\3.2.4_gcc-4.4.0\bin\mkoctfile.exe %*