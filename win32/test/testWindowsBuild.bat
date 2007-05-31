@echo off
cd ..
cd bin
validateSBML ../test/test.xml
if ERRORLEVEL 0 echo validateSBML passed
pause
echoSBML ../test/test.xml ../test/test-echo.xml
if ERRORLEVEL 0 echo echoSBML passed
pause
convertSBML ../test/test.xml ../test/test-new.xml
if ERRORLEVEL 0 validateSBML ../test/test-new.xml
if ERRORLEVEL 0 echo conversion produced a valid level 2 file
pause
convertSBML ../test/test-new.xml ../test/test-new1.xml
if ERRORLEVEL 0 validateSBML ../test/test-new1.xml
if ERRORLEVEL 0 echo conversion produced a valid level 1 file
pause
cd ..
cd test
