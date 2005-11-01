@echo off
cd ..
cd bin
readSBML ../test/test.xml
if ERRORLEVEL 0 echo readSBML passed
pause
validateSBML ../test/test.xml
if ERRORLEVEL 0 echo validateSBML passed
pause
printSBML ../test/test.xml
if ERRORLEVEL 0 echo printSBML passed
pause
printMath ../test/test.xml
if ERRORLEVEL 0 echo printMath passed
pause
convertSBML ../test/test.xml ../test/test-new.xml
if ERRORLEVEL 0 validateSBML ../test/test-new.xml
if ERRORLEVEL 0 echo conversion produced a valid level 2 file
pause
convertSBML ../test/test-new.xml ../test/test-new1.xml
if ERRORLEVEL 0 validateSBML ../test/test-new1.xml
if ERRORLEVEL 0 echo conversion produced a valid level 1 file
drawMath ../test/test.xml ../test/drawout.dot
if ERRORLEVEL 0 echo drawMath passed
pause
cd ..
cd test
