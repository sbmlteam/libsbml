echo off
echo '========================'
echo 'libsbml test'
echo '========================'
call testLibsbml.bat
cd csharp
call testCSharp.bat
cd ..
cd java
echo '========================'
echo 'Java test'
echo '========================'
call testJava.bat
cd ..
cd perl
echo '========================'
echo 'Perl test'
echo '========================'
call testPerl.bat
cd ..
cd python
echo '========================'
echo 'Python test'
echo '========================'
call testPython.bat
cd ..
