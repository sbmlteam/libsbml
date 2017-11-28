@echo off
SET THIS_DIR=%~dp0
SET CMAKE=cmake
pushd %THIS_DIR%
%CMAKE% -DSRC_DIR="..\..\.." -P create_archives.cmake
popd