#!/bin/bash
CURRENT_DIR=$(cd `dirname $0` && pwd)
OUT_DIR=$CURRENT_DIR/out
SRC_DIR=$CURRENT_DIR/../../../src
BIN_DIR=~/Development/build_libsbml
CMAKE=cmake
PYTHON=python2.7

$CMAKE -DOUT_DIR=$OUT_DIR -DSRC_DIR=$SRC_DIR -DBIN_DIR=$BIN_DIR -P create_package.cmake

cd $OUT_DIR
$PYTHON setup.py sdist
$PYTHON setup.py bdist
cd ..