#!/usr/bin/env bash
# 
# This script counts lines of code.
# 

THIS_DIR=$(cd `dirname $0` && pwd)

CLOC=${CLOC:="${THIS_DIR}/bin/cloc-1.62"}

command -v $CLOC >/dev/null 2>&1 || { CLOC=cloc; }
command -v $CLOC >/dev/null 2>&1 || { echo >&2 "cloc cannot be found, please update the CLOC variable or add it to the path."; }

CLOC_DEFAULT_ARGS=--autoconf
LIBSBML_DIR=$THIS_DIR/../../../..
LIBSBML_DIR=$(cd `dirname $LIBSBML_DIR` && pwd)
OUT_DIR=$THIS_DIR

# make sure it also works for CYGWIN
UNAME=$(uname -s)
UNAME=${UNAME:0:6}
if [ "$UNAME" == "CYGWIN" ];then
  LIBSBML_DIR=$(cygpath -w $LIBSBML_DIR)
  OUT_DIR=$(cygpath -w $OUT_DIR)  
fi

EXCLUDE_DIRS=dev,docs,examples,macosx

# additional things to do
# make .t files counted as Perl (these are Perl test files)
FORCED="Perl",t

# need to set these as appropriate for the count wanted
ADDITIONAL_EXCLUDES=bindings
OUTPUT_FILE=$OUT_DIR/no-bindings.csv

# run the count
$CLOC $CLOC_DEFAULT_ARGS $LIBSBML_DIR --exclude-dir=$EXCLUDE_DIRS,$ADDITIONAL_EXCLUDES --report-file=$OUTPUT_FILE --csv --force-lang=$FORCED

