#!/bin/bash
DIRECTORY=$(cd `dirname $0` && pwd)
BASEDIR=$DIRECTORY/../../../
cd $BASEDIR
PERL=perl

FILE=$1
SUBDIR=
case "$FILE" in
  *math*)
    SUBDIR=math
    ;;
  *annotation*)
    SUBDIR=annotation
    ;;
  *xml*)
    SUBDIR=xml
    ;;
  *sbml*)
    SUBDIR=sbml
    ;;
esac

if [ "$FILE" = "" ]
then
echo "usage: convertTests testfile"
echo ""
echo "Converts libSBML tests into Python, Ruby, CSharp and Java"
echo ""
fi

if [ ! -f "./configure" ];
then
  echo "Please run this script from the libsbml root directory."
  exit 1
fi
if [ ! -f $FILE ];
then
    echo "File not found!"
    exit 1
fi

$PERL  ./dev/utilities/translateTests/translateTests.pl -o ./src/bindings/python/test/$SUBDIR -p $FILE
$PERL  ./dev/utilities/translateTests/translateTests.pl -o ./src/bindings/ruby/test/$SUBDIR -r $FILE
$PERL  ./dev/utilities/translateTests/translateTests.pl -o ./src/bindings/csharp/test/$SUBDIR -c $FILE
$PERL  ./dev/utilities/translateTests/translateTests.pl -o  ./src/bindings/java/test/org/sbml/libsbml/test/$SUBDIR -j $FILE
