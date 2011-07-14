An overview of this - since it is not the same as the other unit tests.

The code that tests the validators basically initialises each different
validator individually, reads a whole set of numbered tests through it and
checks that the output is as expected.

So the main function has lines

  failed += runTests( "Testing General XML Consistency Constraints (10000 -
  10199)",
          "test-data", 10000, 10199, runMainTest, library);

  failed += runTests( "Testing L3v1 Compatibility Constraints (96000 -
  96999)",
          "test-data-conversion", 96000, 96999, runL3v1Test, library);

which indicate the directory in which to look for files, a number range and
a validator. The library parameter is used as a double check on which xml
parser is being used as in some cases certain tests are excluded. The code
opens the relevant test-directories and reads files starting with a five
digit number in the range given.

The xml files are all named either:
12345-fail-0n-ab.xml
12345-pass-00-ab.xml
12345-fail-0n-ab-67890.xml

which basically tells the code what is expected -  whether the file should
report a constraint ; which number constraint should be reported; how many
times the error should be reported; whether a different constraint should
also be reported.

12345 is the number of the constraint being tested.
fail-0n - this file should fail
with the n indicating how many times the failure should occur.

Generally it is 01 - ie one fail. However sometimes when reporting a cycle
the code will report the error more than once. Also in the case of schema
error where I ran out of numbers (the ab in the filename)  later tests are
written to fail twice so I can reuse the numbers.

pass-00 - this file should pass
00 - since it it does not fail

ab - is just the number to create unique instances of the file.

Some files are numbered

12345-fail-01-ab-67890.xml

These are files that fail two different numbered constraints with what is
essentially one error. The second number tells the test code that that is
going to happen but the number of fails is still related to the first
constraint.

e.g. 21116-fail-01-02-21111.xml

fails 21116: A speciesReference must have the attribute species
and also fails 21111: the species attribute must be the identifier of an
existing species

[since the species attribute is missing - it is also not the id of a
species !!]

When adding validation it is useful to add as many fail cases so as to test
all possible branches of the code that logs the failure plus at least one
pass case. It is also a good idea to do this in all levels/versions since
some rules only apply to certain L/V combinations. Bit of a pain since
libsbml generally cannot be used to convert invalid sbml and these cases
are all about invalid sbml :-)

Hope this makes it clear. We do intend to split the test-data directory and
I would do that to be consistent with which validator is being tested.

Sarah

