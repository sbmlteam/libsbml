use Test;
BEGIN { plan tests => 64 };

use File::Spec;
use LibSBML;
use strict;
use vars qw/$testDataDir $perlTestDir $file $rd $d $fatals $str $pm/;
    
#########################

my @dataPathFrags = (File::Spec->updir(),
                     File::Spec->updir(),
                     qw/sbml test test-data/);
my @testPathFrags = (File::Spec->curdir(), 't');
$testDataDir = File::Spec->catdir(@dataPathFrags);
$perlTestDir = File::Spec->catdir(@testPathFrags);
$rd = new LibSBML::SBMLReader;

# nonexisting file
$file = File::Spec->catfile($testDataDir, 'nonexistent.xml');
$d = $rd->readSBML($file);
$fatals = $d->getNumFatals();
ok($fatals > 0);
$pm = $d->getFatal($fatals);
ok(!defined($pm));
$pm = $d->getFatal($fatals-1);
ok(defined($pm));
ok($pm->getId() == $LibSBML::SBML_READ_ERROR_FILE_NOT_FOUND);

# non-sbml file: xsd file
$file = File::Spec->catfile($testDataDir, 'sbml-l1v1.xsd');
$d = $rd->readSBML($file);
$fatals = $d->getNumFatals();
ok($fatals > 0);
$pm = $d->getFatal($fatals-1);
ok(defined($pm));
ok($pm->getId() == $LibSBML::SBML_READ_ERROR_NOT_SBML);

# non-sbml file: perl test file
$file = File::Spec->catfile($perlTestDir, '01__LoadModule.t');
$d = $rd->readSBML($file);
$fatals = $d->getNumFatals();
ok($fatals > 0);
$pm = $d->getFatal($fatals-1);
ok(defined($pm));
ok($pm->getId() == 100);
ok($pm->getMessage() eq 'Invalid document structure');
ok($pm->getLine() == 1);
ok($pm->getColumn() == 1);

# turn on basic validation
ok($rd->getSchemaValidationLevel == $LibSBML::XML_SCHEMA_VALIDATION_NONE);
$rd->setSchemaValidationLevel($LibSBML::XML_SCHEMA_VALIDATION_BASIC)
    unless($^O eq 'cygwin');

# sbml file l1v1 from file: schema error
$file = File::Spec->catfile($testDataDir, 'l1v1-branch-schema-error.xml');
$d = $rd->readSBML($file);
ok($d->getNumFatals(), 0);
$^O eq 'cygwin' ? ok($d->getNumErrors(), 0) : ok($d->getNumErrors(), 1);
ok($d->getNumWarnings(), 0);
ok($d->checkConsistency(), 0);
ok($d->getLevel(), 1);
ok($d->getVersion(), 1);
# sbml file l1v1 from string: schema error
$str  = slurp_file($file);
$d = $rd->readSBMLFromString($str) if defined($str);
skip(!defined($str), $d->getNumFatals(), 0);
$^O eq 'cygwin' ? skip(!defined($str), $d->getNumErrors(), 0) : skip(!defined($str), $d->getNumErrors(), 1);
skip(!defined($str), $d->getNumWarnings(), 0);
skip(!defined($str), $d->checkConsistency(), 0);
skip(!defined($str), $d->getLevel(), 1);
skip(!defined($str), $d->getVersion(), 1);

# proper sbml file l1v1 from file
$file = File::Spec->catfile($testDataDir,'l1v1-branch.xml');
$d = $rd->readSBML($file);
ok($d->getNumFatals(), 0, $d->LibSBML::printFatals(\*STDERR));
ok($d->getNumErrors(), 0);
ok($d->getNumWarnings(), 0);
ok($d->checkConsistency(), 0);
ok($d->getLevel(), 1);
ok($d->getVersion(), 1);
# proper sbm file l1v1 from string
$str  = slurp_file($file);
$d = $rd->readSBMLFromString($str) if defined($str);
skip(!defined($str), $d->getNumFatals(), 0);
skip(!defined($str), $d->getNumErrors(), 0);
skip(!defined($str), $d->getNumWarnings(), 0);
skip(!defined($str), $d->checkConsistency(), 0);
skip(!defined($str), $d->getLevel(), 1);
skip(!defined($str), $d->getVersion(), 1);

# proper sbm file l1v2 from file
$file = File::Spec->catfile($testDataDir,'l1v2-branch.xml');
$d = $rd->readSBML($file);
ok($d->getNumFatals(), 0);
ok($d->getNumErrors(), 0);
ok($d->getNumWarnings(), 0);
ok($d->checkConsistency(), 0);
ok($d->getLevel(), 1);
ok($d->getVersion(), 2);
# proper sbm file l1v2 from string
$str  = slurp_file($file);
$d = $rd->readSBMLFromString($str) if defined($str);
skip(!defined($str), $d->getNumFatals(), 0);
skip(!defined($str), $d->getNumErrors(), 0);
skip(!defined($str), $d->getNumWarnings(), 0);
skip(!defined($str), $d->checkConsistency(), 0);
skip(!defined($str), $d->getLevel(), 1);
skip(!defined($str), $d->getVersion(), 2);

# turn of schema validation (takes quite a )
$^O eq 'cygwin' 
    ? ok($rd->getSchemaValidationLevel(),
	 $LibSBML::XML_SCHEMA_VALIDATION_NONE) 
    : ok($rd->getSchemaValidationLevel(),
	 $LibSBML::XML_SCHEMA_VALIDATION_BASIC);
$rd->setSchemaValidationLevel($LibSBML::XML_SCHEMA_VALIDATION_NONE);
ok($rd->getSchemaValidationLevel == $LibSBML::XML_SCHEMA_VALIDATION_NONE);

# proper sbm file l2v1 from file
$file = File::Spec->catfile($testDataDir,'l2v1-branch.xml');
$d = $rd->readSBML($file);
ok($d->getNumFatals(), 0);
ok($d->getNumErrors(), 0);
ok($d->getNumWarnings(), 0);
ok($d->checkConsistency(), 0);
ok($d->getLevel(), 2);
ok($d->getVersion(), 1);
# proper sbm file l2v1 from string
$str  = slurp_file($file);
$d = $rd->readSBMLFromString($str) if defined($str);
skip(!defined($str), $d->getNumFatals(), 0);
skip(!defined($str), $d->getNumErrors(), 0);
skip(!defined($str), $d->getNumWarnings(), 0);
skip(!defined($str), $d->checkConsistency(), 0);
skip(!defined($str), $d->getLevel(), 2);
skip(!defined($str), $d->getVersion(), 1);

#---
sub slurp_file {
    # file is automatically close at block exit
    return do { local $/; <FH> } if open(FH, '<', "@{[shift()]}");
    return undef;
}
