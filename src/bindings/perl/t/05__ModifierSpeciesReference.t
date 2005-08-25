use Test;
BEGIN { plan tests => 17 };

use LibSBML;
use strict;

#########################

my $species = 's5';

# create w/ species
my $msr = new LibSBML::ModifierSpeciesReference($species);
ok($msr->getTypeCode() == $LibSBML::SBML_MODIFIER_SPECIES_REFERENCE);
ok($msr->getMetaId(), '');
ok($msr->getNotes(), '');
ok($msr->getAnnotation(), '');
ok($msr->isSetSpecies(), 1);
ok($msr->getSpecies(), $species);


# create w/o arguments
$msr = new LibSBML::ModifierSpeciesReference();
ok($msr->getTypeCode() == $LibSBML::SBML_MODIFIER_SPECIES_REFERENCE);
ok($msr->getMetaId(), '');
ok($msr->getNotes(), '');
ok($msr->getAnnotation(), '');
ok($msr->isSetSpecies(), 0);
$msr->setSpecies($species);
ok($msr->isSetSpecies(), 1);
ok($msr->getSpecies(), $species);
# reflexive case
$msr->setSpecies($msr->getSpecies());
ok($msr->isSetSpecies(), 1);
ok($msr->getSpecies(), $species);
$msr->setSpecies('');
ok($msr->isSetSpecies(), 0);
ok($msr->getSpecies(), '');

__END__
