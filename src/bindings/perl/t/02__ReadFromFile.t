# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl GRN-Models.t'

#########################

# change 'tests => 1' to 'tests => last_test_to_print';

use Test;
BEGIN { plan tests => 69 };

use LibSBML;
use File::Spec;
use strict;

#########################

# Insert your test code below, the Test::More module is use()ed here so read
# its man page ( perldoc Test::More ) for help writing this test script.
my $file = File::Spec->catfile(File::Spec->rel2abs('t'),'l1v1-branch.xml');
my $rd = new LibSBML::SBMLReader;
my $d = $rd->readSBML($file);
ok($d->getLevel(), 1);
ok($d->getVersion(), 1);

my $m = $d->getModel();
ok($m->getName(), 'Branch');
ok($m->getNumCompartments(), 1);
ok($m->getNumSpecies(), 4);
ok($m->getNumReactions(), 3);

my $c = $m->getCompartment(0);
ok($c->getName(), 'compartmentOne');
ok($c->getVolume(), 1.0);

my $s = $m->getSpecies(0);
ok($s->getName(), 'S1');
ok($s->getCompartment(), 'compartmentOne');
ok($s->getInitialAmount(), 0.0);
ok($s->getBoundaryCondition(), 0);

$s = $m->getSpecies(1);
ok($s->getName(), 'X0');
ok($s->getCompartment(), 'compartmentOne');
ok($s->getInitialAmount(), 0.0);
ok($s->getBoundaryCondition(), 1);

$s = $m->getSpecies(2);
ok($s->getName(), 'X1');
ok($s->getCompartment(), 'compartmentOne');
ok($s->getInitialAmount(), 0.0);
ok($s->getBoundaryCondition(), 1);

$s = $m->getSpecies(3);
ok($s->getName(), 'X2');
ok($s->getCompartment(), 'compartmentOne');
ok($s->getInitialAmount(), 0.0);
ok($s->getBoundaryCondition(), 1);

#
my $r = $m->getReaction(0);
ok($r->getName(), 'reaction_1');
ok($r->getReversible(), 0);
ok($r->getFast(), 0);
ok($r->getNumReactants(), 1);
ok($r->getNumProducts(), 1);
my $sr = $r->getReactant(0);
ok($sr->getSpecies(), 'X0');
ok($sr->getStoichiometry(), 1);
ok($sr->getDenominator(), 1);
$sr = $r->getProduct(0);
ok($sr->getSpecies(), 'S1');
ok($sr->getStoichiometry(), 1);
ok($sr->getDenominator(), 1);
my $kl = $r->getKineticLaw();
ok($kl->getFormula(), 'k1 * X0');
ok($kl->getNumParameters(), 1);
my $p = $kl->getParameter(0);
ok($p->getName(), 'k1');
ok($p->getValue(), 0);

$r = $m->getReaction(1);
ok($r->getName(), 'reaction_2');
ok($r->getReversible(), 0);
ok($r->getFast(), 0);
ok($r->getNumReactants(), 1);
ok($r->getNumProducts(), 1);
$sr = $r->getReactant(0);
ok($sr->getSpecies(), 'S1');
ok($sr->getStoichiometry(), 1);
ok($sr->getDenominator(), 1);
$sr = $r->getProduct(0);
ok($sr->getSpecies(), 'X1');
ok($sr->getStoichiometry(), 1);
ok($sr->getDenominator(), 1);
$kl = $r->getKineticLaw();
ok($kl->getFormula(), 'k2 * S1');
ok($kl->getNumParameters(), 1);
$p = $kl->getParameter(0);
ok($p->getName(), 'k2');
ok($p->getValue(), 0);

$r = $m->getReaction(2);
ok($r->getName(), 'reaction_3');
ok($r->getReversible(), 0);
ok($r->getFast(), 0);
ok($r->getNumReactants(), 1);
ok($r->getNumProducts(), 1);
$sr = $r->getReactant(0);
ok($sr->getSpecies(), 'S1');
ok($sr->getStoichiometry(), 1);
ok($sr->getDenominator(), 1);
$sr = $r->getProduct(0);
ok($sr->getSpecies(), 'X2');
ok($sr->getStoichiometry(), 1);
ok($sr->getDenominator(), 1);
$kl = $r->getKineticLaw();
ok($kl->getFormula(), 'k3 * S1');
ok($kl->getNumParameters(), 1);
$p = $kl->getParameter(0);
ok($p->getName(), 'k3');
ok($p->getValue(), 0);
