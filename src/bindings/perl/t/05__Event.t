use Test;
BEGIN { plan tests => 64 };

use LibSBML;
use strict;
use vars qw/$id $name $trigger $t $delay $d $units/;

#########################

$id = 'e1';
$trigger = 'leq(P1,t)';
$t = '';
$delay = 't+1';
$d = '';
$units = 'second';
$name = 'Set k2 to zero when P1 <= t';

# create w/ AST
my $e = new LibSBML::Event('e1', LibSBML::parseFormula($trigger));
ok($e->getTypeCode() == $LibSBML::SBML_EVENT);
ok($e->getMetaId(), '');
ok($e->getNotes(), '');
ok($e->getAnnotation(), '');
ok($e->isSetId(), 1);
ok($e->getId(), 'e1');
ok($e->isSetName(), 0);
ok($e->getName(), '');
ok($e->isSetTrigger(), 1);
($t = LibSBML::formulaToString($e->getTrigger())) =~ s/\s+//g;
ok($t, $trigger);

# creation w/o arguments
$e = new LibSBML::Event();
ok($e->getTypeCode() == $LibSBML::SBML_EVENT);
ok($e->getMetaId(), '');
ok($e->getNotes(), '');
ok($e->getAnnotation(), '');
ok($e->isSetId(), 0);
ok($e->getId(), '');
ok($e->isSetName(), 0);
ok($e->getName(), '');
ok($e->isSetTrigger(), 0);
ok($e->getTrigger(), undef);
ok($e->isSetDelay(), 0);
ok($e->getDelay(), undef);
ok($e->isSetTimeUnits(), 0);
ok($e->getTimeUnits(), '');

# set/get id
$e->setId($id);
ok($e->isSetId(), 1);
ok($e->getId(), $id);
# reflexive case
$e->setId($e->getId());
ok($e->isSetId(), 1);
ok($e->getId(), $id);
$e->setId('');
ok($e->isSetId(), 0);
ok($e->getId(), '');

# set/get name
$e->setName($name);
ok($e->isSetName(), 1);
ok($e->getName(), $name);
# reflexive case
$e->setName($e->getName());
ok($e->isSetName(), 1);
ok($e->getName(), $name);
$e->setName('');
ok($e->isSetName(), 0);
ok($e->getName(), '');

# set/get trigger
$e->setTrigger(LibSBML::parseFormula($trigger));
ok($e->isSetTrigger(), 1);
($t = LibSBML::formulaToString($e->getTrigger())) =~ s/\s+//g;
ok($t, $trigger);
# reflexive case
$e->setTrigger($e->getTrigger());
ok($e->isSetTrigger(), 1);
($t = LibSBML::formulaToString($e->getTrigger())) =~ s/\s+//g;
ok($t, $trigger);
$e->setTrigger(undef);
ok($e->isSetTrigger(), 0);
ok($e->getTrigger(), undef);

# set/get delay
$e->setDelay(LibSBML::parseFormula($delay));
ok($e->isSetDelay(), 1);
($d = LibSBML::formulaToString($e->getDelay())) =~ s/\s+//g;
ok($d, $delay);
# reflexive case
$e->setDelay($e->getDelay());
ok($e->isSetDelay(), 1);
($d = LibSBML::formulaToString($e->getDelay())) =~ s/\s+//g;
ok($d, $delay);
$e->setDelay(undef);
ok($e->isSetDelay(), 0);
ok($e->getDelay(), undef);

# set/get timeunits
$e->setTimeUnits($units);
ok($e->isSetTimeUnits(), 1);
ok($e->getTimeUnits(), $units);
# reflexive case
$e->setTimeUnits($e->getTimeUnits());
ok($e->isSetTimeUnits(), 1);
ok($e->getTimeUnits(), $units);

# add/get EventAssignments
$e = new LibSBML::Event($id, LibSBML::parseFormula($trigger));
ok($e->getTypeCode(), $LibSBML::SBML_EVENT);
my $ea = new LibSBML::EventAssignment('k', LibSBML::parseFormula('0'));
ok($ea->getTypeCode(), $LibSBML::SBML_EVENT_ASSIGNMENT);

ok($e->isSetId(), 1);
ok($e->getId(), $id);
$e->setName($name);
ok($e->isSetName(), 1);
ok($e->getName(), $name);
$e->addEventAssignment($ea);
ok($e->getNumEventAssignments(), 1);
$ea = $e->getEventAssignment($e->getNumEventAssignments()-1);
ok($ea->getTypeCode(), $LibSBML::SBML_EVENT_ASSIGNMENT);
ok($ea->isSetVariable(), 1);
ok($ea->getVariable(), 'k');
ok($ea->isSetMath(), 1);
(my $f = LibSBML::formulaToString($ea->getMath())) =~ s/\s+//g;
ok($f, '0');
