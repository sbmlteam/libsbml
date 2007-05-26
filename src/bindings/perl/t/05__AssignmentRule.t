use Test;
BEGIN { plan tests => 18 };

use LibSBML;
use strict;
use vars qw/$formula $f/;

#########################

$formula = 'X^n/(1+X^n)';
$f = '';

# creation with formula
my $r = new LibSBML::AssignmentRule('Y', $formula);
ok($r->getTypeCode() == $LibSBML::SBML_ASSIGNMENT_RULE);
ok($r->isSetVariable(), 1);
ok($r->isSetMath(), 1);
ok($r->getType(), $LibSBML::RULE_TYPE_SCALAR);
($f = LibSBML::formulaToString($r->getMath())) =~ s/\s+//g;
ok($f, $formula);

# creation with AST
$r = new LibSBML::AssignmentRule('Y', LibSBML::parseFormula($formula));
ok($r->getTypeCode() == $LibSBML::SBML_ASSIGNMENT_RULE);
ok($r->isSetVariable(), 1);
ok($r->isSetMath(), 1);
ok($r->getType(), $LibSBML::RULE_TYPE_SCALAR);
($f = LibSBML::formulaToString($r->getMath())) =~ s/\s+//g;
ok($f, $formula);

# creation w/o arguments
$r = new LibSBML::AssignmentRule();
ok($r->getTypeCode() == $LibSBML::SBML_ASSIGNMENT_RULE);
ok($r->isSetVariable(), 0);
ok($r->isSetMath(), 0);
ok($r->getType(), $LibSBML::RULE_TYPE_SCALAR);

# set/get variable
$r->setVariable('Y');
ok($r->isSetVariable(), 1);
ok($r->getVariable(), 'Y');

# set/get math
$r->setMath(LibSBML::parseFormula($formula));
($f = LibSBML::formulaToString($r->getMath())) =~ s/\s+//g;
ok($f, $formula);

# creat a document and a model
my $d = new LibSBML::SBMLDocument(2,3);
my $m = $d->createModel();
$m->setId('assignment_rule');

# create and add two species and a parameter and the rule to the model
my $s1 = $m->createSpecies();
$s1->setId('X');
my $s2 = $m->createSpecies();
$s2->setId('Y');
my $p = $m->createParameter();
$p->setId('n');
$m->addRule($r);

# check the model
my $ref = join '', <DATA>;
my $doc = $d->writeSBMLToString();
ok($doc, $ref);

# functions not wrapped

__DATA__
<?xml version="1.0" encoding="UTF-8"?>
<sbml xmlns="http://www.sbml.org/sbml/level2/version3" level="2" version="3">
  <model id="assignment_rule">
    <listOfSpecies>
      <species id="X"/>
      <species id="Y"/>
    </listOfSpecies>
    <listOfParameters>
      <parameter id="n"/>
    </listOfParameters>
    <listOfRules>
      <assignmentRule variable="Y">
        <math xmlns="http://www.w3.org/1998/Math/MathML">
          <apply>
            <divide/>
            <apply>
              <power/>
              <ci> X </ci>
              <ci> n </ci>
            </apply>
            <apply>
              <plus/>
              <cn type="integer"> 1 </cn>
              <apply>
                <power/>
                <ci> X </ci>
                <ci> n </ci>
              </apply>
            </apply>
          </apply>
        </math>
      </assignmentRule>
    </listOfRules>
  </model>
</sbml>
