use Test;
BEGIN { plan tests => 22 };

use LibSBML;
use strict;
use vars qw/$formula $f/;

#########################

$formula = 'X^n/(1+X^n)';
$f = '';

# creation w/o arguments
my $ar = new LibSBML::AlgebraicRule();
ok($ar->getTypeCode() == $LibSBML::SBML_ALGEBRAIC_RULE);
ok($ar->getMetaId(), '');
ok($ar->getNotes(), '');
ok($ar->getAnnotation(), '');
ok($ar->isSetFormula(), 0);
ok($ar->getFormula(), '');
ok($ar->isSetMath(), 0);
ok($ar->getMath(), undef);

# creation w/ formula
$ar = new LibSBML::AlgebraicRule($formula);
ok($ar->getTypeCode() == $LibSBML::SBML_ALGEBRAIC_RULE);
ok($ar->getMetaId(), '');
ok($ar->getNotes(), '');
ok($ar->getAnnotation(), '');
ok($ar->isSetFormula(), 1);
($f = LibSBML::formulaToString($ar->getMath())) =~ s/\s+//g;
ok($f, $formula);
ok($ar->isSetMath(), 1);

# creation w/ AST
$ar = new LibSBML::AlgebraicRule(LibSBML::parseFormula($formula));
ok($ar->getTypeCode() == $LibSBML::SBML_ALGEBRAIC_RULE);
ok($ar->getMetaId(), '');
ok($ar->getNotes(), '');
ok($ar->getAnnotation(), '');
ok($ar->isSetFormula(), 1);
ok($ar->isSetMath(), 1);
($f = LibSBML::formulaToString($ar->getMath())) =~ s/\s+//g;
ok($f, $formula);
