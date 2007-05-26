# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl GRN-Models.t'

#########################

# change 'tests => 1' to 'tests => last_test_to_print';

use Test;

BEGIN { plan tests => 11 };

use LibSBML;
use strict;

#########################

# Insert your test code below, the Test::More module is use()ed here so read
# its man page ( perldoc Test::More ) for help writing this test script.

my $m = new LibSBML::Model;
ok(ref $m, 'LibSBML::Model');
ok($m->getNumRules(), 0);

my $r1 = new LibSBML::AssignmentRule('x', LibSBML::parseFormula('1 + 1'));
ok(ref $r1, 'LibSBML::AssignmentRule'); 
$m->addRule($r1);
ok($m->getNumRules(), 1);

my $r2 = new LibSBML::RateRule('x', '1 + 1');
ok(ref $r2, 'LibSBML::RateRule'); 
$m->addRule($r2);
ok($m->getNumRules(), 2);

my $r3 = new LibSBML::AlgebraicRule('1 + 1');
ok(ref $r3, 'LibSBML::AlgebraicRule'); 
$m->addRule($r3);
ok($m->getNumRules(), 3);

ok(ref $m->getRule(0), 'LibSBML::AssignmentRule'); 
ok(ref $m->getRule(1), 'LibSBML::RateRule'); 
ok(ref $m->getRule(2), 'LibSBML::AlgebraicRule'); 
