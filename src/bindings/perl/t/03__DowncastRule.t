# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl GRN-Models.t'

#########################

# change 'tests => 1' to 'tests => last_test_to_print';

use Test;

BEGIN { plan tests => 5 };

use LibSBML;
use strict;

#########################

# Insert your test code below, the Test::More module is use()ed here so read
# its man page ( perldoc Test::More ) for help writing this test script.

my $m = new LibSBML::Model;
ok(ref $m, 'LibSBML::Model');
ok($m->getNumRules(), 0);

my $ar = new LibSBML::AssignmentRule('x', '1 + 1', $LibSBML::RULE_TYPE_SCALAR);
ok(ref $ar, 'LibSBML::AssignmentRule'); 

$m->addRule($ar);
ok($m->getNumRules(), 1);

my $x = $m->getRule(0);
ok(ref $x, 'LibSBML::AssignmentRule'); 
