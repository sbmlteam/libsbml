# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl GRN-Models.t'

#########################

# change 'tests => 1' to 'tests => last_test_to_print';

use Test;

BEGIN { plan tests => 10 };

use LibSBML;
use strict;
use Data::Dumper;

#########################

# Insert your test code below, the Test::More module is use()ed here so read
# its man page ( perldoc Test::More ) for help writing this test script.

my $lo = new LibSBML::ListOf;
ok(ref $lo, 'LibSBML::ListOf');
ok($lo->getNumItems, 0);

my $cm = new LibSBML::Compartment;
ok(ref $cm, 'LibSBML::Compartment');

my $sp = new LibSBML::Species;
ok(ref $sp, 'LibSBML::Species');
$sp->setId('xtof');
ok($sp->getId, 'xtof');

my $pa = new LibSBML::Parameter;
ok(ref $pa, 'LibSBML::Parameter');

# testing appanding to list
$lo->append($sp);
ok($lo->getNumItems, 1);
$lo->append($cm);
ok($lo->getNumItems, 2);
$lo->append($pa);
ok($lo->getNumItems, 3);

my $x = $lo->get(0);
ok($x->getId, 'xtof');
print Dumper($lo);
print Dumper($x);
