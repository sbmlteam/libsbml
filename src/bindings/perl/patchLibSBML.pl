#!/usr/bin/perl
# -*-Perl-*-

#
# \file    patchLibSBML.pl
# \brief   Patches SWIG generated LibSBML.pm
# \author  TBI {xtof,raim}@tbi.univie.ac.at
#
# $Id$
# $Source$

#
#
# Copyright 2004 TBI
#
# This library is free software; you can redistribute it and/or modify it
# under the terms of the GNU Lesser General Public License as published
# by the Free Software Foundation; either version 2.1 of the License, or
# any later version.
#
# This library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY, WITHOUT EVEN THE IMPLIED WARRANTY OF
# MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  The software and
# documentation provided hereunder is on an "as is" basis, and the
# California Institute of Technology and Japan Science and Technology
# Corporation have no obligations to provide maintenance, support,
# updates, enhancements or modifications.  In no event shall the
# California Institute of Technology or the Japan Science and Technology
# Corporation be liable to any party for direct, indirect, special,
# incidental or consequential damages, including lost profits, arising
# out of the use of this software and its documentation, even if the
# California Institute of Technology and/or Japan Science and Technology
# Corporation have been advised of the possibility of such damage.  See
# the GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with this library; if not, write to the Free Software Foundation,
# Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
#
# The original code contained here was initially developed by:
#
#     Christoph Flamm and Rainer Machne
#
# Contributor(s):
#


use Data::Dumper;
use strict;

my $hash = parse_local_i();
patch_libSBML_pm($hash);

#---
sub parse_local_i {
  local ($/, $_);
  my $hash = {};
  $/=""; # read in paragraph mode
  open(LFH, "< local.i") || die "Couldn't open file local.i: $!";
  while (<LFH>) {
    # extract the %feature directives from file local.i
    if (m/^%feature\(\"shadow\"\)\s+([\w:]+)/) { #.+%\{(.+)%\}/m) {
      my $func = $1; s/\n/\\n/g;
      (my $pkg = $func) =~ s/\A(\w+):.*/$1/;
       (my $code = $_) =~ s/.*%{\\n(.*)%}.*/$1/;
      $hash->{$func} = $code;
      $hash->{$pkg} = 1;
    }
  }
  close(LFH) || die "Couldn't close file local.i: $!";
  return $hash;
}

#---
sub patch_libSBML_pm {
  local($_);
  my ($hash, $pkg, @funcs) = (shift(), '', ());
  open(PFH, "< LibSBML.pm") || die "Couldn't open file libSBML.pm: $!";
  while (<PFH>) {
    # recognize packages
    if (m/^package ([\w:]+);/) {
      ($pkg = $1) =~ s/\ALibSBML:://;
      @funcs = ();
      if (exists $hash->{$pkg}) {
	@funcs = grep {m/\A$pkg\:\:/} keys %$hash ;
	@funcs = map { s/$pkg\:\://; $_ } @funcs;
      }
      print;
      next;
    }

    # replace package functions
    if (m/^\*(\w+) =/ && @funcs && grep(m/$1/, @funcs)) {
      @funcs = grep(!m/$1/, @funcs);
      (my $code = $hash->{"$pkg\:\:$1"}) =~ s/\\n/\n/g;
      print "#$_$code";
      next;
    }
    else { print }
  }
}

#---
sub dumpHash {
  my $href = shift;
  for my $key (sort keys %$href) {
    (my $code = $href->{$key}) =~ s/\\n/\n/g;
    print STDERR "$key\n$code\n";
  }
}

__END__
