# Description : libsbmlextras.perl
# Author(s)   : Michael Hucka <mhucka@@cds.caltech.edu>
# Organization: CDS, California Institute of Technology
# Created     : 2004-09-20
# Revision    : $Id$
# $Source$
#

package main;

sub do_cmd_libsbml { join('', "L<SMALL>IB</SMALL>SBML", @_[0]) };

# This reads the version number assigned to PACKAGE_VERSION
# in ../makefile-common-vars.mk.

sub do_cmd_libsbmlversion { 
  open(VARFILE, "<../../../config/makefile-common-vars.mk") 
    || die "Couldn't open makefile-common-vars.mk";

  while (<VARFILE>) {
    chomp;			# Remove newlines.
    s/#.*//;			# Remove comments.
    s/^\s+//;			# Remove leading whitespace.
    s/\s+$//;			# Remove trailing whitespace.
    next unless length;		# Skip empty lines.
    my ( $var, $value ) = split(/\s*=\s*/, $_, 2);
    if ($var =~ /PACKAGE_VERSION/) {
      $version = $value;
      last;
    }
  }
  close(VARFILE);
  $version;
};

#-----------------------------------------------------------------------------
# End of file.
#-----------------------------------------------------------------------------

1;	# Must be last line
