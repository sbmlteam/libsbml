## src/bindings/perl/Makefile.PL.  Generated from Makefile.PL.in by configure.
##
## \file    Makefile.PL
## \brief   Perl-specific SWIG directives for wrapping libSBML API
## \author  TBI {xtof,raim}@tbi.univie.ac.at
##
## $Id$
## $Source$

##
## Copyright 2004 TBI
##
## This library is free software; you can redistribute it and/or modify it
## under the terms of the GNU Lesser General Public License as published
## by the Free Software Foundation; either version 2.1 of the License, or
## any later version.
##
## This library is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY, WITHOUT EVEN THE IMPLIED WARRANTY OF
## MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  The software and
## documentation provided hereunder is on an "as is" basis, and the
## California Institute of Technology and Japan Science and Technology
## Corporation have no obligations to provide maintenance, support,
## updates, enhancements or modifications.  In no event shall the
## California Institute of Technology or the Japan Science and Technology
## Corporation be liable to any party for direct, indirect, special,
## incidental or consequential damages, including lost profits, arising
## out of the use of this software and its documentation, even if the
## California Institute of Technology and/or Japan Science and Technology
## Corporation have been advised of the possibility of such damage.  See
## the GNU Lesser General Public License for more details.
##
## You should have received a copy of the GNU Lesser General Public License
## along with this library; if not, write to the Free Software Foundation,
## Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
##
## The original code contained here was initially developed by:
##
##     Christoph Flamm and Rainer Machne
##     Institut fuer Theoretische Chemie
##     Universitaet Wien
##     Waehringerstrasse 17/3/308
##     A-1090 Wien, Austria


use ExtUtils::MakeMaker;
WriteMakefile(
              NAME        => "LibSBML",
              MAKEFILE    => 'Makefile-perl',
              INSTALLDIRS => "site",
              LIBS        => ["-lxerces-c", "-lm"],
              INC         => "-I../swig -I../..",
              MYEXTLIB    => "../../libsbml.a",
              OBJECT      => "LibSBML_wrap.o",
              AUTHOR      => "Christoph Flamm <xtof@tbi.univie.ac.at>",
              VERSION     => "2.3.0",
);

package MY;
sub postamble {
	return <<'MAKE_FRAG';
LibSBML_wrap.cxx:
	/usr/local/bin/swig  -Wall -c++ -perl5 -proxy -I../.. LibSBML.i
MAKE_FRAG
}
# End of file
