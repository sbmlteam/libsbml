#
# File name   : Makefile
# Description : Makefile for libSBML document
# Author(s)   : Michael Hucka <mhucka@caltech.edu>
# Organization: CDS, Caltech
# Language    : Make
# Created     : 2000-10-17
# Revision    : Makefile,v 1.1.1.1 2002/12/05 23:59:38 mhucka Exp
#
# Copyright 2003-2004 California Institute of Technology and
# Japan Science and Technology Corporation.
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
#     Ben Bornstein and Michael Hucka
#     SBML Team
#     Control and Dynamical Systems, MC 107-81
#     California Institute of Technology
#     Pasadena, CA, 91125, USA
#
#     http://www.sbml.org
#     mailto:sbml-team@caltech.edu
#
# Contributor(s):
#

BASENAME = libsbml-manual
MANUAL_HTML_DIR = libsbml-manual-html

# The default action is to make everything, completely.
# If you would like to generate only a specific document, use the command
# "make CHOICE" where "CHOICE" is manual-pdf, api-pdf, api-html, etc.

.PHONY: manual api manual-dvi manual-pdf manual-html api-dvi api-pdf api-html

default: manual api
all: default

#
# Code for generating the libSBML manual.
#

manual: manual-dvi manual-pdf manual-html
api:    api-dvi api-pdf api-html

manual-dvi:
	-@/bin/rm -f $(BASENAME).toc $(BASENAME).aux
	latex $(BASENAME).tex
	-bibtex $(BASENAME)
	latex $(BASENAME).tex
	latex $(BASENAME).tex
	latex $(BASENAME).tex

manual-ps: manual-dvi
	dvips -o $(BASENAME).ps $(BASENAME)

manual-pdf: manual-pdffigs
	-@/bin/rm -f $(BASENAME).toc $(BASENAME).aux
	pdflatex $(BASENAME).tex
	-bibtex $(BASENAME)
	pdflatex $(BASENAME).tex
	pdflatex $(BASENAME).tex
	pdflatex $(BASENAME).tex
	pdflatex $(BASENAME).tex

manual-pdffigs:
	@(for i in *.eps; do echo "epstopdf $$i"; epstopdf $$i; done)

manual-html: sbmlmanual.perl
	latex2html -white -split +0 -show_section_numbers -image_type gif \
		   -no_navigation -local_icons -discard \
		   -mkdir -dir $(MANUAL_HTML_DIR) $(BASENAME)

sbmlmanual.perl:
	ln -s ../../../project/tex/sbmlmanual.perl


#
# Code for generating the libSBML API manual.
#

api-html: api_sections_files doxygen-api-manual

libsbml_version = $(shell cat ../../VERSION.txt)

api_sections_files:
	$(SHELL) createAPISections.sh ../../src/sbml

doxygen-api-manual: Doxyfile
	doxygen Doxyfile

Doxyfile: Doxyfile.input
	rm -f $@ $@.tmp
	sed 's,@LIBSBML_VERSION\@,$(libsbml_version),g' $< >$@.tmp
	mv $@.tmp $@
