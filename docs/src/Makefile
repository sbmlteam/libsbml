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
#     Michael Hucka and Ben Bornstein
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

MANUALS := libsbml-manual libsbml-api-ref
MANUAL_HTML_DIR := libsbml-manual-html

# The default action is to make everything, completely.
# If you would like to generate only a specific document, use the command
# "make CHOICE" where "CHOICE" is manual-pdf, api-pdf, api-html, etc.

.PHONY: all

all: default

default: $(foreach manual,$(MANUALS),$(manual).dvi) \
	 $(foreach manual,$(MANUALS),$(manual).pdf) \
	 $(foreach manual,$(MANUALS),$(manual).ps)

#
# libsbml-manual
#

libsbml-manual.pdf: libsbml-manual.tex pdffigs
	-@/bin/rm -f libsbml-manual.toc libsbml-manual.aux
	pdflatex libsbml-manual
	-bibtex libsbml-manual
	pdflatex libsbml-manual
	pdflatex libsbml-manual
	pdflatex libsbml-manual
	pdflatex libsbml-manual

libsbml-manual.dvi: libsbml-manual.tex
	-@/bin/rm -f libsbml-manual.toc libsbml-manual.aux
	latex libsbml-manual
	-bibtex libsbml-manual
	latex libsbml-manual
	latex libsbml-manual
	latex libsbml-manual

libsbml-manual.ps: libsbml-manual.tex libsbml-manual.dvi
	dvips -o libsbml-manual.ps libsbml-manual

libsbml-manual.html: sbmlmanual.perl
	-@/bin/rm libsbml-manual.html
	latex2html -white -split +0 -show_section_numbers -image_type gif \
		   -no_navigation -local_icons -discard \
		   -mkdir -dir $(MANUAL_HTML_DIR) libsbml-manual
	ln -s $(MANUAL_HTML_DIR)/libsbml-manual.html

sbmlmanual.perl:
	ln -s ../../../project/tex/sbmlmanual.perl


#
# libsbml-api-ref
#

libsbml-api-ref.pdf: libsbml-api-ref.tex api_sections_files
	-@/bin/rm -f libsbml-api-ref.toc libsbml-api-ref.aux
	pdflatex libsbml-api-ref
	-bibtex libsbml-api-ref
	pdflatex libsbml-api-ref
	pdflatex libsbml-api-ref
	pdflatex libsbml-api-ref
	pdflatex libsbml-api-ref

libsbml-api-ref.dvi: libsbml-api-ref.tex
	-@/bin/rm -f libsbml-api-ref.toc libsbml-api-ref.aux
	latex libsbml-api-ref
	-bibtex libsbml-api-ref
	latex libsbml-api-ref
	latex libsbml-api-ref
	latex libsbml-api-ref

libsbml-api-ref.ps: libsbml-api-ref.tex libsbml-api-ref.dvi
	dvips -o libsbml-api-ref.ps libsbml-api-ref

libsbml-api-ref.html: api_sections_files doxygen-api-manual

libsbml_version = $(shell cat ../../VERSION.txt)

doxygen-api-manual: Doxyfile
	doxygen Doxyfile

Doxyfile: Doxyfile.input
	rm -f $@ $@.tmp
	sed 's,@LIBSBML_VERSION\@,$(libsbml_version),g' $< >$@.tmp
	mv $@.tmp $@

#
# Miscellaneous helper directives.
#

pdffigs:
	@(for i in *.eps; do echo "epstopdf $$i"; epstopdf $$i; done)

api_sections_files:
	$(SHELL) createAPISections.sh ../../src/sbml
