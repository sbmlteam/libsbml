# File name   : Makefile
# Description : Makefile for sbml document
# Author(s)   : Michael Hucka <mhucka@cds.caltech.edu>
# Organization: CDS, Caltech
# Language    : Make
# Created     : 2000-10-17
# Revision    : Makefile,v 1.1.1.1 2002/12/05 23:59:38 mhucka Exp


BASENAME = libsbml-manual

default: dvi

dvi:;
	-@/bin/rm -f $(BASENAME).toc $(BASENAME).aux
	latex $(BASENAME).tex
	bibtex $(BASENAME)
	latex $(BASENAME).tex
	latex $(BASENAME).tex
	latex $(BASENAME).tex

pdf:;
	make eps
	-@/bin/rm -f $(BASENAME).toc $(BASENAME).aux
	pdflatex $(BASENAME).tex
	bibtex $(BASENAME)
	pdflatex $(BASENAME).tex
	pdflatex $(BASENAME).tex
	pdflatex $(BASENAME).tex
	pdflatex $(BASENAME).tex

eps:;
	@(for i in *.eps; do echo "epstopdf $$i"; epstopdf $$i; done)

html:;
	latex2html -white -split +0 -show_section_numbers -image_type gif \
		   -no_navigation -local_icons -discard \
		   -mkdir -dir html $(BASENAME)
