## @configure_input@
##
## Filename    : makefile-common-actions.mk
## Description : Makefile include file containing common actions
## Author(s)   : SBML Team <sbml-team@caltech.edu>
## Organization: California Institute of Technology
## Created     : 2004-06-17
## Revision    : $Id$
## Source      : $Source$
##
## Copyright 2004 California Institute of Technology.
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
##     Michael Hucka
##     SBML Team
##     Control and Dynamical Systems, MC 107-81
##     California Institute of Technology
##     Pasadena, CA, 91125, USA
##
##     http://www.sbml.org
##     mailto:sbml-team@caltech.edu
##
## Contributor(s):
##

# General explanations: 
#
# The rules in this file are meant to be generic and included in other
# makefiles.  This file is meant to be included last by other makefiles.
#
# As a general rule, variables with names in all lower-case letters (such
# as `cxxcompile') are assumed to be defined by the enclosing makefile,
# while those in all upper-case letters (such as `TOP_SRCDIR') are assumed
# to be defined in `makefile-common-vars.mk' which in turn must be included
# by the enclosing makefile.


# -----------------------------------------------------------------------------
# Common compilation rules for object files and libraries.
# -----------------------------------------------------------------------------

.SUFFIXES:
.SUFFIXES: .a .so .dylib .jnilib .c .h .cpp .hpp .o .obj .Po .py .pyc .pyo .i

# The following define default values of variables like `cxxcompile'.  An
# enclosing makefile can define other values, in which case those
# definitions will override what's given here.

default_includes ?= -I. -I$(INCLUDEDIR)

compile ?= $(CC) $(extra_CPPFLAGS) $(extra_CFLAGS) $(default_includes) \
	$(CPPFLAGS) $(CFLAGS) $(DEFS) $(INCLUDES) -fPIC

cxxcompile ?= $(CXX) $(extra_CPPFLAGS) $(extra_CXXFLAGS) $(default_includes) \
	 $(CPPFLAGS) $(CXXFLAGS) $(DEFS) $(INCLUDES) -fPIC

# For linking libraries, we try to follow the result of the libtool
# numbering scheme, but at the final end, not in the input format.  (The
# libtool input format is peculiar to us.)  Curious, this makes the
# numbering very easy: it's a direct mapping of the libsbml version number.

library_version = $(PACKAGE_VERSION)

# `platform_link_flags' is used below in the definition of link_shared_lib.
# Generally, gcc and ld need -shared, but some systems think different.

ifeq "$(HOST_TYPE)" "darwin"
  # MacOS X's normal libraries have the extension .dylib, and "bundles"
  # have .so.  The default shared library definition here builds .dylib.
  platform_link_flags ?= -dynamiclib -flat_namespace \
	-current_version $(library_version)
else
  platform_link_flags ?= -shared
endif

# The following defines the default function for linking objects into a
# shared library. It gets used thus: $(call link_shared_lib,ARGS...).  The
# forms $(1), $(2) in the expressions below, etc. are the arguments passed
# to the call.  An enclosing makefile can provide another definition, in
# which case, the definition below will not be used.

ifndef link_shared_lib
  define link_shared_lib 
    $(CXX) $(LDFLAGS) $(extra_LDFLAGS) $(platform_link_flags) \
	-o $(1) $(objfiles) $(extra_LIBS) $(LIBS)
  endef
endif

# The following defines the default function for linking objects into a
# static library. It gets used thus: $(call link_static_lib,ARGS...).  The
# forms $(1), $(2) in the expressions below, etc. are the arguments passed
# to the call.  An enclosing makefile can provide another definition, in
# which case the definition below will not be used.

define link_static_lib
  -rm -f $(1)
  $(AR) -cru $(1) $(objfiles)
  $(RANLIB) $(1)
endef

# The following generate the list of object file names and dependency file
# names from the list of source files.  They're used for the generic
# compilation rules further below.

srclist  ?= $(sources:.cpp=.$(OBJEXT)) $(sources:.c=.$(OBJEXT))
objfiles ?= $(filter %.$(OBJEXT),$(srclist))
depfiles ?= $(addprefix $(DEPDIR)/,$(objfiles:.$(OBJEXT)=.$(DEPEXT)))

# This next line includes the dependency files.  This doesn't use
# $depfiles, but rather a wildcard on the actual files, so that if they
# don't exist yet, `make' won't generate errors about being unable to
# include such-and-such file.  If the files are missing, the wildcard will
# expand to nothing.  The /dev/null at the end makes sure that we don't
# have an empty `include' line.

include $(wildcard $(DEPDIR)/*.$(DEPEXT)) /dev/null

# The next set of rules are generic for creating .a, .so, and other styles
# of static and shared library files.

%.a: $(objfiles)
	$(call link_static_lib,$@)

%.so: $(objfiles)
	$(call link_shared_lib,$@)

%.$(JNIEXT): $(objfiles)
	$(call link_shared_lib,$@)

%.$(SHAREDLIBEXT): $(objfiles)
	$(call link_shared_lib,$@)

# The following define generic rules for creating object files.

.c.$(OBJEXT):
	$(compile) -MT $@ -MD -MP -MF "$(DEPDIR)/$*.$(DEPEXT)" -c -o $@ $<

.cpp.$(OBJEXT):
	$(cxxcompile) -MT $@ -MD -MP -MF "$(DEPDIR)/$*.$(DEPEXT)" -c -o $@ $<

.c.obj:
	if $(compile) -MT $@ -MD -MP -MF "$(DEPDIR)/$*.Tpo" \
	  -c -o $@ `if test -f '$<'; then $(CYGPATH_W) '$<'; else $(CYGPATH_W) '$(srcdir)/$<'; fi`; \
	then mv "$(DEPDIR)/$*.Tpo" "$(DEPDIR)/$*.$(DEPEXT)"; \
	else rm -f "$(DEPDIR)/$*.Tpo"; exit 1; \
	fi

.cpp.obj:
	if $(cxxcompile) -MT $@ -MD -MP -MF "$(DEPDIR)/$*.Tpo" \
	  -c -o $@ `if test -f '$<'; then $(CYGPATH_W) '$<'; else $(CYGPATH_W) '$(srcdir)/$<'; fi`; \
	then mv "$(DEPDIR)/$*.Tpo" "$(DEPDIR)/$*.$(DEPEXT)"; \
	else rm -f "$(DEPDIR)/$*.Tpo"; exit 1; \
	fi

# This next line ensures that the object output directory is created first.
# Got this from a posting by Paul D. Smith to gnu.utils.help, 2001-12-03.

__dummy := $(shell [ -d $(DEPDIR) ] || mkdir $(DEPDIR))


# -----------------------------------------------------------------------------
# Generic recursive targets.
# -----------------------------------------------------------------------------

recursive_targets = all-recursive install-data-recursive \
	install-exec-recursive installdirs-recursive install-recursive \
	uninstall-recursive uninstall-info-recursive install-info-recursive \
	ps-recursive info-recursive dvi-recursive pdf-recursive \
	check-recursive installcheck-recursive mostlyclean-recursive \
	clean-recursive distclean-recursive maintainer-clean-recursive \
	install-man-recursive tags-recursive ctags-recursive \
	install-docs-recursive dist-recursive distcheck-recursive

$(recursive_targets): subdirs

subdirs_recursive = $(addsuffix -recursive,$(subdirs))

subdirs: $(subdirs_recursive)

$(subdirs_recursive): 
	$(MAKE) -wC $(subst -recursive,,$@) $(MAKECMDGOALS)


# -----------------------------------------------------------------------------
# Installation
# -----------------------------------------------------------------------------

# The following defines a macro that is invoked like this:
# $(call install_library,$(libname),$(dest))

define install_library
  @if test "$(suffix $(1))" = ".so" -o "$(suffix $(1))" = ".dylib" -o "$(suffix $(1))" = ".jnilib"; then \
    echo $(INSTALL) $(1) $(2)/$(basename $(1)).$(library_version)$(suffix $(1)); \
    $(INSTALL) $(1) $(2)/$(basename $(1)).$(library_version)$(suffix $(1)); \
    echo ln -fs $(basename $(1)).$(library_version)$(suffix $(1)) $(2)/$(1); \
    ln -fs $(basename $(1)).$(library_version)$(suffix $(1)) $(2)/$(1); \
  else \
    echo $(INSTALL) $(1) $(2); \
    $(INSTALL) $(1) $(2); \
  fi
endef

to_install_libraries = $(addprefix install-,$(libraries))

$(to_install_libraries): $(libraries) installdirs
	$(call install_library,$(subst install-,,$@),$(DESTDIR)$(LIBDIR))

install-libraries: $(libraries) $(to_install_libraries)
	$(MKINSTALLDIRS) "$(DESTDIR)$(LIBDIR)"

install-docs: $(docs)
	$(shell [ -d $(DESTDIR)$(DOCDIR) ] || mkdir -p $(DESTDIR)$(DOCDIR))
	@if test -z '$(docs)'; then \
	  echo 'Nothing to be done for install-docs'; \
	else \
	  list='$(docs)'; for file in $$list; do \
	    if test -f $$file || test -d $$file; then d=.; else d=$(srcdir); fi; \
	    dir=`echo "$$file" | sed -e 's,/[^/]*$$,,'`; \
	    if test "$$dir" != "$$file" && test "$$dir" != "."; then \
	      dir="/$$dir"; \
	      $(MKINSTALLDIRS) "$(DOCDIR)/$$dir"; \
	    else \
	      dir=''; \
	    fi; \
	    if test -d $$d/$$file; then \
	      if test -d $(srcdir)/$$file && test $$d != $(srcdir); then \
	        echo Copying $(srcdir)/$$file; \
	        cp -pR $(srcdir)/$$file $(DOCDIR)$$dir || exit 1; \
	      fi; \
	      cp -pR $$d/$$file $(DOCDIR)$$dir || exit 1; \
	    else \
	      echo Copying $(DOCDIR)/$$file; \
	      test -f $(DOCDIR)/$$file \
	      || cp -p $$d/$$file $(DOCDIR)/$$file \
	      || exit 1; \
	    fi; \
	  done; \
	fi

installdirs: 
	$(MKINSTALLDIRS) $(DESTDIR)$(LIBDIR) \
			 $(DESTDIR)$(INCLUDEDIR) \
			 $(DESTDIR)$(DATADIR) \
			 $(DESTDIR)$(DOCDIR)


# -----------------------------------------------------------------------------
# Creating distribution (for libSBML maintainers only)
# -----------------------------------------------------------------------------

# The `dist-nor(mal' case uses the list of files and diretories in
# $(distfiles) and mirrors their structure in $(DISTDIR)/$(thisdir)/,
# except that files and directories that are also listed in
# $(distfile_exclude) are not copied.

dist-normal: DESTINATION := $(DISTDIR)/$(thisdir)
dist-normal: $(distfiles)
	$(shell [ -d $(DESTINATION) ] || mkdir -p $(DESTINATION))
	@list='$(distfiles)'; for file in $$list; do \
	  exlist='$(distfiles_exclude)'; for ex in $$exlist; do \
	    if test $$file = $$ex; then continue 2; fi; \
          done; \
	  if test -f $$file || test -d $$file; then d=.; else d=$(srcdir); fi; \
	  dir=`echo "$$file" | sed -e 's,/[^/]*$$,,'`; \
	  if test "$$dir" != "$$file" && test "$$dir" != "."; then \
	    dir="/$$dir"; \
	    $(MKINSTALLDIRS) "$(DESTINATION)/$$dir"; \
	  else \
	    dir=''; \
	  fi; \
	  if test -d $$d/$$file; then \
	    if test -d $(srcdir)/$$file && test $$d != $(srcdir); then \
	      echo Copying $(srcdir)/$$file; \
	      cp -pR $(srcdir)/$$file $(DESTINATION)/$$dir || exit 1; \
	    fi; \
	    cp -pR $$d/$$file $(DESTINATION)/$$dir || exit 1; \
	  else \
	    echo Copying $(DESTINATION)/$$file; \
	    test -f $(DESTINATION)/$$file \
	    || cp -p $$d/$$file $(DESTINATION)/$$file \
	    || exit 1; \
	  fi; \
	done


# -----------------------------------------------------------------------------
# Cleaning
# -----------------------------------------------------------------------------

# This tries to follow the recommended GNU make guidelines.

# Although we don't explicitly use libtool in the hand-written libSBML
# makefiles, the cleaning targets for libtool are included below in case
# someone ever decides to use libtool as part of some make actions.

mostlyclean: clean-generic mostlyclean-libtool

clean: mostlyclean clean-libraries clean-libtool clean-extras

clean-generic:
	-rm -f *.$(OBJEXT) core *.core

clean-libtool:
	-rm -rf .libs _libs

ifdef libraries
  clean-libraries:
	-test -z "$(libraries)" || rm -f $(libraries)
else
  clean-libraries:
endif

ifdef extra_clean
  clean-extras:
	-test -z "$(extra_clean)" || rm -rf $(extra_clean)
else
  clean-extras:
endif

mostlyclean-libtool:
	-rm -f *.lo

distclean: clean distclean-compile distclean-depend distclean-generic \
	distclean-tags distclean-libtool

distclean-compile:
	-rm -f *.tab.c

distclean-depend:
	-test -z "./$(DEPDIR)" || rm -rf ./$(DEPDIR)

distclean-generic:
	-rm -f Makefile
ifdef extra_distclean
	-rm -rf $(extra_distclean)
endif

distclean-tags:
	-rm -f TAGS ID GTAGS GRTAGS GSYMS GPATH tags

distclean-libtool:
	-rm -f libtool

maintainer-clean: distclean maintainer-clean-generic

maintainer-clean-generic:
	-rm -rf $(extra_maintainer_clean)


# -----------------------------------------------------------------------------
# Miscellaneous definitions.
# -----------------------------------------------------------------------------

# The following is for running shell commands with the right XX_LIBRARY_PATH
# variable set for different platforms.

ifeq "$(HOST_TYPE)" "darwin"

  define libsbmlrun
	DYLD_LIBRARY_PATH=$(TOP_BUILDDIR)/src; export DYLD_LIBRARY_PATH; \
	$(1)
  endef

endif
ifeq "$(HOST_TYPE)" "linux"

  define libsbmlrun
	LD_LIBRARY_PATH=$(TOP_BUILDDIR)/src; export LD_LIBRARY_PATH; \
	$(1)
  endef

endif


# -----------------------------------------------------------------------------
# Common dependency rules.
# -----------------------------------------------------------------------------
# These rules ensure that files like makefiles are recreated when
# dependencies change.  The rules are generic enough that they apply to the
# top-level source directory as well as the source subdirectories.

Makefile: Makefile.in $(TOP_SRCDIR)/configure \
	     $(TOP_SRCDIR)/config/makefile-common-vars.mk
	cd $(TOP_BUILDDIR) && $(SHELL) ./config.status $(thisdir)/$@

$(TOP_SRCDIR)/config/makefile-common-vars.mk: \
	     $(TOP_SRCDIR)/config/makefile-common-vars.mk.in \
	     $(TOP_SRCDIR)/configure
	cd $(TOP_BUILDDIR) && $(SHELL) ./config.status config/makefile-common-vars.mk

$(TOP_SRCDIR)/config.status: $(TOP_SRCDIR)/configure
	$(SHELL) ./config.status --recheck

$(TOP_SRCDIR)/configure: $(TOP_SRCDIR)/configure.ac $(ACLOCAL_M4)
	cd $(TOP_SRCDIR) && $(AUTOCONF)
	cd $(TOP_SRCDIR) && $(SHELL) ./config.status --recheck

$(ACLOCAL_M4): $(TOP_SRCDIR)/configure.ac $(wildcard $(TOP_SRCDIR)/config/*.m4)
	cd $(TOP_SRCDIR) && $(ACLOCAL) $(ACLOCAL_FLAGS)

$(TOP_SRCDIR)/src/sbml/config.h: src/sbml/stamp-h1
	@if test ! -f $@; then \
	  rm -f src/sbml/stamp-h1; \
	  $(MAKE) src/sbml/stamp-h1; \
	else :; fi

$(TOP_SRCDIR)/src/sbml/stamp-h1: $(TOP_SRCDIR)/./src/sbml/config.h.in \
	     $(TOP_BUILDDIR)/config.status
	@rm -f src/sbml/stamp-h1
	cd $(TOP_BUILDDIR) && $(SHELL) ./config.status src/sbml/config.h


# -----------------------------------------------------------------------------
# Tags files.
# -----------------------------------------------------------------------------

tags: TAGS
ctags: CTAGS

ID: $(headers) $(sources) $(LISP) $(TAGS_FILES)
	list='$(sources) $(headers) $(LISP) $(TAGS_FILES)'; \
	unique=`for i in $$list; do \
	    if test -f "$$i"; then echo $$i; else echo $(srcdir)/$$i; fi; \
	  done | \
	  $(AWK) '    { files[$$0] = 1; } \
	       END { for (i in files) print i; }'`; \
	mkid -fID $$unique

TAGS: $(headers) $(sources)  $(TAGS_DEPENDENCIES) \
		$(TAGS_FILES) $(LISP)
	tags=; \
	here=`pwd`; \
	list='$(sources) $(headers)  $(LISP) $(TAGS_FILES)'; \
	unique=`for i in $$list; do \
	    if test -f "$$i"; then echo $$i; else echo $(srcdir)/$$i; fi; \
	  done | \
	  $(AWK) '    { files[$$0] = 1; } \
	       END { for (i in files) print i; }'`; \
	test -z "$(ETAGS_ARGS)$$tags$$unique" \
	  || $(ETAGS) $(ETAGSFLAGS) $(AM_ETAGSFLAGS) $(ETAGS_ARGS) \
	     $$tags $$unique

CTAGS: $(headers) $(sources)  $(TAGS_DEPENDENCIES) \
		$(TAGS_FILES) $(LISP)
	tags=; \
	here=`pwd`; \
	list='$(sources) $(headers)  $(LISP) $(TAGS_FILES)'; \
	unique=`for i in $$list; do \
	    if test -f "$$i"; then echo $$i; else echo $(srcdir)/$$i; fi; \
	  done | \
	  $(AWK) '    { files[$$0] = 1; } \
	       END { for (i in files) print i; }'`; \
	test -z "$(CTAGS_ARGS)$$tags$$unique" \
	  || $(CTAGS) $(CTAGSFLAGS) $(AM_CTAGSFLAGS) $(CTAGS_ARGS) \
	     $$tags $$unique

GTAGS:
	here=`$(CD) $(TOP_BUILDDIR) && pwd` \
	  && cd $(TOP_SRCDIR) \
	  && gtags -i $(GTAGS_ARGS) $$here


# -----------------------------------------------------------------------------
# Common special targets.
# -----------------------------------------------------------------------------

.PHONY: $(recursive_targets) CTAGS GTAGS all all-am check check-am clean \
	clean-generic clean-libtool ctags \
	dist dist-all dist-gzip distcheck distclean \
	distclean-generic distclean-libtool distclean-tags distcleancheck \
	distdir distuninstallcheck dvi dvi-am info info-am \
	install install-am install-data install-data-am \
	install-exec install-exec-am install-info install-info-am \
	install-man install-strip installcheck installcheck-am installdirs \
	installdirs-am maintainer-clean maintainer-clean-generic mostlyclean \
	mostlyclean-generic mostlyclean-libtool \
	pdf pdf-am ps ps-am tags uninstall uninstall-am uninstall-info-am \
	clean-extras subdirs $(subdirs_recursive) subdist subdistcheck

# Tell versions [3.59,3.63) of GNU make to not export all variables.
# Otherwise a system limit (for SysV at least) may be exceeded.
.NOEXPORT:


# -----------------------------------------------------------------------------
# End.
# -----------------------------------------------------------------------------

## The following is for [X]Emacs users.  Please leave in place.
## Local Variables:
## mode: Makefile
## End:
