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

default_includes ?= -I.

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

# Most of the sources are a mix of C and C++ files.  They have separate
# extensions, and simply using something like $(sources:.cpp=.o) doesn't
# work for converting the source file names into object file names because
# you have to do it twice (once each for .cpp and .c) and then you have to
# filter the results.  This abstracts out this common operation.

make_objects_list = \
  $(filter %.$(OBJEXT),\
    $(patsubst %.cpp,%.$(OBJEXT),$(1)) $(patsubst %.c,%.$(OBJEXT),$(1)))

# The following generate the list of object file names and dependency file
# names from the list of source files.  They're used for the generic
# compilation rules further below.

tmplist  ?= $(sources:.cpp=.$(OBJEXT)) $(sources:.c=.$(OBJEXT))
objfiles ?= $(filter %.$(OBJEXT),$(tmplist))
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

%.a ../%.a: $(objfiles)
	$(call link_static_lib,$@)

%.so ../%.so: $(objfiles)
	$(call link_shared_lib,$@)

%.$(JNIEXT) ../%.$(JNIEXT): $(objfiles)
	$(call link_shared_lib,$@)

%.$(SHAREDLIBEXT) ../%.$(SHAREDLIBEXT): $(objfiles)
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
# Generic default.
# -----------------------------------------------------------------------------

default: $(objfiles) $(test_objfiles) $(libraries)


# -----------------------------------------------------------------------------
# Generic recursive targets.
# -----------------------------------------------------------------------------

recursive_targets = all-recursive install-data-recursive \
	install-exec-recursive installdirs-recursive install-recursive \
	uninstall-recursive uninstall-info-recursive install-info-recursive \
	ps-recursive info-recursive dvi-recursive pdf-recursive \
	check-recursive installcheck-recursive mostlyclean-recursive \
	clean-recursive distclean-recursive maintainer-clean-recursive \
	install-man-recursive tags-recursive ctags-recursive doc-recursive \
	install-docs-recursive dist-recursive distcheck-recursive

$(recursive_targets): subdirs

subdirs_recursive = $(addsuffix -recursive,$(subdirs))

subdirs: $(subdirs_recursive)

$(subdirs_recursive): 
ifneq "$(MAKEFLAGS)" ""
	$(MAKE) -w -C $(subst -recursive,,$@) -$(MAKEFLAGS) $(MAKECMDGOALS)
else
	$(MAKE) -w -C $(subst -recursive,,$@) $(MAKECMDGOALS)
endif


# -----------------------------------------------------------------------------
# Running checks.
# -----------------------------------------------------------------------------

# This depends on $(check_driver) and $(test_objfiles) to have been defined
# in the including Makefile.

$(check_driver): $(test_objfiles)
	$(CXX) $(extra_CPPFLAGS) $(extra_CXXFLAGS) $(default_includes) \
	  $(CPPFLAGS) $(CFLAGS) $(DEFS) $(INCLUDES) \
	  $(test_objfiles) $(objfiles) $(extra_LDFLAGS) $(LDFLAGS) \
	  $(LIBS) $(extra_LIBS) -o $@

run-checks: $(check_driver) $(libraries)
	@echo
	@echo
	@echo Running Tests in $(thisdir)
	@echo -----------------------------------------------------------------
	@$(call libsbmlrun,./$(check_driver))
	@echo -----------------------------------------------------------------
	@echo


# -----------------------------------------------------------------------------
# Installation
# -----------------------------------------------------------------------------

# It doesn't look like you can usefully strip binary libraries at installation
# time on Darwin.  The man page for 'strip' says you'd have to do it as 'ld -s'
# to really make it work, but you don't want to strip the library that's built
# in the source tree -- you only want to strip the copy that gets installed
# in the destination directory.  So, we do the following conditional, until
# we can figure out something better.

ifneq "$(HOST_TYPE)" "darwin"
  install_strip = -s
endif

# The following defines a macro that is invoked like this:
# $(call install_library,$(libname),$(dest))

define install_library
  $(MKINSTALLDIRS) $(DESTDIR)$(LIBDIR)
  @if test "$(suffix $(1))" = ".so" -o "$(suffix $(1))" = ".dylib" -o "$(suffix $(1))" = ".jnilib"; then \
    finalname="$(notdir $(basename $(1))).$(library_version)$(suffix $(1))"; \
    echo $(INSTALL) $(install_strip) $(1) $(2)/$$finalname; \
    $(INSTALL) $(install_strip) $(1) $(2)/$$finalname; \
    echo ln -fs $$finalname $(2)/$(notdir $(1)); \
    ln -fs $$finalname $(2)/$(notdir $(1)); \
  else \
    echo $(INSTALL) $(install_strip) $(1) $(2); \
    $(INSTALL) $(install_strip) $(1) $(2); \
  fi
endef

to_install_libraries = $(addprefix install-,$(libraries))

$(to_install_libraries): $(libraries) installdirs
	$(call install_library,$(subst install-,,$@),$(DESTDIR)$(LIBDIR))

install-libraries: $(libraries) $(to_install_libraries)

install-headers: $(headers) installdirs
	if test -z '$(headers)'; then \
	  echo 'Nothing to be done for install-headers'; \
	else \
	  list='$(headers)'; for file in $$list; do \
	    targetdir="$(DESTDIR)$(INCLUDEDIR)"; \
	    if test -n '$(INCLUDEPREFIX)'; then \
	      targetdir="$$targetdir/$(INCLUDEPREFIX)"; \
	    fi; \
	    if test -n '$(header_inst_prefix)'; then \
	      targetdir="$$targetdir/$(header_inst_prefix)"; \
	    fi; \
	    $(MKINSTALLDIRS) $$targetdir; \
	    if test -f $$file || test -d $$file; then d=.; else d=$(srcdir); fi; \
	    dir=`echo "$$file" | sed -e 's,/[^/]*$$,,'`; \
	    if test "$$dir" != "$$file" && test "$$dir" != "."; then \
	      dir="/$$dir"; \
	      $(MKINSTALLDIRS) $$targetdir/$$dir; \
	    else \
	      dir=''; \
	    fi; \
	    if test -d $$d/$$file; then \
	      if test -d $(srcdir)/$$file && test $$d != $(srcdir); then \
	        echo Copying $(srcdir)/$$file; \
	        $(INSTALL) -m 644 $(srcdir)/$$file $$targetdir$$dir || exit 1; \
	      fi; \
	      $(INSTALL) -m 644 $$d/$$file $$targetdir$$dir || exit 1; \
	    else \
	      echo Copying $$targetdir/$$file; \
	      test -f $$targetdir/$$file \
	      || $(INSTALL) -m 644 $$d/$$file $$targetdir/$$file \
	      || exit 1; \
	    fi; \
	  done; \
	fi


# -----------------------------------------------------------------------------
# Creating distribution (for libSBML maintainers only)
# -----------------------------------------------------------------------------

# The `dist-nor(mal' case uses the list of files and diretories in
# $(distfiles) and mirrors their structure in $(DISTDIR)/$(thisdir)/,
# except that files and directories that are also listed in
# $(distfile_exclude) are not copied.

# The $(sort ...) removes duplicates.

sortedfiles = $(sort $(distfiles))

dist-normal: DESTINATION := $(DISTDIR)/$(thisdir)
dist-normal: $(distfiles)
	$(shell [ -d $(DESTINATION) ] || mkdir -p $(DESTINATION))
	@list='$(sortedfiles)'; for file in $$list; do \
	  exlist='$(distfiles_exclude)'; for ex in $$exlist; do \
	    if test $$file = $$ex; then continue 2; fi; \
          done; \
	  if test -d $$file; then \
	    $(MKINSTALLDIRS) "$(DESTINATION)/$$file"; \
	  else \
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
	        $(INSTALL) $(srcdir)/$$file $(DESTINATION)/$$dir || exit 1; \
	      fi; \
	      $(INSTALL) $$d/$$file $(DESTINATION)/$$dir || exit 1; \
	    else \
	      echo Copying $(DESTINATION)/$$file; \
	      test -f $(DESTINATION)/$$file \
	      || $(INSTALL) $$d/$$file $(DESTINATION)/$$file \
	      || exit 1; \
	    fi; \
	  fi; \
	done


# -----------------------------------------------------------------------------
# Cleaning
# -----------------------------------------------------------------------------

# This tries to follow the recommended GNU make guidelines.

# Although we don't explicitly use libtool in the hand-written libSBML
# makefiles, the cleaning targets for libtool are included below in case
# someone ever decides to use libtool as part of some make actions.

mostlyclean:        clean-generic mostlyclean-libtool
mostlyclean-normal: clean-generic mostlyclean-libtool

clean:        mostlyclean clean-libraries clean-libtool clean-extras
clean-normal: mostlyclean clean-libraries clean-libtool clean-extras

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
distclean-normal: clean distclean-compile distclean-depend distclean-generic \
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

maintainer-clean:        distclean maintainer-clean-generic
maintainer-clean-normal: distclean maintainer-clean-generic

maintainer-clean-generic:
	-rm -rf $(extra_maintainer_clean)


# -----------------------------------------------------------------------------
# Miscellaneous definitions.
# -----------------------------------------------------------------------------

# The following is for running shell commands with the right XX_LIBRARY_PATH
# variable set for different platforms.

ifeq "$(HOST_TYPE)" "darwin"

  define libsbmlrun
	DYLD_LIBRARY_PATH="$(TOP_BUILDDIR)/src:$(DYLD_LIBRARY_PATH):."; export DYLD_LIBRARY_PATH; export srcdir=.; \
	$(1)
  endef

endif
ifeq "$(HOST_TYPE)" "linux"

  define libsbmlrun
	LD_LIBRARY_PATH="$(TOP_BUILDDIR)/src:$(LD_LIBRARY_PATH):."; export LD_LIBRARY_PATH; export srcdir=.; \
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

$(TOP_SRCDIR)/config.status: $(TOP_SRCDIR)/configure $(TOP_SRCDIR)/VERSION.txt
	$(SHELL) ./config.status --recheck

$(TOP_SRCDIR)/configure: $(TOP_SRCDIR)/configure.ac $(ACLOCAL_M4)
	cd $(TOP_SRCDIR) && $(AUTOCONF)
	cd $(TOP_SRCDIR) && $(SHELL) ./config.status --recheck

$(ACLOCAL_M4): $(wildcard $(TOP_SRCDIR)/config/*.m4)
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

ID: $(headers) $(sources)
	list='$(sources) $(headers)'; \
	unique=`for i in $$list; do \
	    if test -f "$$i"; then echo $$i; else echo $(srcdir)/$$i; fi; \
	  done | \
	  $(AWK) '    { files[$$0] = 1; } \
	       END { for (i in files) print i; }'`; \
	mkid -fID $$unique

TAGS: $(headers) $(sources)
	tags=; \
	here=`pwd`; \
	list='$(sources) $(headers)'; \
	unique=`for i in $$list; do \
	    if test -f "$$i"; then echo $$i; else echo $(srcdir)/$$i; fi; \
	  done | \
	  $(AWK) '    { files[$$0] = 1; } \
	       END { for (i in files) print i; }'`; \
	test -z "$$tags$$unique" || $(ETAGS) $(ETAGSFLAGS) $$tags $$unique

CTAGS: $(headers) $(sources)
	tags=; \
	here=`pwd`; \
	list='$(sources) $(headers)'; \
	unique=`for i in $$list; do \
	    if test -f "$$i"; then echo $$i; else echo $(srcdir)/$$i; fi; \
	  done | \
	  $(AWK) '    { files[$$0] = 1; } \
	       END { for (i in files) print i; }'`; \
	test -z "$$tags$$unique" || $(CTAGS) $(CTAGSFLAGS) $$tags $$unique


# -----------------------------------------------------------------------------
# Common special targets.
# -----------------------------------------------------------------------------

.PHONY: $(recursive_targets) CTAGS GTAGS all all-am check check-am docs \
	clean clean-generic clean-libtool ctags \
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
