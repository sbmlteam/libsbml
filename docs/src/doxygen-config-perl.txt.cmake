# @configure_input@
# -----------------------------------------------------------------------------
# File name         : doxyfile-config-perl.txt
# Description       : Doxygen config for Perl libSBML API manual 
# Original author(s): Michael Hucka <mhucka@caltech.edu>
# Organization      : California Institute of Technology
# -----------------------------------------------------------------------------

# Include libSBML's common Doxygen settings:

@INCLUDE               = doxygen-config-common.txt

# -----------------------------------------------------------------------------
# Beginning of Perl-specific configuration settings
# -----------------------------------------------------------------------------

# The PROJECT_NAME tag is a single word (or a sequence of words surrounded 
# by quotes) that should identify the project.

PROJECT_NAME           = "@PACKAGE_NAME@ Perl API"

# The PROJECT_NUMBER tag can be used to enter a project or revision number. 
# This could be handy for archiving the generated documentation or 
# if some version control system is used.

PROJECT_NUMBER         = "@PACKAGE_VERSION@"

# The HTML_OUTPUT tag is used to specify where the HTML docs will be put. 
# If a relative path is entered the value of OUTPUT_DIRECTORY will be 
# put in front of it. If left blank `html' will be used as the default path.

HTML_OUTPUT            = ../formatted/perl-api

# If you use STL classes (i.e. std::string, std::vector, etc.) but do not
# want to include (a tag file for) the STL sources as input, then you should
# set this tag to YES in order to let doxygen match functions declarations
# and definitions whose arguments contain STL classes
# (e.g. func(std::string); v.s. func(std::string) {}). This also make the
# inheritance and collaboration diagrams that involve STL classes more
# complete and accurate.

BUILTIN_STL_SUPPORT    = YES

# If the GENERATE_PERLMOD tag is set to YES Doxygen will 
# generate a Perl module file that captures the structure of 
# the code including all documentation. Note that this 
# feature is still experimental and incomplete at the 
# moment.

GENERATE_PERLMOD       = YES

# If the PERLMOD_LATEX tag is set to YES Doxygen will generate 
# the necessary Makefile rules, Perl scripts and LaTeX code to be able 
# to generate PDF and DVI output from the Perl module output.

PERLMOD_LATEX          = NO

# If the PERLMOD_PRETTY tag is set to YES the Perl module output will be 
# nicely formatted so it can be parsed by a human reader.  This is useful 
# if you want to understand what is going on.  On the other hand, if this 
# tag is set to NO the size of the Perl module output will be much smaller 
# and Perl will parse it just the same.

PERLMOD_PRETTY         = NO

# The names of the make variables in the generated doxyrules.make file 
# are prefixed with the string contained in PERLMOD_MAKEVAR_PREFIX. 
# This is useful so different doxyrules.make files included by the same 
# Makefile don't overwrite each other's variables.

PERLMOD_MAKEVAR_PREFIX = 

# The PREDEFINED tag can be used to specify one or more macro names that 
# are defined before the preprocessor is started (similar to the -D option of 
# gcc). The argument of the tag is a list of macros of the form: name 
# or name=definition (no spaces). If the definition and the = are 
# omitted =1 is assumed.

PREDEFINED             = __cplusplus  \
		         LIBSBML_EXTERN:="" \
			 BEGIN_C_DECLS:="" \
			 END_C_DECLS:="" \
                         LIBSBML_CPP_NAMESPACE_BEGIN:="" \
                         LIBSBML_CPP_NAMESPACE_END:="" \
			 SWIG=1

# The ENABLED_SECTIONS tag can be used to enable conditional 
# documentation sections, marked by \if sectionname ... \endif.

ENABLED_SECTIONS       = notcpp doxygenPerlOnly 

BRIEF_MEMBER_DESC      = YES

# Override the settings in doxygen-config-common.txt to refer to just
# the files relevant for the Python bindings.

#INPUT = \
#  libsbml-accessing.txt            \
#  libsbml-python-mainpage.txt      \
#  libsbml-python-reading-files.txt \
#  libsbml-features.txt             \
#  libsbml-installation.txt         \
#  libsbml-communications.txt       \
#  libsbml-coding.txt               \
#  libsbml-issues.txt               \
#  libsbml-license.txt              \
#  ../../src/bindings/python/libsbml.py

# Blank definitions for some constructs used only for some languages.

ALIASES += sbmldefgroup{2}=""
ALIASES += sbmlingroup{1}=""
ALIASES += sbmlendgroup=""
