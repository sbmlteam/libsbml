####################################################################
#
# SBML Hierarchical Composition package 
#
# $Author Lucian Smith$
# $Id$
# $HeadURL$
#

option(ENABLE_COMP     "Enable hierarchical model composition package"    OFF )

if(ENABLE_COMP)
	add_definitions(-DUSE_COMP)
	set(LIBSBML_PACKAGE_INCLUDES ${LIBSBML_PACKAGE_INCLUDES} "LIBSBML_HAS_PACKAGE_COMP")
	list(APPEND SWIG_EXTRA_ARGS -DUSE_COMP)	
endif()
