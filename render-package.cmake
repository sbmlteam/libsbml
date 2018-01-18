####################################################################
#
# SBML RENDER package 
#
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
#
# Copyright (C) 2013-2018 jointly by the following organizations:
#     1. California Institute of Technology, Pasadena, CA, USA
#     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
#     3. University of Heidelberg, Heidelberg, Germany
#
# Copyright (C) 2009-2013 jointly by the following organizations: 
#     1. California Institute of Technology, Pasadena, CA, USA
#     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
#  
# Copyright (C) 2006-2008 by the California Institute of Technology,
#     Pasadena, CA, USA 
#  
# Copyright (C) 2002-2005 jointly by the following organizations: 
#     1. California Institute of Technology, Pasadena, CA, USA
#     2. Japan Science and Technology Agency, Japan
# 
# This library is free software; you can redistribute it and/or modify it
# under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation.  A copy of the license agreement is provided
# in the file named "LICENSE.txt" included with this software distribution
# and also available online as http://sbml.org/software/libsbml/license.html
#
###############################################################################


option(ENABLE_RENDER     "Enable libSBML support for the SBML Level 3 Rendering ('render') package."    OFF )

# provide summary status                                    =
list(APPEND LIBSBML_PACKAGE_SUMMARY "SBML 'render' package  = ${ENABLE_RENDER}")

if(ENABLE_RENDER)	
	if (NOT ENABLE_LAYOUT)
		# instead simply force layout to be true, as it will have to be present!
		message(WARNING " The layout package is required to build the render package, setting ENABLE_LAYOUT to ON")
		set(ENABLE_LAYOUT ON CACHE BOOL "Enable Layout package" FORCE)
	endif()
    SET(USE_RENDER TRUE)
	set(LIBSBML_PACKAGE_INCLUDES ${LIBSBML_PACKAGE_INCLUDES} "LIBSBML_HAS_PACKAGE_RENDER")
	add_definitions(-DUSE_RENDER)
	
	# the render class defines 'Rectangle' and 'Ellipse' these classes are by default
	# defined by GDI, so there would be a name conflict if just using them here
	# so for our builds we disable it
	if (WIN32)
	  add_definitions(-DNOGDI -DWIN32_LEAN_AND_MEAN ) 
	  list(APPEND SWIG_EXTRA_ARGS -DNOGDI -DWIN32_LEAN_AND_MEAN )	  
	endif()
	
	list(APPEND SWIG_EXTRA_ARGS -DUSE_RENDER)
	list(APPEND SWIG_SWIGDOCDEFINES --define USE_RENDER)
endif()
