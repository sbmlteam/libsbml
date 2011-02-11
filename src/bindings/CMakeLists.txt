###############################################################################
#
# $URL$
# $Id$
#
# Description       : CMake build script for libSBML language bindings
# Original author(s): Frank Bergmann <fbergman@caltech.edu>
# Organization      : California Institute of Technology
#
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
#
# Copyright 2005-2011 California Institute of Technology.
# Copyright 2002-2005 California Institute of Technology and
#                     Japan Science and Technology Corporation.
# 
# This library is free software; you can redistribute it and/or modify it
# under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation.  A copy of the license agreement is provided
# in the file named "LICENSE.txt" included with this software distribution
# and also available online as http://sbml.org/software/libsbml/license.html
#
###############################################################################

#
# and build selected language bindings
#

if(WITH_CSHARP)
add_subdirectory(csharp)
endif()

if(WITH_JAVA)
add_subdirectory(java)
endif()

if(WITH_PERL)
add_subdirectory(perl)
endif()

if(WITH_MATLAB)
add_subdirectory(matlab)
endif()

if(WITH_OCTAVE)
add_subdirectory(octave)
endif()

if(WITH_PYTHON)
add_subdirectory(python)
endif()

if(WITH_RUBY)
add_subdirectory(ruby)
endif()
