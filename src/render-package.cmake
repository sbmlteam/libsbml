#################################################
 # @file render-package.cmake
 # @brief Src CMake file for render package
 # @author SBMLTeam
 #
 # <!--------------------------------------------------------------------------
 # This file is part of libSBML. Please visit http://sbml.org for more
 # information about SBML, and the latest version of libSBML.
 #
 # Copyright (C) 2013-2017 jointly by the following organizations:
 # 1. California Institute of Technology, Pasadena, CA, USA
 # 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 # 3. University of Heidelberg, Heidelberg, Germany
 #
 # Copyright (C) 2009-2013 jointly by the following organizations:
 # 1. California Institute of Technology, Pasadena, CA, USA
 # 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 #
 # Copyright (C) 2006-2008 by the California Institute of Technology,
 # Pasadena, CA, USA
 #
 # Copyright (C) 2002-2005 jointly by the following organizations:
 # 1. California Institute of Technology, Pasadena, CA, USA
 # 2. Japan Science and Technology Agency, Japan
 #
 # This library is free software; you can redistribute it and/or modify it
 # under the terms of the GNU Lesser General Public License as published by the
 # Free Software Foundation. A copy of the license agreement is provided in the
 # file named "LICENSE.txt" included with this software distribution and also
 # available online as http://sbml.org/software/libsbml/license.html
 # ------------------------------------------------------------------------ -->
 #

if(ENABLE_RENDER)

include(${LIBSBML_ROOT_SOURCE_DIR}/render-package.cmake)

#build up sources
set(RENDER_SOURCES)

 # go through all directories
foreach(dir common extension sbml util validator validator/constraints)

	# add to include directory
	include_directories(${CMAKE_CURRENT_SOURCE_DIR}/sbml/packages/render/${dir})
	
	# file sources
	file(GLOB current ${CMAKE_CURRENT_SOURCE_DIR}/sbml/packages/render/${dir}/*.cpp
			${CMAKE_CURRENT_SOURCE_DIR}/sbml/packages/render/${dir}/*.h)
	
        # Set the *Constraints.cpp files to be 'header' files so they won't be compiled--
        #  they are #included directly, instead.
        if (dir STREQUAL "validator/constraints")
            foreach(tempFile ${current})
                if (tempFile MATCHES ".*Constraints.cpp")
                    set_source_files_properties(
                        ${tempFile}
                        PROPERTIES HEADER_FILE_ONLY true
                        )
                endif()
            endforeach()
        endif()

	# add sources 
	set(RENDER_SOURCES ${RENDER_SOURCES} ${current})
	
   # mark headers for installation
  file(GLOB render_headers ${CMAKE_CURRENT_SOURCE_DIR}/sbml/packages/render/${dir}/*.h)

    install(FILES ${render_headers} DESTINATION include/sbml/packages/render/${dir})	
	
endforeach()

# create source group for IDEs
source_group(render_package FILES ${RENDER_SOURCES})

 # add render sources to SBML sources
SET(LIBSBML_SOURCES ${LIBSBML_SOURCES} ${RENDER_SOURCES})

####################################################################
#
# add test scripts
#
if(WITH_CHECK)

		add_subdirectory(sbml/packages/render/sbml/test)
	add_subdirectory(sbml/packages/render/validator/test)

endif()

endif()
