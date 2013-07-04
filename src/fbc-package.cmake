####################################################################
#
# SBML fbc package - include files to build fbc
#
# $Author$
# $Id$
# $HeadURL$
#

if(ENABLE_FBC)

include(${CMAKE_SOURCE_DIR}/fbc-package.cmake)

#build up sources
set(FBC_SOURCES)

# go through all directtories: common, extension and sbml
foreach(dir common extension sbml util validator validator/constraints)

	# add to include directory
	include_directories(${CMAKE_CURRENT_SOURCE_DIR}/sbml/packages/fbc/${dir})
	
	# file sources
	file(GLOB current ${CMAKE_CURRENT_SOURCE_DIR}/sbml/packages/fbc/${dir}/*.cpp
			${CMAKE_CURRENT_SOURCE_DIR}/sbml/packages/fbc/${dir}/*.h)
	
	# add sources 
	set(FBC_SOURCES ${FBC_SOURCES} ${current})
	
	# mark header files for installation 
	file(GLOB fbc_headers ${CMAKE_CURRENT_SOURCE_DIR}/sbml/packages/fbc/${dir}/*.h)
    install(FILES ${fbc_headers} DESTINATION include/sbml/packages/fbc/${dir})	
	
endforeach()

# create source group for IDEs
source_group(fbc_package FILES ${FBC_SOURCES})

# add fbc sources to SBML sources
SET(LIBSBML_SOURCES ${LIBSBML_SOURCES} ${FBC_SOURCES})

####################################################################
#
# add test scripts
#
if(WITH_CHECK)

		add_subdirectory(sbml/packages/fbc/extension/test)
		add_subdirectory(sbml/packages/fbc/validator/test)

endif()

endif()
