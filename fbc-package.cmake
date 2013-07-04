####################################################################
#
# SBML FBC package 
#

option(ENABLE_FBC     "Enable FBC package"    OFF )

if(ENABLE_FBC)
    SET(USE_FBC TRUE)
	mark_as_advanced(USE_FBC)
	set(LIBSBML_PACKAGE_INCLUDES ${LIBSBML_PACKAGE_INCLUDES} "LIBSBML_HAS_PACKAGE_FBC")
	add_definitions(-DUSE_FBC)
	list(APPEND SWIG_EXTRA_ARGS -DUSE_FBC)
endif()
