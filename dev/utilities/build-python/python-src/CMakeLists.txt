###############################################################################
#
# 
#
# Description       : CMake build script for compiling and linking the 
#                     python bindings
# Original author(s): Frank Bergmann <fbergman@caltech.edu>
# Organization      : California Institute of Technology
#
# This file is supposed to be called by buildPython32.bat / buildPython64.bat
#
###############################################################################

cmake_minimum_required(VERSION 2.8)
project(_libsbml)

include (CMakeTestCCompiler)
include (CheckCSourceCompiles)
include (CheckCXXSourceCompiles)
include (CheckStructHasMember)
include (CheckLibraryExists)
include (CheckFunctionExists)
include (CheckCCompilerFlag)
include (CheckCSourceRuns)
include (CheckSymbolExists)
include (CheckTypeSize)

find_package(PythonInterp REQUIRED)

# determine the library path and include path
# for the given interpreter
get_filename_component(path "${PYTHON_EXECUTABLE}" PATH)	
file(GLOB python_lib "${path}/libs/python*.lib")
foreach(lib ${python_lib})
  get_filename_component(name "${lib}" NAME)
  string(TOLOWER "${name}" lower)
  if ("${lower}" STREQUAL "python3.lib" 
      OR "${lower}" MATCHES "_d.lib$" 
      OR "${lower}" MATCHES "debug" 
      OR "${lower}" MATCHES "optimized" 
  )
    list(REMOVE_ITEM python_lib "${lib}")
  endif()
endforeach()
set(PYTHON_LIBRARY ${python_lib})
set(PYTHON_INCLUDE_DIR ${path}/include)
find_package(PythonLibs REQUIRED)


include_directories(${CMAKE_CURRENT_SOURCE_DIR})
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../swig)
if (CMAKE_SIZEOF_VOID_P EQUAL 4)
include_directories(${CMAKE_SOURCE_DIR}/../../../win32/include)
else()
include_directories(${CMAKE_SOURCE_DIR}/../../../win64/include)
endif()
include_directories(${PYTHON_INCLUDE_DIRS})
include_directories(${DEP_DIR}/include)

add_definitions(-DUSE_COMP 
				-DUSE_FBC 
				-DUSE_QUAL 
				-DUSE_LAYOUT 
				-DUSE_MULTI
				-DUSE_GROUPS
				-DUSE_RENDER
				-DUSE_L3V2EXTENDEDMATH
)


# set build type default
set(CMAKE_BUILD_TYPE "Release" CACHE STRING "Choose the type of build, options are: None (CMAKE_CXX_FLAGS or CMAKE_C_FLAGS are used), Debug, Release, RelWithDebInfo, MinSizeRel" FORCE )


###############################################################################
#
# Set up remaining variables, add option for universal binaries
#
set(BUILD_DEFINITIONS)
if(UNIX)
	if(APPLE)
		add_definitions(-DMACOSX)
		set(BUILD_DEFINITIONS "${BUILD_DEFINITIONS} -DMACOSX")
		
		# On OSX it is common to build universal binaries to support multiple
		# processor architectures. The default behavior is not to build 
		# multiple architectures, as most users might not need that. 
		option(ENABLE_UNIVERSAL "Create Universal Binaries" OFF)
		
		set(CMAKE_OSX_ARCHITECTURES "${CMAKE_OSX_ARCHITECTURES}" CACHE STRING "A semicolon separated list of build architectures to be used")
		if(ENABLE_UNIVERSAL)
			# if universal binaries are requested and none defined so far
			# overwrite them with all three common architectures. If the user 
			# specified their own list of architectures do not touch!
			if (CMAKE_OSX_ARCHITECTURES STREQUAL "")						
				set(CMAKE_OSX_ARCHITECTURES "i386;ppc;x86_64" CACHE STRING "A semicolon separated list of build architectures to be used" FORCE)				
			endif()
		endif(ENABLE_UNIVERSAL)
	else(APPLE)
		add_definitions(-DLINUX)
		
		if (NOT CYGWIN)
			# on cygwin all code is position independent so -fPIC is not needed
			set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-strict-aliasing -fPIC")
			set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fno-strict-aliasing -fPIC")
		endif()
		
		set(BUILD_DEFINITIONS "${BUILD_DEFINITIONS} -DLINUX")
	endif(APPLE)

	add_definitions( -DPACKAGE_VERSION=\"${PACKAGE_VERSION}\"  -DPACKAGE_NAME=\"${PROJECT_NAME}\")
	set(BUILD_DEFINITIONS "${BUILD_DEFINITIONS} -DPACKAGE_VERSION=\"${PACKAGE_VERSION}\"  -DPACKAGE_NAME=\"${PROJECT_NAME}\"")

else(UNIX)
	add_definitions(-DWIN32 -DLIBSBML_EXPORTS -DLIBLAX_EXPORTS)
	set(BUILD_DEFINITIONS "${BUILD_DEFINITIONS} -DWIN32 -DLIBSBML_EXPORTS -DLIBLAX_EXPORTS")
	if(MSVC)
		add_definitions(-D_CRT_SECURE_NO_WARNINGS -D_CRT_NONSTDC_NO_DEPRECATE)
		set(BUILD_DEFINITIONS "${BUILD_DEFINITIONS} -D_CRT_SECURE_NO_WARNINGS")
		option(WITH_STATIC_RUNTIME "Compile using the static MSVC Runtime" OFF)
		if (NOT WITHOUT_STATIC_RUNTIME)
			foreach(flag_var
				CMAKE_CXX_FLAGS CMAKE_CXX_FLAGS_DEBUG CMAKE_CXX_FLAGS_RELEASE
				CMAKE_CXX_FLAGS_MINSIZEREL CMAKE_CXX_FLAGS_RELWITHDEBINFO
				CMAKE_C_FLAGS CMAKE_C_FLAGS_DEBUG CMAKE_C_FLAGS_RELEASE
				CMAKE_C_FLAGS_MINSIZEREL CMAKE_C_FLAGS_RELWITHDEBINFO)
		
				if(${flag_var} MATCHES "/MD")
					string(REGEX REPLACE "/MD" "/MT" ${flag_var} "${${flag_var}}")
				endif(${flag_var} MATCHES "/MD")
			endforeach(flag_var)
		add_definitions( -D_MT)
		endif(NOT WITHOUT_STATIC_RUNTIME)
	
		file(GLOB WIN32_BINARIES ${CMAKE_SOURCE_DIR}/dependencies/bin/*.dll)
		INSTALL(FILES ${WIN32_BINARIES} DESTINATION bin)
		
	elseif(CYGWIN)
		add_definitions(-DCYGWIN)
		set(BUILD_DEFINITIONS "${BUILD_DEFINITIONS} -DCYGWIN")
	elseif(MINGW)
		if(WITH_LIBXML)
			# this is necessary to build with libxml2 on mingw
			add_definitions(-DLIBXML_STATIC)
		endif(WITH_LIBXML)
	endif(MSVC)
	
endif(UNIX)


set(LIB_PATH)
if (CMAKE_SIZEOF_VOID_P EQUAL 4)
	set(LIB_PATH "${CMAKE_SOURCE_DIR}/../install_32/lib")
  include_directories("${CMAKE_SOURCE_DIR}/../install_32/include")
else()
	set(LIB_PATH "${CMAKE_SOURCE_DIR}/../install_64/lib")
  include_directories("${CMAKE_SOURCE_DIR}/../install_64/include")
endif()



find_library(SBML_LIBRARY 
	NAMES libsbml-static.lib
	PATHS ${LIB_PATH} ${DEP_DIR}/lib
)
find_library(XML_LIBRARY 
	NAMES libxml2.lib
	PATHS ${LIB_PATH} ${DEP_DIR}/lib
)
find_library(ICONV_LIBRARY 
	NAMES libiconv.lib iconv.lib
	PATHS ${LIB_PATH} ${DEP_DIR}/lib
)
find_library(BZ2_LIBRARY 
	NAMES libbz2.lib bzip2.lib
	PATHS ${LIB_PATH} ${DEP_DIR}/lib
)

find_library(ZLIB_LIBRARY 
	NAMES zdll.lib zlib.lib
	PATHS ${LIB_PATH} ${DEP_DIR}/lib
)

if (MSVC)
add_definitions(/bigobj)
endif()

set(LIBS 
	${LIBS} ${SBML_LIBRARY} 
	${XML_LIBRARY} ${ICONV_LIBRARY}
	${BZ2_LIBRARY} ${ZLIB_LIBRARY}
	${PYTHON_LIBRARIES}
)


# remove additional list for python3
foreach(lib ${LIBS})
  get_filename_component(name "${lib}" NAME)
  string(TOLOWER "${name}" lower)
  if ("${lower}" STREQUAL "python3.lib" 
      OR "${lower}" MATCHES "_d.lib$" 
      OR "${lower}" MATCHES "debug" 
      OR "${lower}" MATCHES "optimized" 
  )
    list(REMOVE_ITEM LIBS "${lib}")
  endif()
endforeach()

add_library(binding_python_lib SHARED libsbml_wrap.cpp)

set_target_properties (binding_python_lib PROPERTIES OUTPUT_NAME "_libsbml")
if (UNIX)
	set_target_properties (binding_python_lib PROPERTIES PREFIX "")
	set_target_properties (binding_python_lib PROPERTIES SUFFIX ".so")
else()		
	if (CYGWIN)
		set_target_properties (binding_python_lib PROPERTIES PREFIX "")
		set_target_properties (binding_python_lib PROPERTIES SUFFIX ".dll")
	else()
		set(LIBS ${LIBS} ws2_32.lib)
		set_target_properties (binding_python_lib PROPERTIES SUFFIX ".pyd")	
	endif()
endif()

target_link_libraries(binding_python_lib ${LIBS} )

