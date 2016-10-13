# cmake build script wrapping built libsbml python bindings into conda binaries
#
#
 
if ("${SRC_DIR}" STREQUAL "" OR "${OUT_DIR}" STREQUAL "" OR "${BUILD_DIR}" STREQUAL "" OR "${NAME}" STREQUAL "" )
message(FATAL_ERROR 
"
  Need the following parameters: 
    - SRC_DIR:   the libsbml directory (containing VERSION.txt)
    - BUILD_DIR: the build directory containing libsbml as created 
                 by setup tools for example 'lib.win32-2.7'
    - OUT_DIR:   the directory where to place the resulting bz2 file
    - NAME:      the NAME of the package

")
endif()

set(LIBSBML_VERSION_MAJOR)
set(LIBSBML_VERSION_MINOR)
set(LIBSBML_VERSION_PATCH)
set(LIBSBML_VERSION_RELEASE)
set(LIBSBML_DOTTED_VERSION)

if(EXISTS "${SRC_DIR}/VERSION.txt")

    file(STRINGS "${SRC_DIR}/VERSION.txt" VersionString NEWLINE_CONSUME)
    string(STRIP "${VersionString}" VersionString)
    set(LIBSBML_DOTTED_VERSION ${VersionString})
    string(REPLACE "." ";" VersionString "${VersionString}" )
    string(REPLACE "-" ";" VersionString "${VersionString}" )
    list(LENGTH VersionString versionLength)
    list(GET VersionString 0 LIBSBML_VERSION_MAJOR )
    list(GET VersionString 1 LIBSBML_VERSION_MINOR )
    list(GET VersionString 2 LIBSBML_VERSION_PATCH )

    if(${versionLength} GREATER 3)
        list(GET VersionString 3 LIBSBML_VERSION_RELEASE )
    endif()

endif()

# version number needs to be calculated correctly
MATH(EXPR LIBSBML_VERSION_NUMERIC "${LIBSBML_VERSION_MAJOR} * 10000 + ${LIBSBML_VERSION_MINOR} * 100 + ${LIBSBML_VERSION_PATCH}" )
set(PACKAGE_VERSION "${LIBSBML_VERSION_MAJOR}.${LIBSBML_VERSION_MINOR}.${LIBSBML_VERSION_PATCH}${LIBSBML_VERSION_RELEASE}")

# figure out python version and arch from the BUILD_DIR
string(FIND ${BUILD_DIR} "lib." index)
math(EXPR index "${index} + 4")
string(SUBSTRING ${BUILD_DIR} ${index} -1 ARCH_VERSION)

string(FIND ${ARCH_VERSION} "-" index REVERSE)
string(SUBSTRING ${ARCH_VERSION} 0 ${index} BUILD_ARCH)
math(EXPR index "${index} + 1")
string(SUBSTRING ${ARCH_VERSION} ${index} -1 PYTHON_VERSION)
string(REPLACE "." "" SHORT_VERSION "${PYTHON_VERSION}" )
string(REPLACE "-" "_" OTHER_NAME "${NAME}")
set(TARGET_NAME "${OTHER_NAME}-${PACKAGE_VERSION}-py${PYTHON_VERSION}")
set(TARGET_DIR "${NAME}-${PACKAGE_VERSION}-py${SHORT_VERSION}_0")


message(STATUS "creating archive for version: ${PACKAGE_VERSION}")
message(STATUS " from : ${ARCH_VERSION}")
message(STATUS " from : ${BUILD_ARCH}")
message(STATUS " from : ${PYTHON_VERSION}")
message(STATUS " from : ${SHORT_VERSION}")
message(STATUS " cur : ${CMAKE_CURRENT_SOURCE_DIR}")
message(STATUS " target : ${TARGET_NAME}")

SET(SUBDIR "win-64")
SET(ARCH "x86_64")
if ("${BUILD_ARCH}" STREQUAL "win32")
SET(SUBDIR "win-32")
SET(ARCH "x86")
endif()

# create output dir 
if( NOT EXISTS "${OUT_DIR}")
  execute_process( COMMAND ${CMAKE_COMMAND} 
                   -E  make_directory ${OUT_DIR})
endif()

# create temp dir 
execute_process( COMMAND ${CMAKE_COMMAND} -E  make_directory ${TARGET_DIR})
execute_process( COMMAND ${CMAKE_COMMAND} -E  make_directory ${TARGET_DIR}/Lib)
execute_process( COMMAND ${CMAKE_COMMAND} -E  make_directory ${TARGET_DIR}/Lib/site-packages)
execute_process( COMMAND ${CMAKE_COMMAND} -E  make_directory ${TARGET_DIR}/Lib/site-packages/libsbml)
execute_process( COMMAND ${CMAKE_COMMAND} -E  make_directory ${TARGET_DIR}/info)
execute_process( COMMAND ${CMAKE_COMMAND} -E  make_directory ${TARGET_DIR}/info/recipe)

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/source/egg-info 
               ${TARGET_DIR}/Lib/site-packages/${TARGET_NAME}.egg-info)

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/source/bld.bat 
               ${TARGET_DIR}/info/recipe/bld.bat)

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/source/build.sh 
               ${TARGET_DIR}/info/recipe/build.sh)

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/source/meta.yaml 
               ${TARGET_DIR}/info/recipe/meta.yaml)

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/source/files 
               ${TARGET_DIR}/info/files)

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/source/index.json 
               ${TARGET_DIR}/info/index.json)

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/source/recipe.json 
               ${TARGET_DIR}/info/recipe.json)

file(GLOB BINARIES "${BUILD_DIR}/libsbml/*")
file(COPY ${BINARIES} DESTINATION ${TARGET_DIR}/Lib/site-packages/libsbml)

# create bz2
execute_process(COMMAND ${CMAKE_COMMAND} 
      -E tar cjf ../${TARGET_DIR}.tar.bz2 info Lib
      WORKING_DIRECTORY ${TARGET_DIR})
      
# cleanup 
execute_process(COMMAND ${CMAKE_COMMAND} 
      -E remove_directory ${TARGET_DIR})
