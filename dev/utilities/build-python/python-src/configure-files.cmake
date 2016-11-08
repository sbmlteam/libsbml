# cmake build script wrapping built libsbml python bindings into conda binaries
#
#
 
if ("${SRC_DIR}" STREQUAL "" OR "${BUILD_DIR}" STREQUAL "" OR "${NAME}" STREQUAL "" )
message(FATAL_ERROR 
"
  Need the following parameters: 
    - SRC_DIR:   the libsbml directory (containing VERSION.txt)
    - BUILD_DIR: the build directory where libsbml was build with SWIG
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

# configure files 
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/setup.py.in ${CMAKE_CURRENT_SOURCE_DIR}/setup.py)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/setup32.py.in ${CMAKE_CURRENT_SOURCE_DIR}/setup32.py)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/setup64.py.in ${CMAKE_CURRENT_SOURCE_DIR}/setup64.py)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/setupStable.py.in ${CMAKE_CURRENT_SOURCE_DIR}/setupStable.py)

# copy files
file(GLOB PYTHON_FILES 
    "${SRC_DIR}/src/bindings/python/*.h"
    "${SRC_DIR}/src/bindings/python/*.cpp"
    "${SRC_DIR}/src/bindings/python/*.cxx"
    "${BUILD_DIR}/src/bindings/python/*.pth"
    "${BUILD_DIR}/src/bindings/python/*.cpp"
    "${BUILD_DIR}/src/bindings/python/*.h"
)
    
file(COPY ${PYTHON_FILES} DESTINATION ${CMAKE_CURRENT_SOURCE_DIR})
file(COPY "${BUILD_DIR}/src/bindings/python/libsbml.py" DESTINATION ${CMAKE_CURRENT_SOURCE_DIR}/libsbml)
file(RENAME "${CMAKE_CURRENT_SOURCE_DIR}/libsbml/libsbml.py" "${CMAKE_CURRENT_SOURCE_DIR}/libsbml/__init__.py")

file(GLOB SWIG_FILES 
    "${SRC_DIR}/src/bindings/swig/*.h"
    "${SRC_DIR}/src/bindings/swig/*.cpp"
    "${SRC_DIR}/src/bindings/swig/*.cxx"
    "${BUILD_DIR}/src/bindings/swig/*.cpp"
    "${BUILD_DIR}/src/bindings/swig/*.h"
)
    
file(COPY ${SWIG_FILES} DESTINATION ${CMAKE_CURRENT_SOURCE_DIR}/../swig)

