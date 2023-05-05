string(TOUPPER ${PROJECT_NAME} _UPPER_PROJECT_NAME)
set(_PROJECT_DEPENDENCY_DIR ${_UPPER_PROJECT_NAME}_DEPENDENCY_DIR)

find_library(XERCES_LIBRARY
NAMES xerces-c_3.lib xerces-c_4.lib xerces-c
PATHS /usr/lib /usr/local/lib
      ${${_PROJECT_DEPENDENCY_DIR}}/lib
DOC "The file name of the Xerces library."
        )

find_path(XERCES_INCLUDE_DIR
NAMES xercesc/parsers/XercesDOMParser.hpp
PATHS /usr/include /usr/local/include
      ${CMAKE_OSX_SYSROOT}/usr/include/xercesc
      /usr/include/xercesc
      /usr/local/include/xercesc
      ${${_PROJECT_DEPENDENCY_DIR}}/include
DOC "The directory containing the Xerces include files."
      )

if (EXISTS "${XERCES_INCLUDE_DIR}/xercesc/util/XercesVersion.hpp")
function(_myXercesC_GET_VERSION  version_hdr)
    file(STRINGS ${version_hdr} _contents REGEX "^[ \t]*#define XERCES_VERSION_.*")
    if(_contents)
        string(REGEX REPLACE ".*#define XERCES_VERSION_MAJOR[ \t]+([0-9]+).*" "\\1" XercesC_MAJOR "${_contents}")
        string(REGEX REPLACE ".*#define XERCES_VERSION_MINOR[ \t]+([0-9]+).*" "\\1" XercesC_MINOR "${_contents}")
        string(REGEX REPLACE ".*#define XERCES_VERSION_REVISION[ \t]+([0-9]+).*" "\\1" XercesC_PATCH "${_contents}")

        if(NOT XercesC_MAJOR MATCHES "^[0-9]+$")
            message(FATAL_ERROR "Version parsing failed for XERCES_VERSION_MAJOR!")
        endif()
        if(NOT XercesC_MINOR MATCHES "^[0-9]+$")
            message(FATAL_ERROR "Version parsing failed for XERCES_VERSION_MINOR!")
        endif()
        if(NOT XercesC_PATCH MATCHES "^[0-9]+$")
            message(FATAL_ERROR "Version parsing failed for XERCES_VERSION_REVISION!")
        endif()

        set(XERCES_VERSION "${XercesC_MAJOR}.${XercesC_MINOR}.${XercesC_PATCH}" PARENT_SCOPE)
    else()
        message(FATAL_ERROR "Include file ${version_hdr} does not exist or does not contain expected version information")
    endif()
endfunction()

_myXercesC_GET_VERSION("${XERCES_INCLUDE_DIR}/xercesc/util/XercesVersion.hpp")
if (XERCES_VERSION VERSION_EQUAL 2.6.0)
 message(FATAL_ERROR
"Xerces version ${XERCES_VERSION} contains known bugs
and is not compatible with libSBML. Please use a newer version of
Xerces or an alternate XML parser.")
endif()
endif ()

if(NOT TARGET XERCES::XERCES)
  add_library(XERCES::XERCES UNKNOWN IMPORTED)
  set_target_properties(XERCES::XERCES PROPERTIES
    IMPORTED_LINK_INTERFACE_LANGUAGES "C"
    IMPORTED_LOCATION "${XERCES_LIBRARY}"
    INTERFACE_INCLUDE_DIRECTORIES "${XERCES_INCLUDE_DIR}")
endif()


include(FindPackageHandleStandardArgs)

find_package_handle_standard_args(
    XERCES
    VERSION_VAR   XERCES_VERSION
    REQUIRED_VARS XERCES_LIBRARY XERCES_INCLUDE_DIR)

mark_as_advanced(XERCES_VERSION)
