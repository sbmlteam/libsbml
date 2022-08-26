include(CheckLibraryExists)

string(TOUPPER ${PROJECT_NAME} _UPPER_PROJECT_NAME)
set(_PROJECT_DEPENDENCY_DIR ${_UPPER_PROJECT_NAME}_DEPENDENCY_DIR)

find_library(LIBXML_LIBRARY
    NAMES  libxml2s xml2s libxml2.lib xml2
    PATHS ${${_PROJECT_DEPENDENCY_DIR}}/lib64
          ${${_PROJECT_DEPENDENCY_DIR}}/lib
    NO_DEFAULT_PATH
)


if (NOT LIBXML_LIBRARY)
    find_library(LIBXML_LIBRARY
        NAMES  libxml2s xml2s libxml2.lib xml2
        PATHS /usr/lib /usr/local/lib
              ${${_PROJECT_DEPENDENCY_DIR}}/lib
        DOC "The file name of the libxml2 library."
    )
endif()

find_path(LIBXML_INCLUDE_DIR
    NAMES libxml/parser.h
    PATHS ${${_PROJECT_DEPENDENCY_DIR}}/include
          ${${_PROJECT_DEPENDENCY_DIR}}/include/libxml2
    NO_DEFAULT_PATH
)


if (NOT LIBXML_INCLUDE_DIR)
    find_path(LIBXML_INCLUDE_DIR
        NAMES libxml/parser.h
        PATHS ${${_PROJECT_DEPENDENCY_DIR}}/include
              ${${_PROJECT_DEPENDENCY_DIR}}/include/libxml2
              /usr/include /usr/local/include
              /usr/include/libxml2
              ${CMAKE_OSX_SYSROOT}/usr/include/libxml2
              /usr/local/include/libxml2

        DOC "The directory containing the libxml2 include files."
    )
endif()

if(LIBXML_INCLUDE_DIR AND EXISTS "${LIBXML_INCLUDE_DIR}/libxml/xmlversion.h")
    file(STRINGS "${LIBXML_INCLUDE_DIR}/libxml/xmlversion.h" libxml2_version_str
         REGEX "^#define[\t ]+LIBXML_DOTTED_VERSION[\t ]+\".*\"")

    string(REGEX REPLACE "^#define[\t ]+LIBXML_DOTTED_VERSION[\t ]+\"([^\"]*)\".*" "\\1"
           LIBXML_VERSION "${libxml2_version_str}")
    unset(libxml2_version_str)
endif()

# populate EXTRA_LIBS variable
find_library(LIBICONV_LIBRARY
NAMES libiconv.lib iconv.lib iconv
PATHS ${${_PROJECT_DEPENDENCY_DIR}}/lib64
      ${${_PROJECT_DEPENDENCY_DIR}}/lib
      /usr/lib /usr/local/lib
      ${CMAKE_OSX_SYSROOT}/usr/lib
DOC "The file name of the libiconv library."
)

set(ADDITIONAL_LIBS)
if (EXISTS ${LIBICONV_LIBRARY})
set(ADDITIONAL_LIBS "${ADDITIONAL_LIBS}${LIBICONV_LIBRARY};")
endif()
find_package(ZLIB QUIET)
if (EXISTS ${ZLIB_LIBRARY})
set(ADDITIONAL_LIBS "${ADDITIONAL_LIBS}ZLIB::ZLIB;")
endif()
if (WIN32)
set(ADDITIONAL_LIBS "ws2_32.lib;${ADDITIONAL_LIBS}")
endif()


CHECK_LIBRARY_EXISTS(m sin "" HAVE_LIB_M)

if (HAVE_LIB_M)
    set(ADDITIONAL_LIBS ${ADDITIONAL_LIBS} m)
endif (HAVE_LIB_M)

if(NOT TARGET LIBXML::LIBXML)
  add_library(LIBXML::LIBXML UNKNOWN IMPORTED)
  set_target_properties(LIBXML::LIBXML PROPERTIES
    IMPORTED_LINK_INTERFACE_LANGUAGES "C"
    IMPORTED_LOCATION "${LIBXML_LIBRARY}"
    INTERFACE_INCLUDE_DIRECTORIES "${LIBXML_INCLUDE_DIR}"
    INTERFACE_LINK_LIBRARIES "${ADDITIONAL_LIBS}"
  )
endif()

# figure out if we need XML_STATIC flag
if (LIBXML_INCLUDE_DIR AND LIBXML_LIBRARY)

  set(LIBXML_LIBXML_CODE
"
#include <libxml/xmlversion.h>
#include <libxml/parser.h>
#include <stdio.h>

int 
main(void)
{
  LIBXML_TEST_VERSION
  xmlKeepBlanksDefault(0);
  xmlDocPtr doc = xmlParseFile(\"none\");
  xmlCleanupParser();
  return 0;
}
" 
)

set(CMAKE_REQUIRED_LIBRARIES_CACHE ${CMAKE_REQUIRED_LIBRARIES})
set(CMAKE_REQUIRED_INCLUDES_CACHE ${CMAKE_REQUIRED_INCLUDES})
set(CMAKE_REQUIRED_DEFINITIONS_CACHE ${CMAKE_REQUIRED_DEFINITIONS})

unset(LIBXML_LIBXML_TEST)
unset(LIBXML_LIBXML_TEST2)

set(LIBXML_LIBXML_TEST)
set(CMAKE_REQUIRED_LIBRARIES "${LIBXML_LIBRARY};${ADDITIONAL_LIBS}")
set(CMAKE_REQUIRED_INCLUDES "${LIBXML_INCLUDE_DIR}")
CHECK_C_SOURCE_COMPILES("${LIBXML_LIBXML_CODE}" LIBXML_LIBXML_TEST)


message(STATUS "LIBXML_LIBXML_TEST = ${LIBXML_LIBXML_TEST}")

if (NOT LIBXML_LIBXML_TEST)
set(CMAKE_REQUIRED_LIBRARIES "${LIBXML_LIBRARY};${ADDITIONAL_LIBS}")
set(CMAKE_REQUIRED_INCLUDES "${LIBXML_INCLUDE_DIR}")
set(CMAKE_REQUIRED_DEFINITIONS "-DLIBXML_STATIC=1")

CHECK_C_SOURCE_COMPILES("${LIBXML_LIBXML_CODE}" LIBXML_LIBXML_TEST2)
message(STATUS "LIBXML_LIBXML_TEST2 = ${LIBXML_LIBXML_TEST2}")
if (LIBXML_LIBXML_TEST2)
  set_target_properties(LIBXML::LIBXML PROPERTIES
    INTERFACE_COMPILE_DEFINITIONS "LIBXML_STATIC=1"
    )
else()
    message(FATAL_ERROR "Unable to compile a test executable against LIBXML

    LIBXML_INCLUDE_DIR = ${LIBXML_INCLUDE_DIR}
    LIBXML_LIBRARY     = ${LIBXML_LIBRARY}

    ")
endif()

endif()

set(CMAKE_REQUIRED_LIBRARIES ${CMAKE_REQUIRED_LIBRARIES_CACHE})
set(CMAKE_REQUIRED_INCLUDES ${CMAKE_REQUIRED_INCLUDES_CACHE})
set(CMAKE_REQUIRED_DEFINITIONS ${CMAKE_REQUIRED_DEFINITIONS_CACHE})
endif()

include(FindPackageHandleStandardArgs)

find_package_handle_standard_args(
    LIBXML
    VERSION_VAR   LIBXML_VERSION
    REQUIRED_VARS LIBXML_LIBRARY LIBXML_INCLUDE_DIR)

mark_as_advanced(LIBXML_VERSION)
