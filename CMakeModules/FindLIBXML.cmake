string(TOUPPER ${PROJECT_NAME} _UPPER_PROJECT_NAME)
set(_PROJECT_DEPENDENCY_DIR ${_UPPER_PROJECT_NAME}_DEPENDENCY_DIR)

if (NOT LIBXML_LIBRARY)
    find_library(LIBXML_LIBRARY
        NAMES libxml2.lib xml2
        PATHS /usr/lib /usr/local/lib
              ${${_PROJECT_DEPENDENCY_DIR}}/lib
        DOC "The file name of the libxml2 library."
                )
  endif()

  if (NOT LIBXML_INCLUDE_DIR)
    find_path(LIBXML_INCLUDE_DIR
        NAMES libxml/parser.h
        PATHS ${${_PROJECT_DEPENDENCY_DIR}}/include
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
PATHS /usr/lib /usr/local/lib
        ${CMAKE_OSX_SYSROOT}/usr/lib
        ${${_PROJECT_DEPENDENCY_DIR}}/lib
DOC "The file name of the libiconv library."
)

set(ADDITIONAL_LIBS)
if (EXISTS ${LIBICONV_LIBRARY})
set(ADDITIONAL_LIBS "${ADDITIONAL_LIBS}${LIBICONV_LIBRARY};")
endif()
if (EXISTS ${LIBZ_LIBRARY})
set(ADDITIONAL_LIBS "${ADDITIONAL_LIBS}${LIBZ_LIBRARY};")
endif()
if (WIN32)
set(ADDITIONAL_LIBS "WS2_32.lib;${ADDITIONAL_LIBS}")
endif()


if(NOT TARGET LIBXML::LIBXML)
  add_library(LIBXML::LIBXML UNKNOWN IMPORTED)
  set_target_properties(LIBXML::LIBXML PROPERTIES
    IMPORTED_LINK_INTERFACE_LANGUAGES "C"
    IMPORTED_LOCATION "${LIBXML_LIBRARY}"
    INTERFACE_INCLUDE_DIRECTORIES "${LIBXML_INCLUDE_DIR}"
    INTERFACE_LINK_LIBRARIES "${ADDITIONAL_LIBS}"
  )
endif()
  
include(FindPackageHandleStandardArgs)

find_package_handle_standard_args(
    LIBXML
    VERSION_VAR   LIBXML_VERSION
    REQUIRED_VARS LIBXML_LIBRARY LIBXML_INCLUDE_DIR)

mark_as_advanced(LIBXML_VERSION)
