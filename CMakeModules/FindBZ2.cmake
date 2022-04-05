string(TOUPPER ${PROJECT_NAME} _UPPER_PROJECT_NAME)
set(_PROJECT_DEPENDENCY_DIR ${_UPPER_PROJECT_NAME}_DEPENDENCY_DIR)

if (NOT LIBBZ_LIBRARY)
find_library(LIBBZ_LIBRARY
    NAMES bzip2.lib bz2 libbz2.lib
    PATHS /usr/lib /usr/local/lib
          ${CMAKE_OSX_SYSROOT}/usr/lib
          ${${_PROJECT_DEPENDENCY_DIR}}/lib
          NO_DEFAULT_PATH
          NO_CMAKE_ENVIRONMENT_PATH
          NO_CMAKE_PATH
          NO_SYSTEM_ENVIRONMENT_PATH
          NO_CMAKE_SYSTEM_PATH

          DOC "The file name of the bzip2 library."
)
endif()

if (NOT LIBBZ_LIBRARY)
find_library(LIBBZ_LIBRARY
    NAMES bzip2.lib bz2 libbz2.lib
    PATHS /usr/lib /usr/local/lib
          ${CMAKE_OSX_SYSROOT}/usr/lib
          ${${_PROJECT_DEPENDENCY_DIR}}/lib
    DOC "The file name of the bzip2 library."
)
endif()

if (NOT LIBBZ_INCLUDE_DIR)
find_path(LIBBZ_INCLUDE_DIR
    NAMES bzlib.h bzip2/bzlib.h
    PATHS ${CMAKE_OSX_SYSROOT}/usr/include
          /usr/include /usr/local/include
          ${${_PROJECT_DEPENDENCY_DIR}}/include
          NO_DEFAULT_PATH
    DOC "The directory containing the bzip2 include files."
    )
endif()

if (NOT LIBBZ_INCLUDE_DIR)
  find_path(LIBBZ_INCLUDE_DIR
    NAMES bzlib.h bzip2/bzlib.h
    PATHS ${CMAKE_OSX_SYSROOT}/usr/include
          /usr/include /usr/local/include
          ${${_PROJECT_DEPENDENCY_DIR}}/include
    DOC "The directory containing the bzip2 include files."
          )
endif()


if (LIBBZ_LIBRARY)
    # make sure that we have a valid bzip2 library
    file(TO_CMAKE_PATH "${LIBBZ_LIBRARY}" LIBBZ2_CMAKE_PATH)
    check_library_exists("${LIBBZ2_CMAKE_PATH}" "BZ2_bzCompressInit" "" LIBBZ_FOUND_SYMBOL)
    if(NOT LIBBZ_FOUND_SYMBOL)
        # this is odd, but on windows this check always fails! must be a
        # bug in the current cmake version so for now only issue this
        # warning on linux
        if(UNIX)
            message(WARNING
"The chosen bz2 library does not appear to be valid because it is
missing some required symbols. Please check that ${LIBBZ_LIBRARY}
is the bzip2 library. For details about the error, please see
${LIBSBML_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log")
        endif()
    endif()
    if(NOT EXISTS "${LIBBZ_INCLUDE_DIR}/bzlib.h")
        message(FATAL_ERROR
"The include directory specified for the bz2 library does not
appear to be valid.  It should contain the file bzlib.h, but
it does not.")
    endif()
endif()

if (LIBBZ_INCLUDE_DIR AND EXISTS "${LIBBZ_INCLUDE_DIR}/bzlib.h")
    file(STRINGS "${LIBBZ_INCLUDE_DIR}/bzlib.h" BZLIB_H REGEX "bzip2/libbzip2 version [0-9]+\\.[^ ]+ of [0-9]+ ")
    string(REGEX REPLACE ".* bzip2/libbzip2 version ([0-9]+\\.[^ ]+) of [0-9]+ .*" "\\1" LIBBZ_VERSION "${BZLIB_H}")
endif ()

if(NOT TARGET BZ2::BZ2)
  add_library(BZ2::BZ2 UNKNOWN IMPORTED)
  set_target_properties(BZ2::BZ2 PROPERTIES
    IMPORTED_LINK_INTERFACE_LANGUAGES "C"
    IMPORTED_LOCATION "${LIBBZ_LIBRARY}"
    INTERFACE_INCLUDE_DIRECTORIES "${LIBBZ_INCLUDE_DIR}")
endif()


include(FindPackageHandleStandardArgs)

find_package_handle_standard_args(
    BZ2
    VERSION_VAR   LIBBZ_VERSION
    REQUIRED_VARS LIBBZ_LIBRARY LIBBZ_INCLUDE_DIR
)

mark_as_advanced(LIBBZ_VERSION)

