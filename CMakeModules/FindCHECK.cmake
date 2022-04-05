string(TOUPPER ${PROJECT_NAME} _UPPER_PROJECT_NAME)
set(_PROJECT_DEPENDENCY_DIR ${_UPPER_PROJECT_NAME}_DEPENDENCY_DIR)

find_library(LIBCHECK_LIBRARY
        NAMES check libcheck
        PATHS /usr/lib /usr/local/lib ${${_PROJECT_DEPENDENCY_DIR}}/lib
        DOC "The file name of the libcheck library."
)

find_path(LIBCHECK_INCLUDE_DIR
    NAMES  check.h
    PATHS  /usr/include 
           /usr/local/include  
           ${${_PROJECT_DEPENDENCY_DIR}}/include
           ~/Library/Frameworks
           /Library/Frameworks
           /sw/include        # Fink
           /opt/local/include # MacPorts
           /opt/csw/include   # Blastwave
           /opt/include
           /usr/freeware/include
    DOC "The directory containing the libcheck include files."
            )

if (LIBCHECK_INCLUDE_DIR AND EXISTS "${LIBCHECK_INCLUDE_DIR}/check.h")
file(STRINGS "${LIBCHECK_INCLUDE_DIR}/check.h" check_version_str
     REGEX "^#[\t ]*define[\t ]+CHECK_(MAJOR|MINOR|MICRO)_VERSION[\t ]+[(][0-9][)]+$")

unset(LIBCHECK_VERSION)
foreach(VPART MAJOR MINOR MICRO)
    foreach(VLINE ${check_version_str})
        if(VLINE MATCHES "^#[\t ]*define[\t ]+CHECK_${VPART}_VERSION[\t ]+[(]([0-9])[)]+$")
            set(LIBCHECK_VERSION_PART "${CMAKE_MATCH_1}")
            if(LIBCHECK_VERSION OR (LIBCHECK_VERSION STREQUAL "0"))
                string(APPEND LIBCHECK_VERSION ".${LIBCHECK_VERSION_PART}")
            else()
                set(LIBCHECK_VERSION "${LIBCHECK_VERSION_PART}")
            endif()
        endif()
    endforeach()
endforeach()
endif ()

# check that check compiles/links - needs cmake 3+
set(CHECK_ADDITIONAL_LIBS)
if (UNIX AND NOT APPLE)
if(${CMAKE_VERSION} VERSION_GREATER 3.0.0)
set(CHECK_CHECK_CODE
"
#include <check.h>

START_TEST (sanity_check)
{
    fail_unless(5 == 5, \"this should succeed\");
    fail_unless(6 == 5, \"this should fail\");
}
END_TEST

int 
main(void)
{
    Suite *s1 = suite_create(\"Core\");
    TCase *tc1_1 = tcase_create(\"Core\");
    SRunner *sr = srunner_create(s1);
    int nf;

    suite_add_tcase(s1, tc1_1);
    tcase_add_test(tc1_1, sanity_check);

    srunner_run_all(sr, CK_ENV);
    nf = srunner_ntests_failed(sr);
    srunner_free(sr);
    ;
    return nf == 0 ? 0 : 1;
}
" 
)

set(CMAKE_REQUIRED_LIBRARIES_CACHE ${CMAKE_REQUIRED_LIBRARIES})
unset(CHECK_CHECK_TEST CACHE)
unset(CHECK_CHECK_TEST2 CACHE)

set(CHECK_CHECK_TEST)
set(CMAKE_REQUIRED_LIBRARIES "-lcheck -lm ${EXTRA_LIBS}")
CHECK_C_SOURCE_COMPILES("${CHECK_CHECK_CODE}" CHECK_CHECK_TEST)

if (NOT CHECK_CHECK_TEST)
    set(CHECK_CHECK_TEST2)
    set(CMAKE_REQUIRED_LIBRARIES "-lcheck -lm -pthread -lrt -lsubunit ${EXTRA_LIBS}")
    CHECK_C_SOURCE_COMPILES("${CHECK_CHECK_CODE}" CHECK_CHECK_TEST2)
    
    if (CHECK_CHECK_TEST2)
        set(CHECK_TMP_LIBS "-pthread -lrt -lsubunit ${EXTRA_LIBS}")
        string(STRIP "${CHECK_TMP_LIBS}" CHECK_TMP_LIBS)
        set(CHECK_ADDITIONAL_LIBS "${CHECK_TMP_LIBS}")
    else ()
        message(WARNING
"Check cannot compile tests, please specify the correct EXTRA_LIBS for your operating system.")
    endif (CHECK_CHECK_TEST2)
    unset(CHECK_CHECK_TEST2 CACHE)

endif (NOT CHECK_CHECK_TEST)

unset(CHECK_CHECK_TEST CACHE)
set(CMAKE_REQUIRED_LIBRARIES ${CMAKE_REQUIRED_FLAGS_CACHE})

endif(${CMAKE_VERSION} VERSION_GREATER 3.0.0)
endif (UNIX AND NOT APPLE)

# create an LIBCHECK target to link against
if(NOT TARGET CHECK::CHECK)
  add_library(CHECK::CHECK UNKNOWN IMPORTED)
  set_target_properties(CHECK::CHECK PROPERTIES
    IMPORTED_LINK_INTERFACE_LANGUAGES "C"
    IMPORTED_LOCATION "${LIBCHECK_LIBRARY}"
    INTERFACE_INCLUDE_DIRECTORIES "${LIBCHECK_INCLUDE_DIR}"
    INTERFACE_LINK_LIBRARIES "${CHECK_ADDITIONAL_LIBS}"
  )
endif()

include(FindPackageHandleStandardArgs)

find_package_handle_standard_args(
    CHECK
    VERSION_VAR   LIBCHECK_VERSION
    REQUIRED_VARS LIBCHECK_LIBRARY LIBCHECK_INCLUDE_DIR)

mark_as_advanced(LIBCHECK_VERSION)
