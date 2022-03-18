
find_library(LIBCHECK_LIBRARY
        NAMES check libcheck
        PATHS /usr/lib /usr/local/lib ${LIBSBML_DEPENDENCY_DIR}/lib
        DOC "The file name of the libcheck library."
)

find_path(LIBCHECK_INCLUDE_DIR
    NAMES check.h
    PATHS /usr/include /usr/local/include  ${LIBSBML_DEPENDENCY_DIR}/include
    DOC "The directory containing the libcheck include files."
            )

if(NOT EXISTS "${LIBCHECK_INCLUDE_DIR}/check.h")
    message(FATAL_ERROR
"The include directory specified for the 'check' library appears to be
invalid. It should contain the file check.h, but it does not.")
 endif()

# check that check compiles/links - needs cmake 3+
set(CHECK_ADDITIONAL_LIBS)
if (UNIX)
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
endif (UNIX)

# create an expat target to link against
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
    REQUIRED_VARS LIBCHECK_LIBRARY LIBCHECK_INCLUDE_DIR)

mark_as_advanced(EXPAT_VERSION)
      