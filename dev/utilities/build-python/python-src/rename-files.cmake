# cmake build script wrapping built libsbml python bindings into conda binaries
#
#
 
if ("${SRC_DIR}" STREQUAL "" OR "${OUT_DIR}" STREQUAL "" )
message(FATAL_ERROR 
"
  Need the following parameters: 
    - SRC_DIR:   the libsbml directory (containing VERSION.txt)
    - OUT_DIR:   the directory where the installers to be 
                 renamed are

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

set (VERSIONS 2.5 2.6 2.7 3.0 3.1 3.2 3.3 3.4 3.5)
foreach(version ${VERSIONS})

  if (EXISTS "${OUT_DIR}/libsbml-${PACKAGE_VERSION}.win32-py${version}.exe")
    file(RENAME 
       "${OUT_DIR}/libsbml-${PACKAGE_VERSION}.win32-py${version}.exe" 
       "${OUT_DIR}/libSBML-${PACKAGE_VERSION}-win-py${version}-x86.exe"
    )
  endif()
  
  if (EXISTS "${OUT_DIR}/libsbml-${PACKAGE_VERSION}.win-amd64-py${version}.exe")
    file(RENAME 
       "${OUT_DIR}/libsbml-${PACKAGE_VERSION}.win-amd64-py${version}.exe" 
       "${OUT_DIR}/libSBML-${PACKAGE_VERSION}-win-py${version}-x64.exe"
    )
  endif()
  
endforeach()
