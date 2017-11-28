
# checking arguments 
if ("${SRC_DIR}" STREQUAL "")
	
	message ( FATAL_ERROR 
"
Need one argument: 
  SRC_DIR: libsbml directory containing the version file
" 

)
	
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

message(STATUS "Creating package archives for version: ${LIBSBML_DOTTED_VERSION}")

file(GLOB BATCH_FILES ${CMAKE_CURRENT_SOURCE_DIR}/*-zip-*.bat)

foreach(batch ${BATCH_FILES})
message(STATUS "Creating package archive for: ${batch}")
execute_process(COMMAND ${batch} "${LIBSBML_DOTTED_VERSION}")


endforeach()
