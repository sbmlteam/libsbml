###############################################################################
#
# Description       : CMake helper saving generated index
# Original author(s): Frank Bergmann <fbergman@caltech.edu>
# Organization      : California Institute of Technology
#

message(VERBOSE "OPERATION: ${OPERATION}")
message(VERBOSE "GENERATOR_DIR: ${GENERATOR_DIR}")
message(VERBOSE "OUTPUT_DIR: ${OUTPUT_DIR}")

if (NOT EXISTS "${GENERATOR_DIR}")
    message(FATAL_ERROR "GENERATOR_DIR must be set")
endif()


if (OPERATION STREQUAL "store")
    file(GLOB INDEX_FILES "${GENERATOR_DIR}/navtreeindex*.js" "${GENERATOR_DIR}/navtree*.js" "${GENERATOR_DIR}/files.js")
    file(MAKE_DIRECTORY "${OUTPUT_DIR}")
    foreach(INDEX_FILE ${INDEX_FILES})
        file(COPY "${INDEX_FILE}" DESTINATION "${OUTPUT_DIR}")
        file(REMOVE "${INDEX_FILE}")
    endforeach()
elseif(OPERATION STREQUAL "restore")
    file(GLOB INDEX_FILES "${OUTPUT_DIR}/*")
    foreach(INDEX_FILE ${INDEX_FILES})
        file(COPY "${INDEX_FILE}" DESTINATION "${GENERATOR_DIR}")
        file(REMOVE "${INDEX_FILE}")
    endforeach()
    file(REMOVE "${OUTPUT_DIR}")
else()
    message(FATAL_ERROR "Unknown operation ${OPERATION}")
endif()