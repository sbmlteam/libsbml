if (NOT FILENAME OR NOT EXISTS ${FILENAME})
	message(FATAL_ERROR "Please specify the filename to the file to patch")
endif()

file(READ "${FILENAME}" SOURCECODE)

string(REPLACE "'get''get'" "'get','get'" SOURCECODE "${SOURCECODE}" )
string(REPLACE "'get''get'" "'get','get'" SOURCECODE "${SOURCECODE}" )
string(REPLACE "'get''get'" "'get','get'" SOURCECODE "${SOURCECODE}" )

file(WRITE "${FILENAME}" "${SOURCECODE}")
message ("DONE")