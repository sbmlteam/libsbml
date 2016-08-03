if (NOT FILENAME OR NOT EXISTS ${FILENAME})
	message(FATAL_ERROR "Please specify the filename to the file to patch")
endif()

file(READ "${FILENAME}" SOURCECODE)

if (FILENAME  MATCHES "libSBML.R")
  
  
  string(REPLACE "'get''get'" "'get','get'" SOURCECODE "${SOURCECODE}" )
  string(REPLACE "'get''get'" "'get','get'" SOURCECODE "${SOURCECODE}" )
  string(REPLACE "'get''get'" "'get','get'" SOURCECODE "${SOURCECODE}" )
  string(REPLACE "if ( &&" "if (" SOURCECODE "${SOURCECODE}" )
  string(REPLACE "if ()" "if(TRUE)" SOURCECODE "${SOURCECODE}" )
  
  file(WRITE "${FILENAME}" "${SOURCECODE}")
  message (STATUS "Patched libSBML.R")

endif()


if (FILENAME  MATCHES "wrap.cpp")

  string(REPLACE "string.h" "cstring" SOURCECODE "${SOURCECODE}" )
  
  file(WRITE "${FILENAME}" "${SOURCECODE}")
  message (STATUS "Patched libsbml_wrap.cpp")

endif()
