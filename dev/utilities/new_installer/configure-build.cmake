

####################################################################
#
# create correct cache files
# 
set(BASE_DIR "C:/Development/libsbml")
set(BUILD_DIR "${BASE_DIR}/build_32")
set(DEPENDENCY_DIR "${BASE_DIR}/dependencies/")

configure_file (
  "${CMAKE_CURRENT_SOURCE_DIR}/CMakeCache_32.txt.in"
  "${CMAKE_CURRENT_BINARY_DIR}/CMakeCache_32.txt"
  )

set(BUILD_DIR "${BASE_DIR}/build_64")

configure_file (
  "${CMAKE_CURRENT_SOURCE_DIR}/CMakeCache_64.txt.in"
  "${CMAKE_CURRENT_BINARY_DIR}/CMakeCache_64.txt"
  )

  
