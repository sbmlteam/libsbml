####################################################################
#
# SBML Spatial Processes package 
#
# $Author Lucian Smith$
# $Id$
# $HeadURL$
#

if (ENABLE_SPATIAL)
#  add_subdirectory(c/spatial)
  add_subdirectory(c++/spatial)
  
  if(WITH_CSHARP)
#  add_subdirectory(csharp/spatial)
  endif(WITH_CSHARP)
  
  if(WITH_JAVA)
    add_subdirectory(java/spatial)
  endif(WITH_JAVA)
  
  file(GLOB c_spatial_samples "${CMAKE_CURRENT_SOURCE_DIR}/c/spatial/*.c"
                         "${CMAKE_CURRENT_SOURCE_DIR}/c/spatial/*.h"
                         "${CMAKE_CURRENT_SOURCE_DIR}/c/spatial/README.txt")
  install(FILES ${c_spatial_samples} DESTINATION ${MISC_PREFIX}examples/c/spatial)

  file(GLOB cpp_spatial_samples "${CMAKE_CURRENT_SOURCE_DIR}/c++/spatial/*.c"
                         "${CMAKE_CURRENT_SOURCE_DIR}/c++/spatial/*.cpp"
                         "${CMAKE_CURRENT_SOURCE_DIR}/c++/spatial/*.h"
                         "${CMAKE_CURRENT_SOURCE_DIR}/c++/spatial/README.txt")
  install(FILES ${cpp_spatial_samples} DESTINATION ${MISC_PREFIX}examples/c++/spatial)

  if(WITH_PERL)
  # install perl examples
  file(GLOB perl_spatial_samples "${CMAKE_CURRENT_SOURCE_DIR}/perl/spatial/*.pl"
                         "${CMAKE_CURRENT_SOURCE_DIR}/perl/spatial/README.txt")
  install(FILES ${perl_spatial_samples} DESTINATION ${MISC_PREFIX}examples/perl/spatial)
  endif()
  
  if (WITH_PYTHON)
  # install python examples
  file(GLOB python_spatial_samples "${CMAKE_CURRENT_SOURCE_DIR}/python/spatial/*.py"
                           "${CMAKE_CURRENT_SOURCE_DIR}/python/spatial/README.txt")
  install(FILES ${python_spatial_samples} DESTINATION ${MISC_PREFIX}examples/python/spatial)
  endif()
  
  
  if(WITH_RUBY)
  # install ruby examples
  file(GLOB ruby_spatial_samples "${CMAKE_CURRENT_SOURCE_DIR}/ruby/spatial/*.rb"
                         "${CMAKE_CURRENT_SOURCE_DIR}/ruby/spatial/README.txt")
  install(FILES ${ruby_spatial_samples} DESTINATION ${MISC_PREFIX}examples/ruby/spatial)
  endif()
  
  if (WITH_R)
  # install r examples
  file(GLOB r_spatial_samples "${CMAKE_CURRENT_SOURCE_DIR}/r/spatial/*.R"
                           "${CMAKE_CURRENT_SOURCE_DIR}/r/spatial/README.txt")
  install(FILES ${r_spatial_samples} DESTINATION ${MISC_PREFIX}examples/r/spatial)
  endif()
  
endif(ENABLE_SPATIAL)
