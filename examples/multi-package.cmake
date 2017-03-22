####################################################################
#
# SBML Multi package 
#
# $Author Lucian Smith$
# $Id$
# $HeadURL$
#

if (ENABLE_MULTI)
  # add_subdirectory(c/fbc)
  add_subdirectory(c++/multi)
  
  if(WITH_CSHARP)
   add_subdirectory(csharp/multi)
  endif(WITH_CSHARP)
  
  if(WITH_JAVA)
   add_subdirectory(java/multi)
  endif(WITH_JAVA)
  
  file(GLOB c_multi_samples "${CMAKE_CURRENT_SOURCE_DIR}/c/multi/*.c"
                         "${CMAKE_CURRENT_SOURCE_DIR}/c/multi/*.h"
                         "${CMAKE_CURRENT_SOURCE_DIR}/c/multi/README.txt")
  install(FILES ${c_multi_samples} DESTINATION ${MISC_PREFIX}examples/c/multi)

  file(GLOB cpp_multi_samples "${CMAKE_CURRENT_SOURCE_DIR}/c++/multi/*.c"
                         "${CMAKE_CURRENT_SOURCE_DIR}/c++/multi/*.cpp"
                         "${CMAKE_CURRENT_SOURCE_DIR}/c++/multi/*.h"
                         "${CMAKE_CURRENT_SOURCE_DIR}/c++/multi/README.txt")
  install(FILES ${cpp_multi_samples} DESTINATION ${MISC_PREFIX}examples/c++/multi)

  if(WITH_PERL)
  # install perl examples
  file(GLOB perl_multi_samples "${CMAKE_CURRENT_SOURCE_DIR}/perl/multi/*.pl"
                         "${CMAKE_CURRENT_SOURCE_DIR}/perl/multi/README.txt")
  install(FILES ${perl_multi_samples} DESTINATION ${MISC_PREFIX}examples/perl/multi)
  endif()
  
  if (WITH_PYTHON)
  # install python examples
  file(GLOB python_multi_samples "${CMAKE_CURRENT_SOURCE_DIR}/python/multi/*.py"
                           "${CMAKE_CURRENT_SOURCE_DIR}/python/multi/README.txt")
  install(FILES ${python_multi_samples} DESTINATION ${MISC_PREFIX}examples/python/multi)
  endif()
  
  
  if(WITH_RUBY)
  # install ruby examples
  file(GLOB ruby_multi_samples "${CMAKE_CURRENT_SOURCE_DIR}/ruby/multi/*.rb"
                         "${CMAKE_CURRENT_SOURCE_DIR}/ruby/multi/README.txt")
  install(FILES ${ruby_multi_samples} DESTINATION ${MISC_PREFIX}examples/ruby/multi)
  endif()
  
  if (WITH_R)
  # install r examples
  file(GLOB r_multi_samples "${CMAKE_CURRENT_SOURCE_DIR}/r/multi/*.R"
                           "${CMAKE_CURRENT_SOURCE_DIR}/r/multi/README.txt")
  install(FILES ${r_multi_samples} DESTINATION ${MISC_PREFIX}examples/r/multi)
  endif()
  
endif(ENABLE_MULTI)
