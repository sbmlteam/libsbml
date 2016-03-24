####################################################################
#
# SBML Groups package 
#
# $Author Lucian Smith$
# $Id$
# $HeadURL$
#

if (ENABLE_GROUPS)
#        add_subdirectory(c/groups)
  add_subdirectory(c++/groups)
  
  if(WITH_CSHARP)
    add_subdirectory(csharp/groups)
  endif(WITH_CSHARP)
  
  if(WITH_JAVA)
    add_subdirectory(java/groups)
  endif(WITH_JAVA)
  
  
  file(GLOB c_groups_samples "${CMAKE_CURRENT_SOURCE_DIR}/c/groups/*.c"
                         "${CMAKE_CURRENT_SOURCE_DIR}/c/groups/*.h"
                         "${CMAKE_CURRENT_SOURCE_DIR}/c/groups/README.txt")
  install(FILES ${c_groups_samples} DESTINATION ${MISC_PREFIX}examples/c/groups)

  file(GLOB cpp_groups_samples "${CMAKE_CURRENT_SOURCE_DIR}/c++/groups/*.c"
                         "${CMAKE_CURRENT_SOURCE_DIR}/c++/groups/*.cpp"
                         "${CMAKE_CURRENT_SOURCE_DIR}/c++/groups/*.h"
                         "${CMAKE_CURRENT_SOURCE_DIR}/c++/groups/README.txt")
  install(FILES ${cpp_groups_samples} DESTINATION ${MISC_PREFIX}examples/c++/groups)

  if(WITH_PERL)
  # install perl examples
  file(GLOB perl_groups_samples "${CMAKE_CURRENT_SOURCE_DIR}/perl/groups/*.pl"
                         "${CMAKE_CURRENT_SOURCE_DIR}/perl/groups/README.txt")
  install(FILES ${perl_groups_samples} DESTINATION ${MISC_PREFIX}examples/perl/groups)
  endif()
  
  if (WITH_PYTHON)
  # install python examples
  file(GLOB python_groups_samples "${CMAKE_CURRENT_SOURCE_DIR}/python/groups/*.py"
                           "${CMAKE_CURRENT_SOURCE_DIR}/python/groups/README.txt")
  install(FILES ${python_groups_samples} DESTINATION ${MISC_PREFIX}examples/python/groups)
  endif()
  
  
  if(WITH_RUBY)
  # install ruby examples
  file(GLOB ruby_groups_samples "${CMAKE_CURRENT_SOURCE_DIR}/ruby/groups/*.rb"
                         "${CMAKE_CURRENT_SOURCE_DIR}/ruby/groups/README.txt")
  install(FILES ${ruby_groups_samples} DESTINATION ${MISC_PREFIX}examples/ruby/groups)
  endif()
  
  if (WITH_R)
  # install r examples
  file(GLOB r_groups_samples "${CMAKE_CURRENT_SOURCE_DIR}/r/groups/*.R"
                           "${CMAKE_CURRENT_SOURCE_DIR}/r/groups/README.txt")
  install(FILES ${r_groups_samples} DESTINATION ${MISC_PREFIX}examples/r/groups)
  endif()
  
endif(ENABLE_GROUPS)
