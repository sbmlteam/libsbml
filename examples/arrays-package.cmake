####################################################################
#
# SBML Arrays package 
#
# $Author Lucian Smith$
# $Id$
# $HeadURL$
#

if (ENABLE_ARRAYS)
#        add_subdirectory(c/arrays)
  add_subdirectory(c++/arrays)

  if(WITH_CSHARP)
    add_subdirectory(csharp/arrays)
  endif(WITH_CSHARP)
  
  if(WITH_JAVA)
    add_subdirectory(java/arrays)
  endif(WITH_JAVA)
  
  file(GLOB c_arrays_samples "${CMAKE_CURRENT_SOURCE_DIR}/c/arrays/*.c"
                         "${CMAKE_CURRENT_SOURCE_DIR}/c/arrays/*.h"
                         "${CMAKE_CURRENT_SOURCE_DIR}/c/arrays/README.txt")
  install(FILES ${c_arrays_samples} DESTINATION ${MISC_PREFIX}examples/c/arrays)

  file(GLOB cpp_arrays_samples "${CMAKE_CURRENT_SOURCE_DIR}/c++/arrays/*.c"
                         "${CMAKE_CURRENT_SOURCE_DIR}/c++/arrays/*.cpp"
                         "${CMAKE_CURRENT_SOURCE_DIR}/c++/arrays/*.h"
                         "${CMAKE_CURRENT_SOURCE_DIR}/c++/arrays/README.txt")
  install(FILES ${cpp_arrays_samples} DESTINATION ${MISC_PREFIX}examples/c++/arrays)

  if(WITH_PERL)
  # install perl examples
  file(GLOB perl_arrays_samples "${CMAKE_CURRENT_SOURCE_DIR}/perl/arrays/*.pl"
                         "${CMAKE_CURRENT_SOURCE_DIR}/perl/arrays/README.txt")
  install(FILES ${perl_arrays_samples} DESTINATION ${MISC_PREFIX}examples/perl/arrays)
  endif()
  
  if (WITH_PYTHON)
  # install python examples
  file(GLOB python_arrays_samples "${CMAKE_CURRENT_SOURCE_DIR}/python/arrays/*.py"
                           "${CMAKE_CURRENT_SOURCE_DIR}/python/arrays/README.txt")
  install(FILES ${python_arrays_samples} DESTINATION ${MISC_PREFIX}examples/python/arrays)
  endif()
  
  
  if(WITH_RUBY)
  # install ruby examples
  file(GLOB ruby_arrays_samples "${CMAKE_CURRENT_SOURCE_DIR}/ruby/arrays/*.rb"
                         "${CMAKE_CURRENT_SOURCE_DIR}/ruby/arrays/README.txt")
  install(FILES ${ruby_arrays_samples} DESTINATION ${MISC_PREFIX}examples/ruby/arrays)
  endif()
  
  if (WITH_R)
  # install r examples
  file(GLOB r_arrays_samples "${CMAKE_CURRENT_SOURCE_DIR}/r/arrays/*.R"
                           "${CMAKE_CURRENT_SOURCE_DIR}/r/arrays/README.txt")
  install(FILES ${r_arrays_samples} DESTINATION ${MISC_PREFIX}examples/r/arrays)
  endif()
  
endif(ENABLE_ARRAYS)
