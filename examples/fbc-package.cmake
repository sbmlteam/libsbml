####################################################################
#
# SBML Flux Balance Constraints package 
#
# $Author Lucian Smith$
# $Id$
# $HeadURL$
#

if (ENABLE_FBC)
  # add_subdirectory(c/fbc)
  add_subdirectory(c++/fbc)
  
  if(WITH_CSHARP)
    add_subdirectory(csharp/fbc)
  endif(WITH_CSHARP)
  
  if(WITH_JAVA)
    add_subdirectory(java/fbc)
  endif(WITH_JAVA)

  
  if(WITH_PERL)
  # install perl examples
  file(GLOB perl_fbc_samples "${CMAKE_CURRENT_SOURCE_DIR}/perl/fbc/*.pl"
                         "${CMAKE_CURRENT_SOURCE_DIR}/perl/fbc/README.txt")
  install(FILES ${perl_fbc_samples} DESTINATION ${MISC_PREFIX}examples/perl/fbc)
  endif()
  
  if (WITH_PYTHON)
  # install python examples
  file(GLOB python_fbc_samples "${CMAKE_CURRENT_SOURCE_DIR}/python/fbc/*.py"
                           "${CMAKE_CURRENT_SOURCE_DIR}/python/fbc/README.txt")
  install(FILES ${python_fbc_samples} DESTINATION ${MISC_PREFIX}examples/python/fbc)
  endif()
  
  
  if(WITH_RUBY)
  # install ruby examples
  file(GLOB perl_fbc_samples "${CMAKE_CURRENT_SOURCE_DIR}/ruby/fbc/*.rb"
                         "${CMAKE_CURRENT_SOURCE_DIR}/ruby/fbc/README.txt")
  install(FILES ${perl_fbc_samples} DESTINATION ${MISC_PREFIX}examples/ruby/fbc)
  endif()
  
  if (WITH_R)
  # install r examples
  file(GLOB python_fbc_samples "${CMAKE_CURRENT_SOURCE_DIR}/r/fbc/*.R"
                           "${CMAKE_CURRENT_SOURCE_DIR}/r/fbc/README.txt")
  install(FILES ${python_fbc_samples} DESTINATION ${MISC_PREFIX}examples/r/fbc)
  endif()
  
endif(ENABLE_FBC)
