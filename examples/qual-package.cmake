####################################################################
#
# SBML Qualitative Models package 
#
# $Author Lucian Smith$
# $Id$
# $HeadURL$
#

if (ENABLE_QUAL)
  # add_subdirectory(c/qual)
  add_subdirectory(c++/qual)
  
  if(WITH_CSHARP)
    add_subdirectory(csharp/qual)
  endif(WITH_CSHARP)
  
  if(WITH_JAVA)
    add_subdirectory(java/qual)
  endif(WITH_JAVA)

  if(WITH_PERL)
  # install perl examples
  file(GLOB perl_qual_samples "${CMAKE_CURRENT_SOURCE_DIR}/perl/qual/*.pl"
                         "${CMAKE_CURRENT_SOURCE_DIR}/perl/qual/README.txt")
  install(FILES ${perl_qual_samples} DESTINATION ${MISC_PREFIX}examples/perl/qual)
  endif()
  
  if (WITH_PYTHON)
  # install python examples
  file(GLOB python_qual_samples "${CMAKE_CURRENT_SOURCE_DIR}/python/qual/*.py"
                           "${CMAKE_CURRENT_SOURCE_DIR}/python/qual/README.txt")
  install(FILES ${python_qual_samples} DESTINATION ${MISC_PREFIX}examples/python/qual)
  endif()
  
  
  if(WITH_RUBY)
  # install ruby examples
  file(GLOB perl_qual_samples "${CMAKE_CURRENT_SOURCE_DIR}/ruby/qual/*.rb"
                         "${CMAKE_CURRENT_SOURCE_DIR}/ruby/qual/README.txt")
  install(FILES ${perl_qual_samples} DESTINATION ${MISC_PREFIX}examples/ruby/qual)
  endif()
  
  if (WITH_R)
  # install r examples
  file(GLOB python_qual_samples "${CMAKE_CURRENT_SOURCE_DIR}/r/qual/*.R"
                           "${CMAKE_CURRENT_SOURCE_DIR}/r/qual/README.txt")
  install(FILES ${python_qual_samples} DESTINATION ${MISC_PREFIX}examples/r/qual)
  endif()
  
endif(ENABLE_QUAL)
