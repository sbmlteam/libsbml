####################################################################
#
# SBML Hierarchical Composition package 
#
# $Author Lucian Smith$
# $Id$
# $HeadURL$
#

if (ENABLE_COMP)
  add_subdirectory(c/comp)
  add_subdirectory(c++/comp)
  
  if(WITH_CSHARP)
    add_subdirectory(csharp/comp)
  endif(WITH_CSHARP)
  
  if(WITH_JAVA)
    add_subdirectory(java/comp)
  endif(WITH_JAVA)
  
  
  if(WITH_PERL)
  # install perl examples
  file(GLOB perl_comp_samples "${CMAKE_CURRENT_SOURCE_DIR}/perl/comp/*.pl"
                         "${CMAKE_CURRENT_SOURCE_DIR}/perl/comp/README.txt")
  install(FILES ${perl_comp_samples} DESTINATION ${MISC_PREFIX}examples/perl/comp)
  endif()
  
  if (WITH_PYTHON)
  # install python examples
  file(GLOB python_comp_samples "${CMAKE_CURRENT_SOURCE_DIR}/python/comp/*.py"
                           "${CMAKE_CURRENT_SOURCE_DIR}/python/comp/README.txt")
  install(FILES ${python_comp_samples} DESTINATION ${MISC_PREFIX}examples/python/comp)
  endif()
  
  
  if(WITH_RUBY)
  # install ruby examples
  file(GLOB perl_comp_samples "${CMAKE_CURRENT_SOURCE_DIR}/ruby/comp/*.rb"
                         "${CMAKE_CURRENT_SOURCE_DIR}/ruby/comp/README.txt")
  install(FILES ${perl_comp_samples} DESTINATION ${MISC_PREFIX}examples/ruby/comp)
  endif()
  
  if (WITH_R)
  # install r examples
  file(GLOB python_comp_samples "${CMAKE_CURRENT_SOURCE_DIR}/r/comp/*.R"
                           "${CMAKE_CURRENT_SOURCE_DIR}/r/comp/README.txt")
  install(FILES ${python_comp_samples} DESTINATION ${MISC_PREFIX}examples/r/comp)
  endif()
  
endif(ENABLE_COMP)
