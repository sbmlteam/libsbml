####################################################################
#
# SBML Layout package 
#
# $Author Lucian Smith$
# $Id$
# $HeadURL$
#

if (ENABLE_LAYOUT)
#  add_subdirectory(layout)
#  add_subdirectory(c/layout)
   add_subdirectory(c++/layout)
   
   if(WITH_CSHARP)
     add_subdirectory(csharp/layout)
   endif(WITH_CSHARP)
   
   if(WITH_JAVA)
     add_subdirectory(java/layout)
   endif(WITH_JAVA)
   
  
  if(WITH_PERL)
  # install perl examples
  file(GLOB perl_layout_samples "${CMAKE_CURRENT_SOURCE_DIR}/perl/layout/*.pl"
                         "${CMAKE_CURRENT_SOURCE_DIR}/perl/layout/README.txt")
  install(FILES ${perl_layout_samples} DESTINATION ${MISC_PREFIX}examples/perl/layout)
  endif()
  
  if (WITH_PYTHON)
  # install python examples
  file(GLOB python_layout_samples "${CMAKE_CURRENT_SOURCE_DIR}/python/layout/*.py"
                           "${CMAKE_CURRENT_SOURCE_DIR}/python/layout/README.txt")
  install(FILES ${python_layout_samples} DESTINATION ${MISC_PREFIX}examples/python/layout)
  endif()
  
  
  if(WITH_RUBY)
  # install ruby examples
  file(GLOB ruby_layout_samples "${CMAKE_CURRENT_SOURCE_DIR}/ruby/layout/*.rb"
                         "${CMAKE_CURRENT_SOURCE_DIR}/ruby/layout/README.txt")
  install(FILES ${ruby_layout_samples} DESTINATION ${MISC_PREFIX}examples/ruby/layout)
  endif()
  
  if (WITH_R)
  # install r examples
  file(GLOB r_layout_samples "${CMAKE_CURRENT_SOURCE_DIR}/r/layout/*.R"
                           "${CMAKE_CURRENT_SOURCE_DIR}/r/layout/README.txt")
  install(FILES ${r_layout_samples} DESTINATION ${MISC_PREFIX}examples/r/layout)
  endif()
   
endif(ENABLE_LAYOUT)
