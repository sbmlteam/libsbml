####################################################################
#
# SBML Distributions package 
#
# $Author Lucian Smith$
# $Id$
# $HeadURL$
#

if (ENABLE_DISTRIB)
#  add_subdirectory(c/distrib)
add_subdirectory(c++/distrib)

   if(WITH_CSHARP)
#     add_subdirectory(csharp/distrib)
   endif(WITH_CSHARP)

   if(WITH_JAVA)
#     add_subdirectory(java/distrib)
   endif(WITH_JAVA)

  file(GLOB c_distrib_samples "${CMAKE_CURRENT_SOURCE_DIR}/c/distrib/*.c"
                         "${CMAKE_CURRENT_SOURCE_DIR}/c/distrib/*.h"
                         "${CMAKE_CURRENT_SOURCE_DIR}/c/distrib/README.txt")
  install(FILES ${c_distrib_samples} DESTINATION ${MISC_PREFIX}examples/c/distrib)

  file(GLOB cpp_distrib_samples "${CMAKE_CURRENT_SOURCE_DIR}/c++/distrib/*.c"
                         "${CMAKE_CURRENT_SOURCE_DIR}/c++/distrib/*.cpp"
                         "${CMAKE_CURRENT_SOURCE_DIR}/c++/distrib/*.h"
                         "${CMAKE_CURRENT_SOURCE_DIR}/c++/distrib/README.txt")
  install(FILES ${cpp_distrib_samples} DESTINATION ${MISC_PREFIX}examples/c++/distrib)

  if(WITH_PERL)
  # install perl examples
  file(GLOB perl_distrib_samples "${CMAKE_CURRENT_SOURCE_DIR}/perl/distrib/*.pl"
                         "${CMAKE_CURRENT_SOURCE_DIR}/perl/distrib/README.txt")
  install(FILES ${perl_distrib_samples} DESTINATION ${MISC_PREFIX}examples/perl/distrib)
  endif()
  
  if (WITH_PYTHON)
  # install python examples
  file(GLOB python_distrib_samples "${CMAKE_CURRENT_SOURCE_DIR}/python/distrib/*.py"
                           "${CMAKE_CURRENT_SOURCE_DIR}/python/distrib/README.txt")
  install(FILES ${python_distrib_samples} DESTINATION ${MISC_PREFIX}examples/python/distrib)
  endif()
  
  
  if(WITH_RUBY)
  # install ruby examples
  file(GLOB ruby_distrib_samples "${CMAKE_CURRENT_SOURCE_DIR}/ruby/distrib/*.rb"
                         "${CMAKE_CURRENT_SOURCE_DIR}/ruby/distrib/README.txt")
  install(FILES ${ruby_distrib_samples} DESTINATION ${MISC_PREFIX}examples/ruby/distrib)
  endif()
  
  if (WITH_R)
  # install r examples
  file(GLOB r_distrib_samples "${CMAKE_CURRENT_SOURCE_DIR}/r/distrib/*.R"
                           "${CMAKE_CURRENT_SOURCE_DIR}/r/distrib/README.txt")
  install(FILES ${r_distrib_samples} DESTINATION ${MISC_PREFIX}examples/r/distrib)
  endif()
  
   
endif(ENABLE_DISTRIB)
