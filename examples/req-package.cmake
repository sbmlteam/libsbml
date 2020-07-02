####################################################################
#
# SBML Required Elements package 
#
# $Author Lucian Smith$
# $Id$
# $HeadURL$
#

if (ENABLE_REQUIREDELEMENTS)
# add_subdirectory(c/req)
  add_subdirectory(c++/req)

  if(WITH_CSHARP)
#    add_subdirectory(csharp/req)
  endif(WITH_CSHARP)

  if(WITH_JAVA)
#    add_subdirectory(java/req)
  endif(WITH_JAVA)
  
  file(GLOB c_req_samples "${CMAKE_CURRENT_SOURCE_DIR}/c/req/*.c"
                         "${CMAKE_CURRENT_SOURCE_DIR}/c/req/*.h"
                         "${CMAKE_CURRENT_SOURCE_DIR}/c/req/README.txt")
  install(FILES ${c_req_samples} DESTINATION ${MISC_PREFIX}examples/c/req)

  file(GLOB cpp_req_samples "${CMAKE_CURRENT_SOURCE_DIR}/c++/req/*.c"
                         "${CMAKE_CURRENT_SOURCE_DIR}/c++/req/*.cpp"
                         "${CMAKE_CURRENT_SOURCE_DIR}/c++/req/*.h"
                         "${CMAKE_CURRENT_SOURCE_DIR}/c++/req/README.txt")
  install(FILES ${cpp_req_samples} DESTINATION ${MISC_PREFIX}examples/c++/req)

  if(WITH_PERL)
  # install perl examples
  file(GLOB perl_req_samples "${CMAKE_CURRENT_SOURCE_DIR}/perl/req/*.pl"
                         "${CMAKE_CURRENT_SOURCE_DIR}/perl/req/README.txt")
  install(FILES ${perl_req_samples} DESTINATION ${MISC_PREFIX}examples/perl/req)
  endif()
  
  if (WITH_PYTHON)
  # install python examples
  file(GLOB python_req_samples "${CMAKE_CURRENT_SOURCE_DIR}/python/req/*.py"
                           "${CMAKE_CURRENT_SOURCE_DIR}/python/req/README.txt")
  install(FILES ${python_req_samples} DESTINATION ${MISC_PREFIX}examples/python/req)
  endif()
  
  
  if(WITH_RUBY)
  # install ruby examples
  file(GLOB ruby_req_samples "${CMAKE_CURRENT_SOURCE_DIR}/ruby/req/*.rb"
                         "${CMAKE_CURRENT_SOURCE_DIR}/ruby/req/README.txt")
  install(FILES ${ruby_req_samples} DESTINATION ${MISC_PREFIX}examples/ruby/req)
  endif()
  
  if (WITH_R)
  # install r examples
  file(GLOB r_req_samples "${CMAKE_CURRENT_SOURCE_DIR}/r/req/*.R"
                           "${CMAKE_CURRENT_SOURCE_DIR}/r/req/README.txt")
  install(FILES ${r_req_samples} DESTINATION ${MISC_PREFIX}examples/r/req)
  endif()
  
  
endif(ENABLE_REQUIREDELEMENTS)
