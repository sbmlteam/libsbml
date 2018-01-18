####################################################################
#
# SBML Rendering package 
#
# $Author Lucian Smith$
# $Id$
# $HeadURL$
#

if (ENABLE_RENDER)
#        add_subdirectory(c/render)
  add_subdirectory(c++/render)

#  if(WITH_CSHARP)
#    add_subdirectory(csharp/render)
#  endif(WITH_CSHARP)

  if(WITH_JAVA)
    add_subdirectory(java/render)
  endif(WITH_JAVA)
  
  file(GLOB c_render_samples "${CMAKE_CURRENT_SOURCE_DIR}/c/render/*.c"
                         "${CMAKE_CURRENT_SOURCE_DIR}/c/render/*.h"
                         "${CMAKE_CURRENT_SOURCE_DIR}/c/render/README.txt")
  install(FILES ${c_render_samples} DESTINATION ${MISC_PREFIX}examples/c/render)

  file(GLOB cpp_render_samples "${CMAKE_CURRENT_SOURCE_DIR}/c++/render/*.c"
                         "${CMAKE_CURRENT_SOURCE_DIR}/c++/render/*.cpp"
                         "${CMAKE_CURRENT_SOURCE_DIR}/c++/render/*.h"
                         "${CMAKE_CURRENT_SOURCE_DIR}/c++/render/README.txt")
  install(FILES ${cpp_render_samples} DESTINATION ${MISC_PREFIX}examples/c++/render)

  if(WITH_PERL)
  # install perl examples
  file(GLOB perl_render_samples "${CMAKE_CURRENT_SOURCE_DIR}/perl/render/*.pl"
                         "${CMAKE_CURRENT_SOURCE_DIR}/perl/render/README.txt")
  install(FILES ${perl_render_samples} DESTINATION ${MISC_PREFIX}examples/perl/render)
  endif()
  
  if (WITH_PYTHON)
  # install python examples
  file(GLOB python_render_samples "${CMAKE_CURRENT_SOURCE_DIR}/python/render/*.py"
                           "${CMAKE_CURRENT_SOURCE_DIR}/python/render/README.txt")
  install(FILES ${python_render_samples} DESTINATION ${MISC_PREFIX}examples/python/render)
  endif()
  
  
  if(WITH_RUBY)
  # install ruby examples
  file(GLOB ruby_render_samples "${CMAKE_CURRENT_SOURCE_DIR}/ruby/render/*.rb"
                         "${CMAKE_CURRENT_SOURCE_DIR}/ruby/render/README.txt")
  install(FILES ${ruby_render_samples} DESTINATION ${MISC_PREFIX}examples/ruby/render)
  endif()
  
  if (WITH_R)
  # install r examples
  file(GLOB r_render_samples "${CMAKE_CURRENT_SOURCE_DIR}/r/render/*.R"
                           "${CMAKE_CURRENT_SOURCE_DIR}/r/render/README.txt")
  install(FILES ${r_render_samples} DESTINATION ${MISC_PREFIX}examples/r/render)
  endif()
  
endif(ENABLE_RENDER)
