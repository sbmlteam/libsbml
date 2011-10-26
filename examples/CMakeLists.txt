# build scripts for examples

# only compile examples if specified, but always copy samples
if(WITH_EXAMPLES)

add_subdirectory(c)
add_subdirectory(c++)

if(WITH_CSHARP)
  add_subdirectory(csharp)
endif(WITH_CSHARP)

if(WITH_JAVA)
  add_subdirectory(java)
endif(WITH_JAVA)

if (ENABLE_LAYOUT)
  add_subdirectory(layout)
endif(ENABLE_LAYOUT)


endif(WITH_EXAMPLES)



# the remaining language bindings do not need to be compiled in order to run. 

# Now we just have to select files for installation# 
install(FILES README.txt DESTINATION ${MISC_PREFIX}examples)

# install c examples
file(GLOB c_samples "${CMAKE_CURRENT_SOURCE_DIR}/c/*.c"
					 "${CMAKE_CURRENT_SOURCE_DIR}/c/README.txt"
					 "${CMAKE_CURRENT_SOURCE_DIR}/c/*.h")
install(FILES ${c_samples} DESTINATION ${MISC_PREFIX}examples/c)

# install c++ examples
file(GLOB cpp_samples "${CMAKE_CURRENT_SOURCE_DIR}/c++/*.c"
					 "${CMAKE_CURRENT_SOURCE_DIR}/c++/README.txt"
					 "${CMAKE_CURRENT_SOURCE_DIR}/c++/*.cpp"
					 "${CMAKE_CURRENT_SOURCE_DIR}/c++/*.h")
install(FILES ${cpp_samples} DESTINATION ${MISC_PREFIX}examples/c++)

if(WITH_CSHARP)
# install c# examples
file(GLOB cs_samples "${CMAKE_CURRENT_SOURCE_DIR}/csharp/*.cs"
					 "${CMAKE_CURRENT_SOURCE_DIR}/csharp/README.txt")
install(FILES ${cs_samples} DESTINATION ${MISC_PREFIX}examples/csharp)
endif()

if(WITH_JAVA)
# install java examples
file(GLOB java_samples "${CMAKE_CURRENT_SOURCE_DIR}/java/*.java"
					 "${CMAKE_CURRENT_SOURCE_DIR}/java/README.txt")
install(FILES ${java_samples} DESTINATION ${MISC_PREFIX}examples/java)
endif()

if(WITH_PERL)
# install perl examples
file(GLOB perl_samples "${CMAKE_CURRENT_SOURCE_DIR}/perl/*.pl"
					 "${CMAKE_CURRENT_SOURCE_DIR}/perl/README.txt")
install(FILES ${perl_samples} DESTINATION ${MISC_PREFIX}examples/perl)
endif()

if (WITH_PYTHON)
# install python examples
file(GLOB python_samples "${CMAKE_CURRENT_SOURCE_DIR}/python/*.py"
					 "${CMAKE_CURRENT_SOURCE_DIR}/python/README.txt")
install(FILES ${python_samples} DESTINATION ${MISC_PREFIX}examples/python)
endif()

if (ENABLE_LAYOUT)
# install layout examples
file(GLOB layout_samples "${CMAKE_CURRENT_SOURCE_DIR}/layout/*.cpp"
					 "${CMAKE_CURRENT_SOURCE_DIR}/layout/*.java"
					 "${CMAKE_CURRENT_SOURCE_DIR}/layout/*.xsl"
					 "${CMAKE_CURRENT_SOURCE_DIR}/layout/*.py"
					 )
install(FILES ${layout_samples} DESTINATION ${MISC_PREFIX}examples/layout)
endif()

# install sample-models
file(GLOB l2_samples "${CMAKE_CURRENT_SOURCE_DIR}/sample-models/from-spec/level-2/*.xml")
install(FILES ${l2_samples} DESTINATION ${MISC_PREFIX}examples/sample-models/from-spec/level-2)

file(GLOB l3_samples "${CMAKE_CURRENT_SOURCE_DIR}/sample-models/from-spec/level-3/*.xml")
install(FILES ${l3_samples} DESTINATION ${MISC_PREFIX}examples/sample-models/from-spec/level-3)

