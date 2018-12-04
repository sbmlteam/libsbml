###############################################################################
#
# Description       : CMake build script for libSBML Python bindings
# Original author(s): Frank Bergmann <fbergman@caltech.edu>
# Organization      : California Institute of Technology
#
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
#
# Copyright (C) 2013-2018 jointly by the following organizations:
#     1. California Institute of Technology, Pasadena, CA, USA
#     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
#     3. University of Heidelberg, Heidelberg, Germany
#
# Copyright (C) 2009-2013 jointly by the following organizations: 
#     1. California Institute of Technology, Pasadena, CA, USA
#     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
#  
# Copyright (C) 2006-2008 by the California Institute of Technology,
#     Pasadena, CA, USA 
#  
# Copyright (C) 2002-2005 jointly by the following organizations: 
#     1. California Institute of Technology, Pasadena, CA, USA
#     2. Japan Science and Technology Agency, Japan
# 
# This library is free software; you can redistribute it and/or modify it
# under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation.  A copy of the license agreement is provided
# in the file named "LICENSE.txt" included with this software distribution
# and also available online as http://sbml.org/software/libsbml/license.html
#
###############################################################################

find_package(SWIG REQUIRED)
include(${SWIG_USE_FILE})
find_package(PythonInterp)

# specify that the same python library should be found 
# as the one the selected interpreter uses
set (Python_ADDITIONAL_VERSIONS ${PYTHON_VERSION_STRING})
find_package(PythonLibs)


####################################################################
#
# determine local dependencies, so as to re-swig if one of them changed
# 

file(GLOB SWIG_DEPENDENCIES 
    ${CMAKE_CURRENT_SOURCE_DIR}/*.i 
    ${CMAKE_CURRENT_SOURCE_DIR}/*.h 
    ${CMAKE_CURRENT_SOURCE_DIR}/*.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/*.cmake
    ${CMAKE_CURRENT_SOURCE_DIR}/../swig/*.i
    ${CMAKE_CURRENT_SOURCE_DIR}/../swig/*.h
    )


####################################################################
#
# generate files that include all packages: 
#


# - local-packages.i
file(GLOB DOWNCAST_EXTENSION RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "${CMAKE_CURRENT_SOURCE_DIR}/local-*.i")
list(REMOVE_ITEM DOWNCAST_EXTENSION "local-packages.i")
# - local-packages.i
update_ifile(
    "local-packages.i" 
    ${CMAKE_CURRENT_BINARY_DIR} 
    "${DOWNCAST_EXTENSION}"
)

# - local-downcast.cpp
file(GLOB DOWNCAST_EXTENSION RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "${CMAKE_CURRENT_SOURCE_DIR}/local-downcast-packages-*.cpp")
update_cfile(
    "local-downcast.cpp" 
    ${CMAKE_CURRENT_BINARY_DIR} 
    "${DOWNCAST_EXTENSION}"
)

# - local-downcast-extension.cpp
file(GLOB DOWNCAST_EXTENSION RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "${CMAKE_CURRENT_SOURCE_DIR}/local-downcast-extension-*.cpp")
update_cfile(
    "local-downcast-extension.cpp" 
    ${CMAKE_CURRENT_BINARY_DIR} 
    "${DOWNCAST_EXTENSION}" 
)

# - local-downcast-namespaces.cpp
file(GLOB DOWNCAST_EXTENSION RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "${CMAKE_CURRENT_SOURCE_DIR}/local-downcast-namespaces-*.cpp")
update_cfile(
    "local-downcast-namespaces.cpp" 
    ${CMAKE_CURRENT_BINARY_DIR} 
    "${DOWNCAST_EXTENSION}"
)

# - local-downcast-plugins.cpp
file(GLOB DOWNCAST_EXTENSION RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "${CMAKE_CURRENT_SOURCE_DIR}/local-downcast-plugins-*.cpp")
update_cfile(
    "local-downcast-plugins.cpp" 
    ${CMAKE_CURRENT_BINARY_DIR} 
    "${DOWNCAST_EXTENSION}"
)

# - local-downcast-plugins.cpp
file(GLOB DOWNCAST_EXTENSION RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "${CMAKE_CURRENT_SOURCE_DIR}/local-downcast-astplugins-*.cpp")
update_cfile(
    "local-downcast-astplugins.cpp" 
    ${CMAKE_CURRENT_BINARY_DIR} 
    "${DOWNCAST_EXTENSION}"
)

# - local-downcast-converters.cpp
file(GLOB DOWNCAST_EXTENSION RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "${CMAKE_CURRENT_SOURCE_DIR}/local-downcast-converters-*.cpp")
update_cfile(
    "local-downcast-converters.cpp" 
    ${CMAKE_CURRENT_BINARY_DIR} 
    "${DOWNCAST_EXTENSION}"
)

#
# Remove SWIG wrappers if requested
#
if (LIBSBML_REMOVE_WRAPPERS)
    foreach(file 
        ${CMAKE_CURRENT_BINARY_DIR}/libsbml_wrap.cpp
        ${CMAKE_CURRENT_BINARY_DIR}/pydoc-normal.i
        ${CMAKE_CURRENT_BINARY_DIR}/pydoc-doxygen.i
    )
        if (EXISTS ${file})
            FILE(REMOVE ${file})
        endif()
    endforeach()
endif(LIBSBML_REMOVE_WRAPPERS)

# the Python bindings will only build if some files are generated, 
# for those files to be generated the following python scripts are 
# needed
file(COPY 
    ${CMAKE_CURRENT_SOURCE_DIR}/../swig/libsbmlutils.py  
    "${CMAKE_CURRENT_SOURCE_DIR}/../../../docs/src/utilities/generate-class-name-list.py"
    "${CMAKE_CURRENT_SOURCE_DIR}/../../../docs/src/utilities/generate-converters-list.py"
    DESTINATION ${CMAKE_CURRENT_BINARY_DIR})


# Normally in Python 2 only ascii strings are accepted as input to 
# libSBML functions SWIG >= 3.0.8 allows one to change this behaviour
# such that both ascii and utf-8 strings can be input. This enables
# this behaviour or prints a warning if Swig version is too old.
if (SWIG_FOUND AND PYTHON_VERSION_STRING VERSION_LESS 3.0)
  if (SWIG_VERSION VERSION_GREATER "3.0.7")
    list(APPEND SWIG_EXTRA_ARGS -DUSE_SWIG_PYTHON_2_UNICODE)
    # message(STATUS SWIG_VERSION "${SWIG_VERSION}")
  else()
    message(WARNING 
"Python ${PYTHON_VERSION_STRING} detected and SWIG version ${SWIG_VERSION}
 < 3.0.8, I cannot enable SWIG_PYTHON_2_UNICODE libSBML Python 
 bindings only accept strings as arugments. Please upgrade Swig to a
 version greater than 3.0.8 to enable unicode input in Python 2. See 
 http://www.swig.org/Doc3.0/Python.html for more details.)")
  endif()
endif()

####################################################################
#
# Swig bindings
#
#
add_custom_command(  

    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/libsbml_wrap.cpp
        ${CMAKE_CURRENT_BINARY_DIR}/libsbml.py
        ${CMAKE_CURRENT_BINARY_DIR}/class-list.txt
        ${CMAKE_CURRENT_BINARY_DIR}/libsbml-converters.txt

    COMMAND "${PYTHON_EXECUTABLE}"
    ARGS  "${CMAKE_CURRENT_SOURCE_DIR}/../swig/swigdoc.py"
        --language python
        --top "${LIBSBML_ROOT_SOURCE_DIR}"
        --master "${CMAKE_CURRENT_SOURCE_DIR}/../swig/libsbml.i"
        --output "${CMAKE_CURRENT_BINARY_DIR}/pydoc-doxygen.i"
        ${SWIG_SWIGDOCDEFINES}

    COMMAND "${PYTHON_EXECUTABLE}"
    ARGS  "${CMAKE_CURRENT_BINARY_DIR}/generate-class-name-list.py"
        "${LIBSBML_ROOT_SOURCE_DIR}/src"
        >
        "${CMAKE_CURRENT_BINARY_DIR}/class-list.txt"

    COMMAND "${PYTHON_EXECUTABLE}"
    ARGS  "${CMAKE_CURRENT_BINARY_DIR}/generate-converters-list.py"  
        "${CMAKE_CURRENT_BINARY_DIR}/class-list.txt"
        >
        "${CMAKE_CURRENT_BINARY_DIR}/libsbml-converters.txt"

    COMMAND "${PYTHON_EXECUTABLE}"
    ARGS  "${CMAKE_CURRENT_SOURCE_DIR}/doc-converter/rewrite_pydoc.py"
        -f "${CMAKE_CURRENT_BINARY_DIR}/pydoc-doxygen.i"
        -o "${CMAKE_CURRENT_BINARY_DIR}/pydoc-normal.i"
        -i ${CMAKE_CURRENT_SOURCE_DIR}/../../../docs/src/common-text
        -g ${CMAKE_CURRENT_SOURCE_DIR}/../../../docs/src/common-graphics

    COMMAND "${CMAKE_COMMAND}"
    ARGS  -E rename pydoc-doxygen.i pydoc.i

    COMMAND "${SWIG_EXECUTABLE}"
    ARGS  -I${CMAKE_CURRENT_SOURCE_DIR}/../swig/
        -I${CMAKE_CURRENT_SOURCE_DIR} 
        -I${CMAKE_CURRENT_BINARY_DIR} 
        -I${LIBSBML_ROOT_BINARY_DIR}/src
        -I${LIBSBML_ROOT_SOURCE_DIR}/src
        -I${LIBSBML_ROOT_SOURCE_DIR}/include
        -DGENERATING_DOCS
        -c++
        -python    
        ${SWIG_EXTRA_ARGS}     
        -o ${CMAKE_CURRENT_BINARY_DIR}/libsbml_wrap.cpp 
        ${CMAKE_CURRENT_SOURCE_DIR}/libsbml.i

    COMMAND "${CMAKE_COMMAND}"
    ARGS  -E rename pydoc.i pydoc-doxygen.i

    COMMAND "${CMAKE_COMMAND}"
    ARGS  -E rename libsbml.py libsbml-doxygen.py

    COMMAND "${CMAKE_COMMAND}"
    ARGS  -E rename pydoc-normal.i pydoc.i

    COMMAND "${SWIG_EXECUTABLE}"
    ARGS  -I${CMAKE_CURRENT_SOURCE_DIR}/../swig/
        -I${CMAKE_CURRENT_SOURCE_DIR} 
        -I${CMAKE_CURRENT_BINARY_DIR} 
        -I${LIBSBML_ROOT_BINARY_DIR}/src
        -I${LIBSBML_ROOT_SOURCE_DIR}/src
        -I${LIBSBML_ROOT_SOURCE_DIR}/include
        -c++
        -python    
        ${SWIG_EXTRA_ARGS}     
        -o ${CMAKE_CURRENT_BINARY_DIR}/libsbml_wrap.cpp 
        ${CMAKE_CURRENT_SOURCE_DIR}/libsbml.i

    COMMAND "${CMAKE_COMMAND}"
    ARGS  -E rename pydoc.i pydoc-normal.i 

    COMMAND "${CMAKE_COMMAND}"
    ARGS -DCUR_BIN_DIRECTORY=\"${CMAKE_CURRENT_BINARY_DIR}\"
         -DVERSION=\"${LIBSBML_DOTTED_VERSION}\"
         -P "${CMAKE_CURRENT_SOURCE_DIR}/add_version.cmake"

    COMMAND "${CMAKE_COMMAND}"
    ARGS -DBIN_DIRECTORY=\"${CMAKE_CURRENT_BINARY_DIR}\"
         -DSRC_DIRECTORY=\"${CMAKE_CURRENT_SOURCE_DIR}\"
         -DPYTHON_USE_API2_WARNINGS=\"${PYTHON_USE_API2_WARNINGS}\"
         -P "${CMAKE_CURRENT_SOURCE_DIR}/patch-python.cmake"

    MAIN_DEPENDENCY ${CMAKE_CURRENT_SOURCE_DIR}/libsbml.i
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/CMakeLists.txt
        ${CMAKE_CURRENT_SOURCE_DIR}/doc-converter/rewrite_pydoc.py
        ${CMAKE_CURRENT_SOURCE_DIR}/../swig/swigdoc.py
        ${SWIG_DEPENDENCIES} ${LIBSBML_HEADER_FILES}

    COMMENT "Swig Python source") 

add_custom_target(binding_python_swig ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/libsbml_wrap.cpp )

####################################################################
#
# Build native library
#


include_directories(${CMAKE_CURRENT_SOURCE_DIR})
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../swig)
include_directories(${LIBSBML_ROOT_SOURCE_DIR}/include)
include_directories(${PYTHON_INCLUDE_DIRS})
include_directories(BEFORE ${LIBSBML_ROOT_BINARY_DIR}/src)
include_directories(BEFORE ${CMAKE_CURRENT_BINARY_DIR})

if (EXTRA_INCLUDE_DIRS) 
    include_directories(${EXTRA_INCLUDE_DIRS})
endif(EXTRA_INCLUDE_DIRS)

if (MSVC)
    # the build fails when compiled with packages as the object file is too 
    # big adding the big flag makes it work!
    add_definitions(/bigobj)
endif(MSVC)

add_library(binding_python_lib SHARED libsbml_wrap.cpp)
add_dependencies( binding_python_lib binding_python_swig) 

set_target_properties (binding_python_lib PROPERTIES OUTPUT_NAME "_libsbml")
if (UNIX)
    set_target_properties (binding_python_lib PROPERTIES PREFIX "")
    set_target_properties (binding_python_lib PROPERTIES SUFFIX ".so")
else()    
    if (CYGWIN)
        set_target_properties (binding_python_lib PROPERTIES PREFIX "")
        set_target_properties (binding_python_lib PROPERTIES SUFFIX ".dll")
    else()
        set_target_properties (binding_python_lib PROPERTIES SUFFIX ".pyd")  
    endif()
endif()

if(APPLE OR UNIX)
    option (PYTHON_USE_DYNAMIC_LOOKUP
    "Do not actually link against the python library, instead use the undefined lookup mechanism, that ." OFF)
    mark_as_advanced(PYTHON_USE_DYNAMIC_LOOKUP)
endif(APPLE OR UNIX)

if (PYTHON_USE_DYNAMIC_LOOKUP)
    if (APPLE)
        set_target_properties (binding_python_lib PROPERTIES LINK_FLAGS "-undefined dynamic_lookup")
    endif()
    target_link_libraries(binding_python_lib ${LIBSBML_LIBRARY}-static)
else()                                          
    target_link_libraries(binding_python_lib ${LIBSBML_LIBRARY}-static ${PYTHON_LIBRARIES})
endif()


# 
# Determine the python installation directory
#
set(PYTHON_PACKAGE_INSTALL_DIR)
if (UNIX OR CYGWIN) 
    execute_process(COMMAND "${PYTHON_EXECUTABLE}" -c "import sys;import platform; sys.stdout.write(platform.python_version()[:3])"
        OUTPUT_VARIABLE PYTHON_VERSION)
    set(PYTHON_PACKAGE_INSTALL_DIR ${CMAKE_INSTALL_LIBDIR}/python${PYTHON_VERSION}/site-packages)
else()
    set(PYTHON_PACKAGE_INSTALL_DIR ${MISC_PREFIX}bindings/python)
endif()

####################################################################
#
# install using the configured install prefix
#

option (PYTHON_INSTALL_IN_PREFIX
    "Install python binaries using the specified install prefix." ON)


if (PYTHON_INSTALL_IN_PREFIX)
    install(TARGETS binding_python_lib DESTINATION ${PYTHON_PACKAGE_INSTALL_DIR}/libsbml )

    file(WRITE "${CMAKE_CURRENT_BINARY_DIR}/libsbml.pth" "libsbml\n")
    install(FILES ${CMAKE_CURRENT_BINARY_DIR}/libsbml.pth  DESTINATION ${PYTHON_PACKAGE_INSTALL_DIR})
    install(FILES ${CMAKE_CURRENT_BINARY_DIR}/libsbml.py  DESTINATION ${PYTHON_PACKAGE_INSTALL_DIR}/libsbml )
    
endif(PYTHON_INSTALL_IN_PREFIX)

####################################################################
#
# install using setup.py
#

option (PYTHON_INSTALL_WITH_SETUP
    "Use setup.py to install the compiled python binaries (be sure to install with the appropriate privileges)." OFF)
if (PYTHON_INSTALL_WITH_SETUP)

    # adjust the setup.py script 
    configure_file(${CMAKE_CURRENT_SOURCE_DIR}/setup.py.cmake ${CMAKE_CURRENT_BINARY_DIR}/setup.py)

    # execute the python interpreter
        INSTALL(TARGETS binding_python_lib DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/libsbml)
        install(FILES ${CMAKE_CURRENT_BINARY_DIR}/libsbml.py  DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/libsbml RENAME __init__.py )
        INSTALL(CODE "execute_process(COMMAND \"${CMAKE_COMMAND}\" -E chdir ${CMAKE_CURRENT_BINARY_DIR} ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/setup.py install)")

endif(PYTHON_INSTALL_WITH_SETUP)

####################################################################
#
# generate source package
#
option(WITH_CREATE_PYTHON_SOURCE   "Generate Python Source Package." OFF)
if (WITH_CREATE_PYTHON_SOURCE AND EXISTS "${LIBSBML_ROOT_SOURCE_DIR}/dev/utilities/python_package/")

SET(PYTHON_SOURCE_OUT_DIR ${CMAKE_CURRENT_BINARY_DIR}/out)
file(TO_NATIVE_PATH ${PYTHON_SOURCE_OUT_DIR} NATIVE_OUT)

add_custom_command(
    OUTPUT ${PYTHON_SOURCE_OUT_DIR}/setup.py

    COMMAND "${CMAKE_COMMAND}" 
    ARGS -E make_directory ${PYTHON_SOURCE_OUT_DIR}

    COMMAND "${CMAKE_COMMAND}"
    ARGS -DOUT_DIR=\"${NATIVE_OUT}\" 
        -DSRC_DIR=\"${LIBSBML_ROOT_SOURCE_DIR}/src\"
        -DBIN_DIR=\"${LIBSBML_ROOT_BINARY_DIR}\"
        -P "${LIBSBML_ROOT_SOURCE_DIR}/dev/utilities/python_package/create_package.cmake"

    DEPENDS ${LIBSBML_ROOT_SOURCE_DIR}/VERSION.txt

    WORKING_DIRECTORY "${LIBSBML_ROOT_SOURCE_DIR}/dev/utilities/python_package/"

    COMMENT "Configure Python source package"
)

add_custom_target(binding_python_source_config ALL DEPENDS ${PYTHON_SOURCE_OUT_DIR}/setup.py)
add_dependencies(binding_python_source_config binding_python_swig binding_python_lib)
add_custom_command(
    OUTPUT ${PYTHON_SOURCE_OUT_DIR}/MANIFEST

    COMMAND "${PYTHON_EXECUTABLE}"
    ARGS setup.py 
        sdist

    DEPENDS ${PYTHON_SOURCE_OUT_DIR}/setup.py
        ${LIBSBML_ROOT_SOURCE_DIR}/dev/utilities/python_package/create_package.cmake

    WORKING_DIRECTORY "${PYTHON_SOURCE_OUT_DIR}"

    COMMENT "Create Python source package"
)

add_custom_target(binding_python_source_package ALL DEPENDS ${PYTHON_SOURCE_OUT_DIR}/MANIFEST)
add_dependencies(binding_python_source_package binding_python_source_config)
add_dependencies( binding_python_source_package binding_python_swig) 


endif(WITH_CREATE_PYTHON_SOURCE AND EXISTS "${LIBSBML_ROOT_SOURCE_DIR}/dev/utilities/python_package/")


####################################################################
#
# testing
#

if(WITH_CHECK)

    # copy test files to build dir
    file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/test.py DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

    # run python tests
    add_test(NAME test_python_binding 
        COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/test.py
        -p $<TARGET_FILE_DIR:binding_python_lib> -b ${CMAKE_CURRENT_SOURCE_DIR}/test)

    # adjust python path so that examples can be run            
    macro(ADJUST_PYTHONPATH target)

    if (UNIX)
        set_tests_properties(${target} PROPERTIES ENVIRONMENT PYTHONPATH=${CMAKE_CURRENT_BINARY_DIR}:$<TARGET_FILE_DIR:binding_python_lib>:$ENV{PYTHONPATH})
    else()
        # need to creatively replace the semicolons - this wasn't working (bgoli)
        SET (NEW_PYTHON_PATH "${CMAKE_CURRENT_BINARY_DIR}\\;$<TARGET_FILE_DIR:binding_python_lib>\\;$ENV{PYTHONPATH}")
        STRING(REPLACE "\\:" ";" NEW_PYTHON_PATH "${NEW_PYTHON_PATH}")
        STRING(REPLACE ";" "\\;" NEW_PYTHON_PATH "${NEW_PYTHON_PATH}")
        set_tests_properties(${target} PROPERTIES ENVIRONMENT PYTHONPATH=${NEW_PYTHON_PATH})
    endif()

    endmacro()

    # test that the bindings are returning the correct version

    if (CMAKE_VERSION VERSION_GREATER 3.0)
        add_test(NAME test_python_printversion 
            COMMAND ${PYTHON_EXECUTABLE} -c "import libsbml; assert libsbml.getLibSBMLDottedVersion() == '${PACKAGE_VERSION}'"
            -p $<TARGET_FILE_DIR:binding_python_lib> -b ${CMAKE_CURRENT_SOURCE_DIR}/test)
        ADJUST_PYTHONPATH(test_python_printversion)
    endif()
    
    # run examples as tests

    if (CMAKE_VERSION VERSION_LESS 3.0)
        message(STATUS "Python example tests check version: ${CMAKE_VERSION} < 3.0, skipping.")
    else()
        add_test(NAME test_python_addCVTerms
            COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_SOURCE_DIR}/examples/python/addCVTerms.py
            ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
            addCVTerms.out.xml
        )
        ADJUST_PYTHONPATH(test_python_addCVTerms)
    
        add_test(NAME test_python_addingEvidenceCodes_1
            COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_SOURCE_DIR}/examples/python/addingEvidenceCodes_1.py
            ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
            addingEvidenceCodes_1.out.xml
        )
        ADJUST_PYTHONPATH(test_python_addingEvidenceCodes_1)    
    
        add_test(NAME test_python_addingEvidenceCodes_2
            COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_SOURCE_DIR}/examples/python/addingEvidenceCodes_2.py
            ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
            addingEvidenceCodes_2.out.xml
        )
        ADJUST_PYTHONPATH(test_python_addingEvidenceCodes_2)
    
        add_test(NAME test_python_addModelHistory
            COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_SOURCE_DIR}/examples/python/addModelHistory.py
            ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
            addModelHistory.out.xml
        )
        ADJUST_PYTHONPATH(test_python_addModelHistory)     
    
        add_test(NAME test_python_append_to_note
            COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_SOURCE_DIR}/examples/python/append_to_note.py
            ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekineticext.xml
            veq
            "a new note"
            append_to_note.out.xml
        )
        ADJUST_PYTHONPATH(test_python_append_to_note)      
    
        add_test(NAME test_python_appendAnnotation
            COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_SOURCE_DIR}/examples/python/appendAnnotation.py
            ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
            appendAnnotation.out.xml
        )
        ADJUST_PYTHONPATH(test_python_appendAnnotation)      

        add_test(NAME test_python_convertSBML
            COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_SOURCE_DIR}/examples/python/convertSBML.py
            ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
            convertSBML.out.xml
        )
        ADJUST_PYTHONPATH(test_python_convertSBML)      
    
        add_test(NAME test_python_createExampleSBML
            COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_SOURCE_DIR}/examples/python/createExampleSBML.py
            ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
            createExampleSBMLy.out.xml
        )
        ADJUST_PYTHONPATH(test_python_createExampleSBML)      

        add_test(NAME test_python_createSimpleModel
            COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_SOURCE_DIR}/examples/python/createSimpleModel.py
            ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
            createSimpleModel.out.xml
        )
        ADJUST_PYTHONPATH(test_python_createSimpleModel)      
    
        add_test(NAME test_python_echoSBML
            COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_SOURCE_DIR}/examples/python/echoSBML.py
            ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
            echoSBML.out.xml
        )
        ADJUST_PYTHONPATH(test_python_echoSBML)      
    
        add_test(NAME test_python_evaluateCustomMath
            COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_SOURCE_DIR}/examples/python/evaluateCustomMath.py
            "2*3+4"
            ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
        )
        ADJUST_PYTHONPATH(test_python_evaluateCustomMath)
    
        add_test(NAME test_python_getAllElementsWithNotes
            COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_SOURCE_DIR}/examples/python/getAllElementsWithNotes.py
            ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
        )
        ADJUST_PYTHONPATH(test_python_getAllElementsWithNotes)    
    
        add_test(NAME test_python_inlineInitialAssignments
            COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_SOURCE_DIR}/examples/python/inlineInitialAssignments.py
            ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
            inlineInitialAssignments.out.xml
        )
        ADJUST_PYTHONPATH(test_python_inlineInitialAssignments)
    
        add_test(NAME test_python_printAnnotation
            COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_SOURCE_DIR}/examples/python/printAnnotation.py
            ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
        )
        ADJUST_PYTHONPATH(test_python_printAnnotation)    
    
        add_test(NAME test_python_printMath
            COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_SOURCE_DIR}/examples/python/printMath.py
            ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
        )
        ADJUST_PYTHONPATH(test_python_printMath)
    
        add_test(NAME test_python_printRegisteredPackages
            COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_SOURCE_DIR}/examples/python/printRegisteredPackages.py
            ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
            printRegisteredPackages.out.xml
        )
        ADJUST_PYTHONPATH(test_python_printRegisteredPackages)    
    
        add_test(NAME test_python_printSBML
            COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_SOURCE_DIR}/examples/python/printSBML.py
            ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
        )
        ADJUST_PYTHONPATH(test_python_printSBML)
    
        add_test(NAME test_python_printsupported
            COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_SOURCE_DIR}/examples/python/printsupported.py
        )
        ADJUST_PYTHONPATH(test_python_printsupported)    
    
        add_test(NAME test_python_printUnits
            COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_SOURCE_DIR}/examples/python/printUnits.py
            ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
        )
        ADJUST_PYTHONPATH(test_python_printUnits)
    
        add_test(NAME test_python_promoteParameters
            COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_SOURCE_DIR}/examples/python/promoteParameters.py
            ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
            promoteParameters.out.xml
        )
        ADJUST_PYTHONPATH(test_python_promoteParameters)    
    
        add_test(NAME test_python_readSBML
            COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_SOURCE_DIR}/examples/python/readSBML.py
            ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
        )
        ADJUST_PYTHONPATH(test_python_readSBML)    
        
        add_test(NAME test_python_replaceOneFD
            COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_SOURCE_DIR}/examples/python/replaceOneFD.py
            ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/functiondef.xml
            f
            reaction_1
            replaceOneFD.out.xml
        )
        ADJUST_PYTHONPATH(test_python_replaceOneFD)    
        
        add_test(NAME test_python_setIdFromNames
            COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_SOURCE_DIR}/examples/python/setIdFromNames.py
            ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
            setIdFromNames.out.xml
        )
        ADJUST_PYTHONPATH(test_python_setIdFromNames)    
        
        add_test(NAME test_python_unsetAnnotation
            COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_SOURCE_DIR}/examples/python/unsetAnnotation.py
            ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
            unsetAnnotation.out.xml
        )
        ADJUST_PYTHONPATH(test_python_unsetAnnotation)    
        
        add_test(NAME test_python_unsetNotes
            COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_SOURCE_DIR}/examples/python/unsetNotes.py
            ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
            unsetNotes.out.xml
        )
        ADJUST_PYTHONPATH(test_python_unsetNotes)    
        
        add_test(NAME test_python_validateSBML
            COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_SOURCE_DIR}/examples/python/validateSBML.py
            ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
        )
        ADJUST_PYTHONPATH(test_python_validateSBML)    

        # Enable only when swig is the correct version

        if (SWIG_VERSION VERSION_GREATER "3.0.7")
         add_test(NAME test_python_stringInput
             COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_SOURCE_DIR}/examples/python/stringInput.py
             ${CMAKE_SOURCE_DIR}/examples/sample-models/from-spec/level-3/enzymekinetics.xml
         )
         ADJUST_PYTHONPATH(test_python_stringInput)
        endif()

        # package specific tests

        # if(ENABLE_COMP)
        # add Python versions of C++ examples that do not require input
        # endif(ENABLE_COMP)

        if(ENABLE_FBC) 
        
          add_test(NAME test_python_stripPackage
              COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_SOURCE_DIR}/examples/python/stripPackage.py
              ${CMAKE_SOURCE_DIR}/examples/sample-models/fbc/fbc_example1.xml
              fbc
              stripPackage.out.xml
          )
          ADJUST_PYTHONPATH(test_python_stripPackage)

          add_test(NAME test_python_fbc_fbc_example1
              COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_SOURCE_DIR}/examples/python/fbc/fbc_example1.py
          )
          ADJUST_PYTHONPATH(test_python_fbc_fbc_example1)    

        endif(ENABLE_FBC)

        if(ENABLE_GROUPS) 
        
          add_test(NAME test_python_groups_groups_example1
              COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_SOURCE_DIR}/examples/python/groups/groups_example1.py
          )
          ADJUST_PYTHONPATH(test_python_groups_groups_example1)    

        endif(ENABLE_GROUPS)

        if(ENABLE_LAYOUT)

          add_test(NAME test_python_layout_example1_L3
              COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_SOURCE_DIR}/examples/python/layout/example1-L3.py
          )
          ADJUST_PYTHONPATH(test_python_layout_example1_L3)    

          add_test(NAME test_python_layout_example1
              COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_SOURCE_DIR}/examples/python/layout/example1.py
          )
          ADJUST_PYTHONPATH(test_python_layout_example1)    

        endif(ENABLE_LAYOUT)

        if(ENABLE_MULTI)

          add_test(NAME test_python_multi_multi_example1
              COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_SOURCE_DIR}/examples/python/multi/multi_example1.py
          )
          ADJUST_PYTHONPATH(test_python_multi_multi_example1)    

          add_test(NAME test_python_multi_multi_example2
              COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_SOURCE_DIR}/examples/python/multi/multi_example2.py
          )
          ADJUST_PYTHONPATH(test_python_multi_multi_example2)    

          add_test(NAME test_python_multi_multi_example3
              COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_SOURCE_DIR}/examples/python/multi/multi_example3.py
          )
          ADJUST_PYTHONPATH(test_python_multi_multi_example3)    

          add_test(NAME test_python_multi_printMulti
              COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_SOURCE_DIR}/examples/python/multi/printMulti.py
          )
          ADJUST_PYTHONPATH(test_python_multi_printMulti)    

        endif(ENABLE_MULTI)

        if(ENABLE_QUAL)

          add_test(NAME test_python_qual_qual_example1
              COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_SOURCE_DIR}/examples/python/qual/qual_example1.py
          )
          ADJUST_PYTHONPATH(test_python_qual_qual_example1)    

        endif(ENABLE_QUAL)


    endif(CMAKE_VERSION VERSION_LESS 3.0)

endif(WITH_CHECK)



