###############################################################################
#
# Description       : CMake build script for libSBML
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

if (WITH_DOXYGEN)
find_package(Doxygen)
find_package(PythonInterp)

if (NOT EXISTS "${DOXYGEN_EXECUTABLE}")
		message(FATAL_ERROR "Cannot generate documentation, please install doxygen / specify the DOXYGEN_EXECUTABLE.")
endif()

if (NOT EXISTS "${PYTHON_EXECUTABLE}")
		message(FATAL_ERROR "Cannot generate documentation, please install python / specify the PYTHON_EXECUTABLE.")
endif()

# -----------------------------------------------------------------------------
# 'comp' package
# -----------------------------------------------------------------------------

SET(PACKAGE_COMP_VERSION 1)
SET(PACKAGE_COMP_RELEASE 3)
SET(PACKAGE_COMP_SPEC_URL "http://sbml.org/specifications/sbml-level-3/version-1/comp/sbml-comp-version-1-release-3.pdf")
SET(PACKAGE_COMP_ERRATA_URL "http://sbml.org/Documents/Specifications/SBML_Level_3/Packages/Hierarchical_Model_Composition_%28comp%29/Confirmed_issues_in_the_Hierarchical_Model_Composition_package_specification")

# -----------------------------------------------------------------------------
# 'fbc' package v1
# -----------------------------------------------------------------------------

SET(PACKAGE_FBC_V1_VERSION 1)
SET(PACKAGE_FBC_V1_RELEASE 1)
SET(PACKAGE_FBC_V1_SPEC_URL "http://identifiers.org/combine.specifications/sbml.level-3.version-1.fbc.version-1.release-1")
SET(PACKAGE_FBC_V1_ERRATA_URL "http://sbml.org/Documents/Specifications/SBML_Level_3/Packages/Flux_Balance_Constraints_%28flux%29/Confirmed_issues_in_the_Flux_Balance_Constraints_package_specification")

# -----------------------------------------------------------------------------
# 'fbc' package v2
# -----------------------------------------------------------------------------

SET(PACKAGE_FBC_V2_VERSION 2)
SET(PACKAGE_FBC_V2_RELEASE 1)
SET(PACKAGE_FBC_V2_SPEC_URL "http://identifiers.org/combine.specifications/sbml.level-3.version-1.fbc.version-2.release-1")
SET(PACKAGE_FBC_V2_ERRATA_URL "http://sbml.org/Documents/Specifications/SBML_Level_3/Packages/Flux_Balance_Constraints_%28flux%29/Confirmed_issues_in_the_Flux_Balance_Constraints_package_specification")

# -----------------------------------------------------------------------------
# 'groups' package v1
# -----------------------------------------------------------------------------

SET(PACKAGE_GROUPS_V1_VERSION 1)
SET(PACKAGE_GROUPS_V1_RELEASE 1)
SET(PACKAGE_GROUPS_V1_SPEC_URL "http://identifiers.org/combine.specifications/sbml.level-3.version-1.groups.version-1.release-1")
SET(PACKAGE_GROUPS_V1_ERRATA_URL "http://sbml.org/Documents/Specifications/SBML_Level_3/Packages/Groups/Confirmed_issues_in_the_Groups_package_specification")

# -----------------------------------------------------------------------------
# 'multi' package v1
# -----------------------------------------------------------------------------

SET(PACKAGE_MULTI_V1_VERSION 1)
SET(PACKAGE_MULTI_V1_RELEASE 1)
SET(PACKAGE_MULTI_V1_SPEC_URL "http://identifiers.org/combine.specifications/sbml.level-3.version-1.multi.version-1.release-1")
SET(PACKAGE_MULTI_V1_ERRATA_URL "http://sbml.org/Documents/Specifications/SBML_Level_3/Packages/Multi/Confirmed_issues_in_the_Multi_package_specification")

# -----------------------------------------------------------------------------
# 'layout' package
# -----------------------------------------------------------------------------

SET(PACKAGE_LAYOUT_VERSION 1)
SET(PACKAGE_LAYOUT_RELEASE 1)
SET(PACKAGE_LAYOUT_SPEC_URL "http://identifiers.org/combine.specifications/sbml.level-3.version-1.layout.version-1.release-1")
SET(PACKAGE_LAYOUT_ERRATA_URL "http://sbml.org/Documents/Specifications/SBML_Level_3/Packages/Layout_%28layout%29/Confirmed_issues_in_the_Layout_package_specification")

# -----------------------------------------------------------------------------
# 'render' package
# -----------------------------------------------------------------------------

SET(PACKAGE_RENDER_VERSION 1)
SET(PACKAGE_RENDER_RELEASE 1)
SET(PACKAGE_RENDER_SPEC_URL "http://identifiers.org/combine.specifications/sbml.level-3.version-1.render.version-1.release-1")
SET(PACKAGE_RENDER_ERRATA_URL "http://sbml.org/Documents/Specifications/SBML_Level_3/Packages/Render_%28render%29/Confirmed_issues_in_the_Render_package_specification")

# -----------------------------------------------------------------------------
# 'qual' package
# -----------------------------------------------------------------------------

SET(PACKAGE_QUAL_VERSION 1)
SET(PACKAGE_QUAL_RELEASE 1)
SET(PACKAGE_QUAL_SPEC_URL "http://identifiers.org/combine.specifications/sbml.level-3.version-1.qual.version-1.release-1")
SET(PACKAGE_QUAL_ERRATA_URL "http://sbml.org/Documents/Specifications/SBML_Level_3/Packages/Qualitative_Models_%28qual%29/Confirmed_issues_in_the_Qualitative_Models_package_specification")


# determine doxygen version
IF(DOXYGEN_EXECUTABLE)
  EXECUTE_PROCESS(COMMAND ${DOXYGEN_EXECUTABLE} "--version" OUTPUT_VARIABLE DOXYGEN_VERSION OUTPUT_STRIP_TRAILING_WHITESPACE)
ENDIF()

# write specific file
if (DOXYGEN_VERSION VERSION_GREATER 1.7)
file(WRITE ${CMAKE_CURRENT_SOURCE_DIR}/src/doxygen-version-specific.txt "MARKDOWN_SUPPORT = NO")
else()
file(WRITE ${CMAKE_CURRENT_SOURCE_DIR}/src/doxygen-version-specific.txt "")
endif()

# configure java documentation file
configure_file(
	${CMAKE_CURRENT_SOURCE_DIR}/src/java-substitutions/libsbmlConstants.java.in
	${CMAKE_CURRENT_SOURCE_DIR}/src/java-substitutions/libsbmlConstants.java
)

# disable dot if we don't have it
set(HAVE_DOT)
set(DOT_PATH)
if (EXISTS "${DOXYGEN_DOT_PATH}")
	set(HAVE_DOT "YES")
	file(TO_NATIVE_PATH ${DOXYGEN_DOT_PATH} DOT_PATH)
else()
	set(HAVE_DOT "NO")
	set(DOT_PATH "")
endif()

set(PACKAGE_NAME "libSBML")

# replace variables as needed
foreach( lang "common" "c" "cpp" "perl" "python" "csharp")
configure_file(
	${CMAKE_CURRENT_SOURCE_DIR}/src/doxygen-config-${lang}.txt.cmake
	${CMAKE_CURRENT_SOURCE_DIR}/src/doxygen-config-${lang}.txt
	@ONLY
)
endforeach()

file(GLOB HTML_TEMPLATES ${CMAKE_CURRENT_SOURCE_DIR}/src/common-text/*.html.in)
foreach(file ${HTML_TEMPLATES})
	get_filename_component(filename ${file} NAME)
	string(REPLACE ".in" "" filename ${filename})
	configure_file(
		${file}
		${CMAKE_CURRENT_SOURCE_DIR}/src/common-text/${filename}
		@ONLY
	)
endforeach()

# create C-api manuals
macro(createManual lang)
	ADD_CUSTOM_COMMAND(
		OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/formatted/${lang}-api/index.html

		# call create manual script
		COMMAND "${CMAKE_COMMAND}"
		ARGS -DLANGUAGE="${lang}"
			 -DROOT_DIR=${LIBSBML_ROOT_SOURCE_DIR}
			 -DSRC_DIR=${CMAKE_CURRENT_SOURCE_DIR}
			 -DOUTPUT_DIR=${CMAKE_CURRENT_SOURCE_DIR}/formatted/${lang}-api/
			 -DPYTHON_EXECUTABLE=${PYTHON_EXECUTABLE}
			 -DDOXYGEN_EXECUTABLE=${DOXYGEN_EXECUTABLE}
			 -P "${CMAKE_CURRENT_SOURCE_DIR}/create-manual.cmake"
			
		COMMENT "Generate ${lang}-API Documentation"
		DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/CMakeLists.txt
		        ${CMAKE_CURRENT_SOURCE_DIR}/create-manual.cmake
				${CMAKE_CURRENT_SOURCE_DIR}/index-helper.cmake
				${CMAKE_CURRENT_SOURCE_DIR}/src/doxygen-config-${lang}.txt
				${CMAKE_CURRENT_SOURCE_DIR}/src/doxygen-config-${lang}.txt.cmake

	)


	add_custom_target(api_docs_${lang} ALL DEPENDS
		${CMAKE_CURRENT_SOURCE_DIR}/formatted/${lang}-api/index.html
		${CMAKE_CURRENT_SOURCE_DIR}/CMakeLists.txt)

	if (${lang} STREQUAL "python")
		add_dependencies(api_docs_${lang} api_docs_python_py)
	endif()
	
	install(DIRECTORY ${output_path} DESTINATION ${MISC_PREFIX}docs )

endmacro()

createManual(c)
createManual(cpp)

# create python-api manuals
if (WITH_PYTHON)
	file(GLOB PYTHON_SUBSTITUTIONS ${CMAKE_CURRENT_SOURCE_DIR}/src/python-substitutions/*.py)
	ADD_CUSTOM_COMMAND(
		OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/src/libsbml.py
		COMMAND ${PYTHON_EXECUTABLE}
		ARGS    ${CMAKE_CURRENT_SOURCE_DIR}/src/utilities/pythondocpreprocessor.py
				${CMAKE_CURRENT_BINARY_DIR}/../src/bindings/python/libsbml-doxygen.py  
				libsbml.py
				${PYTHON_SUBSTITUTIONS}
		WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/src
		COMMENT "Generate libsbml.py"
		DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/CMakeLists.txt
	)	
	add_custom_target(api_docs_python_py ALL DEPENDS
		${CMAKE_CURRENT_SOURCE_DIR}/src/libsbml.py)
	createManual(python)
endif()

# create perl-api manuals
if (WITH_PERL)
	createManual(perl)
endif()

# create C#-api manuals
if (WITH_CSHARP)
	createManual(csharp)
endif()

# create java-api manuals
if (WITH_JAVA)
	# find java doc jar
	if (NOT EXISTS "${Java_JAVADOC_JAR}")
		get_filename_component(JDK_PATH ${Java_JAVA_EXECUTABLE} PATH )
		file(TO_NATIVE_PATH "${JDK_PATH}/../lib/tools.jar" Java_JAVADOC_JAR)
		if (NOT EXISTS "${Java_JAVADOC_JAR}")
			if (UNIX)
				if (APPLE)
					set(Java_JAVADOC_JAR "/System/Library/Frameworks/JavaVM.framework/Versions/CurrentJDK/Classes/classes.jar")
				else()
					set(Java_JAVADOC_JAR ${JAVA_INCLUDE_PATH}/../lib/tools.jar)
				endif()
			endif()
		endif()
	endif()
	set(Java_JAVADOC_JAR ${Java_JAVADOC_JAR} CACHE FILEPATH "Location of javadoc jar")

	if (NOT EXISTS "${Java_JAVADOC_JAR}")
		message(FATAL_ERROR "Cannot generate java documentation, please specify the Java_JAVADOC_JAR.")
	endif()

	set(LIBSBML_JAVA_DIR "${CMAKE_CURRENT_BINARY_DIR}/../src/bindings/java/java-files")
	set(LIBSBML_JAVADOC_DOCTITLE "${PACKAGE_NAME} ${PACKAGE_VERSION} Java API Reference")
	set(LIBSBML_JAVADOC_HEADER "<B>${PACKAGE_NAME}<BR>${PACKAGE_VERSION}</B>")
	set(LIBSBML_JAVADOC_WINDOWTITLE "LibSBML Java API")
	file(READ "${CMAKE_CURRENT_SOURCE_DIR}/src/libsbml-java-footer.html"  LIBSBML_JAVADOC_FOOTER)
	string(STRIP 	${LIBSBML_JAVADOC_FOOTER} LIBSBML_JAVADOC_FOOTER)
	string(REGEX REPLACE "\n" "" LIBSBML_JAVADOC_FOOTER "${LIBSBML_JAVADOC_FOOTER}")


	file(TO_NATIVE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/src/doxygen-config-perl.txt api_doc)
	file(TO_NATIVE_PATH ${CMAKE_CURRENT_BINARY_DIR} current_dir)
	file(TO_NATIVE_PATH ${LIBSBML_JAVA_DIR} java_dir)
	file(TO_NATIVE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/formatted/java-api output_dir)
	file(TO_NATIVE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/formatted/java-api/org/sbml/libsbml/ java_output_dir)
	file(TO_NATIVE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/src/common-graphics/* graphics_files)

	file(GLOB graphics_files ${CMAKE_CURRENT_SOURCE_DIR}/src/common-graphics/*.jpg)
	foreach(graphic ${graphics_files})
		file(COPY ${graphic} DESTINATION ${CMAKE_CURRENT_SOURCE_DIR}/formatted/java-api/org/sbml/libsbml/)
	endforeach()


	ADD_CUSTOM_COMMAND(
		OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/formatted/java-api/index.html
		COMMAND ${Java_JAVAC_EXECUTABLE}
		ARGS -nowarn
			 -cp "${Java_JAVADOC_JAR}"
			 ${CMAKE_CURRENT_SOURCE_DIR}/src/SBMLDoclet.java
			  -d "${current_dir}"
		COMMAND ${Java_JAVA_EXECUTABLE}
		ARGS  -cp "${Java_JAVADOC_JAR}${FILE_SEP}${current_dir}"
		      SBMLDoclet
			  -use -public -version -author -sourcetab 4 -keywords -nohelp
			  -link http://docs.oracle.com/javase/1.6.0/docs/api
			  -tag note:a:"Note:"
			  -tag warning:a:"Warning:"
			  -excludefile java-skip.txt
			  -header "${LIBSBML_JAVADOC_HEADER}"
			  -bottom "${LIBSBML_JAVADOC_FOOTER}"
			  -windowtitle ${LIBSBML_JAVADOC_WINDOWTITLE}
			  -doctitle ${LIBSBML_JAVADOC_DOCTITLE}
			  -overview libsbml-java-fake-overview.html
			  -stylesheetfile css/libsbml-javadoc-stylesheet.css
			  -d ${output_dir}
	          -sourcepath ${java_dir} org.sbml.libsbml
		COMMENT "Generate Java-API Documentation"
		VERBATIM
		WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/src

		COMMAND "${CMAKE_COMMAND}"
		ARGS -E copy
			"${logo_file}"
			"${output_dir}"

		COMMAND "${CMAKE_COMMAND}"
		ARGS -E copy
			"${sbml_js}"
			"${output_dir}"

#		COMMAND "${CMAKE_COMMAND}"
#		ARGS -E copy
#			"${tabs_css}"
#			"${output_dir}"

#		COMMAND "${CMAKE_COMMAND}"
#		ARGS -E copy
#			${logo_file}
#			${java_output_dir}
#
		COMMAND "${CMAKE_COMMAND}"
		ARGS -DPACKAGE_VERSION=\"${PACKAGE_VERSION}\"
			-Djava_manual=${CMAKE_CURRENT_SOURCE_DIR}/formatted/java-api
			-P ${CMAKE_CURRENT_SOURCE_DIR}/process-java-doc.cmake

		DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/CMakeLists.txt
				${CMAKE_CURRENT_SOURCE_DIR}/process-java-doc.cmake

		)

	add_custom_target(api_docs_java ALL DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/formatted/java-api/index.html)

	install(DIRECTORY ${output_dir} DESTINATION ${MISC_PREFIX}docs )

endif()


endif()
