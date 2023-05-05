###############################################################################
#
# Description       : CMake helper generating the doxygen manual for the given language
# Original author(s): Frank Bergmann <fbergman@caltech.edu>
# Organization      : California Institute of Technology
#


message("LANGUAGE:           ${LANGUAGE}")            # the language to generate the manual for (e.g. python, cpp)
message("ROOT_DIR:           ${ROOT_DIR}")            # the libsbml root dir
message("SRC_DIR:            ${SRC_DIR}")             # the libsbml/docs dir
message("OUTPUT_DIR:         ${OUTPUT_DIR}")          # output dir in which to generate manual usually formatted/${LANGUAGE}-api
message("PYTHON_EXECUTABLE:  ${PYTHON_EXECUTABLE}")   # python executable to run scripts with
message("DOXYGEN_EXECUTABLE: ${DOXYGEN_EXECUTABLE}")  # doxygen executable to run

# create output directories
make_directory(${OUTPUT_DIR})
make_directory(${OUTPUT_DIR}/search)

# run python scripts to generate needed files
execute_process(COMMAND ${PYTHON_EXECUTABLE} 
                ${SRC_DIR}/src/utilities/generate-class-name-list.py 
                "${ROOT_DIR}/src" 
                OUTPUT_FILE "${SRC_DIR}/src/class-list.txt"
                WORKING_DIRECTORY ${SRC_DIR}/src)

execute_process(COMMAND ${PYTHON_EXECUTABLE}
                ${SRC_DIR}/src/utilities/generate-converters-list.py "${SRC_DIR}/src/class-list.txt" 
                OUTPUT_FILE "${SRC_DIR}/src/libsbml-converters.txt"
                WORKING_DIRECTORY ${SRC_DIR}/src)

execute_process(COMMAND ${PYTHON_EXECUTABLE}
                ${SRC_DIR}/src/utilities/generate-pkg-stylesheet.py "${ROOT_DIR}/src/sbml/packages" 
                OUTPUT_FILE "${SRC_DIR}/src/css/libsbml-package-stylesheet.css"
                WORKING_DIRECTORY ${SRC_DIR}/src)

# create doxygen config files for index and group runs
# we also add aliases again, as they would have been corrupted 
# by the configure_file run. 
#
# As of doxygen 1.9 the ALIASES are not working as before, so 
# sbmlbrief, will not display the brief on the overview list. 
file(READ   ${SRC_DIR}/src/doxygen-config-${LANGUAGE}.txt DOXYGEN_CONFIG)
file(WRITE  ${SRC_DIR}/src/doxygen-config-${LANGUAGE}.1.txt "${DOXYGEN_CONFIG}")
file(APPEND ${SRC_DIR}/src/doxygen-config-${LANGUAGE}.1.txt "
ALIASES += sbmlpackage{1}=\"@addindex \\1\"

ALIASES += sbmlmarker{1}=\"@htmlinclude pkg-marker-\\1.html\"
ALIASES += sbmlbrief{1}=\"@sbmlpackage{\\1}^^@brief @sbmlmarker{\\1} \"
ALIASES += sbmlfunction{2}=\"@if clike SBML_\\1()@endif @if csharp SBML_\\1()@endif @if python @link libsbml.\\1() \\1()@endlink@endif@~\"
ALIASES += sbmlconstant{2}=\"@if clike @link \\2#\\1 \\1@endlink@endif @if csharp @link libsbml#\\1 \\1@endlink@endif @if python @link libsbml#\\1 \\1@endlink@endif @if java @link libsbmlConstants#\\1 \\1@endlink@endif@~\"

")

file(WRITE  ${SRC_DIR}/src/doxygen-config-${LANGUAGE}.2.txt "${DOXYGEN_CONFIG}")
file(APPEND ${SRC_DIR}/src/doxygen-config-${LANGUAGE}.2.txt "

ALIASES += sbmlpackage{1}=\"@ingroup \\1\"

ALIASES += sbmlmarker{1}=\"@htmlinclude pkg-marker-\\1.html\"
ALIASES += sbmlbrief{1}=\"@sbmlpackage{\\1}^^@brief @sbmlmarker{\\1} \"
ALIASES += sbmlfunction{2}=\"@if clike SBML_\\1()@endif @if csharp SBML_\\1()@endif @if python @link libsbml.\\1() \\1()@endlink@endif@~\"
ALIASES += sbmlconstant{2}=\"@if clike @link \\2#\\1 \\1@endlink@endif @if csharp @link libsbml#\\1 \\1@endlink@endif @if python @link libsbml#\\1 \\1@endlink@endif @if java @link libsbmlConstants#\\1 \\1@endlink@endif@~\"

")

if (NOT SERVER_SEARCH)
file(WRITE ${SRC_DIR}/src/doxygen-search-config.txt "
# Configuration for local search
SEARCHENGINE = YES
SERVER_BASED_SEARCH = NO
")
else()
file(WRITE ${SRC_DIR}/src/doxygen-search-config.txt "
# Config for server-side search
SEARCHENGINE = YES      
SERVER_BASED_SEARCH = YES
EXTERNAL_SEARCH = YES
SEARCHENGINE_URL = db/doxysearch.cgi
")
endif()

# run doxygen twice, once for index, once for package groups 
execute_process(COMMAND ${DOXYGEN_EXECUTABLE} ${SRC_DIR}/src/doxygen-config-${LANGUAGE}.1.txt
                WORKING_DIRECTORY ${SRC_DIR}/src
)

# store index files
execute_process(COMMAND ${CMAKE_COMMAND}
                -DOPERATION=store
                -DGENERATOR_DIR=${OUTPUT_DIR}
                -DOUTPUT_DIR=${OUTPUT_DIR}/tmp-index
                -P "${SRC_DIR}/index-helper.cmake"
)

execute_process(COMMAND ${DOXYGEN_EXECUTABLE} ${SRC_DIR}/src/doxygen-config-${LANGUAGE}.2.txt
                WORKING_DIRECTORY ${SRC_DIR}/src
)

# restore index files
execute_process(COMMAND ${CMAKE_COMMAND}
                -DOPERATION=restore
                -DGENERATOR_DIR=${OUTPUT_DIR}
                -DOUTPUT_DIR=${OUTPUT_DIR}/tmp-index
                -P "${SRC_DIR}/index-helper.cmake"
)
# copy remaining files

set(EXTENSIONS_TO_COPY "*.txt" "*.gif" "*.png" "*.jpg" "*.svg")
foreach(extension ${EXTENSIONS_TO_COPY})
  file(GLOB files ${SRC_DIR}/src/common-graphics/${extension})
  foreach(file ${files})
    file(COPY ${file} DESTINATION ${OUTPUT_DIR})
  endforeach()
endforeach()

set(REMAINING_FILES
    ${SRC_DIR}/src/css/right-arrow-2x.png
    ${SRC_DIR}/src/css/libsbml-reset-stylesheet.css
    ${SRC_DIR}/src/css/libsbml-base-stylesheet.css
    ${SRC_DIR}/src/css/libsbml-package-stylesheet.css
    ${SRC_DIR}/src/css/libsbml-c-stylesheet.css
    ${SRC_DIR}/src/css/libsbml-python-stylesheet.css
    ${SRC_DIR}/src/css/libsbml-doxygen-tabs.css
    ${SRC_DIR}/src/css/libsbml-doxygen-navtree.css
    ${SRC_DIR}/src/sbml.js
)

foreach(file ${REMAINING_FILES})
  file(COPY ${file} DESTINATION ${OUTPUT_DIR})
endforeach()

# rename files (unfortunately the css will need changing for newer doxygen versions)
#file(RENAME ${OUTPUT_DIR}/tabs.css ${OUTPUT_DIR}/doxy_tabs.css)
#file(RENAME ${OUTPUT_DIR}/navtree.css ${OUTPUT_DIR}/doxy_navtree.css)
#file(RENAME ${OUTPUT_DIR}/libsbml-doxygen-tabs.css ${OUTPUT_DIR}/tabs.css)
#file(RENAME ${OUTPUT_DIR}/libsbml-doxygen-navtree.css ${OUTPUT_DIR}/navtree.css)

# append contents of customized css files to doxygen css
file(READ ${OUTPUT_DIR}/libsbml-doxygen-tabs.css TABS_CSS)
file(APPEND ${OUTPUT_DIR}/tabs.css "\n${TABS_CSS}")

file(READ ${OUTPUT_DIR}/libsbml-doxygen-navtree.css NAVTREE_CSS)
file(APPEND ${OUTPUT_DIR}/navtree.css "\n${NAVTREE_CSS}")

# remove temporary files created 
file(REMOVE ${SRC_DIR}/src/doxygen-config-${LANGUAGE}.1.txt)
file(REMOVE ${SRC_DIR}/src/doxygen-config-${LANGUAGE}.2.txt)