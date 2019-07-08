# Copyright (C) 2017 by Pedro Mendes, Virginia Tech Intellectual 
# Properties, Inc., University of Heidelberg, and University of 
# of Connecticut School of Medicine. 
# All rights reserved. 

#
# Description       : CMake script patching SWIG wrappers
# Original author(s): Frank Bergmann <fbergman@caltech.edu>
# Organization      : California Institute of Technology
#


# patch python wrapper file 
set (WRAPPER_FILE "${BIN_DIRECTORY}/libsbml.py")

if (NOT EXISTS ${WRAPPER_FILE})
  message(FATAL_ERROR "Wrapper file does not exist")
endif()

message(STATUS "Patching Python wrapper")

file(READ "${WRAPPER_FILE}" SOURCECODE)

file(WRITE "${WRAPPER_FILE}" "
# import statement needed on some systems
import sys
import os.path
import inspect
try: 
  _filename = inspect.getframeinfo(inspect.currentframe()).filename
except:
  _filename = __file__
_path = os.path.dirname(os.path.abspath(_filename))
if not _path in sys.path:
  sys.path.append(_path)

")

if (PYTHON_USE_API2_WARNINGS)
  file(APPEND "${WRAPPER_FILE}" "
USE_LIBSBML_PYTHON_API2_WARNINGS = True
")
else()
  file(APPEND "${WRAPPER_FILE}" "
USE_LIBSBML_PYTHON_API2_WARNINGS = False
")
endif()

file(APPEND  "${WRAPPER_FILE}" "${SOURCECODE}")

