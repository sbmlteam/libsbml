#!/usr/bin/env python3
##
## @file    flattenModel.py
## @brief   Flattens the comp code from the given SBML file.
## @author  Frank T. Bergmann
## @author  Michael Hucka
##
## <!--------------------------------------------------------------------------
## This sample program is distributed under a different license than the rest
## of libSBML.  This program uses the open-source MIT license, as follows:
##
## Copyright (c) 2013-2018 by the California Institute of Technology
## (California, USA), the European Bioinformatics Institute (EMBL-EBI, UK)
## and the University of Heidelberg (Germany), with support from the National
## Institutes of Health (USA) under grant R01GM070923.  All rights reserved.
##
## Permission is hereby granted, free of charge, to any person obtaining a
## copy of this software and associated documentation files (the "Software"),
## to deal in the Software without restriction, including without limitation
## the rights to use, copy, modify, merge, publish, distribute, sublicense,
## and/or sell copies of the Software, and to permit persons to whom the
## Software is furnished to do so, subject to the following conditions:
##
## The above copyright notice and this permission notice shall be included in
## all copies or substantial portions of the Software.
##
## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
## IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
## FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
## THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
## LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
## FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
## DEALINGS IN THE SOFTWARE.
##
## Neither the name of the California Institute of Technology (Caltech), nor
## of the European Bioinformatics Institute (EMBL-EBI), nor of the University
## of Heidelberg, nor the names of any contributors, may be used to endorse
## or promote products derived from this software without specific prior
## written permission.
## ------------------------------------------------------------------------ -->

import sys, getopt, os.path
from libsbml import *


# Utility function to make it easier to check the return values from
# libSBML calls.

def check(value, message):
  """If 'value' is None, prints an error message constructed using
  'message' and then exits with status code 1.  If 'value' is an integer,
  it assumes it is a libSBML return status code.  If the code value is
  LIBSBML_OPERATION_SUCCESS, returns without further action; if it is not,
  prints an error message constructed using 'message' along with text from
  libSBML explaining the meaning of the code, and exits with status code 1.
  """
  if value is None:
    raise SystemExit('LibSBML returned a null value trying to ' + message + '.')
  elif type(value) is int:
    if value == LIBSBML_OPERATION_SUCCESS:
      return
    else:
      err_msg = 'Error encountered trying to ' + message + '.' \
                + 'LibSBML returned error code ' + str(value) + ': "' \
                + OperationReturnValue_toString(value).strip() + '"'
      raise SystemExit(err_msg)
  else:
    return


# The actual program.

def main (argv):
  """Usage: flattenModel.py [-p] MODEL_FILE OUTPUT_FILE
Arguments:
 -p    (Optional) list unused ports
  """

  # Before we begin, check that this copy of libSBML has the 'comp' package
  # extension compiled in.

  if not SBMLExtensionRegistry.isPackageEnabled("comp"):
    err_msg = 'This copy of libSBML does not contain the "comp" extension.' \
              + 'Unable to proceed with flattening the model.'
    raise SystemExit(err_msg)

  # Read and verify command line arguments.

  try:
    opts, args = getopt.getopt(argv[1:], "p")
  except:
    raise SystemExit(main.__doc__)

  if len(args) < 2:
    raise SystemExit(main.__doc__)

  leave_ports = '-p' in opts
  input_file  = args[0]
  output_file = args[1]

  if not os.path.exists(input_file):
    raise SystemExit('%s : No such file.' % input_file)

  # Read the SBML input file.

  reader  = SBMLReader()
  check(reader, 'create an SBMLReader object.')
  sbmldoc = reader.readSBML(input_file)
  check(sbmldoc, 'create an SBMLDocument object from file input')

  if sbmldoc.getNumErrors() > 0:
    if sbmldoc.getError(0).getErrorId() == XMLFileUnreadable:
      # Handle case of unreadable file here.
      sbmldoc.printErrors()
    elif sbmldoc.getError(0).getErrorId() == XMLFileOperationError:
      # Handle case of other file error here.
      sbmldoc.printErrors()
    else:
      # Handle other error cases here.
      sbmldoc.printErrors()

    raise SystemExit(2)

  # Create the converter options

  props = ConversionProperties()
  props.addOption("flatten comp", True)       # Invokes CompFlatteningConverter
  props.addOption("leave_ports", leave_ports) # Indicates whether to leave ports

  # Do the conversion.

  result = sbmldoc.convert(props)
  if result != LIBSBML_OPERATION_SUCCESS:
    sbmldoc.printErrors()
    raise SystemExit("Conversion failed... ("+ str(result) + ")")

  # Write the results to the output file.

  writer  = SBMLWriter()
  check(writer, 'create an SBMLWriter object.')
  writer.writeSBML(sbmldoc, output_file)
  print("Flattened model written to %s" % output_file)

if __name__ == '__main__':
  main(sys.argv)
