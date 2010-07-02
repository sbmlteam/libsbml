#
# @file    TestSBMLConstructorException.py
# @brief   SBMLConstructorException unit tests
#
# @author  Akiya Jouraku (Python conversion)
# @author  Akiya Jouraku 
#
# $Id$
# $HeadURL$
#
# This test file was converted from src/sbml/test/TestSBMLConstructorException.cpp
# with the help of conversion sciprt (ctest_converter.pl).
#
#<!---------------------------------------------------------------------------
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
#
# Copyright 2005-2010 California Institute of Technology.
# Copyright 2002-2005 California Institute of Technology and
#                     Japan Science and Technology Corporation.
# 
# This library is free software; you can redistribute it and/or modify it
# under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation.  A copy of the license agreement is provided
# in the file named "LICENSE.txt" included with this software distribution
# and also available online as http://sbml.org/software/libsbml/license.html
#--------------------------------------------------------------------------->*/
import sys
import unittest
import libsbml


class TestSBMLConstructorException(unittest.TestCase):


  current_not_converted_by_ctest_converter = 0


def suite():
  suite = unittest.TestSuite()
  suite.addTest(unittest.makeSuite(TestSBMLConstructorException))

  return suite

if __name__ == "__main__":
  if unittest.TextTestRunner(verbosity=1).run(suite()).wasSuccessful() :
    sys.exit(0)
  else:
    sys.exit(1)
