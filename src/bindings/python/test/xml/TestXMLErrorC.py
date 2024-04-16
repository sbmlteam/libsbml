#
# @file    TestXMLErrorC.py
# @brief   XMLError unit tests, C version
#
# @author  Akiya Jouraku (Python conversion)
# @author  Sarah Keating 
# 
# ====== WARNING ===== WARNING ===== WARNING ===== WARNING ===== WARNING ======
#
# DO NOT EDIT THIS FILE.
#
# This file was generated automatically by converting the file located at
# src/xml/test/TestXMLErrorC.c
# using the conversion program dev/utilities/translateTests/translateTests.pl.
# Any changes made here will be lost the next time the file is regenerated.
#
# -----------------------------------------------------------------------------
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
# -----------------------------------------------------------------------------

import sys
import unittest
import libsbml


class TestXMLErrorC(unittest.TestCase):


  def test_XMLError_create_C(self):
    error = libsbml.XMLError()
    self.assertTrue( error != None )
    self.assertTrue( error.isInfo() == False )
    self.assertTrue( error.isWarning() == False )
    self.assertTrue( error.isError() == False )
    self.assertTrue( error.isFatal() == True )
    _dummyList = [ error ]; _dummyList[:] = []; del _dummyList
    error = libsbml.XMLError(12345, "My message")
    self.assertTrue( (  "My message" != error.getMessage() ) == False )
    self.assertTrue( error.getErrorId() == 12345 )
    _dummyList = [ error ]; _dummyList[:] = []; del _dummyList
    pass  

  def test_XMLError_variablesAsStrings(self):
    error = libsbml.XMLError(1003, "")
    self.assertTrue( error.getErrorId() == 1003 )
    self.assertTrue( error.getSeverity() == libsbml.LIBSBML_SEV_ERROR )
    self.assertTrue((  "Error" == error.getSeverityAsString() ))
    self.assertTrue( error.getCategory() == libsbml.LIBSBML_CAT_XML )
    self.assertTrue((  "XML content" == error.getCategoryAsString() ))
    _dummyList = [ error ]; _dummyList[:] = []; del _dummyList
    pass  

def suite():
  suite = unittest.TestSuite()
  suite.addTest(unittest.TestLoader.loadTestsFromTestCase(TestXMLErrorC))

  return suite

if __name__ == "__main__":
  if unittest.TextTestRunner(verbosity=1).run(suite()).wasSuccessful() :
    sys.exit(0)
  else:
    sys.exit(1)

