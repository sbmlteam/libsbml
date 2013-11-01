#!/usr/bin/env python
##
## @file    getAllElementsWithNotes.py
## @brief   Utility program, demontrating how to use the element filter
##          class to search the model for elements with specific attributes
##          in this example, we look for elements with notes
##
## @author  Frank T. Bergmann
## 
## 
## This file is part of libSBML.  Please visit http:#sbml.org for more
## information about SBML, and the latest version of libSBML.
## 
## 

import sys
import os.path
import time 
import libsbml

# This class implements an element filter, that can be used to find elements
# with notes
class NotesFilter(libsbml.ElementFilter):
  def __init__(self):
    # call the constructor of the base class
    libsbml.ElementFilter.__init__(self)
	
  # The function performing the filtering, here we just check 
  # that we have a valid element, and that it has notes. 
  def filter(self, element):
    # return in case we don't have a valid element
    if (element == None or element.isSetNotes() == False):
        return False;
    # otherwise we have notes set and want to keep the element
    if (element.isSetId()):
      print "                     found : {0}".format(element.getId()) 
    else: 
      print "                     found element without id" 
    return True

def main (args):
  """Usage: getAllElementsWithNotes filename
  """
  if len(args) != 2:
    print(main.__doc__)
    sys.exit(1)
  
  filename = args[1];
  
  # read the document
  start = time.time() * 1000;
  document = libsbml.readSBMLFromFile(filename);
  stop = time.time() * 1000;
  
  
  print ""
  print "            filename: {0}".format( filename);
  print "      read time (ms): {0}".format( stop - start);
  
  # stop in case of serious errors
  errors = document.getNumErrors(libsbml.LIBSBML_SEV_ERROR);
  if (errors > 0):
      print "            error(s): {0}".format(errors);
      document.printErrors();
      sys.exit (errors);
  
  
  # create the filter we want to use
  filter = NotesFilter()

  # get a list of all elements with notes
  start = time.time() * 1000;
  print "    searching ......:"
  allElements = document.getListOfAllElements(filter);
  stop = time.time() * 1000;
  print "    search time (ms): {0}".format(stop - start);

  print " elements with notes: {0}".format(allElements.getSize())
  
  # if we got here all went well ... 
  
if __name__ == '__main__':
  main(sys.argv)  
