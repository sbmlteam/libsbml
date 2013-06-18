#!/usr/bin/env python
##
## @file    setIdFromNames.py
## @brief   Utility program, renaming all SIds that also ha
##          names specified. The new id will be derived fro
##          the name, with all invalid characters removed. 
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

# This class implements an identifier transformer, that means it can be used
# to rename all sbase elements. 
class SetIdFromNames(libsbml.IdentifierTransformer):
  def __init__(self, ids):
    # call the constructor of the base class
    libsbml.IdentifierTransformer.__init__(self)
	# remember existing ids ...
    self.existingIds = ids   
	
  # The function actually doing the transforming. This function is called 
  # once for each SBase element in the model. 
  def transform(self, element):
    # return in case we don't have a valid element
    if (element == None or element.getTypeCode() == libsbml.SBML_LOCAL_PARAMETER):
        return libsbml.LIBSBML_OPERATION_SUCCESS;
    
    # or if there is nothing to do
    if (element.isSetName() == False or element.getId() == element.getName()):
        return libsbml.LIBSBML_OPERATION_SUCCESS;

	# find the new id
    newId = self.getValidIdForName(element.getName());
	
    # set it
    element.setId(newId);

    # remember it
    self.existingIds.append(newId);
    
    return libsbml.LIBSBML_OPERATION_SUCCESS;

  def nameToSbmlId(self, name):
    IdStream = []
    count = 0;
    end = len(name)
    
    if '0' <= name[count] and name[count] <= '9':
      IdStream.append('_');
    for  count in range (0, end):     
      if (('0' <= name[count] and name[count] <= '9') or
          ('a' <= name[count] and name[count] <= 'z') or
          ('A' <= name[count] and name[count] <= 'Z')):
          IdStream.append(name[count]);
      else:
          IdStream.append('_');
    Id = ''.join(IdStream);
    if (Id[len(Id) - 1] != '_'):
        return Id;
    
    return Id[:-1]
  # 
  # Generates the id out of the name, and ensures it is unique. 
  # It does so by appending numbers to the original name. 
  # 
  def getValidIdForName(self, name):
    baseString = self.nameToSbmlId(name);
    id = baseString;
    count = 1;
    while (self.existingIds.count(id) != 0):
      id = "{0}_{1}".format(baseString, count);
      count = count + 1
    return id;

      
      
# 
# Returns a list of all ids from the given list of elements
# 
def getAllIds(allElements):
    result = []
    if (allElements == None or allElements.getSize() == 0):
        return result;

    for i in range (0, allElements.getSize()):
        current = allElements.get(i);
        if (current.isSetId() and current.getTypeCode() != libsbml.SBML_LOCAL_PARAMETER):
            result.append(current.getId());
    return result;



def main (args):
  """Usage: setIdFromNames filename output
  """
  if len(args) != 3:
    print(main.__doc__)
    sys.exit(1)
  
  filename = args[1];
  output = args[2];
  
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
  
  
  # get a list of all elements, as we will need to know all identifiers
  # so that we don't create duplicates. 
  allElements = document.getListOfAllElements();
  
  # get a list of all ids
  allIds = getAllIds(allElements);
  
  # create the transformer with the ids
  trans = SetIdFromNames(allIds);
  
  # rename the identifiers (using the elements we already gathered before)
  start = time.time() * 1000;
  document.getModel().renameIDs(allElements, trans);
  stop = time.time() * 1000;
  print "    rename time (ms): {0}".format(stop - start);
  
  # write to file
  start = time.time() * 1000;
  libsbml.writeSBMLToFile(document, output);
  stop = time.time() * 1000;
  print "     write time (ms): {0}".format(stop - start);
  print "";
  
  # if we got here all went well ... 
  
if __name__ == '__main__':
  main(sys.argv)  
