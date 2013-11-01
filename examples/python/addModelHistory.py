#!/usr/bin/env python
## 
## \file    addModelHistory.py
## \brief   adds Model History to a model
## \author  Sarah Keating
## 
## This file is part of libSBML.  Please visit http://sbml.org for more
## information about SBML, and the latest version of libSBML.
## 


import sys
import os.path
from libsbml import *

def printStatus(message, status):
  statusString = "";
  if status == LIBSBML_OPERATION_SUCCESS:
    statusString = "succeeded";
  elif status == LIBSBML_INVALID_OBJECT:
    statusString = "invalid object";
  elif status == LIBSBML_OPERATION_FAILED:
    statusString = "operation failed";
  else:
    statusString = "unknown";          
  
  print (message + statusString );

def main (args):
  """usage: addModelHistory <input-filename> <output-filename>
     Adds a model history to the model
  """
  if len(args) != 3:
    print(main.__doc__)
    sys.exit(2)


  d = readSBML(args[1]);
  errors = d.getNumErrors();

  if (errors > 0):
      print("Read Error(s):" );
      d.printErrors();  
      print("Correct the above and re-run." );
  else:
      h = ModelHistory();
  
      c = ModelCreator();
      c.setFamilyName("Keating");
      c.setGivenName("Sarah");
      c.setEmail("sbml-team@caltech.edu");
      c.setOrganization("University of Hertfordshire");
  
      status = h.addCreator(c);
      printStatus("Status for addCreator: ", status);
  
  
      date = Date("1999-11-13T06:54:32");
      date2 = Date("2007-11-30T06:54:00-02:00");
  
      status = h.setCreatedDate(date);
      printStatus("Set created date:      ", status);
  
      status = h.setModifiedDate(date2);
      printStatus("Set modified date:     ", status);
  
      status = d.getModel().setModelHistory(h);
      printStatus("Set model history:     ", status);
  
  
      writeSBML(d, args[2]);
  return errors;


if __name__ == '__main__':
  main(sys.argv)  
