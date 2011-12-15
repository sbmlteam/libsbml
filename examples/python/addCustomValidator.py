#!/usr/bin/env python
##
## @file    addCustomValidator.py
## @brief   Example creating a custom validator to be called during validation
## @author  Frank T. Bergmann
## 
## 
## This file is part of libSBML.  Please visit http://sbml.org for more
## information about SBML, and the latest version of libSBML.
## 
## 

import sys
import os.path
import libsbml

##  
## Declares a custom validator to be called. This allows you to validate 
## any aspect of an SBML Model that you want to be notified about. You could 
## use this to notify your application that a model contains an unsupported 
## feature of SBML (either as warning). 
## 
## In this example the validator will go through the model and test for the 
## presence of 'fast' reactions and algebraic rules. If either is used a 
## warning will be added to the error log. 
## 
class MyCustomValidator(libsbml.SBMLValidator):
	def __init__(self, orig = None):		
		if (orig == None):
			super(MyCustomValidator,self).__init__()
		else:
			super(MyCustomValidator,self).__init__(orig)	
	def clone(self):
		return MyCustomValidator(self)	
	def validate(self):
		# if we don't have a model we don't apply this validator.
		if (self.getDocument() == None or self.getModel() == None):
			return 0;
		
		# if we have no rules and reactions we don't apply this validator either
		if (self.getModel().getNumReactions() == 0 and self.getModel().getNumRules() == 0):
			return 0;
		
		numErrors = 0;
		# test for algebraic rules
		for i in range(0, self.getModel().getNumRules()):
			if (self.getModel().getRule(i).getTypeCode() == libsbml.SBML_ALGEBRAIC_RULE):
				self.getErrorLog().add(SBMLError(99999, 3, 1,
                  "This model uses algebraic rules, however this application does not support them.",
                  0, 0,
                  libsbml.LIBSBML_SEV_WARNING, # or LIBSBML_SEV_ERROR if you want to stop
                  libsbml.LIBSBML_CAT_SBML # or whatever category you prefer
                  ));
                numErrors = numErrors + 1;
		
		# test for fast reactions
		for i in range (0, self.getModel().getNumReactions()):
			# test whether value is set, and true
			if (self.getModel().getReaction(i).isSetFast() and	
                self.getModel().getReaction(i).getFast()):
				self.getErrorLog().add(SBMLError(99999, 3, 1,
                  "This model uses fast reactions, however this application does not support them.",
                  0, 0,
                  libsbml.LIBSBML_SEV_WARNING, # or LIBSBML_SEV_ERROR if you want to stop
                  libsbml.LIBSBML_CAT_SBML # or whatever category you prefer
                  ));
				numErrors = numErrors + 1;
		return numErrors;

def main (args):
  """Usage: addCustomValidator filename
  """
  if len(args) != 2:
    print(main.__doc__)
    sys.exit(1)

  # read the file name
  document = libsbml.readSBML(args[1]);

  # add a custom validator
  document.addValidator(MyCustomValidator());
  
  # check consistency like before
  numErrors = document.checkConsistency();

  # print errors and warnings
  document.printErrors();

  # return number of errors
  return numErrors;

if __name__ == '__main__':
  main(sys.argv)  
