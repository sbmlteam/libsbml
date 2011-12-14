#!/usr/bin/env ruby
#
##
## @file    addCustomValidator.py
## @brief   Example creating a custom validator to be called during validation
## @author  Frank T. Bergmann
## 
## 
## This file is part of libSBML.  Please visit http:#sbml.org for more
## information about SBML, and the latest version of libSBML.
## 
## 


require 'libSBML'

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
class MyCustomValidator < LibSBML::SBMLValidator
	def initialize()	
  	  super
        end
	def clone
		return MyCustomValidator.new	
        end
	def validate
		# if we don't have a model we don't apply this validator.
		if (getDocument == nil or getModel == nil):
			return 0;
		end
		# if we have no rules and reactions we don't apply this validator either
		if (getModel.getNumReactions == 0 && getModel.getNumRules == 0):
			return 0;
		end
		numErrors = 0;
		# test for algebraic rules
		getModel.getNumRules.times do |i|
		  if getModel.getRule(i).getTypeCode == LibSBML::SBML_ALGEBRAIC_RULE
		    getErrorLog.add(LibSBML::SBMLError.new(99999, 3, 1,
                    "This model uses algebraic rules, however this application does not support them.",
                    0, 0,
                    LibSBML::LIBSBML_SEV_WARNING, # or LIBSBML_SEV_ERROR if you want to stop
                    LibSBML::LIBSBML_CAT_SBML # or whatever category you prefer
                    ))
		  numErrors = numErrors + 1;		
                  end
                end
                
		# test for fast reactions
		getModel.getNumReactions.times do |i|
			# test whether value is set, and true
			if getModel.getReaction(i).isSetFast && getModel.getReaction(i).getFast
		          getErrorLog.add(LibSBML::SBMLError.new(99999, 3, 1,
	                  "This model uses fast reactions, however this application does not support them.",
        	          0, 0,
        	          libsbml.LIBSBML_SEV_WARNING, # or LIBSBML_SEV_ERROR if you want to stop
        	          libsbml.LIBSBML_CAT_SBML # or whatever category you prefer
        	          ))
			  numErrors = numErrors + 1;
                        end
                end
		return numErrors;
           end
end


if ARGV.size != 1
  puts  "Usage: addCustomValidator filename"
  puts ""
  exit(1)
end

# read the file name
document = LibSBML::readSBML(ARGV[0])

# add a custom validator
document.addValidator(MyCustomValidator.new)

# check consistency like before
numErrors = document.checkConsistency

# print errors and warnings
document.printErrors

# return number of errors
exit(numErrors)

