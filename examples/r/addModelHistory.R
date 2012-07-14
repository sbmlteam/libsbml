# 
# \file    addModelHistory.R
# \brief   adds Model History to a model
# \author  Frank Bergmann
# 
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
# 
#
# Usage: R --slave -f addModelHistory.R --args <input-filename> <output-filename>
#
#
library(libSBML)

args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 2) {
  stop(
         "  usage: addModelHistory <input-filename> <output-filename>\n"
      );
}

printStatus <- function(message,status) {
    enumStatus <- enumFromInteger(status, "_OperationReturnValues_t" )
	cat(message, switch(enumStatus, 
		LIBSBML_OPERATION_SUCCESS = "succeeded", 
		LIBSBML_INVALID_OBJECT = "invalid object", 
		LIBSBML_OPERATION_FAILED = "operation failed", 
		LIBSBML_UNEXPECTED_ATTRIBUTE = "unexpected attribute (missing metaid)", 
		"unknown"), "\n");
}

d      = readSBML(args[1]);
errors = SBMLDocument_getNumErrors(d);

if (errors > 0) {
  cat("Read Error(s):\n");
  SBMLDocument_printErrors(d);	 
  cat("Correct the above and re-run.\n");
} else {

  h = ModelHistory();
  c = ModelCreator();

  ModelCreator_setFamilyName(c, "Keating");
  ModelCreator_setGivenName(c, "Sarah");
  ModelCreator_setEmail(c, "sbml-team@caltech.edu");
  ModelCreator_setOrganisation(c, "University of Hertfordshire");

  status = ModelHistory_addCreator(h, c);
  printStatus("Status for addCreator: ", status);
  
  date = Date("1999-11-13T06:54:32");
  date2 = Date("2007-11-30T06:54:00-02:00");
     
  status = ModelHistory_setCreatedDate(h, date);
  printStatus("Set created date:      ", status);

  status = ModelHistory_setModifiedDate(h, date2);
  printStatus("Set modified date:     ", status);

  m = SBMLDocument_getModel(d);
  status =  SBase_setModelHistory(m, h);
  printStatus("Set model history:     ", status);

  writeSBML(d, args[2]);
}

q(status=errors);

