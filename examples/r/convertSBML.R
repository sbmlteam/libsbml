# 
# @file    convertSBML.R
# @brief   Converts SBML documents between levels
# @author  Frank Bergmann
# 
# 
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
# 
#
# Usage: R --slave -f convertSBML.R --args <input-filename> <output-filename>
#
#

library(libSBML)

args <- commandArgs(trailingOnly = TRUE)

latestLevel   = SBMLDocument_getDefaultLevel();
latestVersion = SBMLDocument_getDefaultVersion();

if (length(args) != 2) {
  stop(
		paste(
         "Usage: convertSBML <input-filename> <output-filename>\n",
		 "This program will attempt to convert a model either to\n",
         "SBML Level ",latestLevel," Version ",latestVersion," (if the model is not already) or, if",
	     "the model is already expressed in Level ",latestLevel," Version ",latestVersion,", this\n",
	     "program will attempt to convert the model to Level 1 Version 2.\n",
		 sep = "")
      );
}

d      = readSBML(args[1]);
errors = SBMLErrorLog_getNumFailsWithSeverity(
			SBMLDocument_getErrorLog(d), 
			enumToInteger("LIBSBML_SEV_ERROR", "_XMLErrorSeverity_t")
		 );

if (errors > 0) {
  cat("Encountered the following SBML error(s):\n");
  SBMLDocument_printErrors(d);
  stop("Conversion skipped.  Please correct the problems above first.\n");
} else {
  olevel   = SBase_getLevel(d);
  oversion = SBase_getVersion(d);
  
  if (olevel < latestLevel || oversion < latestVersion) {
    cat("Attempting to convert model to SBML Level ",latestLevel," Version ",latestVersion,".\n");
    success = SBMLDocument_setLevelAndVersion(d, latestLevel, latestVersion);
  } else {
    cat("Attempting to convert model to SBML Level 1 Version 2.\n");
    success = SBMLDocument_setLevelAndVersion(d, 1, 2);
  }

  errors = SBMLErrorLog_getNumFailsWithSeverity(
			SBMLDocument_getErrorLog(d), 
			enumToInteger("LIBSBML_SEV_ERROR", "_XMLErrorSeverity_t")
		 );

  if (!success) {
    cat("Unable to perform conversion due to the following:\n");
    SBMLDocument_printErrors(d);

    cat("Conversion skipped.  Either libSBML does not (yet) have\n");
    cat("ability to convert this model, or (automatic) conversion\n");
    cat("is not possible in this case.\n");
  } else if (errors > 0) {
    cat("Information may have been lost in conversion; but a valid model ");
    cat("was produced by the conversion.\nThe following information ");
    cat("was provided:\n");
    SBMLDocument_printErrors(d);
    writeSBML(d, args[2]);
  } else { 	    
    cat("Conversion completed.\n");
    writeSBML(d, args[2]);
  }
}

q(status=errors);
