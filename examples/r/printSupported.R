# 
# @file    printSupported.R
# @brief   Prints supported SBML Levels and Versions for the LibSBML library
# @author  Frank Bergmann
# 
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
# 
# 
# Usage: R --slave -f printSupported.R 
#

library(libSBML)

list = SBMLNamespaces_getSupportedNamespaces()

cat("LibSBML: ",getLibSBMLDottedVersion()," supports: \n");

for (i  in seq_len(SBMLNamespacesList_getSize(list))) {
     current = SBMLNamespacesList_get(list, i-1);
     cat("\tSBML Level ", SBMLNamespaces_getLevel(current),
	     " Version: ", SBMLNamespaces_getVersion(current),"\n"
     );
}
  
cat("\n");

q(status=0);



