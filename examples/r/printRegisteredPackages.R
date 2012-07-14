# 
# @file    printRegisteredPackages.R
# @brief   Prints the registerd packages for this libSBML
# @author  Frank Bergmann
# 
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
# 
# Usage: R --slave -f printRegisteredPackages.R 
#


library(libSBML)


cat ("This version of LibSBML: ",getLibSBMLDottedVersion()," includes: \n");

for (i in seq_len(SBMLExtensionRegistry_getNumRegisteredPackages())) {
    cat("\t",SBMLExtensionRegistry_getRegisteredPackageName(i-1),"\n");      
}
  
cat("\n");

q(status=0)


