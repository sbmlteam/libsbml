writeErrorTable creates the html table that lists the errors and their severities in different levels and versions.

NEED: 
1) python build of current libsbml

2) list.txt
copied from the enumeration of SBMLError with the leading commas removed
e.g.
UnknownError                          = 10000 /*!< Unknown internal libSBML error */
NotUTF8                               = 10101 /*!< Not UTF8 */

Note that the spacing must be exact between the error code and the number; the trailing comment
is not necessary and is not used.

USAGE:
writeErrorTable.py

FUNCTION:
this reads the file list.txt and loops through each line
it creates a string that is the error code and a number
the number is then used to create an instance of the SBMLError for each number
and each level/version combination - for which the severity is output.

The resulting file should be cut and paste into SBMLError.h in place of the existing rows within the documentation table.
