/**
 * @file    setNamesFromIds.cpp
 * @brief   Utility program, renaming all Names to match their ids. 
 *          
 * @author  Frank T. Bergmann
 *
 *
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */


#include <iostream>
#include <sstream>
#include <vector>
#include <string>

#include <sbml/SBMLTypes.h>
#include <sbml/common/extern.h>
#include <sbml/common/operationReturnValues.h>
#include "util.h"


using namespace std;
LIBSBML_CPP_NAMESPACE_USE


/** 
 * The NameIdTransformer class transforms the name of a given SBase 
 * element by replacing it with its id. 
 * 
 * It won't do anything if the name is the same as the id, or if 
 * no id is set. 
 */ 
class NameIdTransformer : public IdentifierTransformer
{
public:
	NameIdTransformer() 
	: IdentifierTransformer()
	{
	}

	/** 
	 * The actual transform implementation
	 */
	int transform(SBase* element)
	{
		// return in case we don't have a valid element
		if (element == NULL || element->getTypeCode() == SBML_LOCAL_PARAMETER)	
			return LIBSBML_OPERATION_SUCCESS;

		// or if there is nothing to do
		if (!element->isSetId() || element->getId() == element->getName())			
			return LIBSBML_OPERATION_SUCCESS;

		// set it
		element->setName(element->getId());

		
		return LIBSBML_OPERATION_SUCCESS;
			
	}

};

BEGIN_C_DECLS

int
main (int argc, char* argv[])
{
    if (argc != 3)
    {
        cout << endl << "Usage: setNamesFromIds filename output" << endl << endl;
        return 1;
    }
    
    const char* filename   = argv[1];
    const char* output     = argv[2];
    
        
    SBMLDocument* document;
    SBMLReader reader;
    unsigned long long start, stop;
    
    start    = getCurrentMillis();
    document = reader.readSBML(filename);
    stop     = getCurrentMillis();
    
    unsigned int errors = document->getNumErrors(LIBSBML_SEV_ERROR);
    
    cout << endl;
    cout << "            filename: " << filename              << endl;
    cout << "      read time (ms): " << stop - start          << endl;
    
    if (errors > 0)
    {
		cout << "            error(s): " << errors << endl;
        document->printErrors(cerr);
        delete document;
        return errors;
    }
    
	start = stop;
	
	// get a list of all elements, as we will need to know all identifiers
	List* allElements = document->getAllElements(); 
	
	// create the transformer 
	NameIdTransformer trans;
	
	// rename the identifiers (using the elements we already gathered before)
	document->getModel()->renameIDs(allElements, &trans);    
	stop     = getCurrentMillis();
	cout << "    rename time (ms): " << stop - start          << endl;
	start = stop;
	
    // write to file
    writeSBMLToFile(document, output);
    stop     = getCurrentMillis();
	cout << "     write time (ms): " << stop - start          << endl;   
	cout << endl;
	
    delete document;
    return errors;
}

END_C_DECLS
