/**
 * @file    getAllElementsWithNotes.cpp
 * @brief   Utility program, demontrating how to use the element filter
 *          class to search the model for elements with specific attributes 
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
 * This class implements an element filter, that can be used to find elements
 * with notes
 */ 
class NotesFilter : public ElementFilter
{
public:
	NotesFilter() : ElementFilter()
	{
	}

	/** 
	 * The function performing the filtering, here we just check 
     * that we have a valid element, and that it has notes.
	 */
	virtual bool filter(const SBase* element)
	{
		// return in case we don't have a valid element
        if (element == NULL || !element->isSetNotes())
            return false;

        // otherwise we have notes set and want to keep the element
        if (element->isSetId())
            cout << "                     found : " 
			    << element->getId() << endl;
		else
			cout << "                     found : " 
			    << "element without id" << endl;

        return true;			
	}

};


BEGIN_C_DECLS

int
main (int argc, char* argv[])
{
    if (argc != 2)
    {
        cout << endl << "Usage: getAllElementsWithNotes filename" << endl << endl;
        return 1;
    }
    
    const char* filename   = argv[1];
    
        
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
	
	// create the filter we want to use
    NotesFilter filter;
	//  get a list of all elements with notes
	cout << "    searching ......:" << endl;
	List* allElements = document->getAllElements(&filter); 
	stop     = getCurrentMillis();
	cout << "    search time (ms): " << stop - start          << endl;
	cout << " elements with notes: " << allElements->getSize() << endl;
	
    delete document;
    return errors;
}

END_C_DECLS
