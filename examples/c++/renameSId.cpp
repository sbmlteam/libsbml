/**
 * @file    renameSId.cpp
 * @brief   Utility program, renaming a specific SId 
 *          while updating all references to it.
 * @author  Frank T. Bergmann
 *
 *
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */


#include <iostream>

#include <sbml/SBMLTypes.h>
#include <sbml/common/extern.h>
#include "util.h"


using namespace std;
LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS

int
main (int argc, char* argv[])
{
    if (argc != 5)
    {
        cout << endl << "Usage: renameSId filename oldSId newSId output" << endl << endl;
        return 1;
    }
    
    const char* filename   = argv[1];
    const char* oldSId     = argv[2];
    const char* newSId     = argv[3];
    const char* output     = argv[4];
    
    
    if (strcmp(oldSId, newSId) == 0)
    {
        cout << "The Ids are identical, renaming stopped." << endl;
        return 1;
    }

    if (!SyntaxChecker::isValidInternalSId(newSId))
    {
        cout << "The new SId '" << newSId
             << "' does not represent a valid SId."
             << endl;
        return 1;
    }
    
    
    SBMLDocument* document;
    SBMLReader reader;
    unsigned long long start, stop;
    
    start    = getCurrentMillis();
    document = reader.readSBML(filename);
    stop     = getCurrentMillis();
    
    unsigned int errors = document->getNumErrors(LIBSBML_SEV_ERROR);
    
    cout << endl;
    cout << "            filename: " << filename              << endl;
    cout << "           file size: " << getFileSize(filename) << endl;
    cout << "      read time (ms): " << stop - start          << endl;
    cout << "            error(s): " << errors << endl;
    cout << endl;
    
    if (errors > 0)
    {
        document->printErrors(cerr);
        delete document;
        return errors;
    }
    
    // find elements for old id
    SBase* element = document->getElementBySId(oldSId);
    if (element == NULL)
    {
        cout << "Found no element with SId '"
             << oldSId << "'." << endl;
        return 1;
    }
    
    // found element --> renaming
    element->setId(newSId);

    // update all references to this element
    List *allElements = document->getAllElements();
    for (unsigned int i = 0; i < allElements->getSize(); ++i)
        static_cast<SBase*>(allElements->get(i))->renameSIdRefs(oldSId, newSId);
    
    
    // write to file
    writeSBMLToFile(document, output);
    
    delete document;
    return errors;
}

END_C_DECLS
