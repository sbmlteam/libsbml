/**
 * Example, reading an SBML file containing an element with provided 
 * meta id, reading a raw annoation from a file, and adding it to the 
 * element. 
 * 
 */

#include <iostream>
#include <sbml/SBMLTypes.h>

using namespace std;
LIBSBML_CPP_NAMESPACE_USE

bool setAnnotation(const std::string& sbmlFile, const std::string& metaId,
	const std::string& annotationFile, const std::string& outputFile)
{
	SBMLDocument *document = readSBML(sbmlFile.c_str());
	if (document->getNumErrors(LIBSBML_SEV_ERROR) > 0)
	{
		cerr << "Encountered the following SBML errors:" << std::endl;
		document->printErrors();
		return false;
	}
	
	SBase* element = document->getElementByMetaId(metaId);
	if (!element)
	{
		cerr << "No element with meta id " << metaId << " found." << std::endl;
		return false;
	}

	XMLNode* annotation = XMLNode::readXMLNodeFromFile(annotationFile);
	if (!annotation)
	{
		cerr << "the annotation could not be parsed from file: " << annotationFile << "." << std::endl;
		return false;
	}
	std::string annotationString = annotation->toXMLString();
	element->setAnnotation(annotation);

	// at this point we'd expect the annotation to be set precisely to what it was 
	// in the file.

	
	std::string resultingAnnotation = element->getAnnotationString();

	if (annotationString != resultingAnnotation)
	{
		cerr << "Annotation was not set correctly." << std::endl;
		return false;
	}

	// write document to output file
	writeSBMLToFile(document, outputFile.c_str());

	delete document;

	return true;
}

int main(int argc, char* argv[])
{
	std::string sbmlFile;
	std::string metaId;
	std::string annotationFile;
	std::string outputFile;

	if (argc != 5)
	{
		cout << endl
				 << "  usage: setAnnotaion <input-filename> <element-meta-id> <annotation-file> <output-file>" << endl
				 << "  Adds controlled vocabulary term to a species"          << endl
				 << endl;
		return 1;


		sbmlFile = "bm190.xml";
		annotationFile = "annotation.xml";
		metaId = "metaid_0000036";
		outputFile = "bm190_out.xml";

	}
	else
	{
		sbmlFile = argv[1];
		metaId = argv[2];
		annotationFile = argv[3];
		outputFile = argv[4];
	}

	setAnnotation(sbmlFile, metaId, annotationFile, outputFile);
}