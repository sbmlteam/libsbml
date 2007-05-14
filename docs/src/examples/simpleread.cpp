#include <iostream>
#include <sbml/SBMLTypes.h>

using namespace std;

int
main (int argc, char* argv[])
{
  SBMLDocument* document = readSBML(argv[1]);

  unsigned int errors = document->getNumErrors();

  cout << endl;
  cout << "  filename: " << argv[1] << endl;
  cout << "  error(s): " << errors  << endl;
  cout << endl;

  if (errors > 0) document->printErrors(cerr);

  return errors;
}
