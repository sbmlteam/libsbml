#include <iostream>
#include <sbml/SBMLTypes.h>

int
main (int argc, char *argv[])
{
  const char* expected = "1 + f(x)";

  const char* s = "<?xml version='1.0' encoding='UTF-8'?>"
    "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
    "  <apply> <plus/> <cn> 1 </cn>"
    "                  <apply> <ci> f </ci> <ci> x </ci> </apply>"
    "  </apply>"
    "</math>";

  ASTNode *ast    = readMathMLFromString(s);
  char    *result = SBML_formulaToString(ast);

  if ( strcmp(result, expected) == 0 )
    cout << "Got expected result" << endl;
  else
    cout << "Mismatch after readMathMLFromString()" << endl;

  ASTNode *new_mathml = SBML_parseFormula(result);
  char    *new_s      = writeMathMLToString(new_mathml);

  cout << "Result of writing AST:" << endl << new_s << endl;

  return 0;
}
