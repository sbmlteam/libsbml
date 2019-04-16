

%{

#include <sbml/common/libsbml-config-common.h>


#include <sbml/math/ASTNodeType.h>
#include <sbml/math/ASTNode.h>
#include <sbml/math/MathML.h>
#include <sbml/math/L3FormulaFormatter.h>
#include <sbml/math/FormulaFormatter.h>
#include <sbml/math/FormulaParser.h>
#include <sbml/math/L3Parser.h>
#include <sbml/math/L3ParserSettings.h>
#include <sbml/math/DefinitionURLRegistry.h>
#include <sbml/util/MathFilter.h>


%}


  %include sbml/math/ASTNodeType.h
  %include sbml/math/ASTNode.h
  %include sbml/math/MathML.h
  %include sbml/math/FormulaParser.h
  %include sbml/math/L3FormulaFormatter.h
  %include sbml/math/FormulaFormatter.h
  %include sbml/math/L3Parser.h
  %include sbml/math/L3ParserSettings.h
  %include sbml/math/DefinitionURLRegistry.h
  %include sbml/util/MathFilter.h

  %include <sbml/extension/ASTBasePlugin.h>  
