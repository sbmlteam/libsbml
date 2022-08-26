#ifndef VARIABLES_INCLUDED
#define VARIABLES_INCLUDED


#include <mex.h>

#ifndef USE_OCTAVE
#include <matrix.h>
#endif
#include <algorithm>

#include <map>
#include <sbml/SBMLTypes.h>

class ModelDetails;
struct GV
{
	mxArray* modelArray = NULL;
	bool freeMemory;
	ModelDetails* details;

	IdList reqdPkgPrefixes;
	IdList unreqdPkgPrefixes;

	bool fbcUsingId;
	bool fbcAddGeneProducts;
	bool onlyExpectedFields;
	bool applyUserValidation;
};


typedef std::map<const std::string, unsigned int> PkgMap;
typedef PkgMap::iterator  PkgIter;


typedef enum
{
    TYPE_BOOL
  , TYPE_CHAR
  , TYPE_DOUBLE
  , TYPE_INT
  , TYPE_ELEMENT
  , TYPE_UINT
  , TYPE_UNKNOWN
} FieldType_t;


typedef enum {
    OTHER_NAME = 0
  , SBML_NOTES
  , SBML_ANNOT
  , OTHER_NAMESPACES
  , OTHER_CVTERMS
  , SBML_MATH
  , SBML_RULE_TYPE
  , SBML_ISSET
  , SBML_MESSAGE
  , OTHER_TIME_SYMBOL
  , OTHER_DELAY_SYMBOL
  , OTHER_AVOGADRO_SYMBOL
  , OTHER_RATEOF_SYMBOL
  , OTHER_ANOMALOUS_MATH
  , FBC_ASSOCIATION

  , SBML_KNOWN_ATT_ELEM = 20

} FieldnamesType_t;


struct FieldValues_t {
  std::string fieldName;
  std::string sbmlName;
  std::string prefix;
  FieldType_t type;
  bool isSBMLAttribute;
  FieldnamesType_t fieldnameType;
  std::string strValue;
  int iValue;
  double dValue;
 };


#endif // VARIABLES_INCLUDED