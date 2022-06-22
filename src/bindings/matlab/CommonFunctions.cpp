#include "CommonFunctions.h"


//
// report error/free memory and exit when error encountered

void 
FreeMem(GV& gv)
{
  /* destroy arrays created */
  mxDestroyArray(gv.modelArray);
  gv.modelArray = NULL;
}

void
reportError(const std::string&id, const std::string& message, GV& gv)
{
  if (gv.freeMemory)
  {
    FreeMem(gv);
  }

  mexErrMsgIdAndTxt(id.c_str(), message.c_str());
}


void
displayLine(const std::string& line)
{
  mxArray* mxErrors[1];
  mxErrors[0] = mxCreateString(line.c_str());
  mexCallMATLAB(0, NULL, 1, mxErrors, "disp");
  mxDestroyArray(mxErrors[0]);
}


mxArray *
CreateIntScalar (int nValue)
{
  mxArray * pArray;
  int * panData;

  pArray = mxCreateNumericMatrix(1,1,mxINT32_CLASS, mxREAL);
  panData = (int *)mxGetData(pArray);
  panData[0] = nValue;

  return pArray;
}

const char* FIELDTYPE_STRINGS[] =
{
      "bool"
    , "char"
    , "double"
    , "int"
    , "structure"
    , "uint"
};


FieldType_t
getFieldType(const char* type)
{
  if (type != NULL)
  {
    const FieldType_t lo = TYPE_BOOL;
    const FieldType_t hi = (const FieldType_t)(TYPE_UNKNOWN - 1);

    return (FieldType_t)util_bsearchStringsI(FIELDTYPE_STRINGS, type, lo, hi);
  }
  else
    return TYPE_UNKNOWN;
}

// only used by OutputSBML
bool getRequiredStatus(const std::string& prefix, GV& gv)
{
  bool required = false;

  if (gv.reqdPkgPrefixes.contains(prefix))
  {
    required = true;
  }

  return required;
}

void populatePackageLists(GV& gv)
{
  //reqdPkgPrefixes.append("comp");
  //reqdPkgPrefixes.append("spatial");

  gv.unreqdPkgPrefixes.append("fbc");
  gv.unreqdPkgPrefixes.append("qual");
  gv.unreqdPkgPrefixes.append("groups");
}

// only used by OutputSBML
bool
isUnknownType(std::string tc)
{
  // TO DO 
  if (tc == "(Unknown SBML Type)")
    return true;
  else if (tc == "(Unknown SBML Fbc Type)")
    return true;
  else if (tc == "(Unknown SBML Groups Type)")
    return true;
  else if (tc == "(Unknown SBML Qual Type)")
    return true;
  else
    return false;
}

