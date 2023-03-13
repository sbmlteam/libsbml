
#include "StructureFields.h"
#include "CommonFunctions.h"

#include <sbml/SBMLReader.h>
#include <sbml/SBMLTypes.h>
#include <sbml/xml/XMLNode.h>
#include <sbml/math/ASTNode.h>
#include <sbml/extension/SBasePlugin.h>
#include <sbml/extension/SBMLExtensionRegistry.h>
#ifdef USE_FBC
#include <sbml/packages/fbc/sbml/FbcAssociation.h>
#endif // USE_FBC



#ifdef USE_OCTAVE
// this function is not implemented in octave
mxArray *mexCallMATLABWithTrap(int nlhs, mxArray *plhs[], int nrhs, mxArray *prhs[], const char *functionName) {
    mxArray *mx = NULL;
    const char *fields[] = {"identifier", "message", "case", "stack"};
    mexSetTrapFlag(1);
    if (mexCallMATLAB(nlhs, plhs, nrhs, prhs, functionName)) {
        mx = mxCreateStructMatrix(1, 1, 4, fields);
        mxSetFieldByNumber(mx, 0, 0, mxCreateString("MATLAB:error"));
        mxSetFieldByNumber(mx, 0, 1, mxCreateString(functionName));
        mxSetFieldByNumber(mx, 0, 2, mxCreateCellMatrix(0, 0));
        mxSetFieldByNumber(mx, 0, 3, mxCreateStructMatrix(0, 1, 0, NULL));
        return mx;
    }
    else {
        return NULL;
    }
}
#endif

//////////////////////////////////////////////////////////////////////////////
//
// class to store the structure/retrieve fields and write the SBML Model

// only used by OutputSBML
StructureFields::StructureFields(SBase* obj, mxArray* structure, GV& gv_) 
  : gv(gv_)
  ,  mSBase ( NULL)
  , mxStructure ( NULL )
  , sbmlTC ("")
{
  mSBase = obj;
  mxStructure = structure;

  determineTypeCode();
  populateFields();
}

// only used by OutputSBML
StructureFields::StructureFields(SBase* obj, GV& gv_) 
  : gv(gv_)
  , mSBase(NULL)
  , mxStructure ( NULL )
  , sbmlTC ("")
{
  mSBase = obj;

  determineTypeCode();
  populateFields();
}

// only used by TranslateSBML
StructureFields::StructureFields(std::string& tc, GV& gv_)
	: gv(gv_)
	, mSBase(NULL)
  , mxStructure ( NULL )
  , sbmlTC ( tc )
{
  populateFields();
}


StructureFields::~StructureFields()
{
  mFields.clear();
  // must not do this as it can be a part of a larger model
//  delete mSBase;

  // MATLAB says dont call destroy array in a destructor
  // https://uk.mathworks.com/help/matlab/matlab_external/memory-management-issues.html
  //mxDestroyArray(mxStructure);
  //mxDestroyArray(mxFieldnames);
  //mxDestroyArray(mxDefaultValues);
  //mxDestroyArray(mxValueTypes);
}

void
StructureFields::freeStructureMemory()
{
  mxDestroyArray(mxFieldnames);
  mxDestroyArray(mxDefaultValues);
  mxDestroyArray(mxValueTypes);
  // do not delete in output as the structure is being recursively used

  // HACK: this seems strange and we need to figure out why this is 
  // the following line was outcommented in OutputSBML
  // for now leave it out here too
  //if (mxStructure) mxDestroyArray(mxStructure);
  
  
}

void
StructureFields::determineTypeCode()
{
  if (!sbmlTC.empty()) return;
  else if (mSBase == NULL) return;

  PkgMap pm = gv.details->getPackages();

  sbmlTC = SBMLTypeCode_toString(mSBase->getTypeCode(), "core");
  // check whether we are using a package object
  PkgIter it = pm.begin();

  while (isUnknownType(sbmlTC) && it != pm.end())
  {
    sbmlTC = SBMLTypeCode_toString(mSBase->getTypeCode(), (it->first).c_str());
    sbmlTC[0] = tolower(sbmlTC[0]);
    ++it;
  }
}

void
StructureFields::populateFields()
{
  // the array size will need to accomodate all packages
  PkgMap pm = gv.details->getPackages();
  mxArray *mxOutputs[4];
  unsigned int numPlugins = (unsigned int)(pm.size());
  if (numPlugins > 0)
  {
    int numberInputs = 5;

    mxArray *mxInput[5];

    std::string id = std::string("StructureFields:populateFields:") + sbmlTC;

    // hack for level 1 rules
    if (sbmlTC == "AssignmentRule" || sbmlTC == "RateRule")
    {
      int L1TC = ((Rule*)(mSBase))->getL1TypeCode();
      if (L1TC == SBML_UNKNOWN)
      {
        mxInput[0] = mxCreateString(sbmlTC.c_str());
      }
      else
      {
        mxInput[0] = mxCreateString(SBMLTypeCode_toString(L1TC, "core"));
      }
    }
    else
    {
      mxInput[0] = mxCreateString(sbmlTC.c_str());
    }
    mxInput[1] = CreateIntScalar(gv.details->getLevel());
    mxInput[2] = CreateIntScalar(gv.details->getVersion());
    mwSize dims[1] = { numPlugins };
    mxInput[3] = mxCreateCellArray(1, dims);
    mxInput[4] = mxCreateDoubleMatrix(1, numPlugins, mxREAL);
    double *pinput4 = mxGetPr(mxInput[4]);
    unsigned int inputCount = 0;
    for (PkgIter it = pm.begin(); it != pm.end(); ++it)
    {
      mxSetCell(mxInput[3], inputCount, mxCreateString((it->first).c_str()));
      pinput4[inputCount] = it->second;
      inputCount++;
    }

    mxArray * exception = NULL;
    exception = mexCallMATLABWithTrap(4, mxOutputs, numberInputs, mxInput, "getStructure");
    if (exception != 0)
    {
      mexCallMATLAB(0, (mxArray **)NULL, 1, &exception, "throw");

      reportError(id, "Failed to get fieldnames", gv);
    }
    mxDestroyArray(mxInput[0]);
    mxDestroyArray(mxInput[1]);
    mxDestroyArray(mxInput[2]);
    mxDestroyArray(mxInput[3]);
    mxDestroyArray(mxInput[4]);
  }
  else
  {
    int numberInputs = 3;

    mxArray *mxInput[3];

    std::string id = std::string("StructureFields:populateFields:") + sbmlTC;

    // hack for level 1 rules
    if (sbmlTC == "AssignmentRule" || sbmlTC == "RateRule")
    {
      int L1TC = ((Rule*)(mSBase))->getL1TypeCode();
      if (L1TC == SBML_UNKNOWN)
      {
        mxInput[0] = mxCreateString(sbmlTC.c_str());
      }
      else
      {
        mxInput[0] = mxCreateString(SBMLTypeCode_toString(L1TC, "core"));
      }
    }
    else
    {
      mxInput[0] = mxCreateString(sbmlTC.c_str());
    }
    mxInput[1] = CreateIntScalar(gv.details->getLevel());
    mxInput[2] = CreateIntScalar(gv.details->getVersion());

    mxArray * exception = NULL;
    exception = mexCallMATLABWithTrap(4, mxOutputs, numberInputs, mxInput, "getStructure");
    if (exception != 0)
    {
      mexCallMATLAB(0, (mxArray **)NULL, 1, &exception, "throw");

      reportError(id, "Failed to get fieldnames", gv);
    }
    mxDestroyArray(mxInput[0]);
    mxDestroyArray(mxInput[1]);
    mxDestroyArray(mxInput[2]);
  }

  mxFieldnames = mxDuplicateArray(mxOutputs[0]);
  mxDefaultValues = mxDuplicateArray(mxOutputs[1]);
  mxValueTypes = mxDuplicateArray(mxOutputs[2]);
  nNumberFields = (int)mxGetScalar(mxOutputs[3]);

  mxDestroyArray(mxOutputs[0]);
  mxDestroyArray(mxOutputs[1]);
  mxDestroyArray(mxOutputs[2]);
  mxDestroyArray(mxOutputs[3]);


  // get structure returns fieldnames, default values and value types
  // fieldnames are an array of arrays giving values for several aspects of the structure 
  // 		SBMLfieldnames = { {'typecode','','',0,0}, ...
  //{'metaid', 'metaid', '', 1, 20}, ...
  //{'notes', 'notes', '', 1, 1}, ...
  //{'annotation', 'annotation', '', 1, 2}, ...
  //{'formula', 'formula', '', 1, 5}, ...
  //{'variable', 'variable', '', 1, 20}, ...
  //{'species', 'species', '', 1, 20}, ...
  //{'compartment', 'compartment', '', 1, 20}, ...
  //{'name', 'name', '', 1, 20}, ...
  //{'units', 'units', '', 1, 20}, ...
  //
  //{name of the field, sbml name, prefix, is sbml attribute, type enum}

  for (unsigned int i = 0; i < nNumberFields; ++i)
  {
    FieldValues_t field;
    field.fieldName = getFieldname(i, "populate");
    field.type = getValueType(i, "populate");
    field.sbmlName = getSBMLName(i, "populate");
    field.prefix = getSBMLPrefix(i, "populate");
    field.isSBMLAttribute = getIsSBMLAttribute(i, "populate");
    field.fieldnameType = getNameEnumType(i, "populate");
    switch (field.type)
    {
    case TYPE_BOOL:
    case TYPE_INT:
    case TYPE_UINT:
      getDefaultValue(i, "populate", field.iValue);
      break;
    case TYPE_CHAR:
    case TYPE_UNKNOWN:
      field.strValue = getDefaultValue(i, "populate");
      break;
    case TYPE_DOUBLE:
      getDefaultValue(i, "populate", field.dValue);
      break;
    default:
      break;
    }
    mFields.push_back(field);
  }
}


const std::string
StructureFields::getSBMLPrefix(unsigned int i, const std::string& id)
{
  mxArray* mxField = mxGetCell(mxFieldnames, i);
  mxArray* mxAttPrefix = mxGetCell(mxField, 2);
  size_t nBuflen = (mxGetM(mxAttPrefix)*mxGetN(mxAttPrefix) + 1);
  char * fieldname = (char *)mxCalloc(nBuflen, sizeof(char));
  if (mxGetString(mxAttPrefix, (char *)fieldname, (mwSize)(nBuflen)) != 0)
  {
    reportError(id, "Failed in getSBMLPrefix", gv);
  }
  const std::string f = std::string(fieldname);
  mxFree(fieldname);
  return f;
}

const std::string
StructureFields::getSBMLName(unsigned int i, const std::string& id)
{
  mxArray* mxField = mxGetCell(mxFieldnames, i);
  mxArray* mxAttPrefix = mxGetCell(mxField, 1);
  size_t nBuflen = (mxGetM(mxAttPrefix)*mxGetN(mxAttPrefix) + 1);
  char * fieldname = (char *)mxCalloc(nBuflen, sizeof(char));
  if (mxGetString(mxAttPrefix, (char *)fieldname, (mwSize)(nBuflen)) != 0)
  {
    reportError(id, "Failed in getSBMLName", gv);
  }
  const std::string f = std::string(fieldname);
  mxFree(fieldname);
  return f;
}

bool
StructureFields::getIsSBMLAttribute(unsigned int i, const std::string& id)
{
  mxArray* mxField = mxGetCell(mxFieldnames, i);
  mxArray* mxSBMLAtt = mxGetCell(mxField, 3);
  int value = (int)mxGetScalar(mxSBMLAtt);
  if (value == 0) return false;
  else return true;
}

const FieldnamesType_t 
StructureFields::getNameEnumType(unsigned int i, const std::string& id)
{
  mxArray* mxField = mxGetCell(mxFieldnames, i);
  mxArray* mxSBMLAtt = mxGetCell(mxField, 4);
  int value = (int)mxGetScalar(mxSBMLAtt);
  return (FieldnamesType_t)(value);

}

const std::string
StructureFields::getFieldname(unsigned int i, const std::string& id)
{
  mxArray* mxField = mxGetCell(mxFieldnames, i);
  mxArray* mxName = mxGetCell(mxField, 0);
  size_t nBuflen = (mxGetM(mxName)*mxGetN(mxName)+1);
  char * fieldname = (char *) mxCalloc(nBuflen, sizeof(char));
  if (mxGetString(mxName, (char *) fieldname, (mwSize)(nBuflen)) != 0)
  {
    reportError(id, "Failed in GetFieldname", gv);
  }
  const std::string f = std::string(fieldname); 
  mxFree(fieldname);
  return f;
}

const std::string
StructureFields::getDefaultValue(unsigned int i, const std::string& id)
{
  mxArray* mxName = mxGetCell(mxDefaultValues, i);
  size_t nBuflen = (mxGetM(mxName)*mxGetN(mxName)+1);
  char * fieldname = (char *) mxCalloc(nBuflen, sizeof(char));
  if (mxGetString(mxName, (char *) fieldname, (mwSize)(nBuflen)) != 0)
  {
    reportError(id, "Failed in GetDefaultValue", gv);
  }
  const std::string f = std::string(fieldname); 
  mxFree(fieldname);
  return f;
}

void
StructureFields::getDefaultValue(unsigned int i, const std::string& id, double& value)
{
  mxArray* mxName = mxGetCell(mxDefaultValues, i);
  value = (double) mxGetScalar(mxName);
}

void
StructureFields::getDefaultValue(unsigned int i, const std::string& id, int& value)
{
  mxArray* mxName = mxGetCell(mxDefaultValues, i);
  value = (int) mxGetScalar(mxName);
}

const FieldType_t
StructureFields::getValueType(unsigned int i, const std::string& id)
{
  mxArray* mxName = mxGetCell(mxValueTypes, i);
  size_t nBuflen = (mxGetM(mxName)*mxGetN(mxName)+1);
  char * fieldname = (char *) mxCalloc(nBuflen, sizeof(char));
  if (mxGetString(mxName, (char *) fieldname, (mwSize)(nBuflen)) != 0)
  {
    mxFree(fieldname);
    reportError(id, "Failed in GetValueType", gv);
  }
  FieldType_t ft = getFieldType(fieldname); 
  mxFree(fieldname);
  return ft;
}

//////////////////////////

// functions for creating structure from SBML

void
StructureFields::createStructure(const std::string& functionId, SBase* base, 
                                 bool usePlugin_cs, const std::string& prefix)
{
  std::string fieldname;
  unsigned int total_no = base->getNumObjects(sbmlTC);
  if (usePlugin_cs && total_no == 0)
  {
    total_no = base->getPlugin(prefix)->getNumObjects(sbmlTC);
    // we may have an SBase plugin
    if (total_no == 0)
    {
      SBase * sbase = static_cast<SBase*>(base);
      total_no = sbase->getPlugin(prefix)->getNumObjects(sbmlTC);
    }
  }
  mwSize dims[2] = {1, total_no};

  char **field_names = (char**)(safe_malloc(nNumberFields * sizeof(char*)));
  for (unsigned int i = 0; i < nNumberFields; ++i)
  {
    fieldname = mFields.at(i).fieldName;
    field_names[i] = (char*)(safe_malloc((fieldname.size() * sizeof(char))+ 1));
    field_names[i] = safe_strdup(fieldname.c_str());
  }

  mxStructure = mxCreateStructArray(2, dims, (int)nNumberFields, (const char**)(field_names));
  safe_free(field_names);

  for (unsigned int i = 0; i < total_no; ++i)
  {
    SBase* child = base->getObject(sbmlTC, i);
    if (usePlugin_cs)
    {
      child = base->getPlugin(prefix)->getObject(sbmlTC, i);
    }
    populateStructure(functionId, child, i);
  }

}

void
StructureFields::populateStructure(const std::string& functionId, SBase* base, unsigned int index)
{
  if (base == NULL) return;

  FieldType_t type;
  FieldnamesType_t nameType;

  for (unsigned int i = 0; i < nNumberFields; ++i)
  {
    FieldValues_t field = mFields.at(i);
    type = field.type;
    nameType = field.fieldnameType;
    bool usePlugin_ps = usingPlugin(field.prefix, base, field.fieldName);

    if (type == TYPE_ELEMENT)
    {
      switch (nameType) {
      case OTHER_NAMESPACES:
          mxSetField(mxStructure, index, field.fieldName.c_str(), getNamespacesStructure());
          break;
      case OTHER_CVTERMS:
          mxSetField(mxStructure, index, field.fieldName.c_str(), getCVTermsStructure(base));
          break;
      default:
          StructureFields *sf = new StructureFields(field.sbmlName, gv);
          sf->createStructure(functionId + ":" + field.fieldName, base, usePlugin_ps, field.prefix);
          mxSetField(mxStructure, index, field.fieldName.c_str(), mxDuplicateArray(sf->getStructure()));
          sf->freeStructureMemory();
          delete sf;
          break;
      }
    }
    else if (field.fieldnameType == OTHER_ANOMALOUS_MATH)
    {
      addAnomalousChildStructure(functionId, base, index, field);
    }
    else
    {
      addStructureField(functionId, base, index, field, usePlugin_ps);
    }

  }
}

void 
StructureFields::addAnomalousChildStructure(const std::string& functionId, SBase* base, 
                         unsigned int index, FieldValues_t field)
{
  std::string value;
  SBase* child = base->getObject(field.fieldName, 0);
  if (child == NULL)
  {
    value = field.strValue;
  }
  else
  {
    value = getMathString(child);
  }
  
  mxSetField(mxStructure, index, field.fieldName.c_str() ,mxCreateString(value.c_str())); 
}

mxArray* 
StructureFields::getNamespacesStructure()
{
  mxArray* mxNSReturn = NULL;

  const XMLNamespaces * NS = gv.details->getNamespaces()->getNamespaces();
  int n = NS->getLength();
  mwSize dims[2] = {1, (mwSize)(n)};

  /* fields within a namespace structure */
  const int nNoFields = 2;
  const char *field_names[] = {	
    "prefix", 
    "uri"
  };


  const char * pacPrefix = NULL;
  const char * pacURI = NULL;

  int i;

  mxNSReturn = mxCreateStructArray(2, dims, nNoFields, field_names);

  for (i = 0; i < n; ++i)
  {
    pacPrefix = safe_strdup(NS->getPrefix(i).c_str());
    pacURI    = safe_strdup(NS->getURI(i).c_str());

    /**
    * check for NULL strings - Matlab doesnt like creating 
    * a string that is NULL
    */
    if (pacPrefix == NULL) {
      pacPrefix = safe_strdup("");
    }
    if (pacURI == NULL) {
      pacURI = safe_strdup("");
    }

    mxSetField(mxNSReturn, i, "prefix", mxCreateString(pacPrefix)); 
    mxSetField(mxNSReturn, i, "uri",    mxCreateString(pacURI)); 

    safe_free((void*)(pacPrefix));
    safe_free((void*)(pacURI));
  }

  return mxNSReturn;
}

mxArray*
createCVTermStructure(int num)
{
  mxArray* mxCVTermReturn;
  mwSize dims[2] = {1, (mwSize)(num)};

  /* fields within a cvterm structure */
  const int nNoFields = 4;
  const char *field_names[] = {
    "qualifierType",
    "qualifier", 
    "resources",
    "cvterms"
  };

  mxCVTermReturn = mxCreateStructArray(2, dims, nNoFields, field_names);

  return mxCVTermReturn;
}



void
  addCVTerm (mxArray* mxCVTermReturn, int i, CVTerm* cv)
{
  const char * pacQualifier = NULL;
  const char * pacQualifierType = NULL;
  mxArray* mxResources = NULL;

  if (cv->getQualifierType() == BIOLOGICAL_QUALIFIER)
  {
    std::string bq = "biological";
    pacQualifierType = (char*)(safe_malloc((bq.size() * sizeof(char))+ 1));
    pacQualifierType = safe_strdup(bq.c_str());

    size_t s = sizeof((const char *)(BiolQualifierType_toString(cv->getBiologicalQualifierType()))) * sizeof(char);
    pacQualifier = (char*)(safe_malloc(s + 1));
    pacQualifier = safe_strdup(BiolQualifierType_toString(cv->getBiologicalQualifierType()));
  }
  else if  (cv->getQualifierType() == MODEL_QUALIFIER)
  {
    std::string bq = "model";
    pacQualifierType = (char*)(safe_malloc((bq.size() * sizeof(char))+ 1));
    pacQualifierType = safe_strdup(bq.c_str());

    size_t s = sizeof((const char *)(ModelQualifierType_toString(cv->getModelQualifierType()))) * sizeof(char);
    pacQualifier = (char*)(safe_malloc(s + 1));
    pacQualifier = safe_strdup(ModelQualifierType_toString(cv->getModelQualifierType()));
  }
  else
  {
    std::string bq = "unknown";
    pacQualifierType = (char*)(safe_malloc((bq.size() * sizeof(char))+ 1));
    pacQualifierType = safe_strdup(bq.c_str());

    pacQualifier = (char*)(safe_malloc((bq.size() * sizeof(char))+ 1));
    pacQualifier = safe_strdup(bq.c_str());

  }
  mwSize num = cv->getNumResources(); 
  std::string fieldname;

  char **resources = (char**)(safe_malloc(num * sizeof(char*)));
  for (unsigned int j = 0; j < num; j++)
  {
    fieldname = cv->getResourceURI(j);
    size_t s = (fieldname.size() * sizeof(char)) + 1;
    resources[j] = (char*)(safe_malloc(s));
    resources[j] = safe_strdup(fieldname.c_str());
  }

  mxResources = mxCreateCellMatrix(1, num);
  for (unsigned int j = 0; j < num; j++)
  {
    mxSetCell(mxResources, j, mxCreateString(resources[j]));
  }

  mxSetField(mxCVTermReturn, i, "qualifierType", mxCreateString(pacQualifierType)); 
  mxSetField(mxCVTermReturn, i, "qualifier", mxCreateString(pacQualifier)); 
  mxSetField(mxCVTermReturn, i, "resources",   mxResources); 

  unsigned int numNested = cv->getNumNestedCVTerms();
  if (cv->getNumNestedCVTerms() > 0)
  {
    mxArray* mxNested = createCVTermStructure(numNested);
    for (unsigned int j = 0; j < numNested; j++)
    {
      addCVTerm(mxNested, j, cv->getNestedCVTerm(j));
    }
    mxSetField(mxCVTermReturn, i, "cvterms",   mxNested); 
  }

  safe_free((void*)(pacQualifier));
  safe_free((void*)(pacQualifierType));
  safe_free(resources);
}


mxArray* 
StructureFields::getCVTermsStructure(SBase* base)
{
  mxArray* mxCVTermReturn = NULL;
  
  int n = base->getNumCVTerms();
  if (n > 0)
  {
    mxCVTermReturn = createCVTermStructure(n);
  }

  for (int i = 0; i < n; ++i)
  {
    CVTerm * cv = base->getCVTerm(i);
    addCVTerm(mxCVTermReturn, i, cv);
  }

  return mxCVTermReturn;
}


void
StructureFields::addStructureField(const std::string& functionId, SBase* base,
  unsigned int index, FieldValues_t field,
  bool usePlugin_sf)
{
  std::string value;
  int ivalue;
  unsigned int uvalue;
  bool bvalue;
  double dvalue;
  switch (field.type)
  {
  case TYPE_UNKNOWN:

  case TYPE_CHAR:
    value = getStringValue(functionId, base, field, usePlugin_sf);
    mxSetField(mxStructure, index, field.fieldName.c_str(), mxCreateString(value.c_str()));
    break;
  case TYPE_BOOL:
    bvalue = getBoolValue(functionId, base, field, usePlugin_sf);
    mxSetField(mxStructure, index, field.fieldName.c_str(), CreateIntScalar(bvalue));
    break;
  case TYPE_UINT:
    uvalue = getUintValue(functionId, base, field, usePlugin_sf);
    mxSetField(mxStructure, index, field.fieldName.c_str(), CreateIntScalar(uvalue));
    break;
  case TYPE_INT:
    ivalue = getIntValue(functionId, base, field, usePlugin_sf);
    mxSetField(mxStructure, index, field.fieldName.c_str(), CreateIntScalar(ivalue));
    break;
  case TYPE_DOUBLE:
    dvalue = getDoubleValue(functionId, base, field, usePlugin_sf);
    mxSetField(mxStructure, index, field.fieldName.c_str(), mxCreateDoubleScalar(dvalue));
    break;
  case TYPE_ELEMENT:
  default:
    break;
  }
}

const std::string
StructureFields::getStringValue(const std::string& functionId, SBase* base,
  FieldValues_t field,
  bool usePlugin)
{
  std::string value;

  if (field.isSBMLAttribute)
  {
    bool useDefault = true;
    switch (field.fieldnameType)
    {
    case SBML_NOTES:
      value = base->getNotesString();
      useDefault = false;
      break;
    case SBML_ANNOT:
      value = base->getAnnotationString();
      useDefault = false;
      break;
    case SBML_MESSAGE:
      value = base->getMessageString();
      useDefault = false;
      break;
    case SBML_MATH:
      value = getMathString(base);
      useDefault = false;
      break;
    default:
      if (!usePlugin && base->isSetAttribute(field.sbmlName))
      {
        base->getAttribute(field.sbmlName, value);
        useDefault = false;
      }
      else if (usePlugin && base->getPlugin(field.prefix)->isSetAttribute(field.sbmlName))
      {
        base->getPlugin(field.prefix)->getAttribute(field.sbmlName, value);
        useDefault = false;
      }
      break;

    }
#ifdef USE_FBC

    if (field.fieldnameType == FBC_ASSOCIATION)
    {
      value = static_cast<FbcAssociation*>(base)->toInfix(gv.fbcUsingId);//FbcAssociation_toInfix(static_cast<FbcAssociation*>(base));
      useDefault = false;
    }
#endif

    if (useDefault)
    {
      value = field.strValue;// getDefaultValue(fieldIndex, functionId);
    }
  }
  else 
  {
    switch (field.fieldnameType)
    {
    case SBML_RULE_TYPE:
      value = field.strValue;// getDefaultValue(fieldIndex, functionId);
      if (field.fieldName == "type" && base->getTypeCode() == SBML_RATE_RULE)
      {
        value = "rate";
      }
      break;
    case OTHER_AVOGADRO_SYMBOL:
      if (!gv.details->getAvogadroSymbol().empty())
      {
        value = gv.details->getAvogadroSymbol();
      }
      else
      {
        value = field.strValue;// getDefaultValue(fieldIndex, functionId);
      }
      break;
    case OTHER_DELAY_SYMBOL:
      if (!gv.details->getDelaySymbol().empty())
      {
        value = gv.details->getDelaySymbol();
      }
      else
      {
        value = field.strValue;// getDefaultValue(fieldIndex, functionId);
      }
      break;
    case OTHER_TIME_SYMBOL:
      if (!gv.details->getTimeSymbol().empty())
      {
        value = gv.details->getTimeSymbol();
      }
      else
      {
        value = field.strValue;// getDefaultValue(fieldIndex, functionId);
      }
      break;
    case OTHER_RATEOF_SYMBOL:
      if (!gv.details->getRateOfSymbol().empty())
      {
        value = gv.details->getRateOfSymbol();
      }
      else
      {
        value = field.strValue;// getDefaultValue(fieldIndex, functionId);
      }
      break;
    default:
      value = field.strValue;// getDefaultValue(fieldIndex, functionId);
     /* hack for rules that all use the same fieldnames/defaults */
      if (value == "SBML_ALGEBRAIC_RULE")
      {
        value = getRuleTypeCode(base);
      }
      else if (value == "SBML_FBC_ASSOCIATION")
      {
        value = getAssociationTypeCode(base);
      }
      break;
    }
  }

  return (const std::string)(value);
}

double
StructureFields::getDoubleValue(const std::string& functionId, SBase* base,
  FieldValues_t field,
  bool usePlugin)
{
  double value;

  if (field.isSBMLAttribute)
  {
    if (!usePlugin && base->isSetAttribute(field.sbmlName))
    {
      base->getAttribute(field.sbmlName, value);
    }
    else if (usePlugin && base->getPlugin(field.prefix)->isSetAttribute(field.sbmlName))
    {
      base->getPlugin(field.prefix)->getAttribute(field.sbmlName, value);
    }
    else
    {
      value = field.dValue;
    }
  }
  else
  {
    value = field.dValue;
  }

  return value;
}


int
StructureFields::getIntValue(const std::string& functionId, SBase* base,
  FieldValues_t field,
  bool usePlugin)
{
  int value;

  if (field.isSBMLAttribute)
  {
    if (!usePlugin && base->isSetAttribute(field.sbmlName))
    {
      base->getAttribute(field.sbmlName, value);
    }
    else if (usePlugin && base->getPlugin(field.prefix)->isSetAttribute(field.sbmlName))
    {
      base->getPlugin(field.prefix)->getAttribute(field.sbmlName, value);
    }
    else
    {
      value = field.iValue;
    }
  }
  else
  {
    value = field.iValue;
  }

  return value;
}


unsigned int
StructureFields::getUintValue(const std::string& functionId, SBase* base,
  FieldValues_t field,
  bool usePlugin)
{
  unsigned int value;
  int ivalue;
  bool useDefault = false;
  if (field.isSBMLAttribute)
  {
    if (!usePlugin && base->isSetAttribute(field.sbmlName))
    {
      base->getAttribute(field.sbmlName, value);
    }
    else if (usePlugin && base->getPlugin(field.prefix)->isSetAttribute(field.sbmlName))
    {
      base->getPlugin(field.prefix)->getAttribute(field.sbmlName, value);
    }
    else
    {
      ivalue = field.iValue;// getDefaultValue(fieldIndex, functionId, ivalue);
      useDefault = true;
    }
  }
  else
  {
    ivalue = field.iValue;
    useDefault = true;
  }

  if (useDefault)
  {
    return (unsigned int)(ivalue);
  }
  else
  {
    return value;
  }
}


bool
StructureFields::getBoolValue(const std::string& functionId, SBase* base,
  FieldValues_t field,
  bool usePlugin)
{
  bool bvalue;
  int value;
  if (field.isSBMLAttribute)
  {
    if (!usePlugin && base->isSetAttribute(field.sbmlName))
    {
      base->getAttribute(field.sbmlName, bvalue);
      return bvalue;
    }
    else if (usePlugin && base->getPlugin(field.prefix)->isSetAttribute(field.sbmlName))
    {
      base->getPlugin(field.prefix)->getAttribute(field.sbmlName, bvalue);
      return bvalue;
    }
    else
    {
      value = field.iValue;// getDefaultValue(fieldIndex, functionId, value);
    }
  }
  else if (field.fieldnameType == SBML_ISSET)
  {
    if (!usePlugin)
    {
      value = base->isSetAttribute(field.sbmlName);
    }
    else
    {
      value = base->getPlugin(field.prefix)->isSetAttribute(field.sbmlName);
    }
  }
  else
  {
    value = field.iValue;// getDefaultValue(fieldIndex, functionId, value);
  }

  return (value == 0) ? false : true;
}


std::string
StructureFields::getMathString(SBase* base)
{
  const ASTNode* ast = base->getMath();
  lookForCSymbols(const_cast<ASTNode*>(ast));
  char * formula = SBML_formulaToString(ast);
  char * matlab = GetMatlabFormula(formula, sbmlTC);
  std::string math = std::string(matlab);
  mxFree(matlab);
  return math;

}

char * 
StructureFields::GetMatlabFormula(char * pacFormula, std::string object)
{
  mxArray *mxInput[1];
  mxArray *mxOutput[1];
  int nStatus;
  size_t nBuflen;
  char * formula;

  /* convert MathML infix formula to MATLAB */

  mxInput[0] = mxCreateString(pacFormula);
  nStatus = mexCallMATLAB(1, mxOutput, 1, mxInput, "CheckAndConvert");

  if (nStatus != 0)
  {
    std::string id = std::string("TranslateSBML:GetMatlabFormula:") + object;
    reportError(id.c_str(), "Failed to convert formula", gv);
  }

  /* get the formula returned */
  nBuflen = (mxGetM(mxOutput[0])*mxGetN(mxOutput[0])+1);
  formula = (char *) mxCalloc(nBuflen, sizeof(char));
  nStatus = mxGetString(mxOutput[0], (char *) formula, (mwSize)(nBuflen));

  if (nStatus != 0)
  {
    std::string id = std::string("TranslateSBML:GetMatlabFormula") + object;
    reportError(id.c_str(), "Cannot copy formula", gv);
  }

  mxDestroyArray(mxInput[0]);
  mxDestroyArray(mxOutput[0]);

  return formula;
}

void
StructureFields::lookForCSymbols(ASTNode* math)
{
  if (math == NULL) return;

  unsigned int nChild = math->getNumChildren();
  ASTNodeType_t type = math->getType();

  switch (type)
  {
  case AST_NAME_AVOGADRO:
    dealWithAvogadroSymbol(math);
    break;
  case AST_NAME_TIME:
    dealWithTimeSymbol(math);
    break;
  case AST_FUNCTION_DELAY:
    dealWithDelaySymbol(math);
    break;
  case AST_FUNCTION_RATE_OF:
    dealWithRateOfSymbol(math);
    break;
  default:
    break;
  }

  for (unsigned int i = 0; i < nChild; ++i)
  {
    ASTNode* child = math->getChild(i);
    lookForCSymbols(child);
  }
}

void 
StructureFields::dealWithAvogadroSymbol(ASTNode* math)
{
  if (math == NULL) return;
  if (gv.details->getAvogadroSymbol().empty())
  {
    gv.details->setAvogadroSymbol(math->getName());
  }
  else
  {
    math->setName(gv.details->getAvogadroSymbol().c_str());
  }
}

void 
StructureFields::dealWithTimeSymbol(ASTNode* math)
{
  if (math == NULL) return;
  if (gv.details->getTimeSymbol().empty())
  {
    gv.details->setTimeSymbol(math->getName());
  }
  else
  {
    math->setName(gv.details->getTimeSymbol().c_str());
  }
}

void 
StructureFields::dealWithDelaySymbol(ASTNode* math)
{
  if (math == NULL) return;
  if (gv.details->getDelaySymbol().empty())
  {
    gv.details->setDelaySymbol(math->getName());
  }
  else
  {
    math->setName(gv.details->getDelaySymbol().c_str());
  }
}

void 
StructureFields::dealWithRateOfSymbol(ASTNode* math)
{
  if (math == NULL) return;
  if (gv.details->getRateOfSymbol().empty())
  {
    gv.details->setRateOfSymbol(math->getName());
  }
  else
  {
    math->setName(gv.details->getRateOfSymbol().c_str());
  }
}

//////////////////////////////////////////////////////////////

// general need to know function

bool 
StructureFields::usingPlugin(const std::string& prefix, SBase* base, const std::string& name)
{
  bool usePlugin = false;
  if (prefix.size() > 0)
  {
    if (mSBase != NULL && mSBase->getPackageName() != prefix)
    {
      usePlugin = true;
    }
    // need to account for a sbase plugin from same pkg which will be on all elements
    else if (base != NULL && base->getPackageName() != prefix) // THIS FIXES BUT WONT READ 
    {
      usePlugin = true;
    }
#ifdef USE_FBC
    if (name == "fbc_keyValuePair" || name == "fbc_kvp_xmlns")
    {
      usePlugin = true;
    }
#endif
  }

  return usePlugin;
}

//////////////////////////

// functions for creating SBML from structure
void
StructureFields::addAttributes(const std::string& functionId, unsigned int index,
                               unsigned int total_no)
{
  FieldType_t type;
  FieldnamesType_t nameType;

  for (unsigned int i = 0; i < nNumberFields; ++i)
  {
    FieldValues_t field = mFields.at(i);
    type = field.type;
    nameType = field.fieldnameType;
    if (nameType != OTHER_CVTERMS && !field.isSBMLAttribute)
    {
      continue;
    }
    if (type == TYPE_ELEMENT)
    {
      if (nameType == OTHER_CVTERMS)
      {
        addCVTerms(index);
      }
      else 
      {
       addChildElement(field, index);
      }
    }
    else if (field.fieldnameType == OTHER_ANOMALOUS_MATH)
    {
      addAnomalousChild(field);
    }
    else 
    {
      setAttribute(field, index, total_no);
    }

  }
}

CVTerm *
getCVTerm(mxArray* mxCVTerms, unsigned int i, GV& gv)
{
  CVTerm *cv;
  std::string qualType = StructureFields::readString(mxCVTerms, "qualifierType", i, gv);
  std::string qual = StructureFields::readString(mxCVTerms, "qualifier", i, gv);
  if (qualType == "biological")
  {
    cv = new CVTerm(BIOLOGICAL_QUALIFIER);
    cv->setBiologicalQualifierType(qual);
  }
  else if (qualType == "model")
  {
    cv = new CVTerm(MODEL_QUALIFIER);
    cv->setModelQualifierType(qual);
  }
  else
  {
    cv = new CVTerm();
  }

  mxArray* mxResources = mxGetField(mxCVTerms, i, "resources");
  size_t numRes = mxGetNumberOfElements(mxResources);

  for (unsigned int j = 0; j < numRes; j++)
  {
    mxArray* mxField = mxGetCell(mxResources, j);
    char *value = mxArrayToString(mxField);
    if (value != NULL)
    {
      cv->addResource(std::string(value));
    }
  }

  mxArray* mxNestedCV = mxGetField(mxCVTerms, i, "cvterms");
  if (mxNestedCV != NULL)
  {
    size_t numNested = mxGetNumberOfElements(mxNestedCV);

    for (unsigned int n = 0; n < numNested; n++)
    {
      CVTerm *nested = getCVTerm(mxNestedCV, n, gv);
      cv->addNestedCVTerm(nested);
      delete nested;
    }
  }

  return cv;
}

void
StructureFields::addCVTerms(unsigned int index)
{
  mxArray* mxCVTerms = mxGetField(mxStructure, index, "cvterms");
  if (mxCVTerms == NULL)
    return;

  size_t numCV = mxGetNumberOfElements(mxCVTerms);

  for (unsigned int i = 0; i < numCV; ++i)
  {
    CVTerm *cv = getCVTerm(mxCVTerms, i, gv);
    if (!mSBase->isSetMetaId())
      mSBase->setMetaId("temp");
    mSBase->addCVTerm(cv);
    delete cv;
  }

}

void
StructureFields::addChildElement(FieldValues_t field, unsigned int index)
{
  mxArray* mxChild = mxGetField(mxStructure, index, field.fieldName.c_str());
  SBase *pChild = NULL;

  size_t n = mxGetNumberOfElements(mxChild);
  if (mxChild == NULL) return;
  else if (n == 0) return;

  bool usePlugin = usingPlugin(field.prefix, NULL, field.fieldName.c_str());

  for (unsigned int i = 0; i < n; ++i)
  {
    // hack for rules - since a list of rules contains assignmentRule etc..
    if (field.fieldName == "rule")
    {
      pChild = mSBase->createChildObject(getRuleType(mxChild, i));
    }
    else 
    {
      if (usePlugin)
      {
        pChild = mSBase->getPlugin(field.prefix)->createChildObject(field.sbmlName);
      }
      else
      {
        pChild = mSBase->createChildObject(field.sbmlName);
      }
    }

    if (pChild != NULL)
    {
      StructureFields *sf = new StructureFields(pChild, mxChild, gv);

      std::string id = std::string("OutputSBML:addChildElement:") + sf->getTypeCode();
      sf->addAttributes(id, i, (unsigned int)n);

      sf->freeStructureMemory();
    }
  }
}

void 
StructureFields::addAnomalousChild(FieldValues_t field)
{
  std::string value = readString(field.fieldName, 0, 0);

  if (!value.empty())
  {
    SBase *pChild = mSBase->createChildObject(field.fieldName);
    if (pChild != NULL)
    {
      std::string value = readString(field.fieldName, 0, 0);
      const ASTNode* ast = getMathChild(value);
      pChild->setMath(ast);
   }
  }
}

const ASTNode*
StructureFields::getMathChild(const std::string& value)
{
  /* convert MATLAB formula to MathML infix */
  char * cvalue = convertMathFormula(value);
  L3ParserSettings settings;
  settings.setParseLog(L3P_PARSE_LOG_AS_LN);
  const ASTNode *ast = SBML_parseL3FormulaWithSettings(cvalue, &settings);
  adjustForCSymbols(const_cast<ASTNode*>(ast));
  mxFree(cvalue);
  return ast;
}

void
StructureFields::adjustForCSymbols(ASTNode * math)
{
  if (math == NULL)
  {
    return;
  }

  ASTNodeType_t type = math->getType();
  switch (type)
  {
  case AST_FUNCTION:
    if (math->getName() == gv.details->getDelaySymbol())
    {
      math->setType(AST_FUNCTION_DELAY);
    }
    else if (math->getName() == gv.details->getRateOfSymbol())
    {
      math->setType(AST_FUNCTION_RATE_OF);
    }
    break;
  case AST_NAME:
    if (math->getName() == gv.details->getTimeSymbol())
    {
      math->setType(AST_NAME_TIME);
    }
    else if (math->getName() == gv.details->getAvogadroSymbol())
    {
      math->setType(AST_NAME_AVOGADRO);
    }
    break;
  default:
    break;
  }

  for (unsigned int i = 0; i < math->getNumChildren(); ++i)
  {
    adjustForCSymbols(math->getChild(i));
  }
}


void
StructureFields::setAttribute(FieldValues_t field,
                              unsigned int index, unsigned int total_no)
{
  std::string value;
  int ivalue;
  unsigned int uvalue;
  bool bvalue;
  double dvalue;
  bool usePlugin = usingPlugin(field.prefix, NULL, field.fieldName);
  const ASTNode *ast = NULL;

  switch(field.type)
  {
  case TYPE_CHAR:
    value = readString(field.fieldName, index, total_no);
    switch (field.fieldnameType) {
    case SBML_MATH:
      ast = getMathChild(value);
      mSBase->setMath(ast);
      break;
    case SBML_NOTES:
      mSBase->setNotes(value);
      break;
    case SBML_ANNOT:
      mSBase->setAnnotation(value);
      break;
    case SBML_MESSAGE:
      mSBase->setMessage(value);
      break;
    default:
      if (usePlugin)
      {
        mSBase->getPlugin(field.prefix)->setAttribute(field.sbmlName, value);
      }
      else
      {
        mSBase->setAttribute(field.sbmlName, value);
      }
      break;
    }
    break;

  case TYPE_INT:
    ivalue = readInt(field.fieldName, index, total_no);
    if (determineStatus(field.fieldName, index))
    {
      if (usePlugin)
      {
        mSBase->getPlugin(field.prefix)->setAttribute(field.sbmlName, ivalue);
      }
      else
      {
        mSBase->setAttribute(field.sbmlName, ivalue);
      }
    }
    break;

  case TYPE_UINT:
    uvalue = readUint(field.fieldName, index, total_no);
    if (determineStatus(field.fieldName, index))
    {
      if (usePlugin)
      {
        mSBase->getPlugin(field.prefix)->setAttribute(field.sbmlName, uvalue);
      }
      else
      {
        mSBase->setAttribute(field.sbmlName, uvalue);
      }
    }
    break;

  case TYPE_DOUBLE:
    dvalue = readDouble(field.fieldName, index, total_no);
    if (determineStatus(field.fieldName, index))
    {
      if (usePlugin)
      {
        mSBase->getPlugin(field.prefix)->setAttribute(field.sbmlName, dvalue);
      }
      else
      {
        mSBase->setAttribute(field.sbmlName, dvalue);
      }
    }
    break;

  case TYPE_BOOL:
    ivalue = readInt(field.fieldName, index, total_no);
    bvalue = ivalue == 1; // ? true : false;
    if (determineStatus(field.fieldName, index))
    {
      if (usePlugin)
      {
        mSBase->getPlugin(field.prefix)->setAttribute(field.sbmlName, bvalue);
      }
      else
      {
        mSBase->setAttribute(field.sbmlName, bvalue);
      }
    }

  case TYPE_UNKNOWN:
  default:
    break;
  }
}

const std::string
StructureFields::getRuleType(mxArray* mxRuleStructure, unsigned int index)
{
  std::string value = readString(mxRuleStructure, "typecode", index, gv);
  std::string retvalue;
  if (value == "SBML_ALGEBRAIC_RULE")
  {
    retvalue = "algebraicRule";
  }
  else if (value == "SBML_ASSIGNMENT_RULE")
  {
    retvalue = "assignmentRule";
  }
  else if (value == "SBML_RATE_RULE")
  {
    retvalue = "rateRule";
  }
  else
  {
    std::string type = readString(mxRuleStructure, "type", index, gv);
    if (value == "SBML_PARAMETER_RULE")
      if (type == "scalar")
        retvalue = "parameterAssignmentRule";
      else
        retvalue = "parameterRateRule";
    else if (value == "SBML_SPECIES_CONCENTRATION_RULE")
      if (type == "scalar")
        retvalue = "speciesAssignmentRule";
      else
        retvalue = "speciesRateRule";
    else if (value == "SBML_COMPARTMENT_VOLUME_RULE")
      if (type == "scalar")
        retvalue = "compartmentAssignmentRule";
      else
        retvalue = "compartmentRateRule";
    else
      retvalue = "rule";
  }

  return retvalue;
}

std::string
StructureFields::getAssociationTypeCode(SBase* base)
{
  std::string value = SBMLTypeCode_toString(base->getTypeCode(), "fbc");
  std::string retvalue = "SBML_FBC_ASSOCIATION";
  if (value == "GeneProductRef")
  {
    retvalue = "SBML_FBC_GENE_PRODUCT_REF";
  }
  else if (value == "FbcAnd")
  {
    retvalue = "SBML_FBC_AND";
  }
  else if (value == "FbcOr")
  {
    retvalue = "SBML_FBC_OR";
  }
  return retvalue;
}

std::string
StructureFields::getRuleTypeCode(SBase* base)
{
  std::string value = SBMLTypeCode_toString(base->getTypeCode(), "core");
  std::string retvalue = "rule";
  if (value == "AlgebraicRule")
  {
    retvalue = "SBML_ALGEBRAIC_RULE";
  }
  else if (value == "AssignmentRule")
  {
    if (base->getLevel() > 1)
    {
      retvalue = "SBML_ASSIGNMENT_RULE";
    }
    else
    {
      Model *m = static_cast<Model*>(base->getAncestorOfType(SBML_MODEL));
      if (m != NULL)
      {
        if (m->getCompartment(static_cast<Rule*>(base)->getVariable()) != NULL)
        {
          retvalue = "SBML_COMPARTMENT_VOLUME_RULE";
        }
        if (m->getSpecies(static_cast<Rule*>(base)->getVariable()) != NULL)
        {
          retvalue = "SBML_SPECIES_CONCENTRATION_RULE";
        }
        if (m->getParameter(static_cast<Rule*>(base)->getVariable()) != NULL)
        {
          retvalue = "SBML_PARAMETER_RULE";
        }
      }
    }
  }
  else if (value == "RateRule")
  {
    if (base->getLevel() > 1)
    {
      retvalue = "SBML_RATE_RULE";
    }
    else
    {
      Model *m = static_cast<Model*>(base->getAncestorOfType(SBML_MODEL));
      if (m != NULL)
      {
        std::string var = static_cast<Rule*>(base)->getVariable();
        if (m->getCompartment(var) != NULL)
        {
          retvalue = "SBML_COMPARTMENT_VOLUME_RULE";
        }
        if (m->getSpecies(var) != NULL)
        {
          retvalue = "SBML_SPECIES_CONCENTRATION_RULE";
        }
        if (m->getParameter(var) != NULL)
        {
          retvalue = "SBML_PARAMETER_RULE";
        }
      }
    }
  }
  return retvalue;
}


char * 
StructureFields::convertMathFormula(const std::string& pacFormula)
{
  mxArray *mxInput[1];
  mxArray *mxOutput[1];
  int nStatus;
  size_t nBuflen;
  char * formula;

  /* convert MATLAB formula to MathML infix */

  mxInput[0] = mxCreateString(pacFormula.c_str());
  nStatus = mexCallMATLAB(1, mxOutput, 1, mxInput, "ConvertFormulaToMathML");

  if (nStatus != 0)
  {
    displayLine(pacFormula);
    std::string id = std::string("OutputSBML:GetMathMLFormula:") + sbmlTC;
    reportError(id, "Failed to convert formula", gv);
  }

  /* get the formula returned */
  nBuflen = (mxGetM(mxOutput[0])*mxGetN(mxOutput[0])+1);
  formula = (char *) mxCalloc(nBuflen, sizeof(char));
  nStatus = mxGetString(mxOutput[0], (char *) formula, (mwSize)(nBuflen));

  if (nStatus != 0)
  {
    std::string id = std::string("OutputSBML:GetMathMLFormula") + sbmlTC;
    reportError(id, "Cannot copy formula", gv);
  }

  mxDestroyArray(mxInput[0]);
  mxDestroyArray(mxOutput[0]);

  return formula;
}

int
StructureFields::readInt(const std::string& name, unsigned int index, unsigned int total_no)
{
  mxArray * mxField;
  int value = 0;
  int nStatus = 1;

  /* get field */
  mxField = mxGetField(mxStructure, index, name.c_str());
  if (mxField != NULL)
  {
    if (!mxIsEmpty(mxField))
    {
      if (mxIsNumeric(mxField))
      {
        value = (int)(mxGetScalar(mxField));
        nStatus = 0;
      }
    }
    else
    {
      value = 0;
      nStatus = 0;
    }

    if (nStatus != 0)
    {
      reportReadError("Int", name, index, total_no);
    }
  }
  return value;
}

// static version
int
StructureFields::readInt(mxArray* mxArray1, const std::string& name, unsigned int index, GV& gv)
{
  mxArray * mxField;
  int value = 0;
  int nStatus = 1;

  /* get field */
  mxField = mxGetField(mxArray1, index, name.c_str());
  if (mxField != NULL)
  {
    if (!mxIsEmpty(mxField))
    {
      if (mxIsNumeric(mxField))
      {
        value = (int)(mxGetScalar(mxField));
        nStatus = 0;
      }
    }
    else
    {
      value = 0;
      nStatus = 0;
    }

    if (nStatus != 0)
    {
      reportReadError("Int", name, index, 0, "static", gv);
    }
  }

  return value;
}

double
StructureFields::readDouble(const std::string& name, unsigned int index, unsigned int total_no)
{
  mxArray * mxField;
  double value = 0;
  int nStatus = 1;

  /* get field */
  mxField = mxGetField(mxStructure, index, name.c_str());
  if (mxField != NULL)
  {
    if (!mxIsEmpty(mxField))
    {
      if (mxIsNumeric(mxField))
      {
        value = mxGetScalar(mxField);
        nStatus = 0;
      }
    }
    else
    {
      value = util_NaN();
      nStatus = 0;
    }

    if (nStatus != 0)
    {
      reportReadError("Double", name, index, total_no);
    }
  }
  return value;
}

const std::string
StructureFields::readString(const std::string& name, unsigned int index, unsigned int total_no)
{
  mxArray * mxField;
  char *value = NULL;
  int nStatus = 1;
  std::string f = "";

  /* get field */
  mxField = mxGetField(mxStructure, index, name.c_str());
  if (mxField != NULL)
  {
#ifdef USE_UTF8STRING
    value = mxArrayToUTF8String(mxField);
#else
    value = mxArrayToString(mxField);
#endif
    if (value != NULL)
    {
      nStatus = 0;
      f = std::string(value);
    }

    if (nStatus != 0)
    {
      reportReadError("String", name, index, total_no);
    }
  }
  return f;
}

//static version
const std::string
StructureFields::readString(mxArray* mxArray1, const std::string& name, unsigned int index, GV& gv_)
{
  mxArray * mxField;
  char *value = NULL;
  int nStatus = 1;
  std::string f = "";
  /* get field */
  mxField = mxGetField(mxArray1, index, name.c_str());
  if (mxField != NULL)
  {
    value = mxArrayToString(mxField);
    if (value != NULL)
    {
      nStatus = 0;
      f = std::string(value);
    }
    if (nStatus != 0)
    {
      reportReadError("String", name, index, 0, "static", gv_);
    }
  }
  return f;
}

unsigned int
StructureFields::readUint(const std::string& name, unsigned int index, unsigned int total_no)
{
  mxArray * mxField;
  unsigned int value = 0;
  int nStatus = 1;

  /* get field */
  mxField = mxGetField(mxStructure, index, name.c_str());
  if (mxField != NULL)
  {
    if (!mxIsEmpty(mxField))
    {
      if (mxIsNumeric(mxField))
      {
        value = (unsigned int)(mxGetScalar(mxField));
        nStatus = 0;
      }
    }
    else
    {
      value = 0;
      nStatus = 0;
    }

    if (nStatus != 0)
    {
      reportReadError("Uint", name, index, total_no);
    }
  }

  return value;
}

// static version
unsigned int
StructureFields::readUint(mxArray* mxArray1, const std::string& name, 
                          unsigned int index, GV& gv_)
{
  mxArray * mxField;
  unsigned int value = 0;
  int nStatus = 1;

  /* get field */
  mxField = mxGetField(mxArray1, index, name.c_str());
  if (mxField != NULL)
  {
    if (!mxIsEmpty(mxField))
    {
      if (mxIsNumeric(mxField))
      {
        value = (unsigned int)(mxGetScalar(mxField));
        nStatus = 0;
      }
    }
    else
    {
      value = 0;
      nStatus = 0;
    }

    if (nStatus != 0)
    {
      reportReadError("Uint", name, index, 0, "static", gv_);
    }
  }
  return value;
}

bool
StructureFields::determineStatus(const std::string& name, unsigned int index)
{
  bool setStatus = true;
  mxArray * mxField;
  // if the field itself is empty then it is clearly not set
  mxField = mxGetField(mxStructure, index, name.c_str());
  if (mxField == NULL || mxIsEmpty(mxField))
  {
    return false;
  }

  // want to know whether there is an isSetXYZ field coming from matlab
  // and if so what is its value
  unsigned int value = 0;
  int nStatus = 1;
  const char * cname = name.c_str();
  char * cpname = safe_strdup(cname);
  const char * isset = "isSet";
  char * isSet = safe_strcat(isset, cpname);
  cpname[0] = toupper(cpname[0]);
  char * isSet1 = safe_strcat(isset, cpname);

  /* get field */
  mxField = mxGetField(mxStructure, index, isSet1);
  // possibly have isSet followed by lower case
  if (mxField == NULL)
    mxField = mxGetField(mxStructure, index, isSet);
  if (mxField != NULL)
  {
    if (!mxIsEmpty(mxField) && mxIsNumeric(mxField))
    {
      value = (unsigned int)(mxGetScalar(mxField));
      nStatus = 0;
    }

    if (nStatus == 0)
    {
      setStatus = true ? value == 1 : false;
    }
  }

  safe_free(cpname);
  safe_free(isSet);
  safe_free(isSet1);

  return setStatus;
}
///////////////////////////////////////////

// report error

void
StructureFields::reportReadError(const std::string& type, const std::string& name, 
                                 unsigned int index, unsigned int total_no)
{
   std::string mid = "OutputSBML:read" + type + ":" + sbmlTC;

   std::ostringstream errMsg;
   if (total_no == 0)
     errMsg << " Cannot copy " << sbmlTC << "." << name << " field";
   else
     errMsg << " Cannot copy " << sbmlTC << "(" << index+1 << ")." << name << " field";
   
   reportError(mid, errMsg.str(), gv);
}

// static version
void
StructureFields::reportReadError(const std::string& type, const std::string& name, 
                                 unsigned int index, unsigned int total_no,
                                 const std::string& tc, GV& gv_)
{
   std::string mid = "OutputSBML:read" + type + ":" + tc;

   std::ostringstream errMsg;
   if (total_no == 0)
     errMsg << " Cannot copy " << tc << "." << name << " field";
   else
     errMsg << " Cannot copy " << tc << "(" << index+1 << ")." << name << " field";
   
   reportError(mid, errMsg.str(), gv_);
}
