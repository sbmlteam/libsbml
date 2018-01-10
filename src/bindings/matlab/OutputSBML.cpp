/**
 * @file    OutputSBML.cpp
 * @brief   MATLAB code for translating SBML-MATLAB structure into a SBML document.
 * @author  Sarah Keating
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *  
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 *     Pasadena, CA, USA 
 *  
 * Copyright (C) 2002-2005 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. Japan Science and Technology Agency, Japan
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ---------------------------------------------------------------------- -->*/

#include <stdio.h>
#include <string.h>

#include <mex.h>

#ifndef USE_OCTAVE
#include <matrix.h>
#endif
#include <algorithm>

#include <sbml/SBMLReader.h>
#include <sbml/SBMLTypes.h>
#include <sbml/xml/XMLNode.h>
#include <sbml/math/ASTNode.h>
#include <sbml/extension/SBasePlugin.h>
#include <sbml/extension/SBMLExtensionRegistry.h>

LIBSBML_CPP_NAMESPACE_USE

#ifdef USE_FBC
#include <sbml/packages/fbc/common/FbcExtensionTypes.h>
#endif

////////////////////////////////

// declarations - since a mex file cannot use local includes

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

const char* FIELDTYPE_STRINGS[] =
{
      "bool"
    , "char"
    , "double"
    , "int"
    , "structure"
    , "uint"
};

class StructureFields
{
public:

  StructureFields(SBase* obj, mxArray* structure);

  StructureFields(SBase* obj);

  StructureFields(std::string& tc);

  ~StructureFields();


  void addAttributes(const std::string& functionId, unsigned int index = 0, 
                     unsigned int total_no = 0);

  void createStructure(const std::string& functionId, SBase* base, bool usePlugin = false, 
                       const std::string& prefix = "");

  const std::string& getTypeCode() const { return sbmlTC; };

  mxArray* getStructure() const { return mxStructure; };

  static const std::string readString(mxArray* mxArray1, const std::string& name, 
                                      unsigned int index);

  static unsigned int readUint(mxArray* mxArry1, const std::string& name, 
                               unsigned int index);

  static int readInt(mxArray* mxArry1, const std::string& name, unsigned int index);

  static void reportReadError(const std::string& type, const std::string& name, 
                       unsigned int index, unsigned int total_no,
                       const std::string& tc);

private:
  void populateFields();

  void determineTypeCode();

  void populateStructure(const std::string& functionId, SBase* base, 
                         unsigned int index);

  void addStructureField(const std::string& functionId, SBase* base, 
                         std::string& fieldname, FieldType_t type, 
                         unsigned int index, unsigned int fieldIndex,
                         bool usePlugin, const std::string& prefix, 
                         const std::string& attName);

  void addChildElement(const std::string& name, unsigned int index);

  void setAttribute(const std::string& name, const FieldType_t type, 
                    unsigned int index = 0, unsigned int total_no = 0);


  const std::string getFieldname(unsigned int i, const std::string& id);
  const std::string getDefaultValue(unsigned int i, const std::string& id);
  void getDefaultValue(unsigned int i, const std::string& id, double& value);
  void getDefaultValue(unsigned int i, const std::string& id, int& value);
  const FieldType_t getValueType(unsigned int i, const std::string& id);

  // read values from structure
  int readInt(const std::string& name, unsigned int index, unsigned int total_no);

  double readDouble(const std::string& name, unsigned int index, unsigned int total_no);

  const std::string readString(const std::string& name, unsigned int index, unsigned int total_no);

  unsigned int readUint(const std::string& name, unsigned int index, unsigned int total_no);

  // need to further work out which type of rule
  const std::string getRuleType(mxArray* mxRuleStructure, unsigned int index);
  std::string getRuleTypeCode(SBase* base);
  std::string getAssociationTypeCode(SBase* base);

  // for some levels/versions some elements where coded as strings rather than structures
  // eg trigger/stoichiometryMath
  bool anomalousMathStructures(const std::string& fieldname, FieldType_t type);

  void addAnomalousChild(const std::string& fieldname, unsigned int index);

  void addAnomalousChildStructure(const std::string& functionId, SBase* base, 
                         std::string& fieldname, 
                         unsigned int index, unsigned int fieldIndex);

  const ASTNode* getMathChild(const std::string& value);

  char * convertMathFormula(const std::string& pacFormula);

  void adjustForCSymbols(ASTNode * math);

  // read values from model or default
  const std::string getStringValue(const std::string& functionId, SBase* base, 
                                   std::string& fieldname, unsigned int fieldIndex,
                                   bool usePlugin, const std::string& prefix);

  double getDoubleValue(const std::string& functionId, SBase* base, 
                                   std::string& fieldname, unsigned int fieldIndex,
                                   bool usePlugin, const std::string& prefix);
  bool getBoolValue(const std::string& functionId, SBase* base, 
                                   std::string& fieldname, unsigned int fieldIndex,
                                   bool usePlugin, const std::string& prefix);
  unsigned int getUintValue(const std::string& functionId, SBase* base, 
                                   std::string& fieldname, unsigned int fieldIndex,
                                   bool usePlugin, const std::string& prefix);
  int getIntValue(const std::string& functionId, SBase* base, 
                                   std::string& fieldname, unsigned int fieldIndex,
                                   bool usePlugin, const std::string& prefix);

  std::string getMathString(SBase* base);

  char * GetMatlabFormula(char * pacFormula, std::string object);

  void lookForCSymbols(ASTNode* math);

  void dealWithTimeSymbol(ASTNode* math);

  void dealWithDelaySymbol(ASTNode* math);

  void dealWithAvogadroSymbol(ASTNode* math);

  void dealWithRateOfSymbol(ASTNode* math);

  bool determineStatus(const std::string& name, unsigned int index);

  void reportReadError(const std::string& type, const std::string& name, 
                       unsigned int index, unsigned int total_no);


  bool usingPlugin(const std::string& prefix, SBase* base = NULL);

  bool isValidSBMLAttribute(const std::string& name, SBase* base = NULL);

  mxArray* getNamespacesStructure();

  mxArray* getCVTermsStructure(SBase* base);

  void addCVTerms(const std::string& name, unsigned int index);

  void freeStructureMemory();

protected:

  mxArray* mxFieldnames;
  mxArray* mxDefaultValues;
  mxArray* mxValueTypes;

  mxArray* mxStructure;

  SBase* mSBase;
  std::string sbmlTC;
};

typedef std::map<const std::string, unsigned int> PkgMap;
typedef PkgMap::iterator  PkgIter;

class ModelDetails
{
public:
  ModelDetails();

  ModelDetails(SBMLDocument *doc);

  ~ModelDetails() { };

  unsigned int getLevel() {return mLevel; } ;
  unsigned int getVersion() { return mVersion; } ;

  const std::string& getDelaySymbol() { return mDelaySymbol; } ;
  const std::string& getTimeSymbol() { return mTimeSymbol; } ;
  const std::string& getAvogadroSymbol() { return mAvogadroSymbol; } ;
  const std::string& getRateOfSymbol() { return mRateOfSymbol; } ;

  void setDelaySymbol(const std::string& symbol) { mDelaySymbol = symbol; } ;
  void setAvogadroSymbol(const std::string& symbol) { mAvogadroSymbol = symbol; } ;
  void setTimeSymbol(const std::string& symbol) { mTimeSymbol = symbol; } ;
  void setRateOfSymbol(const std::string& symbol) { mRateOfSymbol = symbol; } ;

  SBMLNamespaces* getNamespaces() { return mSBMLns; };

  PkgMap getPackages() { return mPackageMap; };

  bool isPackagePresent(const std::string& pkg);


protected:

  void populateNamespaces();
  void populateNamespaces(SBMLDocument* doc);
  void populatePkgMap();
  void populatePkgMap(SBMLDocument* doc);
  void populateSupportedPackages();

  bool isSupportedPackageNS(const std::string& uri, const std::string prefix);

  unsigned int mLevel;
  unsigned int mVersion;

  std::string mDelaySymbol;
  std::string mTimeSymbol;
  std::string mAvogadroSymbol;
  std::string mRateOfSymbol;

  SBMLNamespaces *mSBMLns;

  IdList mSupportedPackages;

  PkgMap mPackageMap;

};

///////////////////////////////////////////////////////////////////////////////

// global variables
mxArray *modelArray = NULL;
bool freeMemory;
ModelDetails * details;

IdList reqdPkgPrefixes;
IdList unreqdPkgPrefixes;

bool fbcUsingId;
bool fbcAddGeneProducts;
bool onlyExpectedFields;
bool applyUserValidation;

//////////////////////////////////////////////////////////////////////////////
//
// StructureFields.cpp

//////////////////////////////////////////////////////////////////////////////
//
// report error/free memory and exit when error encountered

void FreeMem(void)
{
  /* destroy arrays created */
  mxDestroyArray(modelArray);
}

void
reportError(const std::string&id, const std::string& message)
{
  if (freeMemory)
  {
    FreeMem();
  }

  mexErrMsgIdAndTxt(id.c_str(), message.c_str());
}

////////////////////////////////////////////////////////////////////////////

// useful global functions

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

bool getRequiredStatus(const std::string& prefix)
{
  bool required = false;

  if (reqdPkgPrefixes.contains(prefix))
  {
    required = true;
  }

  return required;
}

const std::string getPackagePrefix(const std::string& name)
{
  std::ostringstream prefix;
  size_t pos = name.find("_");
  if (pos == std::string::npos)
  {
    prefix << "";
    return prefix.str();
  }
  bool match = false;

  unsigned int i = 0;
  while (!match && i < reqdPkgPrefixes.size())
  {
    if (name.find(reqdPkgPrefixes.at(i)) != std::string::npos)
    {
      match = true;
      prefix << reqdPkgPrefixes.at(i);
      break;
    }
    i++;
  }

  i = 0;
  while (!match && i < unreqdPkgPrefixes.size())
  {
    if (name.find(unreqdPkgPrefixes.at(i)) != std::string::npos)
    {
      match = true;
      prefix << unreqdPkgPrefixes.at(i);
      break;
    }
    i++;
  }
  if (!match)
  {
    prefix << "";
  }

  return prefix.str();
}


std::string removePackagePrefix(const std::string& name)
{
  size_t len = getPackagePrefix(name).size();
  size_t pos = name.find("isSet");
  std::string cpname = "";
  cpname = name;

  if (len > 0)
  {
    if (pos == std::string::npos)
    {
      cpname.replace(0, len+1, "");
    }
    else
    {
      cpname.replace(5, len+1, "");
    }
  }

  return cpname;
}

void populatePackageLists()
{
  //reqdPkgPrefixes.append("comp");
  //reqdPkgPrefixes.append("spatial");

  unreqdPkgPrefixes.append("fbc");
  unreqdPkgPrefixes.append("qual");
  unreqdPkgPrefixes.append("groups");
}


//////////////////////////////////////////////////////////////////////////////
//
// class to store the structure/retrieve fields and write the SBML Model

StructureFields::StructureFields(SBase* obj, mxArray* structure) :
    mSBase ( NULL)
  , mxStructure ( NULL )
  , sbmlTC ("")
{
  mSBase = obj;
  mxStructure = structure;

  determineTypeCode();
  populateFields();
}

StructureFields::StructureFields(SBase* obj) :
    mSBase ( NULL)
  , mxStructure ( NULL )
  , sbmlTC ("")
{
  mSBase = obj;

  determineTypeCode();
  populateFields();
}

StructureFields::StructureFields(std::string& tc) :
    mSBase ( NULL)
  , mxStructure ( NULL )
  , sbmlTC ( tc )
{
  populateFields();
}


StructureFields::~StructureFields()
{
  // must not do this as it can be a part of a larger model
//  delete mSBase;
  //mxDestroyArray(mxStructure);
  //mxDestroyArray(mxFieldnames);
  //mxDestroyArray(mxDefaultValues);
  //mxDestroyArray(mxValueTypes);
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
}

bool
isUnknownType(std::string tc)
{
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
void
StructureFields::determineTypeCode()
{
  if (!sbmlTC.empty()) return;
  else if (mSBase == NULL) return;

  PkgMap pm = details->getPackages();

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
  int numberInputs = 3;
  // the array size will need to accomadate all packages
  PkgMap pm = details->getPackages();
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
  mxInput[1] = CreateIntScalar(details->getLevel());
  mxInput[2] = CreateIntScalar(details->getVersion());

  // need to add inputs for any plugins
  unsigned int numPlugins = (unsigned int)(pm.size());
  if (numPlugins > 0)
  {
    mwSize dims[1] = {numPlugins};
    numberInputs = 5;

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
  }
  mxArray * exception = NULL;
  exception = mexCallMATLABWithTrap(1, &mxFieldnames, numberInputs, mxInput, "getStructureFieldnames");
  if (exception != 0)
  {
    mexCallMATLAB(0, (mxArray **)NULL, 1, &exception, "throw");

    reportError(id, "Failed to get fieldnames");
  }

  exception = mexCallMATLABWithTrap(1, &mxDefaultValues, numberInputs, mxInput, "getDefaultValues");
  if (exception != 0)
  {
      mexCallMATLAB(0, (mxArray **)NULL, 1, &exception, "throw");

      reportError(id, "Failed to get default value");
  }

  exception = mexCallMATLABWithTrap(1, &mxValueTypes, numberInputs, mxInput, "getValueType");
  if (exception != 0)
  {
      mexCallMATLAB(0, (mxArray **)NULL, 1, &exception, "throw");

      reportError(id, "Failed to get value types");
  }

  mxDestroyArray(mxInput[0]);
  mxDestroyArray(mxInput[1]);
  mxDestroyArray(mxInput[2]);
  if (numPlugins > 0)
  {
    mxDestroyArray(mxInput[3]);
    mxDestroyArray(mxInput[4]);
  }
}

const std::string
StructureFields::getFieldname(unsigned int i, const std::string& id)
{
  mxArray* mxName = mxGetCell(mxFieldnames, i);
  size_t nBuflen = (mxGetM(mxName)*mxGetN(mxName)+1);
  char * fieldname = (char *) mxCalloc(nBuflen, sizeof(char));
  if (mxGetString(mxName, (char *) fieldname, (mwSize)(nBuflen)) != 0)
  {
    reportError(id, "Failed in GetFieldname");
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
    reportError(id, "Failed in GetDefaultValue");
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
    reportError(id, "Failed in GetValueType");
  }
  FieldType_t ft = getFieldType(fieldname); 
  mxFree(fieldname);
  return ft;
}

bool
StructureFields::isValidSBMLAttribute(const std::string& name, SBase* base)
{
  bool valid = true;
  // some fields used in matlab do not relate the sbml attributes/objects
  if (name.find("isSet") != std::string::npos)
  {
    valid = false;
  }
  else if (name.find("SBML_") != std::string::npos)
  {
    valid = false;
  }
  else if (name.find("_version") != std::string::npos)
  {
    valid = false;
  }
  else if (name.find("typecode") != std::string::npos)
  {
    valid = false;
  }
  else if (name.find("_symbol") != std::string::npos)
  {
    valid = false;
  }
  if (valid && base != NULL)
  {
    if (name.find("variable") != std::string::npos && base->getLevel() == 1)
    {
      valid = false;
    }
  }
 
  return valid;
}

//////////////////////////

// functions for creating structure from SBML

void
StructureFields::createStructure(const std::string& functionId, SBase* base, 
                                 bool usePlugin, const std::string& prefix)
{
  std::string fieldname;
  unsigned int total_no = base->getNumObjects(sbmlTC);
  if (usePlugin)
  {
    total_no = base->getPlugin(prefix)->getNumObjects(sbmlTC);
  }
  mwSize dims[2] = {1, total_no};

  size_t n = mxGetNumberOfElements(mxFieldnames);
  char **field_names = (char**)(safe_malloc(n * sizeof(char*)));
  for (unsigned int i = 0; i < n; i++)
  {
    fieldname = getFieldname(i, functionId);
    field_names[i] = (char*)(safe_malloc((fieldname.size() * sizeof(char))+ 1));
    field_names[i] = safe_strdup(fieldname.c_str());
  }

  mxStructure = mxCreateStructArray(2, dims, n, (const char**)(field_names));
  safe_free(field_names);

  for (unsigned int i = 0; i < total_no; i++)
  {
    SBase* child = base->getObject(sbmlTC, i);
    if (usePlugin)
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

  std::string fieldname;
  FieldType_t type;

  size_t n = mxGetNumberOfElements(mxFieldnames);

  for (unsigned int i = 0; i < n; i++)
  {
    fieldname = getFieldname(i, functionId);
    type = getValueType(i, functionId);
    std::string prefix = getPackagePrefix(fieldname);
    std::string attName = fieldname;
    bool usePlugin = usingPlugin(prefix, base);
    if (prefix.size() > 0)
    {
      attName = removePackagePrefix(fieldname);
    }
    if (type == TYPE_ELEMENT)
    {
      if (fieldname == "namespaces")
      {
        mxSetField(mxStructure, index, fieldname.c_str(), getNamespacesStructure());
      }
      else if (fieldname == "cvterms")
      {
        mxSetField(mxStructure, index, fieldname.c_str(), getCVTermsStructure(base));
      }
      else
      {
        StructureFields *sf = new StructureFields(attName);
        sf->createStructure(functionId + ":" + fieldname, base, usePlugin, prefix);
        mxSetField(mxStructure, index, fieldname.c_str(), mxDuplicateArray(sf->getStructure()));
        delete sf;
      }
    }
    else if (anomalousMathStructures(fieldname, type))
    {
      addAnomalousChildStructure(functionId, base, fieldname, index, i);
    }
    else
    {
      addStructureField(functionId, base, fieldname, type, index, i, usePlugin, prefix, attName);
    }

  }
}

void 
StructureFields::addAnomalousChildStructure(const std::string& functionId, SBase* base, 
                         std::string& fieldname, 
                         unsigned int index, unsigned int fieldIndex)
{
  std::string value;
  SBase* child = base->getObject(fieldname, 0);
  if (child == NULL)
  {
    value = getDefaultValue(fieldIndex, functionId);
  }
  else
  {
    value = getMathString(child);
  }
  
  mxSetField(mxStructure, index, fieldname.c_str() ,mxCreateString(value.c_str())); 
}

mxArray* 
StructureFields::getNamespacesStructure()
{
  mxArray* mxNSReturn = NULL;

  const XMLNamespaces * NS = details->getNamespaces()->getNamespaces();
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

  for (i = 0; i < n; i++)
  {
    pacPrefix = safe_strdup(NS->getPrefix(i).c_str());
    pacURI    = safe_strdup(NS->getURI(i).c_str());

    /**
    * check for NULL strings - Matlab doesnt like creating 
    * a string that is NULL
    */
    if (pacPrefix == NULL) {
      pacPrefix = "";
    }
    if (pacURI == NULL) {
      pacURI = "";
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

  for (int i = 0; i < n; i++)
  {
    CVTerm * cv = base->getCVTerm(i);
    addCVTerm(mxCVTermReturn, i, cv);
  }

  return mxCVTermReturn;
}


void
StructureFields::addStructureField(const std::string& functionId, SBase* base, 
                                   std::string& fieldname, FieldType_t type, 
                                   unsigned int index, unsigned int fieldIndex,
                                   bool usePlugin, const std::string& prefix,
                                   const std::string& attName)
{
  std::string value;
  int ivalue;
  unsigned int uvalue;
  bool bvalue;
  double dvalue;
  std::string name = fieldname;
  if (usePlugin)
  {
    name = attName;
  }
  else if (base->getPackageName() == prefix)
  {
    name = attName;
  }
  switch(type)
  {
   case TYPE_UNKNOWN:

   case TYPE_CHAR:
    value = getStringValue(functionId, base, name, fieldIndex, usePlugin, prefix);
    mxSetField(mxStructure, index, fieldname.c_str() ,mxCreateString(value.c_str())); 
    break;
  case TYPE_BOOL:
    bvalue = getBoolValue(functionId, base, name, fieldIndex, usePlugin, prefix);
    mxSetField(mxStructure, index, fieldname.c_str(), CreateIntScalar(bvalue));
    break;
  case TYPE_UINT:
    uvalue = getUintValue(functionId, base, name, fieldIndex, usePlugin, prefix);
    mxSetField(mxStructure, index, fieldname.c_str(), CreateIntScalar(uvalue));
    break;
  case TYPE_INT:
    ivalue = getIntValue(functionId, base, name, fieldIndex, usePlugin, prefix);
    mxSetField(mxStructure, index, fieldname.c_str(), CreateIntScalar(ivalue));
    break;
  case TYPE_DOUBLE:
    dvalue = getDoubleValue(functionId, base, name, fieldIndex, usePlugin, prefix);
    mxSetField(mxStructure, index, fieldname.c_str(), mxCreateDoubleScalar(dvalue));
    break;
  case TYPE_ELEMENT:
  default:
    break;
  }
}

const std::string
StructureFields::getStringValue(const std::string& functionId, SBase* base, 
                                std::string& fieldname, unsigned int fieldIndex,
                                bool usePlugin, const std::string& prefix)
{
  std::string value;
  if (isValidSBMLAttribute(fieldname, base))
  {
    if (!usePlugin && base->isSetAttribute(fieldname))
    {
      base->getAttribute(fieldname, value);
    }
    else if (usePlugin && base->getPlugin(prefix)->isSetAttribute(fieldname))
    {
      base->getPlugin(prefix)->getAttribute(fieldname, value);
    }
    else if (fieldname == "notes")
    {
      value = base->getNotesString();
    }
    else if (fieldname == "annotation")
    {
      value = base->getAnnotationString();
    }
    else if (fieldname == "message")
    {
      value = base->getMessageString();
    }
    else if (fieldname == "math" || fieldname == "formula")
    {
      value = getMathString(base);
    }
#ifdef USE_FBC

    else if (fieldname == "association")
    {
      value = static_cast<FbcAssociation*>(base)->toInfix(fbcUsingId);//FbcAssociation_toInfix(static_cast<FbcAssociation*>(base));
    }
#endif

    else
    {
      value = getDefaultValue(fieldIndex, functionId);
      if (fieldname == "type" && base->getTypeCode() == SBML_RATE_RULE)
      {
        value = "rate";
      }
    }
  }
  else if (fieldname == "avogadro_symbol")
  {
    if (!details->getAvogadroSymbol().empty())
    {
      value = details->getAvogadroSymbol();
    }
    else
    {
      value = getDefaultValue(fieldIndex, functionId);
    }

  }
  else if (fieldname == "delay_symbol")
  {
    if (!details->getDelaySymbol().empty())
    {
      value = details->getDelaySymbol();
    }
    else
    {
      value = getDefaultValue(fieldIndex, functionId);
    }
  }
  else if (fieldname == "time_symbol")
  {
    if (!details->getTimeSymbol().empty())
    {
      value = details->getTimeSymbol();
    }
    else
    {
      value = getDefaultValue(fieldIndex, functionId);
    }
  }
  else if (fieldname == "rateOf_symbol")
  {
    if (!details->getRateOfSymbol().empty())
    {
      value = details->getRateOfSymbol();
    }
    else
    {
      value = getDefaultValue(fieldIndex, functionId);
    }
  }
  else
  {
    value = getDefaultValue(fieldIndex, functionId);
    /* hack for rules that all use the same fieldnames/defaults */
    if (value == "SBML_ALGEBRAIC_RULE")
    {
      value = getRuleTypeCode(base);
    }
    else if (value == "SBML_FBC_ASSOCIATION")
    {
      value = getAssociationTypeCode(base);
    }
  }

  return (const std::string)(value);
}

double
StructureFields::getDoubleValue(const std::string& functionId, SBase* base, 
                                std::string& fieldname, unsigned int fieldIndex, 
                                bool usePlugin, const std::string& prefix)
{
  double value;
  if (isValidSBMLAttribute(fieldname))
  {
    if (!usePlugin && base->isSetAttribute(fieldname))
    {
      base->getAttribute(fieldname, value);
    }
    else if (usePlugin && base->getPlugin(prefix)->isSetAttribute(fieldname))
    {
      base->getPlugin(prefix)->getAttribute(fieldname, value);
    }
    else
    {
      getDefaultValue(fieldIndex, functionId, value);
    }
  }
  else
  {
    getDefaultValue(fieldIndex, functionId, value);
  }

  return value;
}


int
StructureFields::getIntValue(const std::string& functionId, SBase* base, 
                             std::string& fieldname, unsigned int fieldIndex, 
                             bool usePlugin, const std::string& prefix)
{
  int value;
  if (isValidSBMLAttribute(fieldname))
  {
    if (!usePlugin && base->isSetAttribute(fieldname))
    {
      base->getAttribute(fieldname, value);
    }
    else if (usePlugin && base->getPlugin(prefix)->isSetAttribute(fieldname))
    {
      base->getPlugin(prefix)->getAttribute(fieldname, value);
    }
    else
    {
      getDefaultValue(fieldIndex, functionId, value);
    }
  }
  else
  {
    getDefaultValue(fieldIndex, functionId, value);
  }

  return value;
}


unsigned int
StructureFields::getUintValue(const std::string& functionId, SBase* base, 
                              std::string& fieldname, unsigned int fieldIndex, 
                              bool usePlugin, const std::string& prefix)
{
  unsigned int value;
  int ivalue;
  bool useDefault = false;
  if (isValidSBMLAttribute(fieldname))
  {
    if (!usePlugin && base->isSetAttribute(fieldname))
    {
      base->getAttribute(fieldname, value);
    }
    else if (usePlugin && base->getPlugin(prefix)->isSetAttribute(fieldname))
    {
      base->getPlugin(prefix)->getAttribute(fieldname, value);
    }
    else
    {
      getDefaultValue(fieldIndex, functionId, ivalue);
      useDefault = true;
    }
  }
  else
  {
    getDefaultValue(fieldIndex, functionId, ivalue);
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
                              std::string& fieldname, unsigned int fieldIndex, 
                              bool usePlugin, const std::string& prefix)
{
  bool bvalue;
  int value;
  if (isValidSBMLAttribute(fieldname))
  {
    if (!usePlugin && base->isSetAttribute(fieldname))
    {
      base->getAttribute(fieldname, bvalue);
      return bvalue;
    }
    else if (usePlugin && base->getPlugin(prefix)->isSetAttribute(fieldname))
    {
      base->getPlugin(prefix)->getAttribute(fieldname, bvalue);
      return bvalue;
    }
    else
    {
      getDefaultValue(fieldIndex, functionId, value);
    }
  }
  else if (fieldname.find("isSet") != std::string::npos)
  {
    std::string name = fieldname.substr(5);
    name[0] = tolower(name[0]);
    if (!usePlugin)
    {
      value = base->isSetAttribute(name);
    }
    else 
    {
      value = base->getPlugin(prefix)->isSetAttribute(name);
    }
  }
  else
  {
    getDefaultValue(fieldIndex, functionId, value);
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
    reportError(id.c_str(), "Failed to convert formula");
  }

  /* get the formula returned */
  nBuflen = (mxGetM(mxOutput[0])*mxGetN(mxOutput[0])+1);
  formula = (char *) mxCalloc(nBuflen, sizeof(char));
  nStatus = mxGetString(mxOutput[0], (char *) formula, (mwSize)(nBuflen));

  if (nStatus != 0)
  {
    std::string id = std::string("TranslateSBML:GetMatlabFormula") + object;
    reportError(id.c_str(), "Cannot copy formula");
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
  ASTNodeType_t type;

  if (nChild == 0)
  {
    type = math->getType();

    if (type == AST_NAME_AVOGADRO)
    {
      dealWithAvogadroSymbol(math);
    }
    else if (type == AST_NAME_TIME)
    {
      dealWithTimeSymbol(math);
    }
  }
  else
  {
    type = math->getType();

    if (type == AST_FUNCTION_DELAY)
    {
      dealWithDelaySymbol(math);
    }
    else if (type == AST_FUNCTION_RATE_OF)
    {
      dealWithRateOfSymbol(math);
    }
  }

  for (unsigned int i = 0; i < nChild; i++)
  {
    ASTNode* child = math->getChild(i);
    lookForCSymbols(child);
  }
}

void 
StructureFields::dealWithAvogadroSymbol(ASTNode* math)
{
  if (math == NULL) return;
  if (details->getAvogadroSymbol().empty())
  {
    details->setAvogadroSymbol(math->getName());
  }
  else
  {
    math->setName(details->getAvogadroSymbol().c_str());
  }
}

void 
StructureFields::dealWithTimeSymbol(ASTNode* math)
{
  if (math == NULL) return;
  if (details->getTimeSymbol().empty())
  {
    details->setTimeSymbol(math->getName());
  }
  else
  {
    math->setName(details->getTimeSymbol().c_str());
  }
}

void 
StructureFields::dealWithDelaySymbol(ASTNode* math)
{
  if (math == NULL) return;
  if (details->getDelaySymbol().empty())
  {
    details->setDelaySymbol(math->getName());
  }
  else
  {
    math->setName(details->getDelaySymbol().c_str());
  }
}

void 
StructureFields::dealWithRateOfSymbol(ASTNode* math)
{
  if (math == NULL) return;
  if (details->getRateOfSymbol().empty())
  {
    details->setRateOfSymbol(math->getName());
  }
  else
  {
    math->setName(details->getRateOfSymbol().c_str());
  }
}

//////////////////////////////////////////////////////////////

// general need to know function

bool 
StructureFields::usingPlugin(const std::string& prefix, SBase* base)
{
  bool usePlugin = false;
  if (prefix.size() > 0)
  {
    if (mSBase != NULL && mSBase->getPackageName() != prefix)
    {
      usePlugin = true;
    }
    else if (base != NULL && base->getPlugin(prefix) != NULL)
    {
      usePlugin = true;
    }
  }

  return usePlugin;
}

//////////////////////////

// functions for creating SBML from structure
void
StructureFields::addAttributes(const std::string& functionId, unsigned int index,
                               unsigned int total_no)
{
  std::string fieldname;
  FieldType_t type;

  size_t n = mxGetNumberOfElements(mxFieldnames);

  for (unsigned int i = 0; i < n; i++)
  {
    fieldname = getFieldname(i, functionId);
    if (!isValidSBMLAttribute(fieldname))
    {
      continue;
    }
    type = getValueType(i, functionId);
    if (type == TYPE_ELEMENT)
    {
      if (fieldname == "cvterms")
      {
        addCVTerms(fieldname, index);
      }
      else 
      {
       addChildElement(fieldname, index);
      }
    }
    else if (anomalousMathStructures(fieldname, type))
    {
      addAnomalousChild(fieldname, i);
    }
    else 
    {
      setAttribute(fieldname, type, index, total_no);
    }

  }
}

CVTerm *
  getCVTerm(mxArray* mxCVTerms, unsigned int i)
{
  CVTerm *cv;
  std::string qualType = StructureFields::readString(mxCVTerms, "qualifierType", i);
  std::string qual = StructureFields::readString(mxCVTerms, "qualifier", i);
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
      CVTerm *nested = getCVTerm(mxNestedCV, n);
      cv->addNestedCVTerm(nested);
      delete nested;
    }
  }

  return cv;
}

void
StructureFields::addCVTerms(const std::string& name, unsigned int index)
{
  mxArray* mxCVTerms = mxGetField(mxStructure, index, "cvterms");
  if (mxCVTerms == NULL)
    return;

  size_t numCV = mxGetNumberOfElements(mxCVTerms);

  for (unsigned int i = 0; i < numCV; i++)
  {
    CVTerm *cv = getCVTerm(mxCVTerms, i);
    if (!mSBase->isSetMetaId())
      mSBase->setMetaId("temp");
    mSBase->addCVTerm(cv);
    delete cv;
  }

}

void
StructureFields::addChildElement(const std::string& name, unsigned int index)
{
  mxArray* mxChild = mxGetField(mxStructure, index, name.c_str());
  SBase *pChild = NULL;

  size_t n = mxGetNumberOfElements(mxChild);
  if (mxChild == NULL) return;
  else if (n == 0) return;
  std::string prefix = getPackagePrefix(name);
  std::string attName = name;
  bool usePlugin = usingPlugin(prefix);
  if (prefix.size() > 0)
  {
    attName = removePackagePrefix(name);
  }
  for (unsigned int i = 0; i < n; i++)
  {
    // hack for rules - since a list of rules contains assignmentRule etc..
    if (name == "rule")
    {
      pChild = mSBase->createChildObject(getRuleType(mxChild, i));
    }
    else 
    {
      if (usePlugin)
      {
        pChild = mSBase->getPlugin(prefix)->createChildObject(attName);
      }
      else
      {
        pChild = mSBase->createChildObject(attName);
      }
    }

    if (pChild != NULL)
    {
      StructureFields *sf = new StructureFields(pChild, mxChild);

      std::string id = std::string("OutputSBML:addChildElement:") + sf->getTypeCode();
      sf->addAttributes(id, i, n);

      sf->freeStructureMemory();
    }
  }
}

bool
StructureFields::anomalousMathStructures(const std::string& fieldname, FieldType_t type)
{
  bool isAnomaly = false;

  // cases where the field was stored as a string rather than a structure
  if (type == TYPE_CHAR)
  {
    if (fieldname == "trigger" ||
      fieldname == "delay" ||
      fieldname == "stoichiometryMath")
    {
      isAnomaly = true;
    }    
  }

  return isAnomaly;
}

void 
StructureFields::addAnomalousChild(const std::string& fieldname, unsigned int index)
{
  std::string value = readString(fieldname, 0, 0);

  if (!value.empty())
  {
    SBase *pChild = mSBase->createChildObject(fieldname);
    if (pChild != NULL)
    {
      std::string value = readString(fieldname, 0, 0);
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

  if (math->getType() == AST_FUNCTION)
  {
    if (math->getName() == details->getDelaySymbol())
    {
      math->setType(AST_FUNCTION_DELAY);
    }
    else if (math->getName() == details->getRateOfSymbol())
    {
      math->setType(AST_FUNCTION_RATE_OF);
    }  
  }
  else if (math->getType() == AST_NAME)
  {
    if (math->getName() == details->getTimeSymbol())
    {
      math->setType(AST_NAME_TIME);
    }
    else if (math->getName() == details->getAvogadroSymbol())
    {
      math->setType(AST_NAME_AVOGADRO);
    }
  }

  for (unsigned int i = 0; i < math->getNumChildren(); i++)
  {
    adjustForCSymbols(math->getChild(i));
  }
}


void
StructureFields::setAttribute(const std::string& name, const FieldType_t type,
                              unsigned int index, unsigned int total_no)
{
  std::string value;
  int ivalue;
  unsigned int uvalue;
  bool bvalue;
  double dvalue;
  std::string prefix = getPackagePrefix(name);
  std::string attName = name;
  bool usePlugin = usingPlugin(prefix);
  if (prefix.size() > 0)
  {
    attName = removePackagePrefix(name);
  }
  switch(type)
  {
  case TYPE_CHAR:
    value = readString(name, index, total_no);
    if (name == "math" || name == "formula")
    {
      const ASTNode *ast = getMathChild(value);
      mSBase->setMath(ast);
    }
    else if (name == "notes")
    {
      mSBase->setNotes(value);
    }
    else if (name == "annotation")
    {
      mSBase->setAnnotation(value);
    }
    else if (name == "message")
    {
      mSBase->setMessage(value);
    }
    else
    {
      if (usePlugin)
      {
        mSBase->getPlugin(prefix)->setAttribute(attName, value);
      }
      else
      {
        mSBase->setAttribute(attName, value);
      }
    }
    break;

  case TYPE_INT:
    ivalue = readInt(name, index, total_no);
    if (determineStatus(name, index))
    {
      if (usePlugin)
      {
        mSBase->getPlugin(prefix)->setAttribute(attName, ivalue);
      }
      else
      {
        mSBase->setAttribute(attName, ivalue);
      }
    }
    break;

  case TYPE_UINT:
    uvalue = readUint(name, index, total_no);
    if (determineStatus(name, index))
    {
      if (usePlugin)
      {
        mSBase->getPlugin(prefix)->setAttribute(attName, uvalue);
      }
      else
      {
        mSBase->setAttribute(attName, uvalue);
      }
    }
    break;

  case TYPE_DOUBLE:
    dvalue = readDouble(name, index, total_no);
    if (determineStatus(name, index))
    {
      if (usePlugin)
      {
        mSBase->getPlugin(prefix)->setAttribute(attName, dvalue);
      }
      else
      {
        mSBase->setAttribute(attName, dvalue);
      }
    }
    break;

  case TYPE_BOOL:
    ivalue = readInt(name, index, total_no);
    bvalue = true ? ivalue == 1 : false;
    if (determineStatus(name, index))
    {
      if (usePlugin)
      {
        mSBase->getPlugin(prefix)->setAttribute(attName, bvalue);
      }
      else
      {
        mSBase->setAttribute(attName, bvalue);
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
  std::string value = readString(mxRuleStructure, "typecode", index);
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
    std::string type = readString(mxRuleStructure, "type", index);
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
    std::string id = std::string("OutputSBML:GetMathMLFormula:") + sbmlTC;
    reportError(id, "Failed to convert formula");
  }

  /* get the formula returned */
  nBuflen = (mxGetM(mxOutput[0])*mxGetN(mxOutput[0])+1);
  formula = (char *) mxCalloc(nBuflen, sizeof(char));
  nStatus = mxGetString(mxOutput[0], (char *) formula, (mwSize)(nBuflen));

  if (nStatus != 0)
  {
    std::string id = std::string("OutputSBML:GetMathMLFormula") + sbmlTC;
    reportError(id, "Cannot copy formula");
  }

  mxDestroyArray(mxInput[0]);
  mxDestroyArray(mxOutput[0]);

  return formula;
}

int
StructureFields::readInt(const std::string& name, unsigned int index, unsigned int total_no)
{
  mxArray * mxField;
  int value;
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
StructureFields::readInt(mxArray* mxArray1, const std::string& name, unsigned int index)
{
  mxArray * mxField;
  int value;
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
      reportReadError("Int", name, index, 0, "static");
    }
  }

  return value;
}

double
StructureFields::readDouble(const std::string& name, unsigned int index, unsigned int total_no)
{
  mxArray * mxField;
  double value;
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

  /* get field */
  mxField = mxGetField(mxStructure, index, name.c_str());
  value = mxArrayToString(mxField);
  if (value != NULL)
  {
    nStatus = 0;
  }

  if (nStatus != 0)
  {
    reportReadError("String", name, index, total_no);
  }

  const std::string f = std::string(value); 
  return f;
}

//static version
const std::string
StructureFields::readString(mxArray* mxArray1, const std::string& name, unsigned int index)
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
      reportReadError("String", name, index, 0, "static");
    }
  }
  return f;
}

unsigned int
StructureFields::readUint(const std::string& name, unsigned int index, unsigned int total_no)
{
  mxArray * mxField;
  unsigned int value;
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
                          unsigned int index)
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
      reportReadError("Uint", name, index, 0, "static");
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
  if (mxIsEmpty(mxField))
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
   
   reportError(mid, errMsg.str());
}

// static version
void
StructureFields::reportReadError(const std::string& type, const std::string& name, 
                                 unsigned int index, unsigned int total_no,
                                 const std::string& tc)
{
   std::string mid = "OutputSBML:read" + type + ":" + tc;

   std::ostringstream errMsg;
   if (total_no == 0)
     errMsg << " Cannot copy " << tc << "." << name << " field";
   else
     errMsg << " Cannot copy " << tc << "(" << index+1 << ")." << name << " field";
   
   reportError(mid, errMsg.str());
}

///////////////////////////////////////////////////////////////////////////////
//
// class to store model details

ModelDetails::ModelDetails()
{
  mPackageMap.clear();
  mSupportedPackages.clear();

  mLevel = StructureFields::readUint(modelArray, "SBML_level", 0);
  mVersion = StructureFields::readUint(modelArray, "SBML_version", 0);

  mDelaySymbol = StructureFields::readString(modelArray, "delay_symbol", 0); 
  mTimeSymbol = StructureFields::readString(modelArray, "time_symbol", 0); 
  mAvogadroSymbol = StructureFields::readString(modelArray, "avogadro_symbol", 0); 

  populateNamespaces();
  populatePkgMap();
}

ModelDetails::ModelDetails(SBMLDocument* doc)
{
  mPackageMap.clear();
  mSupportedPackages.clear();

  mLevel = doc->getLevel();
  mVersion = doc->getVersion();

  mDelaySymbol = "";
  mTimeSymbol = ""; 
  mAvogadroSymbol = ""; 

  populateNamespaces(doc);
  populatePkgMap(doc);
}


void
ModelDetails::populateNamespaces()
{
  mSBMLns = new SBMLNamespaces(getLevel(), getVersion());

  XMLNamespaces *xmlns = new XMLNamespaces();
  mxArray* mxNamespaces = mxGetField(modelArray, 0, "namespaces");
  size_t nNoNamespaces = mxGetNumberOfElements(mxNamespaces);

  for (unsigned int i = 0; i < nNoNamespaces; i++)
  {
    std::string uri = StructureFields::readString(mxNamespaces, "uri", i);
    std::string prefix = StructureFields::readString(mxNamespaces, "prefix", i);
    xmlns->add(uri, prefix);
  }

  mSBMLns->addNamespaces(xmlns); 
}

void
ModelDetails::populateNamespaces(SBMLDocument* doc)
{
  mSBMLns = doc->getSBMLNamespaces();
}

void
ModelDetails::populateSupportedPackages()
{
  for (unsigned int i = 0; i <  SBMLExtensionRegistry::getNumRegisteredPackages(); i++)
  {
    mSupportedPackages.append(SBMLExtensionRegistry::getRegisteredPackageName(i));
  }
}

void
ModelDetails::populatePkgMap()
{
  populateSupportedPackages();
  XMLNamespaces *xmlns = mSBMLns->getNamespaces();
  for (int i = 0; i < xmlns->getNumNamespaces(); i++)
  {
    if (isSupportedPackageNS(xmlns->getURI(i), xmlns->getPrefix(i)))
    {
      std::string prefix = xmlns->getPrefix(i);
      std::string name = prefix + "_version";
      unsigned int version = StructureFields::readUint(modelArray, name, 0);
      mPackageMap.insert(std::pair<const std::string, unsigned int>(prefix, version));
    }
  }
}

void
ModelDetails::populatePkgMap(SBMLDocument* doc)
{
  populateSupportedPackages();
  XMLNamespaces *xmlns = mSBMLns->getNamespaces();
  for (int i = 0; i < xmlns->getNumNamespaces(); i++)
  {
    if (isSupportedPackageNS(xmlns->getURI(i), xmlns->getPrefix(i)))
    {
      std::string prefix = xmlns->getPrefix(i);
      unsigned int version = doc->getPlugin(prefix)->getPackageVersion();
      mPackageMap.insert(std::pair<const std::string, unsigned int>(prefix, version));
    }
  }
}

bool 
ModelDetails::isSupportedPackageNS(const std::string& uri, const std::string prefix)
{
  bool supported = false;
  if (prefix.empty())
    return supported;

  size_t pos = uri.find("http://www.sbml.org/sbml/level3/version");
  if (pos == 0 && mSupportedPackages.contains(prefix))
  {
    supported = true;
  }

  return supported;
}

bool 
ModelDetails::isPackagePresent(const std::string& pkg)
{
  bool present = false;

  for (PkgIter it = mPackageMap.begin(); it != mPackageMap.end(); ++it)
  {
    if (it->first == pkg)
    {
      present = true;
      break;
    }
  }
  return present;
}

////////////////////////////////////////////////////////////////////////////
//
// Filenames.cpp

////////////////////////////////////////////////////////////////////////////
//
// ensure reading of unicode filenames

#if defined(WIN32) && !defined(CYGWIN) && !defined(USE_OCTAVE)
#define FILE_CHAR wchar_t*
#define FILE_FOPEN(file) _wfopen(file, L"r")
#define USE_FILE_WCHAR 1
#else 
#define FILE_CHAR char*
#define FILE_FOPEN(file) fopen(file, "r")
#endif

#ifndef uint16_t
#define uint16_t unsigned short
#endif


FILE_CHAR readUnicodeString(const mxArray *prhs, mwSize length)
{
#ifdef USE_OCTAVE
  char* ansii = (char *) mxCalloc(length, sizeof(char));
  mxGetString(prhs, ansii, length);
  return ansii;
#else   
  wchar_t* utf16 = (wchar_t *) mxCalloc(length, sizeof(wchar_t));
  char* utf8 = NULL;
  uint16_T *ch = (uint16_T *) mxGetData(prhs);
  wchar_t* p = utf16;
  mwSize i;
  for ( i = 0; i < length-1; ++i)
    *p++ = *ch++;
  *p = 0;

#if USE_FILE_WCHAR
  return utf16;
#else

  utf8 = (char*)mxCalloc(length*2, sizeof(char));

  wcstombs(utf8, utf16, length*2);

  /*mxFree(utf16);*/

  if (utf8 != NULL && strlen(utf8) == 0 && length > 0)
  {
    reportError("readUnicodeString", 
      "This string uses characters that cannot be "
      "expressed in UTF8, please rename the file.");
  }

  return utf8;
#endif /* USE_FILE_WCHAR */ 

#endif /* USE_OCTAVE*/ 

}


FILE_CHAR readUnicodeStringFromArrays(mxArray *mxFilename[2])

{
  mwSize nBuflen = (mxGetM(mxFilename[0])*mxGetN(mxFilename[0])+1);
  FILE_CHAR pacTempString1 = readUnicodeString(mxFilename[0],nBuflen);

  mwSize nBufferLen = (mxGetM(mxFilename[1])*mxGetN(mxFilename[1])+1);
  FILE_CHAR  pacTempString2 = readUnicodeString(mxFilename[1],nBufferLen);
  
#if USE_FILE_WCHAR
  FILE_CHAR  pacFilename = (wchar_t *) mxCalloc(nBufferLen+nBuflen, sizeof(wchar_t));
  wcscpy(pacFilename, pacTempString2);
  wcscat(pacFilename, pacTempString1);
#else
  FILE_CHAR  pacFilename = (char *) mxCalloc(nBufferLen+nBuflen, sizeof(char));
  strcpy(pacFilename, pacTempString2);
  strcat(pacFilename, pacTempString1);
#endif
  
  /*mxFree(pacTempString1);*/
  /*mxFree(pacTempString2);*/
  return pacFilename;
}

#if USE_FILE_WCHAR

int endsWith(const wchar_t* fileName, const char* ext)
{
  size_t len = wcslen(fileName), i;
  size_t targetLen = strlen(ext);
  wchar_t* temp1 =  (wchar_t*)malloc((targetLen + 1) * sizeof(wchar_t));
  char* temp2 =  (char*)malloc((targetLen+1)*sizeof(char));
  int result = 0;
  
  memset(temp1, 0, targetLen*sizeof(wchar_t));
  memset(temp2, 0, targetLen*sizeof(char));

  for (i = 0; i < targetLen; ++i)
  {
    temp1[i] = fileName[len - targetLen + i];
  }
  
  wcstombs(temp2,temp1, targetLen);
  result = strcmp_insensitive(temp2, ext);

  /*mxFree(temp1);*/
  /*mxFree(temp2);*/
  free(temp1);
  free(temp2);
  return result;
}

#endif

FILE_CHAR
browseForFilename()
{
  FILE_CHAR filename = NULL;
  mxArray * mxFilename[2], * mxExt[1];
  mxExt[0] = mxCreateString(".xml");
  int nStatus = mexCallMATLAB(2, mxFilename, 1, mxExt, "uigetfile");

  if (nStatus != 0)
  {
    reportError("TranslateSBML:GUIInput:filename", 
      "Failed to read filename");
  }

  /* get the filename returned */
  filename = readUnicodeStringFromArrays(mxFilename);

  mxDestroyArray(mxExt[0]);
  mxDestroyArray(mxFilename[1]);
  mxDestroyArray(mxFilename[0]);

  return filename;
}

////////////////////////////////////////////////////////////////////////////
//
// Arguments.cpp

/* determine whether we are in octave or matlab */
unsigned int
determinePlatform()
{
  unsigned int usingOctave = 0;
  mxArray * mxOctave[1];

  mexCallMATLAB(1, mxOctave, 0, NULL, "isoctave");

  size_t nBuflen = (mxGetM(mxOctave[0])*mxGetN(mxOctave[0])+1);
  char * pacTempString1 = (char *)(safe_calloc(nBuflen, sizeof(char)));
  int nStatus = mxGetString(mxOctave[0], pacTempString1, (mwSize)(nBuflen));

  if (nStatus != 0)
  {
    reportError("OutputSBML:platformDetection", 
      "Could not determine platform");
  }

  safe_free(pacTempString1);
  mxDestroyArray(mxOctave[0]);

  return usingOctave;
}

bool
answerYesToQuestion(const std::string& question)
{
  bool answer = false;
  mxArray *mxPrompt[2], *mxReply[1];
  char *pacReply;
  mxPrompt[0]= mxCreateString(question.c_str());
  mxPrompt[1]= mxCreateString("s");
  mexCallMATLAB(1, mxReply, 2, mxPrompt, "input");
  mxDestroyArray(mxPrompt[0]);
  mxDestroyArray(mxPrompt[1]);

  size_t nBufferLen = (mxGetM(mxReply[0])*mxGetN(mxReply[0])+1);
  pacReply = (char *) (safe_calloc(nBufferLen, sizeof(char)));
  mxGetString(mxReply[0], pacReply, (mwSize)(nBufferLen));
  mxDestroyArray(mxReply[0]);

  if (strcmp_insensitive(pacReply, "y") == 0)
  {
    answer = true;
  }
  safe_free(pacReply);

  return answer;
}

///////////////////////////////////////////////////////////////////////////////
//
// functions used to check arguments for OutputSBML

void
validateNumberOfInputsForOutput(int nrhs, const mxArray *prhs[], unsigned int usingOctave)
{
  if (nrhs < 1)
  {
    reportError("OutputSBML:inputArgs", 
      "Must supply at least the model as an input argument\n"
      "USAGE: OutputSBML(SBMLModel, (filename), (exclusiveFlag), (applyUserValidation), (fbcGeneProductOptions))");
  }
  if (usingOctave == 1 && nrhs < 2)
  {
    reportError("OutputSBML:Octave:needFilename", 
      "Octave requires the filename to be specified\n"
      "USAGE: OutputSBML(SBMLModel, (filename), (exclusiveFlag), (applyUserValidation), (fbcGeneProductOptions))");
  }
  if (nrhs > 5)
  {
    reportError("OutputSBML:inputArguments", "Too many input arguments\n"
      "USAGE: OutputSBML(SBMLModel, (filename), (exclusiveFlag), (applyUserValidation), (fbcGeneProductOptions))");
  }

  if (nrhs > 1 && ((mxIsChar(prhs[1]) != 1) || (mxGetM(prhs[1]) != 1)))
  {
    reportError("OutputSBML:inputArguments:invalidFilename", 
      "Second argument must be a filename\n"
      "USAGE: OutputSBML(SBMLModel, (filename), (exclusiveFlag), (applyUserValidation), (fbcGeneProductOptions))");
  }
  if (nrhs > 2 && !mxIsNumeric(prhs[2]))
  {
    reportError("OutputSBML:inputArguments:exclusiveFlag", 
      "exclusiveFlag is an optional argument but must be a number\n"
      "USAGE: OutputSBML(SBMLModel, (filename), (exclusiveFlag), (applyUserValidation), (fbcGeneProductOptions))");
  }

  if (nrhs > 3 && !mxIsNumeric(prhs[3]))
  {
    reportError("OutputSBML:inputArguments:applyUserValidation", 
      "applyUserValidation is an optional argument but must be a number\n"
      "USAGE: OutputSBML(SBMLModel, (filename), (exclusiveFlag), (applyUserValidation), (fbcGeneProductOptions))");
  }

  if (nrhs > 4 && (!mxIsNumeric(prhs[4]) || (mxGetM(prhs[4]) != 1) || (mxGetN(prhs[4]) != 2)))
  {
    reportError("OutputSBML:inputArguments:fbcGeneProductOptions", 
      "fbcGeneProductOptions is an optional argument but must be an array with two numbers\n"
      "USAGE: OutputSBML(SBMLModel, (filename), (exclusiveFlag), (applyUserValidation), (fbcGeneProductOptions))");
  }



}

void
validateNumberOfOutputsForOutput(int nlhs)
{
  if (nlhs > 0)
  {
    reportError("OutputSBML:outputArguments", "Too many output arguments\n"
      "USAGE: OutputSBML(SBMLModel, (filename), (exclusiveFlag))");
  }
}

void
populateModelArray(int nrhs, const mxArray *prhs[])
{
  modelArray = mxDuplicateArray(prhs[0]);

  /**
  * note second argument may be the filename
  *
  * we now have the option of a third argument that indicates that we
  * want the structure to ONLY contain expected fields or not
  *
  * and a fourth argument that tells us whether to apply user
  * specific validation
  *
  * and a fifth argument saying whether to use ids/lebels in fbc
  */
  if (nrhs > 4)
  {
    double *pr2 = mxGetPr(prhs[2]);
    if (*pr2 == 0)
    {
      onlyExpectedFields = false;
    }
    else
    {
      onlyExpectedFields = true;
    }
    double *pr3 = mxGetPr(prhs[3]);
    if (*pr3 == 0)
    {
      applyUserValidation = false;
    }
    else
    {
      applyUserValidation = true;
    }
    double *pr = mxGetPr(prhs[4]);
  
    if (*pr == 0)
    {
      fbcUsingId = false;
    }
    else
    {
      fbcUsingId = true;
    }
    pr++;
    if (*pr == 0)
    {
      fbcAddGeneProducts = false;
    }
    else
    {
      fbcAddGeneProducts = true;
    }
  }
  else if (nrhs > 3)
  {
    double *pr2 = mxGetPr(prhs[2]);
    if (*pr2 == 0)
    {
      onlyExpectedFields = false;
    }
    else
    {
      onlyExpectedFields = true;
    }
    double *pr3 = mxGetPr(prhs[3]);
    if (*pr3 == 0)
    {
      applyUserValidation = false;
    }
    else
    {
      applyUserValidation = true;
    }
    fbcUsingId = false;
    fbcAddGeneProducts = true;
  }  
  else if ( nrhs > 2)
  {
    double *pr2 = mxGetPr(prhs[2]);
    if (*pr2 == 0)
    {
      onlyExpectedFields = false;
    }
    else
    {
      onlyExpectedFields = true;
    }
    applyUserValidation = false;
    fbcUsingId = false;
    fbcAddGeneProducts = true;
  }
  else
  {
    onlyExpectedFields = true;
    applyUserValidation = false;
    fbcUsingId = false;
    fbcAddGeneProducts = true;
  }
  
  // we have made memory - need to free it is we exit prematurely
  freeMemory = true;
}

void 
validateModel()
{
  mxArray * mxCheckStructure[2];
  mxArray * mxModel[3];
  mxModel[0] = modelArray;
  if (onlyExpectedFields)
  {
    mxModel[1] = mxCreateDoubleScalar(1);
  }
  else
  {
  
    mxModel[1] = mxCreateDoubleScalar(0);
  }
  if (applyUserValidation)
  {
    mxModel[2] = mxCreateDoubleScalar(1);
  }
  else
  {

    mxModel[2] = mxCreateDoubleScalar(0);
  }
  int nStatus = mexCallMATLAB(2, mxCheckStructure, 3, mxModel, "isSBML_Model");

  int value = (int)(mxGetScalar(mxCheckStructure[0]));
  if ((nStatus != 0) || (value != 1))
  {
    /* there are errors - use the pacTempString1 char * to list these to the user */
    size_t nBuflen = (mxGetM(mxCheckStructure[1])*mxGetN(mxCheckStructure[1])+1);
    char * pacTempString1 = (char *)safe_calloc(nBuflen, sizeof(char));
    nStatus = mxGetString(mxCheckStructure[1], pacTempString1, (mwSize)(nBuflen));
    std::ostringstream errMsg;
    if (nStatus == 0)
    {
      errMsg << "\nFirst input must be a valid MATLAB_SBML Structure\n\n" <<
        "Errors reported: " << pacTempString1 << "\nUSAGE: OutputSBML(SBMLModel"
        << ", (filename), (exclusiveFlag), (applyUserValidation))";
      reportError("OutputSBML:inputArguments:invalidModelSupplied", errMsg.str());
    }
    else
    {
      errMsg << "\nFirst input must be a valid MATLAB_SBML Structure\n\n" <<
        "\nUSAGE: OutputSBML(SBMLModel, (filename), (exclusiveFlag), (applyUserValidation))";
      reportError("OutputSBML:inputArguments:invalidStructureSupplied", errMsg.str());
    } 
    safe_free(pacTempString1);
  }

  mxDestroyArray(mxCheckStructure[0]);
  mxDestroyArray(mxCheckStructure[1]);
}

FILE_CHAR validateFilenameForOutput(int nrhs, const mxArray *prhs[])
{
  FILE_CHAR filename = NULL;
  if (nrhs >= 2)
  {
    if (mxIsChar(prhs[1]) != 1)
    {
      reportError("OutputSBML:inputArguments:invalidFilename", 
        "Second input must be a filename\n"
        "USAGE: OutputSBML(SBMLModel, (filename), (exclusiveFlag))");
    }

    size_t nBuflen = (mxGetM(prhs[1])*mxGetN(prhs[1])+1);
    filename = readUnicodeString(prhs[1], (mwSize)nBuflen);
  }
  else
  {
    filename = browseForFilename();
  }

     /* 
    * check that the extension has been used  
    */
#if USE_FILE_WCHAR
    if (wcsstr(filename, L".xml") == NULL)
    {
      wcscat(filename, L".xml");
    }
#else
    /* check that the extension has been used  */
    if (strstr(filename, ".xml") == NULL)
    {
      strcat(filename, ".xml");
    }
#endif


  return filename;
}

//////////////

// functions for TranslatSBML
void
validateNumberOfInputsForTranslate(int nrhs, const mxArray *prhs[], 
                                   unsigned int usingOctave)
{
  if (nrhs > 4)
  {
    reportError("TranslateSBML:inputArguments", "Too many input arguments\n"
      "USAGE: [model, (errors), (version)] = "
      "TranslateSBML((filename), (validateFlag), (verboseFlag), (fbcGeneProductOptions))");
  }

  if (nrhs > 0 && ((mxIsChar(prhs[0]) != 1) || (mxGetM(prhs[0]) != 1)))
  {
    reportError("TranslateSBML:inputArguments:invalidFilename", 
      "First argument must be a filename\n"
      "USAGE: [model, (errors), (version)] = "
      "TranslateSBML((filename), (validateFlag), (verboseFlag), (fbcGeneProductOptions))");
  }
  if (nrhs > 1 && !mxIsNumeric(prhs[1]))
  {
    reportError("TranslateSBML:inputArguments:validateFlag", 
      "validateFlag is an optional argument but must be a number\n"
      "USAGE: [model, (errors), (version)] = "
      "TranslateSBML((filename), (validateFlag), (verboseFlag), (fbcGeneProductOptions))");
  }

  if (nrhs > 2 && !mxIsNumeric(prhs[2]))
  {
    reportError("TranslateSBML:inputArguments:verboseFlag", 
      "verboseFlag is an optional argument but must be a number\n"
      "USAGE: [model, (errors), (version)] = "
      "TranslateSBML((filename), (validateFlag), (verboseFlag), (fbcGeneProductOptions))");
  }

  if (nrhs > 3 && (!mxIsNumeric(prhs[3]) || (mxGetM(prhs[3]) != 1) || (mxGetN(prhs[3]) != 2)))
  {
    reportError("TranslateSBML:inputArguments:fbcGeneProductOptions", 
      "fbcGeneProductOptions is an optional argument but must be an array with two numbers\n"
      "USAGE: [model, (errors), (version)] = "
      "TranslateSBML((filename), (validateFlag), (verboseFlag), (fbcGeneProductOptions))");
  }

  if (usingOctave && nrhs == 0)
  {
    reportError("TranslateSBML:Octave:needFilename", 
      "Octave requires the filename to be specified\n"
        "USAGE: [model, (errors), (version)] = "
        "TranslateSBML(filename, (validateFlag), (verboseFlag))");
  }
}

void
validateNumberOfOutputsForTranslate(int nlhs, mxArray *plhs[], 
                                    unsigned int& outputErrors,
                                    unsigned int& outputVersion)
{
  switch (nlhs)
  {
  case 3:
    outputErrors = 1;
    outputVersion = 1;
    break;
  case 2:
    outputErrors = 1;
    break;
  case 1:
  case 0:
    break;
  default:
    reportError("TranslateSBML:outputArguments", "Too many output arguments\n"
      "USAGE: [model, (errors), (version)] = "
      "TranslateSBML((filename), (validateFlag), (verboseFlag))");
    break;
  }
}

void
checkFileExists(FILE_CHAR filename)
{
    FILE *fp;
    fp = FILE_FOPEN(filename);
    if(fp == NULL)
    {
      char * msgTxt = NULL;
#if USE_FILE_WCHAR
      msgTxt = (char *) safe_calloc(wcslen(filename)+35, sizeof(char));
      sprintf(msgTxt, "File %ws does not exist on this path", filename);
#else
      msgTxt = (char *) safe_calloc(strlen(filename)+35, sizeof(char));
      sprintf(msgTxt, "File %s does not exist on this path", filename);
#endif
      reportError("TranslateSBML:inputArguments:filename", msgTxt);
      safe_free(msgTxt);
    }
    else
    {
      fclose(fp);
    }

}

FILE_CHAR
getGivenFilename(const mxArray* prhs[])
{
  FILE_CHAR filename = NULL;
  size_t nBufferLen  = mxGetNumberOfElements (prhs[0]) + 1;
  filename = readUnicodeString(prhs[0], nBufferLen);

  if (filename == NULL)
  {
    reportError("TranslateSBML:inputArguments:filename", 
      "Failed to read filename");
  }

  checkFileExists(filename);
  return filename;
}

FILE_CHAR
getFilename(int nrhs, const mxArray* prhs[], unsigned int& validateFlag, 
            unsigned int& verboseFlag)
{
  FILE_CHAR filename = NULL;

  if (nrhs > 0)
  {
    filename = getGivenFilename(prhs);

    /* if a second argument has been given this is the flag indicating
    * whether to validate the Model or not
    */
    if (nrhs > 1)
    {
      validateFlag = (int) mxGetScalar(prhs[1]);   
    }

    /* if a third argument has been given this is the flag indicating
    * whether to print out errors or not
    */
    if (nrhs > 2)
    {
      verboseFlag = (int) mxGetScalar(prhs[2]);   
    }

    /* a fourth argument is an array indicating whether to use geneProduct 
    * labels or ids for expressing the association 
    * and whether to add missing geneProducts when parsing the association
    */
    if (nrhs > 3)
    {
      double *pr = mxGetPr(prhs[3]); 
  
      if (*pr == 0)
      {
        fbcUsingId = false;
      }
      else
      {
        fbcUsingId = true;
      }
      pr++;
      if (*pr == 0)
      {
        fbcAddGeneProducts = false;
      }
      else
      {
        fbcAddGeneProducts = true;
      }
    }
  }
  else
  {
    filename = browseForFilename();
    if (answerYesToQuestion("Do you want to validate the model? Enter y/n "))
    {
      validateFlag = 1;
    }
    fbcUsingId = false;
    fbcAddGeneProducts = true;
  }

  return filename;
}

///////////////////////////////////////////////////////////////////////////

// functions called by main functions 
FILE_CHAR
validateInputOutputForOutput(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], 
                    unsigned int usingOctave)
{
  validateNumberOfInputsForOutput(nrhs, prhs, usingOctave);
  validateNumberOfOutputsForOutput(nlhs);
  populateModelArray(nrhs, prhs);
  validateModel();
  FILE_CHAR filename = validateFilenameForOutput(nrhs, prhs);
  return filename;
}

FILE_CHAR
validateInputOutputForTranslate(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], 
                    unsigned int usingOctave, unsigned int& outputErrors,
                    unsigned int& outputVersion, unsigned int& validateFlag,
                    unsigned int& verboseFlag)
{
  FILE_CHAR filename = NULL;
  validateNumberOfInputsForTranslate(nrhs, prhs, usingOctave);
  validateNumberOfOutputsForTranslate(nlhs, plhs, outputErrors, outputVersion);
  filename = getFilename(nrhs, prhs, validateFlag, verboseFlag);

  return filename;
}
////////////////////////////////////////////////////////////////////////////
//
// OutputSBML.cpp

SBMLDocument *
createSBMLDocument()
{
  SBMLDocument * sbmlDocument;
  sbmlDocument = new SBMLDocument(details->getNamespaces());

  PkgMap pm = details->getPackages();
  for (PkgIter it = pm.begin(); it != pm.end(); ++it)
  {
    const std::string& prefix = it->first;
    sbmlDocument->setPackageRequired(prefix, getRequiredStatus(prefix));
  }
  return sbmlDocument;

}

//////////////////////////////////////////////////////////////////////////

// here deal with things that do not fit with 'normal' 
// hopefully this is the only place that manual coding will be necessary
#ifdef USE_FBC

mxArray*
getAssociation(unsigned int i)
{
  mxArray* mxAssociation = NULL;
  mxArray* mxRn = NULL;
  mxArray* mxGPA = NULL;
  mxRn = mxGetField(modelArray, 0, "reaction");
  if (mxRn != NULL && mxIsStruct(mxRn))
  {
    mxGPA = mxGetField(mxRn, i, "fbc_geneProductAssociation");
    if (mxGPA != NULL)
    {
      mxAssociation = mxGetField(mxGPA, 0, "fbc_association");
    }
  }
  return mxAssociation;
}

void 
addGPAAttributes(FbcAssociation* pAssociation, mxArray* mxAssociation)
{
  IdList* atts = new IdList();
  atts->append("notes");
  atts->append("annotation");
  atts->append("metaid");
  atts->append("id");
  atts->append("name");

  for (unsigned int i = 0; i < atts->size(); i++)
  {
    std::string name = atts->at(i);
    std::string value = StructureFields::readString(mxAssociation, name, 0);
    if (!value.empty()) 
    {
      if (name == "notes")
      {
        pAssociation->setNotes(value);
      }
      else if (name == "annotation")
      {
        pAssociation->setAnnotation(value);
      }
      else
      {
        pAssociation->setAttribute(name, value);
      }
    }
  }
  int sbo = StructureFields::readInt(mxAssociation, "sboTerm", 0);
  if (sbo != -1)
  {
    pAssociation->setSBOTerm(sbo);
  }
}

void
addGeneProductAssociations(SBMLDocument* sbmlDocument)
{
  FbcModelPlugin* mplug = static_cast<FbcModelPlugin*>(sbmlDocument->getModel()->getPlugin("fbc"));
  for (unsigned int i = 0; i < sbmlDocument->getModel()->getNumReactions(); i++)
  {
    Reaction * rn = sbmlDocument->getModel()->getReaction(i);
    FbcReactionPlugin* plug = static_cast<FbcReactionPlugin*>(rn->getPlugin("fbc"));
    if (plug->isSetGeneProductAssociation())
    {
      mxArray* mxAssoc = NULL;
      mxAssoc = getAssociation(i);
      if (mxAssoc != NULL)
      {
        std::string association = StructureFields::readString(mxAssoc, "fbc_association", 0);
        if (!association.empty())
        {
          FbcAssociation* pAssociation = FbcAssociation::parseFbcInfixAssociation(association, mplug,
            fbcUsingId, fbcAddGeneProducts);
          plug->getGeneProductAssociation()->setAssociation(pAssociation);
          addGPAAttributes(plug->getGeneProductAssociation()->getAssociation(), mxAssoc);
        }
      }
    }
  }
}

void
dealWithAnomalies(SBMLDocument* sbmlDocument)
{
  bool fbcPresent = details->isPackagePresent("fbc");

  if (fbcPresent)
  {
    // the fbc active objective on the listOfObjectives
    std::string obj = StructureFields::readString(modelArray, "fbc_activeObjective", 0);
    if (!obj.empty())
    {
      FbcModelPlugin* plug = static_cast<FbcModelPlugin*>(sbmlDocument->getModel()->getPlugin("fbc"));
      plug->setActiveObjectiveId(obj);
    }

    // the gene product associations in fbc v2
    unsigned int vers = StructureFields::readUint(modelArray, "fbc_version", 0);
    if (vers == 2)
    {
      addGeneProductAssociations(sbmlDocument);
    }
  }

}
#endif
///////////////////////////////////////////////////////////////////////////////
/**
 * NAME:    mexFunction
 *
 * PARAMETERS:  int     nlhs     -  number of output arguments  
 *              mxArray *plhs[]  -  output arguments
 *              int     nrhs     -  number of input arguments
 *              mxArray *prhs[]  -  input arguments
 *
 * RETURNS:    
 *
 * FUNCTION:  MATLAB standard dll export function
 *            any returns are made through the mxArray * plhs
 */
void
mexFunction (int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  // we have not made persistent memory
  freeMemory = false;
  FILE_CHAR pacFilename = NULL;
  SBMLDocument *sbmlDocument;
  Model *sbmlModel;

  /* determine whether we are in octave or matlab */
  unsigned int usingOctave = determinePlatform();

  /***************************************************************************
  * validate inputs and outputs
  ****************************************************************************/
  
  pacFilename = validateInputOutputForOutput(nlhs, plhs, nrhs, prhs, usingOctave);


  /**************************************************************************
  * get the details of the model
  ***************************************************************************/
  populatePackageLists();
  details = new ModelDetails();

  ///* look for fbc */

  //if (XMLNamespaces_hasPrefix(SBMLNamespaces_getNamespaces(ns), "fbc") == 1)
  //{
  //  fbcPresent = 1;
  //}

  ///* the fbc namespace may not be set 
  //* but thet fbc package may still be used
  //*/
  //if (fbcPresent == 0)
  //{
  //  nStatus = mxGetFieldNumber(modelArray, "fbc_version");
  //  if (nStatus > 0)
  //  {
  //    fbcPresent = 1;
  //    xmlns = XMLNamespaces_create();
  //    XMLNamespaces_add(xmlns, 
  //      "http://www.sbml.org/sbml/level3/version1/fbc/version1", "fbc");
  //    SBMLNamespaces_addNamespaces(ns, xmlns);
  //  }
  //}

  sbmlDocument =   createSBMLDocument();
  //if (fbcPresent == 1)
  //{
  //  SBMLDocument_setPkgRequired(sbmlDocument, "fbc", 0);
  //  mxFBCVersion = mxGetField(modelArray, 0, "fbc_version");
  //  nFBCVersion = (unsigned int) mxGetScalar(mxFBCVersion);
  //  fbcVersion = nFBCVersion;
  //}

  /* create a model within the document */
  sbmlModel = sbmlDocument->createModel();

  StructureFields *sf = new StructureFields(sbmlModel, modelArray);

  std::string id = std::string("OutputSBML:GetModel:") + sf->getTypeCode();
  sf->addAttributes(id);

#ifdef USE_FBC
  dealWithAnomalies(sbmlDocument);
#endif
  /**********************************************************************
  * output the resulting model to specified file
  **************************************************************************/

  /* write the SBML document to the filename specified */
  unsigned int nStatus = 0;
#if USE_FILE_WCHAR
  {
    char* sbml = writeSBMLToString(sbmlDocument);
    if (sbml != NULL)
    {
      size_t len = strlen(sbml);
      FILE* fp = _wfopen(pacFilename, L"w");
      if (fp != NULL)
      {
        fwrite(sbml, sizeof(char), len, fp);
        fclose(fp);
        nStatus = 1;
      }

      free(sbml);
    }
  }
#else
  nStatus = writeSBML(sbmlDocument, pacFilename);
#endif

  if (nStatus != 1)
  {
    reportError("OutputSBML:writeFile", "Failed to write file");
  }
  else
  {
    mexPrintf("Document written\n");
  }

  delete sbmlDocument;
  delete details;
  delete sf;
}
