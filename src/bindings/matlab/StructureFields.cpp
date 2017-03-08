/**
 * @file    OutputSBML.cpp
 * @brief   MATLAB code for translating SBML-MATLAB structure into a SBML document.
 * @author  Sarah Keating
 *
 * <!--------------------------------------------------------------------------
 * This file is part of SBMLToolbox.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of SBMLToolbox.
 *
 * Copyright (C) 2013-2017 jointly by the following organizations:
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
 * in the file named "LICENSE.txt" included with this software distribution.
 * and also available online as http://sbml.org/software/sbmltoolbox/license.html
 * ---------------------------------------------------------------------- -->*/

#include <mex.h>
#ifndef USE_OCTAVE
#include <matrix.h>
#endif
#include <string.h>
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

  void freeMemory();

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
extern mxArray * mxModel[3];
extern bool freeMemory;
extern ModelDetails * details;

extern IdList reqdPkgPrefixes;
extern IdList unreqdPkgPrefixes;

extern bool fbcUsingId;
extern bool fbcAddGeneProducts;

//////////////////////////////////////////////////////////////////////////////
//
// report error/free memory and exit when error encountered

void FreeMem(void)
{
  /* destroy arrays created */
  mxDestroyArray(mxModel[0]);
  mxDestroyArray(mxModel[1]);
  mxDestroyArray(mxModel[2]);
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
  mxDestroyArray(mxStructure);
  mxDestroyArray(mxFieldnames);
  mxDestroyArray(mxDefaultValues);
  mxDestroyArray(mxValueTypes);
}

void
StructureFields::freeMemory()
{
  mxDestroyArray(mxFieldnames);
  mxDestroyArray(mxDefaultValues);
  mxDestroyArray(mxValueTypes);
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

  while (sbmlTC == "(Unknown SBML Type)" && it != pm.end())
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
  unsigned int numPlugins = pm.size();
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

  if (mexCallMATLAB(1, &mxFieldnames, numberInputs, mxInput, "getStructureFieldnames") != 0)
  {
    reportError(id, "Failed to get fieldnames");
  }

  if (mexCallMATLAB(1, &mxDefaultValues, numberInputs, mxInput, "getDefaultValues") != 0)
  {
    reportError(id, "Failed to get default value");
  }

  if (mexCallMATLAB(1, &mxValueTypes, numberInputs, mxInput, "getValueType") != 0)
  {
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
  unsigned int nBuflen = (mxGetM(mxName)*mxGetN(mxName)+1);
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
  unsigned int nBuflen = (mxGetM(mxName)*mxGetN(mxName)+1);
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
  unsigned int nBuflen = (mxGetM(mxName)*mxGetN(mxName)+1);
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

  unsigned int n = mxGetNumberOfElements(mxFieldnames);
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

  unsigned int n = mxGetNumberOfElements(mxFieldnames);

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
  mwSize dims[2] = {1, n};

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
  int nStatus, nBuflen;
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

  unsigned int n = mxGetNumberOfElements(mxFieldnames);

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
      addChildElement(fieldname, index);
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


void
StructureFields::addChildElement(const std::string& name, unsigned int index)
{
  mxArray* mxChild = mxGetField(mxStructure, index, name.c_str());
  SBase *pChild = NULL;

  unsigned int n = mxGetNumberOfElements(mxChild);
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
      pChild = mSBase->createObject(getRuleType(mxChild, i));
    }
    else 
    {
      if (usePlugin)
      {
        pChild = mSBase->getPlugin(prefix)->createObject(attName);
      }
      else
      {
        pChild = mSBase->createObject(attName);
      }
    }

    if (pChild != NULL)
    {
      StructureFields *sf = new StructureFields(pChild, mxChild);

      std::string id = std::string("OutputSBML:addChildElement:") + sf->getTypeCode();
      sf->addAttributes(id, i, n);

      sf->freeMemory();
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
    SBase *pChild = mSBase->createObject(fieldname);
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
  int nStatus, nBuflen;
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

  mLevel = StructureFields::readUint(mxModel[0], "SBML_level", 0);
  mVersion = StructureFields::readUint(mxModel[0], "SBML_version", 0);

  mDelaySymbol = StructureFields::readString(mxModel[0], "delay_symbol", 0); 
  mTimeSymbol = StructureFields::readString(mxModel[0], "time_symbol", 0); 
  mAvogadroSymbol = StructureFields::readString(mxModel[0], "avogadro_symbol", 0); 

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
  mxArray* mxNamespaces = mxGetField(mxModel[0], 0, "namespaces");
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
      unsigned int version = StructureFields::readUint(mxModel[0], name, 0);
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


