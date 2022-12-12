#ifndef SF_INCLUDED
#define SF_INCLUDED

#include <string>

#include <mex.h>

#ifndef USE_OCTAVE
#include <matrix.h>
#endif
#include <algorithm>


#include "ModelDetails.h"

class SBase;
class ASTNode;

class StructureFields
{
public:

  StructureFields(SBase* obj, mxArray* structure, GV& gv);

  StructureFields(SBase* obj, GV& gv);

  StructureFields(std::string& tc, GV& gv);

  ~StructureFields();


  void addAttributes(const std::string& functionId, unsigned int index = 0, 
                     unsigned int total_no = 0);

  void createStructure(const std::string& functionId, SBase* base, bool usePlugin_cs = false, 
                       const std::string& prefix = "");

  const std::string& getTypeCode() const { return sbmlTC; };

  mxArray* getStructure() const { return mxStructure; };

  static const std::string readString(mxArray* mxArray1, const std::string& name, 
                                      unsigned int index, GV& gv);

  static unsigned int readUint(mxArray* mxArry1, const std::string& name, 
                               unsigned int index, GV& gv);

  static int readInt(mxArray* mxArry1, const std::string& name, unsigned int index, GV& gv);

  static void reportReadError(const std::string& type, const std::string& name, 
                       unsigned int index, unsigned int total_no,
                       const std::string& tc, GV& gv);

private:
  void populateFields();

  void determineTypeCode();

  void populateStructure(const std::string& functionId, SBase* base, 
                         unsigned int index);

  void addStructureField(const std::string& functionId, SBase* base,
    unsigned int index, FieldValues_t field, bool usePlugin_sf);

  void addChildElement(FieldValues_t field, unsigned int index);

  void setAttribute(FieldValues_t field,
                    unsigned int index = 0, unsigned int total_no = 0);


  const std::string getFieldname(unsigned int i, const std::string& id);
  const std::string getDefaultValue(unsigned int i, const std::string& id);
  void getDefaultValue(unsigned int i, const std::string& id, double& value);
  void getDefaultValue(unsigned int i, const std::string& id, int& value);
  const FieldType_t getValueType(unsigned int i, const std::string& id);
  bool getIsSBMLAttribute(unsigned int i, const std::string& id);
  const std::string getSBMLPrefix(unsigned int i, const std::string& id);
  const std::string getSBMLName(unsigned int i, const std::string& id);
  const FieldnamesType_t getNameEnumType(unsigned int i, const std::string& id);

  // read values from structure
  int readInt(const std::string& name, unsigned int index, unsigned int total_no);

  double readDouble(const std::string& name, unsigned int index, unsigned int total_no);

  const std::string readString(const std::string& name, unsigned int index, unsigned int total_no);

  unsigned int readUint(const std::string& name, unsigned int index, unsigned int total_no);

  // need to further work out which type of rule
  const std::string getRuleType(mxArray* mxRuleStructure, unsigned int index);
  std::string getRuleTypeCode(SBase* base);
  std::string getAssociationTypeCode(SBase* base);


  void addAnomalousChild(FieldValues_t field);

  void addAnomalousChildStructure(const std::string& functionId, SBase* base, 
                          unsigned int index, FieldValues_t field);

  const ASTNode* getMathChild(const std::string& value);

  char * convertMathFormula(const std::string& pacFormula);

  void adjustForCSymbols(ASTNode * math);

  // read values from model or default
  const std::string getStringValue(const std::string& functionId, SBase* base,
    FieldValues_t field, bool usePlugin);

  double getDoubleValue(const std::string& functionId, SBase* base,
    FieldValues_t field, bool usePlugin);

  bool getBoolValue(const std::string& functionId, SBase* base,
    FieldValues_t field, bool usePlugin);

  unsigned int getUintValue(const std::string& functionId, SBase* base,
    FieldValues_t field, bool usePlugin);

  int getIntValue(const std::string& functionId, SBase* base,
    FieldValues_t field, bool usePlugin);

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


  bool usingPlugin(const std::string& prefix, SBase* base = NULL, const std::string& name = "" );

  mxArray* getNamespacesStructure();

  mxArray* getCVTermsStructure(SBase* base);

  void addCVTerms(unsigned int index);

  void freeStructureMemory();

protected:

  GV& gv;
  mxArray* mxFieldnames;
  mxArray* mxDefaultValues;
  mxArray* mxValueTypes;

  size_t nNumberFields;

  mxArray* mxStructure;

  SBase* mSBase;
  std::string sbmlTC;

  std::vector<FieldValues_t> mFields;
};

#endif // SF_INCLUDED