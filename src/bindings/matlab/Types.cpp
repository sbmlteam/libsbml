#ifndef TYPES_CODE_INCLUDED
#define TYPES_CODE_INCLUDED

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


const char* FIELDTYPE_STRINGS[] =
{
      "bool"
    , "char"
    , "double"
    , "int"
    , "structure"
    , "uint"
};


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


class StructureFields
{
public:

  StructureFields(SBase* obj, mxArray* structure, GV& gv);

  StructureFields(SBase* obj, GV& gv);

  StructureFields(std::string& tc, GV& gv);

  ~StructureFields();


  void addAttributes(const std::string& functionId, unsigned int index = 0, 
                     unsigned int total_no = 0);

  void createStructure(const std::string& functionId, SBase* base, bool usePlugin = false, 
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
    unsigned int index, FieldValues_t field, bool usePlugin);

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


  bool usingPlugin(const std::string& prefix, SBase* base = NULL);

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


class ModelDetails
{
public:
  ModelDetails(GV &gv);

  ModelDetails(SBMLDocument *doc, GV& gv);

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

  GV& gv;

};


#endif // TYPES_CODE_INCLUDED