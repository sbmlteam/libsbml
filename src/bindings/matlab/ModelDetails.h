#ifndef MODEL_DETAILS_INCLUDED
#define MODEL_DETAILS_INCLUDED

#include "Variables.h"

#include <sbml/extension/SBMLExtensionRegistry.h>

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


#endif // MODEL_DETAILS_INCLUDED