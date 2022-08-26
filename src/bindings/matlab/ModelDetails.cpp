#include "ModelDetails.h"
#include "StructureFields.h"

#include <sbml/extension/SBasePlugin.h>

ModelDetails::ModelDetails(GV& gv_)
    :gv(gv_)
{
  mPackageMap.clear();
  mSupportedPackages.clear();

  mLevel = StructureFields::readUint(gv.modelArray, "SBML_level", 0, gv);
  mVersion = StructureFields::readUint(gv.modelArray, "SBML_version", 0, gv);

  mDelaySymbol = StructureFields::readString(gv.modelArray, "delay_symbol", 0, gv);
  mTimeSymbol = StructureFields::readString(gv.modelArray, "time_symbol", 0, gv);
  mAvogadroSymbol = StructureFields::readString(gv.modelArray, "avogadro_symbol", 0, gv);

  populateNamespaces();
  populatePkgMap();
}

ModelDetails::ModelDetails(SBMLDocument* doc, GV& gv_)
    :gv(gv_)
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
  mxArray* mxNamespaces = mxGetField(gv.modelArray, 0, "namespaces");
  size_t nNoNamespaces = mxGetNumberOfElements(mxNamespaces);

  for (unsigned int i = 0; i < nNoNamespaces; ++i)
  {
    std::string uri = StructureFields::readString(mxNamespaces, "uri", i, gv);
    std::string prefix = StructureFields::readString(mxNamespaces, "prefix", i, gv);
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
  for (unsigned int i = 0; i <  SBMLExtensionRegistry::getNumRegisteredPackages(); ++i)
  {
    mSupportedPackages.append(SBMLExtensionRegistry::getRegisteredPackageName(i));
  }
}

void
ModelDetails::populatePkgMap()
{
  populateSupportedPackages();
  XMLNamespaces *xmlns = mSBMLns->getNamespaces();
  for (int i = 0; i < xmlns->getNumNamespaces(); ++i)
  {
    if (isSupportedPackageNS(xmlns->getURI(i), xmlns->getPrefix(i)))
    {
      std::string prefix = xmlns->getPrefix(i);
      std::string name = prefix + "_version";
      unsigned int version = StructureFields::readUint(gv.modelArray, name, 0, gv);
      mPackageMap.insert(std::pair<const std::string, unsigned int>(prefix, version));
    }
  }
}

void
ModelDetails::populatePkgMap(SBMLDocument* doc)
{
  populateSupportedPackages();
  XMLNamespaces *xmlns = mSBMLns->getNamespaces();
  for (int i = 0; i < xmlns->getNumNamespaces(); ++i)
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


