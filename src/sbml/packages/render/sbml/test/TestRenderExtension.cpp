//
// Filename    : TestRenderExtension.cpp
// Description : Tests for the RenderExtension

#include <sbml/common/common.h>
#include <sbml/common/extern.h>
#include <sbml/conversion/ConversionProperties.h>
#include <sbml/SBMLReader.h>
#include <sbml/SBMLWriter.h>
#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/packages/layout/sbml/test/utility.h>
#include <sbml/packages/render/common/RenderExtensionTypes.h>
#include <sbml/packages/layout/common/LayoutExtensionTypes.h>

#include <RenderPoint.h>

#include <check.h>
#include <limits>
#include <string>


LIBSBML_CPP_NAMESPACE_USE
using namespace std;

BEGIN_C_DECLS

extern char *TestDataDirectory;

START_TEST ( test_RenderExtension_createPkgNs )
{
  RenderPkgNamespaces ns;
  fail_unless(ns.getLevel() == 3);
  fail_unless(ns.getVersion() == 1);
  fail_unless(ns.getPackageVersion() == 1);
  fail_unless(ns.getPackageName() == "render");

  RenderPkgNamespaces ns1 (2, 1, 1, "render");
  fail_unless(ns1.getLevel() == 2);
  fail_unless(ns1.getVersion() == 1);
  fail_unless(ns1.getPackageVersion() == 1);
  fail_unless(ns1.getPackageName() == "render");


}
END_TEST

START_TEST ( test_RenderExtension_writeL3Render )
{
  RenderPkgNamespaces renderns;
  LayoutPkgNamespaces layoutns;
  
  SBMLDocument doc(3,1);

  doc.enablePackage(layoutns.getURI(), "layout", true);
  doc.enablePackage(renderns.getURI(), "render", true);

  Model* model = doc.createModel();

  LayoutModelPlugin *lPlugin = (LayoutModelPlugin*)model->getPlugin("layout");
  fail_unless(lPlugin != NULL);

  Layout* layout = lPlugin->createLayout();
  Dimensions dim(&layoutns, 100,100 );
  layout->setDimensions(&dim);

  GraphicalObject* additional = layout->createAdditionalGraphicalObject();
  additional->setId("go1");
  BoundingBox bb(&layoutns, "bb1", 10, 10, 90, 90);
  additional->setBoundingBox(&bb);

  RenderLayoutPlugin *rPlugin = (RenderLayoutPlugin*)layout->getPlugin("render");
  fail_unless(rPlugin != NULL);  

  LocalRenderInformation* local = rPlugin->createLocalRenderInformation();

  ColorDefinition *black = local->createColorDefinition();
  black->setId("black");
  black->setColorValue("#000000");

  LocalStyle *lStyle = local->createStyle("style_go1");
  lStyle->addId("go1");
  RenderGroup* group = lStyle->getGroup();
  group->setStroke("black");


  std::string sbml = writeSBMLToStdString(&doc);

  fail_unless (sbml.length() != 0);

}
END_TEST


START_TEST ( test_RenderExtension_graphicalObject )
{
  RenderPkgNamespaces renderns;
  LayoutPkgNamespaces layoutns;
  
  SBMLDocument doc(3,1);

  doc.enablePackage(layoutns.getURI(), "layout", true);
  doc.enablePackage(renderns.getURI(), "render", true);

  Model* model = doc.createModel();

  LayoutModelPlugin *lPlugin = (LayoutModelPlugin*)model->getPlugin("layout");
  fail_unless(lPlugin != NULL);

  Layout* layout = lPlugin->createLayout();
  Dimensions dim(&layoutns, 100,100 );
  layout->setDimensions(&dim);

  GraphicalObject* additional = layout->createAdditionalGraphicalObject();
  additional->setId("go1");
  BoundingBox bb(&layoutns, "bb1", 10, 10, 90, 90);
  additional->setBoundingBox(&bb);
  RenderGraphicalObjectPlugin* goPlugin = (RenderGraphicalObjectPlugin*)additional->getPlugin("render");
  fail_unless(goPlugin != NULL);
  goPlugin->setObjectRole("myRole");


  RenderLayoutPlugin *rPlugin = (RenderLayoutPlugin*)layout->getPlugin("render");
  fail_unless(rPlugin != NULL);  

  LocalRenderInformation* local = rPlugin->createLocalRenderInformation();

  ColorDefinition *black = local->createColorDefinition();
  black->setId("black");
  black->setColorValue("#000000");

  LocalStyle *lStyle = local->createStyle("style_go1");
  lStyle->addId("go1");
  RenderGroup* group = lStyle->getGroup();
  group->setStroke("black");


  std::string sbml = writeSBMLToStdString(&doc);

  fail_unless (sbml.length() != 0);

  // read back

  SBMLDocument *doc2 = readSBMLFromString(sbml.c_str());
  LayoutModelPlugin *lPlugin2 = (LayoutModelPlugin*)doc2->getModel()->getPlugin("layout");
  fail_unless(lPlugin2 != NULL);
  GraphicalObject *go2 = lPlugin2->getLayout(0)->getAdditionalGraphicalObject(0);
  fail_unless(go2 != NULL);
  RenderGraphicalObjectPlugin* goPlugin2 = (RenderGraphicalObjectPlugin*)go2->getPlugin("render");
  fail_unless(goPlugin2 != NULL);
  fail_unless(goPlugin2->getObjectRole() == "myRole");

  delete doc2;
}
END_TEST

  
START_TEST ( test_RenderExtension_gradient )
{
  RenderPkgNamespaces renderns;
  LayoutPkgNamespaces layoutns;
  
  SBMLDocument doc(3,1);

  doc.enablePackage(layoutns.getURI(), "layout", true);
  doc.enablePackage(renderns.getURI(), "render", true);

  Model* model = doc.createModel();

  LayoutModelPlugin *lPlugin = (LayoutModelPlugin*)model->getPlugin("layout");
  fail_unless(lPlugin != NULL);

  Layout* layout = lPlugin->createLayout();
  Dimensions dim(&layoutns, 100,100 );
  layout->setDimensions(&dim);

  GraphicalObject* additional = layout->createAdditionalGraphicalObject();
  additional->setId("go1");
  BoundingBox bb(&layoutns, "bb1", 10, 10, 90, 90);
  additional->setBoundingBox(&bb);
  RenderGraphicalObjectPlugin* goPlugin = (RenderGraphicalObjectPlugin*)additional->getPlugin("render");
  fail_unless(goPlugin != NULL);
  goPlugin->setObjectRole("myRole");


  RenderLayoutPlugin *rPlugin = (RenderLayoutPlugin*)layout->getPlugin("render");
  fail_unless(rPlugin != NULL);  

  LocalRenderInformation* local = rPlugin->createLocalRenderInformation();

  ColorDefinition *black = local->createColorDefinition();
  black->setId("black");
  black->setColorValue("#000000");

  ColorDefinition *grey = local->createColorDefinition();
  grey->setId("grey");
  grey->setColorValue("#F0F0F0");

  LinearGradient* gradient = local->createLinearGradientDefinition();
  gradient->setId("test");
  gradient->setPoint1(RelAbsVector(), RelAbsVector());
  gradient->setPoint2(RelAbsVector(0,100), RelAbsVector(0,100));

  GradientStop *stop = gradient->createGradientStop();
  stop->setOffset(RelAbsVector());
  stop->setStopColor("white");

  stop = gradient->createGradientStop();
  stop->setOffset(RelAbsVector(0, 100));
  stop->setStopColor("silver");

  std::string smodel = writeSBMLToStdString(&doc);

  fail_unless( stop->getStopColor() == "silver" );
}
END_TEST

START_TEST ( test_RenderExtension_convertLevel )
{
  string filename(TestDataDirectory);
  filename += "FutileCycle.xml";
  // read document
  SBMLDocument* doc = readSBMLFromFile(filename.c_str());
  doc->checkConsistency();

  // assert all is good
  fail_unless (doc->getModel() != NULL);
  fail_unless (doc->getNumErrors(LIBSBML_SEV_ERROR) == 0);

  // convert to L3 *without* upgrading the layout / render package
  bool result = doc->setLevelAndVersion(3,1, false, true);
  fail_unless(result == true);

  // write to string  
  std::string l3SBML = writeSBMLToStdString(doc);

  delete doc;

  // read string back
  doc = readSBMLFromString(l3SBML.c_str());
  doc->checkConsistency();

  // assert all is good
  fail_unless (doc->getModel() != NULL);
  fail_unless (doc->getNumErrors(LIBSBML_SEV_ERROR) == 0);

  delete doc;

  filename = TestDataDirectory;
  filename += "FutileCycle.xml";
  // read document
  doc = readSBMLFromFile(filename.c_str());

  SBMLNamespaces nsl3v1(3, 1);
  ConversionProperties prop(&nsl3v1);
  prop.addOption("strict", false);
  prop.addOption("setLevelAndVersion", true);
  prop.addOption("ignorePackages", true);

  fail_unless(doc->convert(prop) == LIBSBML_OPERATION_SUCCESS);

  // now upgrade layout / render package
  SBMLDocumentPlugin *docPlugin = (SBMLDocumentPlugin*)doc->getPlugin("layout");
  if (docPlugin != NULL)
      docPlugin->setElementNamespace(LayoutExtension::getXmlnsL3V1V1());
  doc->getSBMLNamespaces()->addPackageNamespace("layout", 1);
  doc->setPackageRequired("layout", false);
  

  docPlugin = (SBMLDocumentPlugin*)doc->getPlugin("render");
  if (docPlugin != NULL)
      docPlugin->setElementNamespace(RenderExtension::getXmlnsL3V1V1());
  else 
    doc->enablePackage(RenderExtension::getXmlnsL3V1V1(), "render", true);

  doc->getSBMLNamespaces()->addPackageNamespace("render", 1);
  doc->setPackageRequired("render", false);


  doc->checkConsistency();

  // assert all is good
  fail_unless (doc->getModel() != NULL);
  fail_unless (doc->getNumErrors(LIBSBML_SEV_ERROR) == 0);

  //l3SBML = writeSBMLToString(doc);

  delete doc;
}
END_TEST

Suite *
create_suite_RenderExtension (void)
{
  Suite *suite = suite_create("RenderExtension");
  TCase *tcase = tcase_create("RenderExtension");


  tcase_add_test( tcase, test_RenderExtension_createPkgNs            );
  tcase_add_test( tcase, test_RenderExtension_writeL3Render          );
  tcase_add_test( tcase, test_RenderExtension_convertLevel           );
  tcase_add_test( tcase, test_RenderExtension_graphicalObject        );
  tcase_add_test( tcase, test_RenderExtension_gradient               );
  
  suite_add_tcase(suite, tcase);

  return suite;
}

END_C_DECLS
