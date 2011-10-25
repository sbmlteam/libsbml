/**
 * @file    addLayout.cpp
 * @brief   SBML Layout example
 * @author  Sarah Keating
 * 
 * This example adds a layout to the loaded model. 
 */

#include <iostream>
#include <string>

#include <sbml/SBMLDocument.h>
#include <sbml/Model.h>
#include <sbml/Compartment.h>
#include <sbml/Species.h>
#include <sbml/SpeciesReference.h>
#include <sbml/Reaction.h>

#include <sbml/packages/layout/sbml/Layout.h>
#include <sbml/packages/layout/sbml/CompartmentGlyph.h>
#include <sbml/packages/layout/sbml/SpeciesGlyph.h>
#include <sbml/packages/layout/sbml/ReactionGlyph.h>
#include <sbml/packages/layout/sbml/SpeciesReferenceGlyph.h>
#include <sbml/packages/layout/sbml/SpeciesReferenceGlyph.h>
#include <sbml/packages/layout/extension/LayoutExtension.h>
#include <sbml/packages/layout/extension/LayoutModelPlugin.h>
#include <sbml/packages/layout/sbml/Curve.h>
#include <sbml/packages/layout/sbml/Dimensions.h>
#include <sbml/packages/layout/sbml/BoundingBox.h>
#include <sbml/packages/layout/sbml/Point.h>
#include <sbml/packages/layout/sbml/LineSegment.h>
#include <sbml/packages/layout/sbml/CubicBezier.h>
#include <sbml/SBMLWriter.h>

using namespace std;
LIBSBML_CPP_NAMESPACE_USE

int main(int argc,char** argv){

  if (argc != 2)
  {
    cout << endl << "Usage: addLayout input-filename"
         << endl << endl;
    return 2;
  }

  SBMLDocument *d = readSBML(argv[1]);
  if ( d->getNumErrors() > 0)
  {
    d->printErrors();
  }
  else
  {
    // enable the layout package
    // note layout in L2 uses a different namespace than L3
    if (d->getLevel() == 2)
    {
      d->enablePackage(LayoutExtension::getXmlnsL2(),"layout", true);
    }
    else if (d->getLevel() == 3)
    {
      d->enablePackage(LayoutExtension::getXmlnsL3V1V1(),"layout", true);
    }

    // get the model plugin
    Model * model = d->getModel();

    LayoutModelPlugin* mplugin = static_cast<LayoutModelPlugin*>(model->getPlugin("layout"));
    
    // create the Layout
    LayoutPkgNamespaces layoutns(d->getLevel(), d->getVersion());
    Layout* layout=mplugin->createLayout();

    layout->setId("Layout_1");
    Dimensions dim(&layoutns, 400.0,220.0);
    layout->setDimensions(&dim);

    // write out the model
    writeSBML(d, "added-layout.xml");
  }
}

