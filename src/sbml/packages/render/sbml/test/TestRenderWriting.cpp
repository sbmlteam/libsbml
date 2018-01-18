/**
 * Filename    : TestRenderWriting.cpp
 * Description : Unit tests for writing render information in the 
 *               context of a complete model with layout.
 * Organization: University of Heidelberg
 * Created     : 2008-10-24
 *
 * Copyright 2008 University of Heidelberg
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 2.1 of the License, or
 * any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY, WITHOUT EVEN THE IMPLIED WARRANTY OF
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  The software and
 * documentation provided hereunder is on an "as is" basis, and the
 * European Media Laboratories Research gGmbH have no obligations to
 * provide maintenance, support, updates, enhancements or modifications.
 * In no event shall the European Media Laboratories Research gGmbH be
 * liable to any party for direct, indirect, special, incidental or
 * consequential damages, including lost profits, arising out of the use of
 * this software and its documentation, even if the European Media
 * Laboratories Research gGmbH have been advised of the possibility of such
 * damage.  See the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * The original code contained here was initially developed by:
 *
 *     Ralph Gauges
 *     Group for Modelling of Biological Systems 
 *     University of Heidelberg 
 *     Im Neuenheimer Feld 267
 *     69120 Heidelberg
 *     Germany
 *
 *     http://otto.bioquant.uni-heidelberg.de
 *     mailto:ralph.gauges@bioquant.uni-heidelberg.de
 *
 * Contributor(s):
 */

#include <sbml/common/common.h>
#include <sbml/common/extern.h>

#include <sbml/SBMLReader.h>
#include <sbml/SBMLWriter.h>
#include <sbml/SBMLDocument.h>
#include <sbml/Model.h>
#include <sbml/packages/layout/sbml/test/utility.h>
#include <sbml/packages/layout/extension/LayoutModelPlugin.h>
#include <sbml/packages/layout/sbml/Layout.h>
#include <sbml/packages/layout/sbml/Point.h>
#include <sbml/packages/layout/sbml/LineSegment.h>
#include <sbml/packages/layout/sbml/CubicBezier.h>
#include <sbml/packages/layout/sbml/Curve.h>

#include <sbml/packages/render/extension/RenderLayoutPlugin.h>
#include <sbml/packages/render/extension/RenderListOfLayoutsPlugin.h>
#include <sbml/packages/render/extension/RenderGraphicalObjectPlugin.h>
#include <sbml/packages/render/sbml/GlobalRenderInformation.h>
#include <sbml/packages/render/sbml/LocalRenderInformation.h>
#include <sbml/packages/render/sbml/ColorDefinition.h>
#include <sbml/packages/render/sbml/LocalStyle.h>
#include <sbml/packages/render/sbml/GlobalStyle.h>
#include <sbml/packages/render/sbml/Rectangle.h>
#include <sbml/packages/render/sbml/Polygon.h>
#include <sbml/packages/render/sbml/Text.h>
#include <sbml/packages/render/sbml/RenderPoint.h>
#include <sbml/packages/render/sbml/RenderCubicBezier.h>

#include <check.h>
#include <limits>

#include <string>

LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS

static RenderPkgNamespaces* renderns;
static LayoutPkgNamespaces* layoutns;

void
RenderWriting_setup (void)
{
  renderns = new (std::nothrow) RenderPkgNamespaces();
  layoutns = new (std::nothrow) LayoutPkgNamespaces();
}

void 
RenderWriting_teardown (void)
{
  delete renderns;
  delete layoutns;
}


START_TEST (test_RenderWriting_write_l2_ojectrole)
{
  SBMLDocument doc(2, 4);
  doc.getSBMLNamespaces()->addPackageNamespace("layout", 1);
  doc.getSBMLNamespaces()->addPackageNamespace("render", 1);
  Model* model = doc.createModel();
  model->setId("test");
  Compartment *comp = model->createCompartment();
  comp->initDefaults();
  comp->setId("c1");
  Species* species = model->createSpecies();
  species->initDefaults();
  species->setId("s1");
  species->setCompartment("c1");
  LayoutModelPlugin* layoutPlugin = (LayoutModelPlugin*)model->getPlugin("layout");
  Layout* layout= layoutPlugin ->createLayout();
  layout->setId("l1");
  SpeciesGlyph* glyph = layout->createSpeciesGlyph();
  glyph->initDefaults();
  glyph->setId("g1");

  RenderGraphicalObjectPlugin* goPlugin = (RenderGraphicalObjectPlugin*)glyph->getPlugin("render");
  goPlugin->setObjectRole("myRole");

  std::string  modelString = writeSBMLToStdString(&doc);

 
  std::string::size_type index = modelString.find("objectRole=\"myRole");
  fail_unless(index != std::string::npos);

}
END_TEST


START_TEST (test_RenderWriting_write_model_1)
{
  const char* s = 
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<sbml xmlns=\"http://www.sbml.org/sbml/level2\" level=\"2\" version=\"1\">\n"
    "  <model id=\"TestModel\">\n"
    "          <annotation>\n"
    "  <listOfLayouts xmlns=\"http://projects.eml.org/bcb/sbml/level2\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">\n"
    "    <annotation xmlns=\"http://www.sbml.org/sbml/level2\">\n"
    "      <listOfGlobalRenderInformation xmlns=\"http://projects.eml.org/bcb/sbml/render/level2\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">\n"
    "        <renderInformation id=\"wireFrame\" name=\"wireframe style\" \n"
    "                           programName=\"Ralph Gauges\" programVersion=\"1.0\" backgroundColor='#FFFFFFFF'>\n"
    "          <listOfColorDefinitions>\n"
    "            <colorDefinition id=\"white\" value=\"#ffffff\"/>\n"
    "            <colorDefinition id=\"black\" value=\"#000000\"/>\n"
    "          </listOfColorDefinitions>\n"
    "          <listOfStyles>\n"
    "            <style id=\"compartmentGlyphStyle\" typeList=\"COMPARTMENTGLYPH\">\n"
    "              <g stroke=\"black\" stroke-width=\"1\">\n"
    "                <rectangle x=\"0\" y=\"0\" width=\"100%\" height=\"100%\"/>\n"
    "              </g>\n"
    "            </style>  \n"
    "            <style id=\"speciesGlyphStyle\" typeList=\"SPECIESGLYPH\">\n"
    "              <g stroke=\"black\" stroke-width=\"1\">\n"
    "                <rectangle x=\"0\" y=\"0\" width=\"100%\" height=\"100%\"/>\n"
    "              </g>\n"
    "            </style>\n"
    "            <style id=\"reactionGlyphStyle\" typeList=\"REACTIONGLYPH SPECIESREFERENCEGLYPH TEXTGLYPH\">\n"
    "              <g stroke=\"black\" stroke-width=\"1\" font-size=\"12\" text-anchor=\"middle\" />\n"
    "            </style>\n"
    "          </listOfStyles>\n"
    "        </renderInformation>\n"
    "        <renderInformation id=\"defaultGrayStyle\" name=\"grayscale style\" \n"
    "                           programName=\"Ralph Gauges\" programVersion=\"1.0\" backgroundColor='#FFFFFFFF'>\n"
    "          <listOfColorDefinitions>\n"
    "            <colorDefinition id=\"lightGray\" value=\"#cecece\"/>\n"
    "            <colorDefinition id=\"white\" value=\"#ffffff\"/>\n"
    "            <colorDefinition id=\"black\" value=\"#000000\"/>\n"
    "            <colorDefinition id=\"lightGray2\" value=\"#f0f0f0\" />\n"
    "            <colorDefinition id=\"gray\" value=\"#0b0b0b\" />\n"
    "          </listOfColorDefinitions>\n"
    "          <listOfGradientDefinitions>\n"
    "            <radialGradient id=\"speciesGlyphGradient\">\n"
    "              <stop offset=\"0\" stop-color=\"white\" />\n"
    "              <stop offset=\"100%\" stop-color=\"lightGray\" />\n"
    "            </radialGradient>\n"
    "          </listOfGradientDefinitions>\n"
    "          <listOfLineEndings>\n"
    "           <lineEnding id=\"simpleHead_black\">\n"
    "             <boundingBox  xmlns=\"http://projects.eml.org/bcb/sbml/level2\" >\n"
    "               <position x=\"-8\" y=\"-3\" />\n"
    "               <dimensions width=\"10\" height=\"6\" />\n"
    "             </boundingBox>\n"
    "             <g stroke=\"black\" stroke-width=\"1\" fill=\"black\">\n"
    "               <polygon>\n"
    "                 <listOfElements>\n"
    "                    <element xsi:type=\"RenderPoint\" x=\"0\" y=\"0\"/>\n"
    "                    <element xsi:type=\"RenderPoint\" x=\"10\" y=\"3\"/>\n"
    "                    <element xsi:type=\"RenderPoint\" x=\"0\" y=\"6\"/>\n"
    "                 </listOfElements>\n"
    "               </polygon>\n"
    "             </g>\n"
    "           </lineEnding>\n"
    "          </listOfLineEndings>\n"
    "          <listOfStyles>\n"
    "            <style id=\"compartmentGlyphStyle\" typeList=\"COMPARTMENTGLYPH\">\n"
    "              <g stroke=\"gray\" stroke-width=\"1\">\n"
    "                <rectangle fill=\"lightGray2\" x=\"0\" y=\"0\" width=\"100%\" height=\"100%\" rx=\"5%\" />\n"
    "              </g>\n"
    "            </style>  \n"
    "            <style id=\"speciesGlyphStyle\" typeList=\"SPECIESGLYPH\">\n"
    "              <g stroke=\"black\" stroke-width=\"1\">\n"
    "                <rectangle fill=\"speciesGlyphGradient\" x=\"0\" y=\"0\" width=\"100%\" height=\"100%\" rx=\"5%\" />\n"
    "              </g>\n"
    "            </style>\n"
    "            <style id=\"reactionGlyphStyle\" typeList=\"REACTIONGLYPH TEXTGLYPH\">\n"
    "              <g stroke=\"black\" stroke-width=\"1\" font-size=\"12\" text-anchor=\"middle\" />\n"
    "            </style>\n"
    "            <style id=\"reactantSpeciesReferenceGlyphStyle\" roleList=\"product sideproduct sidesubstrate substrate\">\n"
    "              <g stroke=\"#000000\" stroke-width=\"1\" />\n"
    "            </style>\n"
    "            <style id=\"activatorSpeciesReferenceGlyphStyle\" roleList=\"activator\">\n"
    "              <g stroke=\"#000000\" stroke-width=\"1\" />\n"
    "            </style>\n"
    "            <style id=\"modifierSpeciesReferenceGlyphStyle\" roleList=\"modifier\">\n"
    "              <g stroke=\"#000000\" stroke-width=\"1\" />\n"
    "            </style>\n"
    "            <style id=\"inhibitorSpeciesReferenceGlyphStyle\" roleList=\"inhibitor\">\n"
    "              <g stroke=\"#000000\" stroke-width=\"1\" />\n"
    "            </style>\n"
    "          </listOfStyles>\n"
    "        </renderInformation>\n"
    "        <renderInformation id=\"shortGrayStyle\" name=\"modified default style to grayscale\" referenceRenderInformation=\"defaultStyle\" \n"
    "                           programName=\"Ralph Gauges\" programVersion=\"1.0\" backgroundColor='#FFFFFFFF'>\n"
    "          <listOfColorDefinitions>\n"
    "            <colorDefinition id=\"lightBlue\" value=\"#cecece\"/>\n"
    "            <colorDefinition id=\"white\" value=\"#ffffff\"/>\n"
    "            <colorDefinition id=\"black\" value=\"#000000\"/>\n"
    "            <colorDefinition id=\"red\" value=\"#000000\"/>\n"
    "            <colorDefinition id=\"green\" value=\"#000000\"/>\n"
    "            <colorDefinition id=\"blue\" value=\"#000000\"/>\n"
    "            <colorDefinition id=\"lightYellow\" value=\"#f0f0f0\" />\n"
    "            <colorDefinition id=\"darkGreen\" value=\"#0b0b0b\" />\n"
    "          </listOfColorDefinitions>\n"
    "        </renderInformation>\n"
    "        <renderInformation id=\"defaultStyle\" name=\"default style\" \n"
    "                           programName=\"Ralph Gauges\" programVersion=\"1.0\" backgroundColor='#FFFFFFFF'>\n"
    "          <listOfColorDefinitions>\n"
    "            <colorDefinition id=\"lightBlue\" value=\"#add8e6\"/>\n"
    "            <colorDefinition id=\"white\" value=\"#ffffff\"/>\n"
    "            <colorDefinition id=\"black\" value=\"#000000\"/>\n"
    "            <colorDefinition id=\"red\" value=\"#ff0000\"/>\n"
    "            <colorDefinition id=\"green\" value=\"#00ff00\"/>\n"
    "            <colorDefinition id=\"blue\" value=\"#0000ff\"/>\n"
    "            <colorDefinition id=\"lightYellow\" value=\"#ffffd1\" />\n"
    "            <colorDefinition id=\"darkGreen\" value=\"#002000\" />\n"
    "          </listOfColorDefinitions>\n"
    "          <listOfGradientDefinitions>\n"
    "            <radialGradient id=\"speciesGlyphGradient\">\n"
    "              <stop offset=\"0\" stop-color=\"white\" />\n"
    "              <stop offset=\"100%\" stop-color=\"lightBlue\" />\n"
    "            </radialGradient>\n"
    "          </listOfGradientDefinitions>\n"
    "          <listOfLineEndings>\n"
    "           <lineEnding id=\"simpleHead_black\">\n"
    "             <boundingBox xmlns=\"http://projects.eml.org/bcb/sbml/level2\" >\n"
    "               <position x=\"-8\" y=\"-3\" />\n"
    "               <dimensions width=\"10\" height=\"6\" />\n"
    "             </boundingBox>\n"
    "             <g stroke=\"black\" stroke-width=\"1\" fill=\"black\">\n"
    "               <polygon>\n"
    "                 <listOfElements>\n"
    "                    <element xsi:type=\"RenderPoint\" x=\"0\" y=\"0\"/>\n"
    "                    <element xsi:type=\"RenderPoint\" x=\"10\" y=\"3\"/>\n"
    "                    <element xsi:type=\"RenderPoint\" x=\"0\" y=\"6\"/>\n"
    "                 </listOfElements>\n"
    "               </polygon>\n"
    "             </g>\n"
    "           </lineEnding>\n"
    "           <lineEnding id=\"simpleHead_red\">\n"
    "             <boundingBox xmlns=\"http://projects.eml.org/bcb/sbml/level2\" >\n"
    "               <position x=\"-8\" y=\"-3\" />\n"
    "               <dimensions width=\"10\" height=\"6\" />\n"
    "             </boundingBox>\n"
    "             <g stroke=\"red\" stroke-width=\"1\" fill=\"red\">\n"
    "               <polygon>\n"
    "                 <listOfElements>\n"
    "                    <element xsi:type=\"RenderPoint\" x=\"0\" y=\"0\"/>\n"
    "                    <element xsi:type=\"RenderPoint\" x=\"10\" y=\"3\"/>\n"
    "                    <element xsi:type=\"RenderPoint\" x=\"0\" y=\"6\"/>\n"
    "                 </listOfElements>\n"
    "               </polygon>\n"
    "             </g>\n"
    "           </lineEnding>\n"
    "           <lineEnding id=\"simpleHead_green\">\n"
    "             <boundingBox xmlns=\"http://projects.eml.org/bcb/sbml/level2\" >\n"
    "               <position x=\"-8\" y=\"-3\" />\n"
    "               <dimensions width=\"10\" height=\"6\" />\n"
    "             </boundingBox>\n"
    "             <g stroke=\"green\" stroke-width=\"1\" fill=\"green\">\n"
    "               <polygon>\n"
    "                 <listOfElements>\n"
    "                    <element xsi:type=\"RenderPoint\" x=\"0\" y=\"0\"/>\n"
    "                    <element xsi:type=\"RenderPoint\" x=\"10\" y=\"3\"/>\n"
    "                    <element xsi:type=\"RenderPoint\" x=\"0\" y=\"6\"/>\n"
    "                 </listOfElements>\n"
    "               </polygon>\n"
    "             </g>\n"
    "           </lineEnding>\n"
    "           <lineEnding id=\"simpleHead_blue\">\n"
    "             <boundingBox xmlns=\"http://projects.eml.org/bcb/sbml/level2\" >\n"
    "               <position x=\"-8\" y=\"-3\" />\n"
    "               <dimensions width=\"10\" height=\"6\" />\n"
    "             </boundingBox>\n"
    "             <g stroke=\"blue\" stroke-width=\"1\" fill=\"blue\">\n"
    "               <polygon>\n"
    "                 <listOfElements>\n"
    "                    <element xsi:type=\"RenderPoint\" x=\"0\" y=\"0\"/>\n"
    "                    <element xsi:type=\"RenderPoint\" x=\"10\" y=\"3\"/>\n"
    "                    <element xsi:type=\"RenderPoint\" x=\"0\" y=\"6\"/>\n"
    "                 </listOfElements>\n"
    "               </polygon>\n"
    "             </g>\n"
    "           </lineEnding>\n"
    "          </listOfLineEndings>\n"
    "          <listOfStyles>\n"
    "            <style id=\"compartmentGlyphStyle\" typeList=\"COMPARTMENTGLYPH\">\n"
    "              <g stroke=\"darkGreen\" stroke-width=\"1\">\n"
    "                <rectangle fill=\"lightYellow\" x=\"0\" y=\"0\" width=\"100%\" height=\"100%\" rx=\"10%\" ry=\"10%\" />\n"
    "              </g>\n"
    "            </style>  \n"
    "            <style id=\"speciesGlyphStyle\" typeList=\"SPECIESGLYPH\">\n"
    "              <g stroke=\"black\" stroke-width=\"1\">\n"
    "                <rectangle fill=\"speciesGlyphGradient\" x=\"0\" y=\"0\" width=\"100%\" height=\"100%\" rx=\"5\" ry=\"50%\" />\n"
    "              </g>\n"
    "            </style>\n"
    "            <style id=\"reactionGlyphStyle\" typeList=\"REACTIONGLYPH TEXTGLYPH\">\n"
    "              <g stroke=\"black\" stroke-width=\"1\" font-size=\"12\" text-anchor=\"middle\" />\n"
    "            </style>\n"
    "            <style id=\"reactantSpeciesReferenceGlyphStyle\" roleList=\"product sideproduct sidesubstrate substrate\">\n"
    "              <g stroke=\"#000000\" stroke-width=\"1\" endHead=\"simpleHead_black\" />\n"
    "            </style>\n"
    "            <style id=\"activatorSpeciesReferenceGlyphStyle\" roleList=\"activator\">\n"
    "              <g stroke=\"green\" stroke-width=\"1\" endHead=\"simpleHead_green\" />\n"
    "            </style>\n"
    "            <style id=\"modifierSpeciesReferenceGlyphStyle\" roleList=\"modifier\">\n"
    "              <g stroke=\"blue\" stroke-width=\"1\" endHead=\"simpleHead_blue\" />\n"
    "            </style>\n"
    "            <style id=\"inhibitorSpeciesReferenceGlyphStyle\" roleList=\"inhibitor\">\n"
    "              <g stroke=\"red\" stroke-width=\"1\" endHead=\"simpleHead_red\" />\n"
    "            </style>\n"
    "          </listOfStyles>\n"
    "        </renderInformation>\n"
    "      </listOfGlobalRenderInformation>\n"
    "    </annotation>\n"
    "    <layout id=\"Layout_1\">\n"
    "      <annotation xmlns=\"http://www.sbml.org/sbml/level2\">\n"
    "        <listOfRenderInformation xmlns=\"http://projects.eml.org/bcb/sbml/render/level2\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" versionMajor='2' versionMinor='1' >\n"
    "          <renderInformation id=\"highlightGlucose\" referenceRenderInformation=\"defaultStyle\"\n"
    "                             programName=\"Ralph Gauges\" programVersion=\"1.0\" backgroundColor='#FFFFFFFF'>\n"
    "            <listOfColorDefinitions>\n"
    "              <colorDefinition id=\"lightRed\" value=\"#e6add8\"/>\n"
    "              <colorDefinition id=\"white\" value=\"#ffffff\"/>\n"
    "            </listOfColorDefinitions>\n"
    "            <listOfGradientDefinitions>\n"
    "              <radialGradient id=\"highlightedSpeciesGlyphGradient\">\n"
    "                <stop offset=\"0\" stop-color=\"white\" />\n"
    "                <stop offset=\"100%\" stop-color=\"lightRed\" />\n"
    "              </radialGradient>\n"
    "            </listOfGradientDefinitions>\n"
    "            <listOfStyles>\n"
    "              <style id=\"highlightedGlucose\" idList=\"SpeciesGlyph_Glucose\" >\n"
    "                <g stroke=\"black\" stroke-width=\"1\">\n"
    "                  <rectangle fill=\"highlightedSpeciesGlyphGradient\" x=\"0\" y=\"0\" width=\"100%\" height=\"100%\" rx=\"5\" ry=\"50%\" />\n"
    "                </g>\n"
    "              </style>\n"
    "            </listOfStyles>\n"
    "          </renderInformation>\n"
    "        </listOfRenderInformation>\n"
    "      </annotation>\n"
    "      <dimensions width=\"2320\" height=\"1000\"/>\n"
    "      <listOfCompartmentGlyphs>\n"
    "        <compartmentGlyph id=\"CompartmentGlyph_1\" compartment=\"Hepatocyte\">\n"
    "          <boundingBox id=\"bb_compartment\">\n"
    "            <position x=\"10\" y=\"10\"/>\n"
    "            <dimensions width=\"2300\" height=\"980\"/>\n"
    "          </boundingBox>\n"
    "        </compartmentGlyph>\n"
    "        <compartmentGlyph id=\"Mito1_Glyph\" compartment=\"Mito_1\">\n"
    "          <boundingBox id=\"bb_mito1\">\n"
    "            <position x=\"100\" y=\"100\"/>\n"
    "            <dimensions width=\"300\" height=\"100\"/>\n"
    "          </boundingBox>\n"
    "        </compartmentGlyph>\n"
    "        <compartmentGlyph id=\"Mito2_Glyph\" compartment=\"Mito_2\">\n"
    "          <boundingBox id=\"bb_mito2\">\n"
    "            <position x=\"200\" y=\"650\"/>\n"
    "            <dimensions width=\"300\" height=\"100\"/>\n"
    "          </boundingBox>\n"
    "        </compartmentGlyph>\n"
    "        <compartmentGlyph id=\"Mito3_Glyph_2\" compartment=\"Mito_3\">\n"
    "          <boundingBox id=\"bb_mito3_2\">\n"
    "            <position x=\"1470\" y=\"30\"/>\n"
    "            <dimensions width=\"820\" height=\"536\"/>\n"
    "          </boundingBox>\n"
    "        </compartmentGlyph>\n"
    "      </listOfCompartmentGlyphs>\n"
    "      <listOfSpeciesGlyphs>\n"
    "        <speciesGlyph id=\"SpeciesGlyph_malate_cyt\" species=\"malate_cyt\">\n"
    "          <boundingBox id=\"bb_sg_malate_cyt\">\n"
    "            <position x=\"580\" y=\"280\"/>\n"
    "            <dimensions width=\"240\" height=\"36\"/>\n"
    "          </boundingBox>\n"
    "        </speciesGlyph>\n"
    "        <speciesGlyph id=\"SpeciesGlyph_oxaloacetate_cyt\" species=\"oxaloacetate_cyt\">\n"
    "          <boundingBox id=\"bb_sg_oxaloacetate_cyt\">\n"
    "            <position x=\"580\" y=\"480\"/>\n"
    "            <dimensions width=\"240\" height=\"36\"/>\n"
    "          </boundingBox>\n"
    "        </speciesGlyph>\n"
    "        <speciesGlyph id=\"SpeciesGlyph_aspartate_cyt\" species=\"aspartate_cyt\">\n"
    "          <boundingBox id=\"bb_sg_aspartate_cyt\">\n"
    "            <position x=\"580\" y=\"680\"/>\n"
    "            <dimensions width=\"240\" height=\"36\"/>\n"
    "          </boundingBox>\n"
    "        </speciesGlyph>\n"
    "        <speciesGlyph id=\"SpeciesGlyph_glutamate_cyt\" species=\"glutamate_cyt\">\n"
    "          <boundingBox id=\"bb_sg_glutamate_cyt\">\n"
    "            <position x=\"800\" y=\"610\"/>\n"
    "            <dimensions width=\"240\" height=\"36\"/>\n"
    "          </boundingBox>\n"
    "        </speciesGlyph>\n"
    "        <speciesGlyph id=\"SpeciesGlyph_aKetoglutarate_cyt\" species=\"aKetoglutarate_cyt\">\n"
    "          <boundingBox id=\"bb_sg_aKetoglutarate_cyt\">\n"
    "            <position x=\"860\" y=\"500\"/>\n"
    "            <dimensions width=\"280\" height=\"36\"/>\n"
    "          </boundingBox>\n"
    "        </speciesGlyph>\n"
    "        <speciesGlyph id=\"SpeciesGlyph_nad_cyt\" species=\"nad_cyt\">\n"
    "          <boundingBox id=\"bb_sg_nad_cyt\">\n"
    "            <position x=\"520\" y=\"350\"/>\n"
    "            <dimensions width=\"100\" height=\"24\"/>\n"
    "          </boundingBox>\n"
    "        </speciesGlyph>\n"
    "        <speciesGlyph id=\"SpeciesGlyph_nadh_cyt\" species=\"nadh_cyt\">\n"
    "          <boundingBox id=\"bb_sg_nadh_cyt\">\n"
    "            <position x=\"520\" y=\"430\"/>\n"
    "            <dimensions width=\"100\" height=\"24\"/>\n"
    "          </boundingBox>\n"
    "        </speciesGlyph>\n"
    "        <speciesGlyph id=\"SpeciesGlyph_h_cyt\" species=\"h_cyt\">\n"
    "          <boundingBox id=\"bb_sg_h_cyt\">\n"
    "            <position x=\"430\" y=\"430\"/>\n"
    "            <dimensions width=\"40\" height=\"24\"/>\n"
    "          </boundingBox>\n"
    "        </speciesGlyph>\n"
    "        <speciesGlyph id=\"SpeciesGlyph_malate_mito3\" species=\"malate_mito3\">\n"
    "          <boundingBox id=\"bb_sg_malate_mito3\">\n"
    "            <position x=\"1850\" y=\"80\"/>\n"
    "            <dimensions width=\"240\" height=\"36\"/>\n"
    "          </boundingBox>\n"
    "        </speciesGlyph>\n"
    "        <speciesGlyph id=\"SpeciesGlyph_oxaloacetate_mito3\" species=\"oxaloacetate_mito3\">\n"
    "          <boundingBox id=\"bb_sg_oxaloacetate_mito3\">\n"
    "            <position x=\"1850\" y=\"280\"/>\n"
    "            <dimensions width=\"240\" height=\"36\"/>\n"
    "          </boundingBox>\n"
    "        </speciesGlyph>\n"
    "        <speciesGlyph id=\"SpeciesGlyph_aspartate_mito3\" species=\"aspartate_mito3\">\n"
    "          <boundingBox id=\"bb_sg_aspartate_mito3\">\n"
    "            <position x=\"1850\" y=\"480\"/>\n"
    "            <dimensions width=\"240\" height=\"36\"/>\n"
    "          </boundingBox>\n"
    "        </speciesGlyph>\n"
    "        <speciesGlyph id=\"SpeciesGlyph_glutamate_mito3\" species=\"glutamate_mito3\">\n"
    "          <boundingBox id=\"bb_sg_glutamate_mito3\">\n"
    "            <position x=\"1550\" y=\"430\"/>\n"
    "            <dimensions width=\"240\" height=\"36\"/>\n"
    "          </boundingBox>\n"
    "        </speciesGlyph>\n"
    "        <speciesGlyph id=\"SpeciesGlyph_aKetoglutarate_mito3\" species=\"aKetoglutarate_mito3\">\n"
    "          <boundingBox id=\"bb_sg_aKetoglutarate_mito3\">\n"
    "            <position x=\"1530\" y=\"300\"/>\n"
    "            <dimensions width=\"280\" height=\"36\"/>\n"
    "          </boundingBox>\n"
    "        </speciesGlyph>\n"
    "        <speciesGlyph id=\"SpeciesGlyph_nad_mito3\" species=\"nad_mito3\">\n"
    "          <boundingBox id=\"bb_sg_nad_mito3\">\n"
    "            <position x=\"2050\" y=\"150\"/>\n"
    "            <dimensions width=\"100\" height=\"24\"/>\n"
    "          </boundingBox>\n"
    "        </speciesGlyph>\n"
    "        <speciesGlyph id=\"SpeciesGlyph_nadh_mito3\" species=\"nadh_mito3\">\n"
    "          <boundingBox id=\"bb_sg_nadh_mito3\">\n"
    "            <position x=\"2050\" y=\"230\"/>\n"
    "            <dimensions width=\"100\" height=\"24\"/>\n"
    "          </boundingBox>\n"
    "        </speciesGlyph>\n"
    "        <speciesGlyph id=\"SpeciesGlyph_h_mito3\" species=\"h_mito3\">\n"
    "          <boundingBox id=\"bb_sg_h_mito3\">\n"
    "            <position x=\"2200\" y=\"230\"/>\n"
    "            <dimensions width=\"40\" height=\"24\"/>\n"
    "          </boundingBox>\n"
    "        </speciesGlyph>\n"
    "      </listOfSpeciesGlyphs>\n"
    "      <listOfReactionGlyphs>\n"
    "        <reactionGlyph id=\"rg_malatedh_cyt\" reaction=\"reaction_malatedh_cyt\">\n"
    "          <curve>\n"
    "            <listOfCurveSegments>\n"
    "              <curveSegment xsi:type=\"LineSegment\">\n"
    "                <start x=\"700\" y=\"381\"/>\n"
    "                <end x=\"700\" y=\"415\"/>\n"
    "              </curveSegment>\n"
    "            </listOfCurveSegments>\n"
    "          </curve>\n"
    "          <listOfSpeciesReferenceGlyphs>\n"
    "            <speciesReferenceGlyph id=\"srg_malate_cyt_1\" speciesReference=\"sr_malate_cyt\" speciesGlyph=\"SpeciesGlyph_malate_cyt\" role=\"substrate\">\n"
    "              <curve>\n"
    "                <listOfCurveSegments>\n"
    "                  <curveSegment xsi:type=\"LineSegment\">\n"
    "                    <start x=\"700\" y=\"381\"/>\n"
    "                    <end x=\"700\" y=\"316\"/>\n"
    "                  </curveSegment>\n"
    "                </listOfCurveSegments>\n"
    "              </curve>\n"
    "            </speciesReferenceGlyph>\n"
    "            <speciesReferenceGlyph id=\"srg_nad_cyt\" speciesReference=\"sr_nad_cyt\" speciesGlyph=\"SpeciesGlyph_nad_cyt\" role=\"substrate\">\n"
    "              <curve>\n"
    "                <listOfCurveSegments>\n"
    "                  <curveSegment xsi:type=\"CubicBezier\">\n"
    "                    <start x=\"700\" y=\"381\"/>\n"
    "                    <end x=\"620\" y=\"362\"/>\n"
    "                    <basePoint1 x=\"700\" y=\"362\"/>\n"
    "                    <basePoint2 x=\"700\" y=\"362\"/>\n"
    "                  </curveSegment>\n"
    "                </listOfCurveSegments>\n"
    "              </curve>\n"
    "            </speciesReferenceGlyph>\n"
    "            <speciesReferenceGlyph id=\"srg_oxaloacetate_cyt_1\" speciesReference=\"sr_oxaloacetate_cyt_1\" speciesGlyph=\"SpeciesGlyph_oxaloacetate_cyt\" role=\"product\">\n"
    "              <curve>\n"
    "                <listOfCurveSegments>\n"
    "                  <curveSegment xsi:type=\"LineSegment\">\n"
    "                    <start x=\"700\" y=\"415\"/>\n"
    "                    <end x=\"700\" y=\"480\"/>\n"
    "                  </curveSegment>\n"
    "                </listOfCurveSegments>\n"
    "              </curve>\n"
    "            </speciesReferenceGlyph>\n"
    "            <speciesReferenceGlyph id=\"srg_nadh_cyt\" speciesReference=\"sr_nadh_cyt\" speciesGlyph=\"SpeciesGlyph_nadh_cyt\" role=\"product\">\n"
    "              <curve>\n"
    "                <listOfCurveSegments>\n"
    "                  <curveSegment xsi:type=\"CubicBezier\">\n"
    "                    <start x=\"700\" y=\"415\"/>\n"
    "                    <end x=\"620\" y=\"442\"/>\n"
    "                    <basePoint1 x=\"700\" y=\"442\"/>\n"
    "                    <basePoint2 x=\"700\" y=\"442\"/>\n"
    "                  </curveSegment>\n"
    "                </listOfCurveSegments>\n"
    "              </curve>\n"
    "            </speciesReferenceGlyph>\n"
    "            <speciesReferenceGlyph id=\"srg_h_cyt\" speciesReference=\"sr_h_cyt\" speciesGlyph=\"SpeciesGlyph_h_cyt\" role=\"product\">\n"
    "              <curve>\n"
    "                <listOfCurveSegments>\n"
    "                  <curveSegment xsi:type=\"CubicBezier\">\n"
    "                    <start x=\"700\" y=\"415\"/>\n"
    "                    <end x=\"470\" y=\"430\"/>\n"
    "                    <basePoint1 x=\"570\" y=\"415\"/>\n"
    "                    <basePoint2 x=\"570\" y=\"415\"/>\n"
    "                  </curveSegment>\n"
    "                </listOfCurveSegments>\n"
    "              </curve>\n"
    "            </speciesReferenceGlyph>\n"
    "          </listOfSpeciesReferenceGlyphs>\n"
    "        </reactionGlyph>\n"
    "        <reactionGlyph id=\"rg_aspartateat_cyt\" reaction=\"reaction_aspartateat_cyt\">\n"
    "          <curve>\n"
    "            <listOfCurveSegments>\n"
    "              <curveSegment xsi:type=\"LineSegment\">\n"
    "                <start x=\"700\" y=\"581\"/>\n"
    "                <end x=\"700\" y=\"615\"/>\n"
    "              </curveSegment>\n"
    "            </listOfCurveSegments>\n"
    "          </curve>\n"
    "          <listOfSpeciesReferenceGlyphs>\n"
    "            <speciesReferenceGlyph id=\"srg_oxaloacetate_cyt_2\" speciesReference=\"sr_oxaloacetate_cyt_2\" speciesGlyph=\"SpeciesGlyph_oxaloacetate_cyt\" role=\"substrate\">\n"
    "              <curve>\n"
    "                <listOfCurveSegments>\n"
    "                  <curveSegment xsi:type=\"LineSegment\">\n"
    "                    <start x=\"700\" y=\"581\"/>\n"
    "                    <end x=\"700\" y=\"516\"/>\n"
    "                  </curveSegment>\n"
    "                </listOfCurveSegments>\n"
    "              </curve>\n"
    "            </speciesReferenceGlyph>\n"
    "            <speciesReferenceGlyph id=\"srg_glutamate_cyt_1\" speciesReference=\"sr_glutamate_cyt_1\" speciesGlyph=\"SpeciesGlyph_glutamate_cyt\" role=\"substrate\">\n"
    "              <curve>\n"
    "                <listOfCurveSegments>\n"
    "                  <curveSegment xsi:type=\"CubicBezier\">\n"
    "                    <start x=\"700\" y=\"581\"/>\n"
    "                    <end x=\"800\" y=\"628\"/>\n"
    "                    <basePoint1 x=\"750\" y=\"581\"/>\n"
    "                    <basePoint2 x=\"750\" y=\"628\"/>\n"
    "                  </curveSegment>\n"
    "                </listOfCurveSegments>\n"
    "              </curve>\n"
    "            </speciesReferenceGlyph>\n"
    "            <speciesReferenceGlyph id=\"srg_aspartate_cyt_1\" speciesReference=\"sr_aspartate_cyt_1\" speciesGlyph=\"SpeciesGlyph_aspartate_cyt\" role=\"product\">\n"
    "              <curve>\n"
    "                <listOfCurveSegments>\n"
    "                  <curveSegment xsi:type=\"LineSegment\">\n"
    "                    <start x=\"700\" y=\"615\"/>\n"
    "                    <end x=\"700\" y=\"680\"/>\n"
    "                  </curveSegment>\n"
    "                </listOfCurveSegments>\n"
    "              </curve>\n"
    "            </speciesReferenceGlyph>\n"
    "            <speciesReferenceGlyph id=\"srg_aKetoglutaratecyt_1\" speciesReference=\"sr_aKetoglutarate_cyt_1\" speciesGlyph=\"SpeciesGlyph_aKetoglutarate_cyt\" role=\"product\">\n"
    "              <curve>\n"
    "                <listOfCurveSegments>\n"
    "                  <curveSegment xsi:type=\"CubicBezier\">\n"
    "                    <start x=\"700\" y=\"615\"/>\n"
    "                    <end x=\"860\" y=\"515\"/>\n"
    "                    <basePoint1 x=\"790\" y=\"615\"/>\n"
    "                    <basePoint2 x=\"790\" y=\"515\"/>\n"
    "                  </curveSegment>\n"
    "                </listOfCurveSegments>\n"
    "              </curve>\n"
    "            </speciesReferenceGlyph>\n"
    "          </listOfSpeciesReferenceGlyphs>\n"
    "        </reactionGlyph>\n"
    "        <reactionGlyph id=\"rg_malatedh_mito3\" reaction=\"reaction_malatedh_mito3\">\n"
    "          <curve>\n"
    "            <listOfCurveSegments>\n"
    "              <curveSegment xsi:type=\"LineSegment\">\n"
    "                <start x=\"1970\" y=\"181\"/>\n"
    "                <end x=\"1970\" y=\"215\"/>\n"
    "              </curveSegment>\n"
    "            </listOfCurveSegments>\n"
    "          </curve>\n"
    "          <listOfSpeciesReferenceGlyphs>\n"
    "            <speciesReferenceGlyph id=\"srg_malate_mito3_1\" speciesReference=\"sr_malate_mito3\" speciesGlyph=\"SpeciesGlyph_malate_mito3\" role=\"substrate\">\n"
    "              <curve>\n"
    "                <listOfCurveSegments>\n"
    "                  <curveSegment xsi:type=\"LineSegment\">\n"
    "                    <start x=\"1970\" y=\"181\"/>\n"
    "                    <end x=\"1970\" y=\"116\"/>\n"
    "                  </curveSegment>\n"
    "                </listOfCurveSegments>\n"
    "              </curve>\n"
    "            </speciesReferenceGlyph>\n"
    "            <speciesReferenceGlyph id=\"srg_nad_mito3\" speciesReference=\"sr_nad_mito3\" speciesGlyph=\"SpeciesGlyph_nad_mito3\" role=\"substrate\">\n"
    "              <curve>\n"
    "                <listOfCurveSegments>\n"
    "                  <curveSegment xsi:type=\"CubicBezier\">\n"
    "                    <start x=\"1970\" y=\"181\"/>\n"
    "                    <end x=\"2050\" y=\"162\"/>\n"
    "                    <basePoint1 x=\"1970\" y=\"162\"/>\n"
    "                    <basePoint2 x=\"1970\" y=\"162\"/>\n"
    "                  </curveSegment>\n"
    "                </listOfCurveSegments>\n"
    "              </curve>\n"
    "            </speciesReferenceGlyph>\n"
    "            <speciesReferenceGlyph id=\"srg_oxaloacetate_mito3_1\" speciesReference=\"sr_oxaloacetate_mito3_1\" speciesGlyph=\"SpeciesGlyph_oxaloacetate_mito3\" role=\"product\">\n"
    "              <curve>\n"
    "                <listOfCurveSegments>\n"
    "                  <curveSegment xsi:type=\"LineSegment\">\n"
    "                    <start x=\"1970\" y=\"215\"/>\n"
    "                    <end x=\"1970\" y=\"280\"/>\n"
    "                  </curveSegment>\n"
    "                </listOfCurveSegments>\n"
    "              </curve>\n"
    "            </speciesReferenceGlyph>\n"
    "            <speciesReferenceGlyph id=\"srg_nadh_mito3\" speciesReference=\"sr_nadh_mito3\" speciesGlyph=\"SpeciesGlyph_nadh_mito3\" role=\"product\">\n"
    "              <curve>\n"
    "                <listOfCurveSegments>\n"
    "                  <curveSegment xsi:type=\"CubicBezier\">\n"
    "                    <start x=\"1970\" y=\"215\"/>\n"
    "                    <end x=\"2050\" y=\"242\"/>\n"
    "                    <basePoint1 x=\"1970\" y=\"242\"/>\n"
    "                    <basePoint2 x=\"1970\" y=\"242\"/>\n"
    "                  </curveSegment>\n"
    "                </listOfCurveSegments>\n"
    "              </curve>\n"
    "            </speciesReferenceGlyph>\n"
    "            <speciesReferenceGlyph id=\"srg_h_mito3\" speciesReference=\"sr_h_mito3\" speciesGlyph=\"SpeciesGlyph_h_mito3\" role=\"product\">\n"
    "              <curve>\n"
    "                <listOfCurveSegments>\n"
    "                  <curveSegment xsi:type=\"CubicBezier\">\n"
    "                    <start x=\"1970\" y=\"215\"/>\n"
    "                    <end x=\"2200\" y=\"230\"/>\n"
    "                    <basePoint1 x=\"2100\" y=\"215\"/>\n"
    "                    <basePoint2 x=\"2100\" y=\"215\"/>\n"
    "                  </curveSegment>\n"
    "                </listOfCurveSegments>\n"
    "              </curve>\n"
    "            </speciesReferenceGlyph>\n"
    "          </listOfSpeciesReferenceGlyphs>\n"
    "        </reactionGlyph>\n"
    "        <reactionGlyph id=\"rg_aspartateat_mito3\" reaction=\"reaction_aspartateat_mito3\">\n"
    "          <curve>\n"
    "            <listOfCurveSegments>\n"
    "              <curveSegment xsi:type=\"LineSegment\">\n"
    "                <start x=\"1970\" y=\"381\"/>\n"
    "                <end x=\"1970\" y=\"415\"/>\n"
    "              </curveSegment>\n"
    "            </listOfCurveSegments>\n"
    "          </curve>\n"
    "          <listOfSpeciesReferenceGlyphs>\n"
    "            <speciesReferenceGlyph id=\"srg_oxaloacetate_mito3_2\" speciesReference=\"sr_oxaloacetate_mito3_2\" speciesGlyph=\"SpeciesGlyph_oxaloacetate_mito3\" role=\"substrate\">\n"
    "              <curve>\n"
    "                <listOfCurveSegments>\n"
    "                  <curveSegment xsi:type=\"LineSegment\">\n"
    "                    <start x=\"1970\" y=\"381\"/>\n"
    "                    <end x=\"1970\" y=\"316\"/>\n"
    "                  </curveSegment>\n"
    "                </listOfCurveSegments>\n"
    "              </curve>\n"
    "            </speciesReferenceGlyph>\n"
    "            <speciesReferenceGlyph id=\"srg_glutamate_mito3_1\" speciesReference=\"sr_glutamate_mito3_1\" speciesGlyph=\"SpeciesGlyph_glutamate_mito3\" role=\"substrate\">\n"
    "              <curve>\n"
    "                <listOfCurveSegments>\n"
    "                  <curveSegment xsi:type=\"CubicBezier\">\n"
    "                    <start x=\"1970\" y=\"381\"/>\n"
    "                    <end x=\"1790\" y=\"448\"/>\n"
    "                    <basePoint1 x=\"1880\" y=\"381\"/>\n"
    "                    <basePoint2 x=\"1880\" y=\"448\"/>\n"
    "                  </curveSegment>\n"
    "                </listOfCurveSegments>\n"
    "              </curve>\n"
    "            </speciesReferenceGlyph>\n"
    "            <speciesReferenceGlyph id=\"srg_aspartate_mito3_1\" speciesReference=\"sr_aspartate_mito3_1\" speciesGlyph=\"SpeciesGlyph_aspartate_mito3\" role=\"product\">\n"
    "              <curve>\n"
    "                <listOfCurveSegments>\n"
    "                  <curveSegment xsi:type=\"LineSegment\">\n"
    "                    <start x=\"1970\" y=\"415\"/>\n"
    "                    <end x=\"1970\" y=\"480\"/>\n"
    "                  </curveSegment>\n"
    "                </listOfCurveSegments>\n"
    "              </curve>\n"
    "            </speciesReferenceGlyph>\n"
    "            <speciesReferenceGlyph id=\"srg_aKetoglutaratemito3_1\" speciesReference=\"sr_aKetoglutarate_mito3_1\" speciesGlyph=\"SpeciesGlyph_aKetoglutarate_mito3\" role=\"product\">\n"
    "              <curve>\n"
    "                <listOfCurveSegments>\n"
    "                  <curveSegment xsi:type=\"CubicBezier\">\n"
    "                    <start x=\"1970\" y=\"415\"/>\n"
    "                    <end x=\"1810\" y=\"315\"/>\n"
    "                    <basePoint1 x=\"1880\" y=\"415\"/>\n"
    "                    <basePoint2 x=\"1880\" y=\"315\"/>\n"
    "                  </curveSegment>\n"
    "                </listOfCurveSegments>\n"
    "              </curve>\n"
    "            </speciesReferenceGlyph>\n"
    "          </listOfSpeciesReferenceGlyphs>\n"
    "        </reactionGlyph>\n"
    "        <reactionGlyph id=\"rg_aspartateCarrier\" reaction=\"aspartateCarrier\">\n"
    "          <curve>\n"
    "            <listOfCurveSegments>\n"
    "              <curveSegment xsi:type=\"LineSegment\">\n"
    "                <start x=\"1420\" y=\"530\"/>\n"
    "                <end x=\"1360\" y=\"550\"/>\n"
    "              </curveSegment>\n"
    "            </listOfCurveSegments>\n"
    "          </curve>\n"
    "          <listOfSpeciesReferenceGlyphs>\n"
    "            <speciesReferenceGlyph id=\"srg_aspartate_mito3_2\" speciesReference=\"sr_aspartate_mito3_2\" speciesGlyph=\"SpeciesGlyph_aspartate_mito3\" role=\"substrate\">\n"
    "              <curve>\n"
    "                <listOfCurveSegments>\n"
    "                  <curveSegment xsi:type=\"LineSegment\">\n"
    "                    <start x=\"1420\" y=\"530\"/>\n"
    "                    <end x=\"1850\" y=\"498\"/>\n"
    "                  </curveSegment>\n"
    "                </listOfCurveSegments>\n"
    "              </curve>\n"
    "            </speciesReferenceGlyph>\n"
    "            <speciesReferenceGlyph id=\"srg_aspartate_cyt_2\" speciesReference=\"sr_aspartate_cyt_2\" speciesGlyph=\"SpeciesGlyph_aspartate_cyt\" role=\"product\">\n"
    "              <curve>\n"
    "                <listOfCurveSegments>\n"
    "                  <curveSegment xsi:type=\"CubicBezier\">\n"
    "                    <start x=\"1360\" y=\"550\"/>\n"
    "                    <end x=\"820\" y=\"698\"/>\n"
    "                    <basePoint1 x=\"1390\" y=\"698\"/>\n"
    "                    <basePoint2 x=\"1390\" y=\"698\"/>\n"
    "                  </curveSegment>\n"
    "                </listOfCurveSegments>\n"
    "              </curve>\n"
    "            </speciesReferenceGlyph>\n"
    "            <speciesReferenceGlyph id=\"srg_glutamate_cyt_2\" speciesReference=\"sr_glutamate_cyt_2\" speciesGlyph=\"SpeciesGlyph_glutamate_cyt\" role=\"substrate\">\n"
    "              <curve>\n"
    "                <listOfCurveSegments>\n"
    "                  <curveSegment xsi:type=\"CubicBezier\">\n"
    "                    <start x=\"1420\" y=\"530\"/>\n"
    "                    <end x=\"1050\" y=\"628\"/>\n"
    "                    <basePoint1 x=\"1390\" y=\"648\"/>\n"
    "                    <basePoint2 x=\"1390\" y=\"648\"/>\n"
    "                  </curveSegment>\n"
    "                </listOfCurveSegments>\n"
    "              </curve>\n"
    "            </speciesReferenceGlyph>\n"
    "            <speciesReferenceGlyph id=\"srg_glutamate_mito3_2\" speciesReference=\"sr_glutamate_mito3_2\" speciesGlyph=\"SpeciesGlyph_glutamate_mito3\" role=\"product\">\n"
    "              <curve>\n"
    "                <listOfCurveSegments>\n"
    "                  <curveSegment xsi:type=\"CubicBezier\">\n"
    "                    <start x=\"1360\" y=\"550\"/>\n"
    "                    <end x=\"1550\" y=\"448\"/>\n"
    "                    <basePoint1 x=\"1390\" y=\"448\"/>\n"
    "                    <basePoint2 x=\"1390\" y=\"448\"/>\n"
    "                  </curveSegment>\n"
    "                </listOfCurveSegments>\n"
    "              </curve>\n"
    "            </speciesReferenceGlyph>\n"
    "          </listOfSpeciesReferenceGlyphs>\n"
    "        </reactionGlyph>\n"
    "        <reactionGlyph id=\"rg_malateCarrier\" reaction=\"malateCarrier\">\n"
    "          <curve>\n"
    "            <listOfCurveSegments>\n"
    "              <curveSegment xsi:type=\"LineSegment\">\n"
    "                <start x=\"1420\" y=\"320\"/>\n"
    "                <end x=\"1360\" y=\"340\"/>\n"
    "              </curveSegment>\n"
    "            </listOfCurveSegments>\n"
    "          </curve>\n"
    "          <listOfSpeciesReferenceGlyphs>\n"
    "            <speciesReferenceGlyph id=\"srg_aKetoglutarate_mito3_2\" speciesReference=\"sr_aKetoglutarate_mito3_2\" speciesGlyph=\"SpeciesGlyph_aKetoglutarate_mito3\" role=\"substrate\">\n"
    "              <curve>\n"
    "                <listOfCurveSegments>\n"
    "                  <curveSegment xsi:type=\"LineSegment\">\n"
    "                    <start x=\"1420\" y=\"320\"/>\n"
    "                    <end x=\"1530\" y=\"318\"/>\n"
    "                  </curveSegment>\n"
    "                </listOfCurveSegments>\n"
    "              </curve>\n"
    "            </speciesReferenceGlyph>\n"
    "            <speciesReferenceGlyph id=\"srg_aKetoglutarate_cyt_2\" speciesReference=\"sr_aKetoglutarate_cyt_2\" speciesGlyph=\"SpeciesGlyph_aKetoglutarate_cyt\" role=\"product\">\n"
    "              <curve>\n"
    "                <listOfCurveSegments>\n"
    "                  <curveSegment xsi:type=\"LineSegment\">\n"
    "                    <start x=\"1360\" y=\"340\"/>\n"
    "                    <end x=\"1140\" y=\"518\"/>\n"
    "                  </curveSegment>\n"
    "                </listOfCurveSegments>\n"
    "              </curve>\n"
    "            </speciesReferenceGlyph>\n"
    "            <speciesReferenceGlyph id=\"srg_malate_cyt_2\" speciesReference=\"sr_malate_cyt_2\" speciesGlyph=\"SpeciesGlyph_malate_cyt\" role=\"substrate\">\n"
    "              <curve>\n"
    "                <listOfCurveSegments>\n"
    "                  <curveSegment xsi:type=\"CubicBezier\">\n"
    "                    <start x=\"1420\" y=\"320\"/>\n"
    "                    <end x=\"820\" y=\"298\"/>\n"
    "                    <basePoint1 x=\"1390\" y=\"250\"/>\n"
    "                    <basePoint2 x=\"1390\" y=\"250\"/>\n"
    "                  </curveSegment>\n"
    "                </listOfCurveSegments>\n"
    "              </curve>\n"
    "            </speciesReferenceGlyph>\n"
    "            <speciesReferenceGlyph id=\"srg_malate_mito3_2\" speciesReference=\"sr_malate_mito3_2\" speciesGlyph=\"SpeciesGlyph_malate_mito3\" role=\"product\">\n"
    "              <curve>\n"
    "                <listOfCurveSegments>\n"
    "                  <curveSegment xsi:type=\"CubicBezier\">\n"
    "                    <start x=\"1360\" y=\"340\"/>\n"
    "                    <end x=\"1850\" y=\"98\"/>\n"
    "                    <basePoint1 x=\"1390\" y=\"150\"/>\n"
    "                    <basePoint2 x=\"1390\" y=\"150\"/>\n"
    "                  </curveSegment>\n"
    "                </listOfCurveSegments>\n"
    "              </curve>\n"
    "            </speciesReferenceGlyph>\n"
    "          </listOfSpeciesReferenceGlyphs>\n"
    "        </reactionGlyph>\n"
    "      </listOfReactionGlyphs>\n"
    "      <listOfTextGlyphs>\n"
    "        <textGlyph id=\"TextGlyph_Hepatocyte\" graphicalObject=\"CompartmentGlyph_1\" originOfText=\"Hepatocyte\">\n"
    "          <boundingBox id=\"bb_tg_compartment\">\n"
    "            <position x=\"50\" y=\"870\"/>\n"
    "            <dimensions width=\"300\" height=\"72\"/>\n"
    "          </boundingBox>\n"
    "        </textGlyph>\n"
    "        <textGlyph id=\"TextGlyph_mito1\" graphicalObject=\"Mito1_Glyph\" originOfText=\"Mito_1\">\n"
    "          <boundingBox id=\"bb_tg_mito1\">\n"
    "            <position x=\"110\" y=\"110\"/>\n"
    "            <dimensions width=\"280\" height=\"72\"/>\n"
    "          </boundingBox>\n"
    "        </textGlyph>\n"
    "        <textGlyph id=\"TextGlyph_mito2\" graphicalObject=\"Mito2_Glyph\" originOfText=\"Mito_2\">\n"
    "          <boundingBox id=\"bb_tg_mito2\">\n"
    "            <position x=\"210\" y=\"660\"/>\n"
    "            <dimensions width=\"280\" height=\"72\"/>\n"
    "          </boundingBox>\n"
    "        </textGlyph>\n"
    "        <textGlyph id=\"TextGlyph_mito3_2\" graphicalObject=\"Mito3_Glyph_2\" originOfText=\"Mito_3\">\n"
    "          <boundingBox id=\"bb_tg_mito3_2\">\n"
    "            <position x=\"1475\" y=\"35\"/>\n"
    "            <dimensions width=\"200\" height=\"72\"/>\n"
    "          </boundingBox>\n"
    "        </textGlyph>\n"
    "        <textGlyph id=\"TextGlyph_malate_cyt\" graphicalObject=\"SpeciesGlyph_malate_cyt\" originOfText=\"malate_cyt\">\n"
    "          <boundingBox id=\"bb_tg_malatate_cyt\">\n"
    "            <position x=\"590\" y=\"280\"/>\n"
    "            <dimensions width=\"220\" height=\"36\"/>\n"
    "          </boundingBox>\n"
    "        </textGlyph>\n"
    "        <textGlyph id=\"TextGlyph_oxaloacetate_cyt\" graphicalObject=\"SpeciesGlyph_oxaloacetate_cyt\" originOfText=\"oxaloacetate_cyt\">\n"
    "          <boundingBox id=\"bb_tg_oxaloacetate_cyt\">\n"
    "            <position x=\"590\" y=\"480\"/>\n"
    "            <dimensions width=\"220\" height=\"36\"/>\n"
    "          </boundingBox>\n"
    "        </textGlyph>\n"
    "        <textGlyph id=\"TextGlyph_aspartate_cyt\" graphicalObject=\"SpeciesGlyph_aspartate_cyt\" originOfText=\"aspartate_cyt\">\n"
    "          <boundingBox id=\"bb_tg_aspartate_cyt\">\n"
    "            <position x=\"590\" y=\"680\"/>\n"
    "            <dimensions width=\"220\" height=\"36\"/>\n"
    "          </boundingBox>\n"
    "        </textGlyph>\n"
    "        <textGlyph id=\"TextGlyph_glutamate_cyt\" graphicalObject=\"SpeciesGlyph_glutamate_cyt\" originOfText=\"glutamate_cyt\">\n"
    "          <boundingBox id=\"bb_tg_glutamate_cyt\">\n"
    "            <position x=\"810\" y=\"610\"/>\n"
    "            <dimensions width=\"220\" height=\"36\"/>\n"
    "          </boundingBox>\n"
    "        </textGlyph>\n"
    "        <textGlyph id=\"TextGlyph_aKetoglutarate_cyt\" graphicalObject=\"SpeciesGlyph_aKetoglutarate_cyt\" originOfText=\"aKetoglutarate_cyt\">\n"
    "          <boundingBox id=\"bb_tg_aKetoglutarate_cyt\">\n"
    "            <position x=\"870\" y=\"500\"/>\n"
    "            <dimensions width=\"260\" height=\"36\"/>\n"
    "          </boundingBox>\n"
    "        </textGlyph>\n"
    "        <textGlyph id=\"TextGlyph_nad_cyt\" graphicalObject=\"SpeciesGlyph_nad_cyt\" originOfText=\"nad_cyt\">\n"
    "          <boundingBox id=\"bb_tg_nad_cyt\">\n"
    "            <position x=\"525\" y=\"350\"/>\n"
    "            <dimensions width=\"80\" height=\"24\"/>\n"
    "          </boundingBox>\n"
    "        </textGlyph>\n"
    "        <textGlyph id=\"TextGlyph_nadh_cyt\" graphicalObject=\"SpeciesGlyph_nadh_cyt\" originOfText=\"nadh_cyt\">\n"
    "          <boundingBox id=\"bb_tg_nadh_cyt\">\n"
    "            <position x=\"525\" y=\"430\"/>\n"
    "            <dimensions width=\"80\" height=\"24\"/>\n"
    "          </boundingBox>\n"
    "        </textGlyph>\n"
    "        <textGlyph id=\"TextGlyph_h_cyt\" graphicalObject=\"SpeciesGlyph_h_cyt\" originOfText=\"h_cyt\">\n"
    "          <boundingBox id=\"bb_tg_h_cyt\">\n"
    "            <position x=\"435\" y=\"430\"/>\n"
    "            <dimensions width=\"30\" height=\"24\"/>\n"
    "          </boundingBox>\n"
    "        </textGlyph>\n"
    "        <textGlyph id=\"tg_rg_malaltedh_cyt\" graphicalObject=\"rg_malatedh_cyt\" originOfText=\"reaction_malatedh_cyt\">\n"
    "          <boundingBox id=\"bb_tg_rg_malaltedh_cyt\">\n"
    "            <position x=\"700\" y=\"385\"/>\n"
    "            <dimensions width=\"210\" height=\"24\"/>\n"
    "          </boundingBox>\n"
    "        </textGlyph>\n"
    "        <textGlyph id=\"tg_rg_aspartateat_cyt\" graphicalObject=\"rg_aspartateat_cyt\" originOfText=\"reaction_aspartateat_cyt\">\n"
    "          <boundingBox id=\"bb_tg_rg_aspartateat_cyt\">\n"
    "            <position x=\"440\" y=\"585\"/>\n"
    "            <dimensions width=\"260\" height=\"24\"/>\n"
    "          </boundingBox>\n"
    "        </textGlyph>\n"
    "        <textGlyph id=\"TextGlyph_malate_mito3\" graphicalObject=\"SpeciesGlyph_malate_mito3\" originOfText=\"malate_mito3\">\n"
    "          <boundingBox id=\"bb_tg_malatate_mito3\">\n"
    "            <position x=\"1860\" y=\"80\"/>\n"
    "            <dimensions width=\"220\" height=\"36\"/>\n"
    "          </boundingBox>\n"
    "        </textGlyph>\n"
    "        <textGlyph id=\"TextGlyph_oxaloacetate_mito3\" graphicalObject=\"SpeciesGlyph_oxaloacetate_mito3\" originOfText=\"oxaloacetate_mito3\">\n"
    "          <boundingBox id=\"bb_tg_oxaloacetate_mito3\">\n"
    "            <position x=\"1860\" y=\"280\"/>\n"
    "            <dimensions width=\"220\" height=\"36\"/>\n"
    "          </boundingBox>\n"
    "        </textGlyph>\n"
    "        <textGlyph id=\"TextGlyph_aspartate_mito3\" graphicalObject=\"SpeciesGlyph_aspartate_mito3\" originOfText=\"aspartate_mito3\">\n"
    "          <boundingBox id=\"bb_tg_aspartate_mito3\">\n"
    "            <position x=\"1860\" y=\"480\"/>\n"
    "            <dimensions width=\"220\" height=\"36\"/>\n"
    "          </boundingBox>\n"
    "        </textGlyph>\n"
    "        <textGlyph id=\"TextGlyph_glutamate_mito3\" graphicalObject=\"SpeciesGlyph_glutamate_mito3\" originOfText=\"glutamate_mito3\">\n"
    "          <boundingBox id=\"bb_tg_glutamate_mito3\">\n"
    "            <position x=\"1560\" y=\"430\"/>\n"
    "            <dimensions width=\"220\" height=\"36\"/>\n"
    "          </boundingBox>\n"
    "        </textGlyph>\n"
    "        <textGlyph id=\"TextGlyph_aKetoglutarate_mito3\" graphicalObject=\"SpeciesGlyph_aKetoglutarate_mito3\" originOfText=\"aKetoglutarate_mito3\">\n"
    "          <boundingBox id=\"bb_tg_aKetoglutarate_mito3\">\n"
    "            <position x=\"1540\" y=\"300\"/>\n"
    "            <dimensions width=\"260\" height=\"36\"/>\n"
    "          </boundingBox>\n"
    "        </textGlyph>\n"
    "        <textGlyph id=\"TextGlyph_nad_mito3\" graphicalObject=\"SpeciesGlyph_nad_mito3\" originOfText=\"nad_mito3\">\n"
    "          <boundingBox id=\"bb_tg_nad_mito3\">\n"
    "            <position x=\"2055\" y=\"150\"/>\n"
    "            <dimensions width=\"80\" height=\"24\"/>\n"
    "          </boundingBox>\n"
    "        </textGlyph>\n"
    "        <textGlyph id=\"TextGlyph_nadh_mito3\" graphicalObject=\"SpeciesGlyph_nadh_mito3\" originOfText=\"nadh_mito3\">\n"
    "          <boundingBox id=\"bb_tg_nadh_mito3\">\n"
    "            <position x=\"2055\" y=\"230\"/>\n"
    "            <dimensions width=\"80\" height=\"24\"/>\n"
    "          </boundingBox>\n"
    "        </textGlyph>\n"
    "        <textGlyph id=\"TextGlyph_h_mito3\" graphicalObject=\"SpeciesGlyph_h_mito3\" originOfText=\"h_mito3\">\n"
    "          <boundingBox id=\"bb_tg_h_mito3\">\n"
    "            <position x=\"2205\" y=\"230\"/>\n"
    "            <dimensions width=\"30\" height=\"24\"/>\n"
    "          </boundingBox>\n"
    "        </textGlyph>\n"
    "        <textGlyph id=\"tg_rg_malatedh_mito3\" graphicalObject=\"rg_malatedh_mito3\" originOfText=\"reaction_malatedh_mito3\">\n"
    "          <boundingBox id=\"bb_tg_rg_malatedh_mito3\">\n"
    "            <position x=\"1740\" y=\"185\"/>\n"
    "            <dimensions width=\"220\" height=\"24\"/>\n"
    "          </boundingBox>\n"
    "        </textGlyph>\n"
    "        <textGlyph id=\"tg_rg_aspartateat_mito3\" graphicalObject=\"rg_aspartateat_mito3\" originOfText=\"reaction_aspartateat_mito3\">\n"
    "          <boundingBox id=\"bb_tg_rg_aspartateat_mito3\">\n"
    "            <position x=\"1970\" y=\"385\"/>\n"
    "            <dimensions width=\"260\" height=\"24\"/>\n"
    "          </boundingBox>\n"
    "        </textGlyph>\n"
    "        <textGlyph id=\"tg_rg_aspartateCarrier\" graphicalObject=\"rg_aspartateCarrier\" originOfText=\"aspartateCarrier\">\n"
    "          <boundingBox id=\"bb_tg_rg_aspartateCarrier\">\n"
    "            <position x=\"1380\" y=\"500\"/>\n"
    "            <dimensions width=\"160\" height=\"24\"/>\n"
    "          </boundingBox>\n"
    "        </textGlyph>\n"
    "        <textGlyph id=\"tg_rg_malateCarrier\" graphicalObject=\"rg_malateCarrier\" originOfText=\"malateCarrier\">\n"
    "          <boundingBox id=\"bb_tg_rg_malateCarrier\">\n"
    "            <position x=\"1360\" y=\"330\"/>\n"
    "            <dimensions width=\"140\" height=\"24\"/>\n"
    "          </boundingBox>\n"
    "        </textGlyph>\n"
    "      </listOfTextGlyphs>\n"
    "    </layout>\n"
    "  </listOfLayouts>\n"
    "            </annotation>\n"
    "    <listOfCompartments>\n"
    "      <compartment id=\"Hepatocyte\" name=\"Hepatocyte\"/>\n"
    "      <compartment id=\"Mito_1\" name=\"Mito 1\" outside=\"Hepatocyte\"/>\n"
    "      <compartment id=\"Mito_2\" name=\"Mito 2\" outside=\"Hepatocyte\"/>\n"
    "      <compartment id=\"Mito_3\" name=\"Mito 3\" outside=\"Hepatocyte\"/>\n"
    "    </listOfCompartments>\n"
    "    <listOfSpecies>\n"
    "      <species id=\"malate_cyt\" name=\"Malate\" compartment=\"Hepatocyte\"/>\n"
    "      <species id=\"malate_mito1\" name=\"Malate\" compartment=\"Mito_1\"/>\n"
    "      <species id=\"malate_mito2\" name=\"Malate\" compartment=\"Mito_2\"/>\n"
    "      <species id=\"malate_mito3\" name=\"Malate\" compartment=\"Mito_3\"/>\n"
    "      <species id=\"oxaloacetate_cyt\" name=\"Oxaloacetate\" compartment=\"Hepatocyte\"/>\n"
    "      <species id=\"oxaloacetate_mito1\" name=\"Oxaloacetate\" compartment=\"Mito_1\"/>\n"
    "      <species id=\"oxaloacetate_mito2\" name=\"Oxaloacetate\" compartment=\"Mito_2\"/>\n"
    "      <species id=\"oxaloacetate_mito3\" name=\"Oxaloacetate\" compartment=\"Mito_3\"/>\n"
    "      <species id=\"aspartate_cyt\" name=\"Aspartate\" compartment=\"Hepatocyte\"/>\n"
    "      <species id=\"aspartate_mito1\" name=\"Aspartate\" compartment=\"Mito_1\"/>\n"
    "      <species id=\"aspartate_mito2\" name=\"Aspartate\" compartment=\"Mito_2\"/>\n"
    "      <species id=\"aspartate_mito3\" name=\"Aspartate\" compartment=\"Mito_3\"/>\n"
    "      <species id=\"glutamate_cyt\" name=\"Glutamate\" compartment=\"Hepatocyte\"/>\n"
    "      <species id=\"glutamate_mito1\" name=\"Glutamate\" compartment=\"Mito_1\"/>\n"
    "      <species id=\"glutamate_mito2\" name=\"Glutamate\" compartment=\"Mito_2\"/>\n"
    "      <species id=\"glutamate_mito3\" name=\"Glutamate\" compartment=\"Mito_3\"/>\n"
    "      <species id=\"aKetoglutarate_cyt\" name=\"alpha-Ketoglutarate\" compartment=\"Hepatocyte\"/>\n"
    "      <species id=\"aKetoglutarate_mito1\" name=\"alpha-Ketoglutarate\" compartment=\"Mito_1\"/>\n"
    "      <species id=\"aKetoglutarate_mito2\" name=\"alpha-Ketoglutarate\" compartment=\"Mito_2\"/>\n"
    "      <species id=\"aKetoglutarate_mito3\" name=\"alpha-Ketoglutarate\" compartment=\"Mito_3\"/>\n"
    "      <species id=\"h_cyt\" name=\"H+\" compartment=\"Hepatocyte\"/>\n"
    "      <species id=\"h_mito1\" name=\"H+\" compartment=\"Mito_1\"/>\n"
    "      <species id=\"h_mito2\" name=\"H+\" compartment=\"Mito_2\"/>\n"
    "      <species id=\"h_mito3\" name=\"H+\" compartment=\"Mito_3\"/>\n"
    "      <species id=\"nad_cyt\" name=\"NAD+\" compartment=\"Hepatocyte\"/>\n"
    "      <species id=\"nad_mito1\" name=\"NAD+\" compartment=\"Mito_1\"/>\n"
    "      <species id=\"nad_mito2\" name=\"NAD+\" compartment=\"Mito_2\"/>\n"
    "      <species id=\"nad_mito3\" name=\"NAD+\" compartment=\"Mito_3\"/>\n"
    "      <species id=\"nadh_cyt\" name=\"NADH\" compartment=\"Hepatocyte\"/>\n"
    "      <species id=\"nadh_mito1\" name=\"NADH\" compartment=\"Mito_1\"/>\n"
    "      <species id=\"nadh_mito2\" name=\"NADH\" compartment=\"Mito_2\"/>\n"
    "      <species id=\"nadh_mito3\" name=\"NADH\" compartment=\"Mito_3\"/>\n"
    "    </listOfSpecies>\n"
    "    <listOfReactions>\n"
    "      <reaction id=\"reaction_malatedh_cyt\" name=\"malate dehydrogenase\" reversible=\"false\">\n"
    "        <listOfReactants>\n"
    "          <speciesReference species=\"malate_cyt\">\n"
    "            <annotation>\n"
    "  <layoutId xmlns=\"http://projects.eml.org/bcb/sbml/level2\" id=\"sr_malate_cyt\"/>\n"
    "</annotation>\n"
    "          </speciesReference>\n"
    "          <speciesReference species=\"nad_cyt\">\n"
    "            <annotation>\n"
    "  <layoutId xmlns=\"http://projects.eml.org/bcb/sbml/level2\" id=\"sr_nad_cyt\"/>\n"
    "</annotation>\n"
    "          </speciesReference>\n"
    "        </listOfReactants>\n"
    "        <listOfProducts>\n"
    "          <speciesReference species=\"nadh_cyt\">\n"
    "            <annotation>\n"
    "  <layoutId xmlns=\"http://projects.eml.org/bcb/sbml/level2\" id=\"sr_nadh_cyt\"/>\n"
    "</annotation>\n"
    "          </speciesReference>\n"
    "          <speciesReference species=\"h_cyt\">\n"
    "            <annotation>\n"
    "  <layoutId xmlns=\"http://projects.eml.org/bcb/sbml/level2\" id=\"sr_h_cyt\"/>\n"
    "</annotation>\n"
    "          </speciesReference>\n"
    "          <speciesReference species=\"oxaloacetate_cyt\">\n"
    "            <annotation>\n"
    "  <layoutId xmlns=\"http://projects.eml.org/bcb/sbml/level2\" id=\"sr_oxaloacetate_cyt_1\"/>\n"
    "</annotation>\n"
    "          </speciesReference>\n"
    "        </listOfProducts>\n"
    "      </reaction>\n"
    "      <reaction id=\"reaction_aspartateat_cyt\" name=\"aspartate aminotransferase\" reversible=\"false\">\n"
    "        <listOfReactants>\n"
    "          <speciesReference species=\"oxaloacetate_cyt\">\n"
    "            <annotation>\n"
    "  <layoutId xmlns=\"http://projects.eml.org/bcb/sbml/level2\" id=\"sr_oxaloacetate_cyt_2\"/>\n"
    "</annotation>\n"
    "          </speciesReference>\n"
    "          <speciesReference species=\"glutamate_cyt\">\n"
    "            <annotation>\n"
    "  <layoutId xmlns=\"http://projects.eml.org/bcb/sbml/level2\" id=\"sr_glutamate_cyt_1\"/>\n"
    "</annotation>\n"
    "          </speciesReference>\n"
    "        </listOfReactants>\n"
    "        <listOfProducts>\n"
    "          <speciesReference species=\"aspartate_cyt\">\n"
    "            <annotation>\n"
    "  <layoutId xmlns=\"http://projects.eml.org/bcb/sbml/level2\" id=\"sr_aspartate_cyt_1\"/>\n"
    "</annotation>\n"
    "          </speciesReference>\n"
    "          <speciesReference species=\"aKetoglutarate_cyt\">\n"
    "            <annotation>\n"
    "  <layoutId xmlns=\"http://projects.eml.org/bcb/sbml/level2\" id=\"sr_aKetoglutarate_cyt_1\"/>\n"
    "</annotation>\n"
    "          </speciesReference>\n"
    "        </listOfProducts>\n"
    "      </reaction>\n"
    "      <reaction id=\"reaction_malatedh_mito1\" name=\"malate dehydrogenase\" reversible=\"false\">\n"
    "        <listOfReactants>\n"
    "          <speciesReference species=\"malate_mito1\">\n"
    "            <annotation>\n"
    "  <layoutId xmlns=\"http://projects.eml.org/bcb/sbml/level2\" id=\"sr_malate_mito1\"/>\n"
    "</annotation>\n"
    "          </speciesReference>\n"
    "          <speciesReference species=\"nad_mito1\">\n"
    "            <annotation>\n"
    "  <layoutId xmlns=\"http://projects.eml.org/bcb/sbml/level2\" id=\"sr_nad_mito1\"/>\n"
    "</annotation>\n"
    "          </speciesReference>\n"
    "        </listOfReactants>\n"
    "        <listOfProducts>\n"
    "          <speciesReference species=\"nadh_mito1\">\n"
    "            <annotation>\n"
    "  <layoutId xmlns=\"http://projects.eml.org/bcb/sbml/level2\" id=\"sr_nadh_mito1\"/>\n"
    "</annotation>\n"
    "          </speciesReference>\n"
    "          <speciesReference species=\"h_mito1\">\n"
    "            <annotation>\n"
    "  <layoutId xmlns=\"http://projects.eml.org/bcb/sbml/level2\" id=\"sr_h_mito1\"/>\n"
    "</annotation>\n"
    "          </speciesReference>\n"
    "          <speciesReference species=\"oxaloacetate_mito1\">\n"
    "            <annotation>\n"
    "  <layoutId xmlns=\"http://projects.eml.org/bcb/sbml/level2\" id=\"sr_oxaloacetate_mito1_1\"/>\n"
    "</annotation>\n"
    "          </speciesReference>\n"
    "        </listOfProducts>\n"
    "      </reaction>\n"
    "      <reaction id=\"reaction_aspartateat_mito1\" name=\"aspartate aminotransferase\" reversible=\"false\">\n"
    "        <listOfReactants>\n"
    "          <speciesReference species=\"oxaloacetate_mito1\">\n"
    "            <annotation>\n"
    "  <layoutId xmlns=\"http://projects.eml.org/bcb/sbml/level2\" id=\"sr_oxaloacetate_mito1_2\"/>\n"
    "</annotation>\n"
    "          </speciesReference>\n"
    "          <speciesReference species=\"glutamate_mito1\">\n"
    "            <annotation>\n"
    "  <layoutId xmlns=\"http://projects.eml.org/bcb/sbml/level2\" id=\"sr_glutamate_mito1\"/>\n"
    "</annotation>\n"
    "          </speciesReference>\n"
    "        </listOfReactants>\n"
    "        <listOfProducts>\n"
    "          <speciesReference species=\"aspartate_mito1\">\n"
    "            <annotation>\n"
    "  <layoutId xmlns=\"http://projects.eml.org/bcb/sbml/level2\" id=\"sr_aspartate_mito1\"/>\n"
    "</annotation>\n"
    "          </speciesReference>\n"
    "          <speciesReference species=\"aKetoglutarate_mito1\">\n"
    "            <annotation>\n"
    "  <layoutId xmlns=\"http://projects.eml.org/bcb/sbml/level2\" id=\"sr_aKetoglutarate_mito1\"/>\n"
    "</annotation>\n"
    "          </speciesReference>\n"
    "        </listOfProducts>\n"
    "      </reaction>\n"
    "      <reaction id=\"reaction_malatedh_mito2\" name=\"malate dehydrogenase\" reversible=\"false\">\n"
    "        <listOfReactants>\n"
    "          <speciesReference species=\"malate_mito2\">\n"
    "            <annotation>\n"
    "  <layoutId xmlns=\"http://projects.eml.org/bcb/sbml/level2\" id=\"sr_malate_mito2\"/>\n"
    "</annotation>\n"
    "          </speciesReference>\n"
    "          <speciesReference species=\"nad_mito2\">\n"
    "            <annotation>\n"
    "  <layoutId xmlns=\"http://projects.eml.org/bcb/sbml/level2\" id=\"sr_nad_mito2\"/>\n"
    "</annotation>\n"
    "          </speciesReference>\n"
    "        </listOfReactants>\n"
    "        <listOfProducts>\n"
    "          <speciesReference species=\"nadh_mito2\">\n"
    "            <annotation>\n"
    "  <layoutId xmlns=\"http://projects.eml.org/bcb/sbml/level2\" id=\"sr_nadh_mito2\"/>\n"
    "</annotation>\n"
    "          </speciesReference>\n"
    "          <speciesReference species=\"h_mito2\">\n"
    "            <annotation>\n"
    "  <layoutId xmlns=\"http://projects.eml.org/bcb/sbml/level2\" id=\"sr_h_mito2\"/>\n"
    "</annotation>\n"
    "          </speciesReference>\n"
    "          <speciesReference species=\"oxaloacetate_mito2\">\n"
    "            <annotation>\n"
    "  <layoutId xmlns=\"http://projects.eml.org/bcb/sbml/level2\" id=\"sr_oxaloacetate_mito2_1\"/>\n"
    "</annotation>\n"
    "          </speciesReference>\n"
    "        </listOfProducts>\n"
    "      </reaction>\n"
    "      <reaction id=\"reaction_aspartateat_mito2\" name=\"aspartate aminotransferase\" reversible=\"false\">\n"
    "        <listOfReactants>\n"
    "          <speciesReference species=\"oxaloacetate_mito2\">\n"
    "            <annotation>\n"
    "  <layoutId xmlns=\"http://projects.eml.org/bcb/sbml/level2\" id=\"sr_oxaloacetate_mito2_2\"/>\n"
    "</annotation>\n"
    "          </speciesReference>\n"
    "          <speciesReference species=\"glutamate_mito2\">\n"
    "            <annotation>\n"
    "  <layoutId xmlns=\"http://projects.eml.org/bcb/sbml/level2\" id=\"sr_glutamate_mito2\"/>\n"
    "</annotation>\n"
    "          </speciesReference>\n"
    "        </listOfReactants>\n"
    "        <listOfProducts>\n"
    "          <speciesReference species=\"aspartate_mito2\">\n"
    "            <annotation>\n"
    "  <layoutId xmlns=\"http://projects.eml.org/bcb/sbml/level2\" id=\"sr_aspartate_mito2\"/>\n"
    "</annotation>\n"
    "          </speciesReference>\n"
    "          <speciesReference species=\"aKetoglutarate_mito2\">\n"
    "            <annotation>\n"
    "  <layoutId xmlns=\"http://projects.eml.org/bcb/sbml/level2\" id=\"sr_aKetoglutarate_mito2\"/>\n"
    "</annotation>\n"
    "          </speciesReference>\n"
    "        </listOfProducts>\n"
    "      </reaction>\n"
    "      <reaction id=\"reaction_malatedh_mito3\" name=\"malate dehydrogenase\" reversible=\"false\">\n"
    "        <listOfReactants>\n"
    "          <speciesReference species=\"malate_mito3\">\n"
    "            <annotation>\n"
    "  <layoutId xmlns=\"http://projects.eml.org/bcb/sbml/level2\" id=\"sr_malate_mito3\"/>\n"
    "</annotation>\n"
    "          </speciesReference>\n"
    "          <speciesReference species=\"nad_mito3\">\n"
    "            <annotation>\n"
    "  <layoutId xmlns=\"http://projects.eml.org/bcb/sbml/level2\" id=\"sr_nad_mito3\"/>\n"
    "</annotation>\n"
    "          </speciesReference>\n"
    "        </listOfReactants>\n"
    "        <listOfProducts>\n"
    "          <speciesReference species=\"nadh_mito3\">\n"
    "            <annotation>\n"
    "  <layoutId xmlns=\"http://projects.eml.org/bcb/sbml/level2\" id=\"sr_nadh_mito3\"/>\n"
    "</annotation>\n"
    "          </speciesReference>\n"
    "          <speciesReference species=\"h_mito3\">\n"
    "            <annotation>\n"
    "  <layoutId xmlns=\"http://projects.eml.org/bcb/sbml/level2\" id=\"sr_h_mito3\"/>\n"
    "</annotation>\n"
    "          </speciesReference>\n"
    "          <speciesReference species=\"oxaloacetate_mito3\">\n"
    "            <annotation>\n"
    "  <layoutId xmlns=\"http://projects.eml.org/bcb/sbml/level2\" id=\"sr_oxaloacetate_mito3_1\"/>\n"
    "</annotation>\n"
    "          </speciesReference>\n"
    "        </listOfProducts>\n"
    "      </reaction>\n"
    "      <reaction id=\"reaction_aspartateat_mito3\" name=\"aspartate aminotransferase\" reversible=\"false\">\n"
    "        <listOfReactants>\n"
    "          <speciesReference species=\"oxaloacetate_mito3\">\n"
    "            <annotation>\n"
    "  <layoutId xmlns=\"http://projects.eml.org/bcb/sbml/level2\" id=\"sr_oxaloacetate_mito3_2\"/>\n"
    "</annotation>\n"
    "          </speciesReference>\n"
    "          <speciesReference species=\"glutamate_mito3\">\n"
    "            <annotation>\n"
    "  <layoutId xmlns=\"http://projects.eml.org/bcb/sbml/level2\" id=\"sr_glutamate_mito3_1\"/>\n"
    "</annotation>\n"
    "          </speciesReference>\n"
    "        </listOfReactants>\n"
    "        <listOfProducts>\n"
    "          <speciesReference species=\"aspartate_mito3\">\n"
    "            <annotation>\n"
    "  <layoutId xmlns=\"http://projects.eml.org/bcb/sbml/level2\" id=\"sr_aspartate_mito3_1\"/>\n"
    "</annotation>\n"
    "          </speciesReference>\n"
    "          <speciesReference species=\"aKetoglutarate_mito3\">\n"
    "            <annotation>\n"
    "  <layoutId xmlns=\"http://projects.eml.org/bcb/sbml/level2\" id=\"sr_aKetoglutarate_mito3_1\"/>\n"
    "</annotation>\n"
    "          </speciesReference>\n"
    "        </listOfProducts>\n"
    "      </reaction>\n"
    "      <reaction id=\"aspartateCarrier\" name=\"aspartate carrier\">\n"
    "        <listOfReactants>\n"
    "          <speciesReference species=\"glutamate_mito3\">\n"
    "            <annotation>\n"
    "  <layoutId xmlns=\"http://projects.eml.org/bcb/sbml/level2\" id=\"sr_glutamate_mito3_2\"/>\n"
    "</annotation>\n"
    "          </speciesReference>\n"
    "          <speciesReference species=\"aspartate_cyt\">\n"
    "            <annotation>\n"
    "  <layoutId xmlns=\"http://projects.eml.org/bcb/sbml/level2\" id=\"sr_aspartate_cyt_2\"/>\n"
    "</annotation>\n"
    "          </speciesReference>\n"
    "        </listOfReactants>\n"
    "        <listOfProducts>\n"
    "          <speciesReference species=\"glutamate_cyt\">\n"
    "            <annotation>\n"
    "  <layoutId xmlns=\"http://projects.eml.org/bcb/sbml/level2\" id=\"sr_glutamate_cyt_2\"/>\n"
    "</annotation>\n"
    "          </speciesReference>\n"
    "          <speciesReference species=\"aspartate_mito3\">\n"
    "            <annotation>\n"
    "  <layoutId xmlns=\"http://projects.eml.org/bcb/sbml/level2\" id=\"sr_aspartate_mito3_2\"/>\n"
    "</annotation>\n"
    "          </speciesReference>\n"
    "        </listOfProducts>\n"
    "      </reaction>\n"
    "      <reaction id=\"malateCarrier\" name=\"malate carrier\">\n"
    "        <listOfReactants>\n"
    "          <speciesReference species=\"aKetoglutarate_mito3\">\n"
    "            <annotation>\n"
    "  <layoutId xmlns=\"http://projects.eml.org/bcb/sbml/level2\" id=\"sr_aKetoglutarate_mito3_2\"/>\n"
    "</annotation>\n"
    "          </speciesReference>\n"
    "          <speciesReference species=\"malate_cyt\">\n"
    "            <annotation>\n"
    "  <layoutId xmlns=\"http://projects.eml.org/bcb/sbml/level2\" id=\"sr_malate_cyt_2\"/>\n"
    "</annotation>\n"
    "          </speciesReference>\n"
    "        </listOfReactants>\n"
    "        <listOfProducts>\n"
    "          <speciesReference species=\"aKetoglutarate_cyt\">\n"
    "            <annotation>\n"
    "  <layoutId xmlns=\"http://projects.eml.org/bcb/sbml/level2\" id=\"sr_aKetoglutarate_cyt_2\"/>\n"
    "</annotation>\n"
    "          </speciesReference>\n"
    "          <speciesReference species=\"malate_mito3\">\n"
    "            <annotation>\n"
    "  <layoutId xmlns=\"http://projects.eml.org/bcb/sbml/level2\" id=\"sr_malate_mito3_2\"/>\n"
    "</annotation>\n"
    "          </speciesReference>\n"
    "        </listOfProducts>\n"
    "      </reaction>\n"
    "    </listOfReactions>\n"
    "  </model>\n"
    "</sbml> \n"
    ;

  XMLInputStream stream(s,false);
  XMLNode node(stream);
  // create the document

  SBMLDocument *document=new SBMLDocument(2,1);
  
  document->enablePackage(LayoutExtension::getXmlnsL2(), "layout", true);  
  document->enablePackage(RenderExtension::getXmlnsL2(), "render", true);

  LayoutPkgNamespaces layoutns(2);

  // create the Model

  Model* model=document->createModel();
  model->setId("TestModel");
  document->setModel(model);

  // create the Compartment

  Compartment* compartment=model->createCompartment();
  compartment->setId("Hepatocyte");
  compartment->setName("Hepatocyte");

  Compartment* mito1=model->createCompartment();
  mito1->setId("Mito_1");
  mito1->setName("Mito 1");
  mito1->setOutside(compartment->getId());

  Compartment* mito2=model->createCompartment();
  mito2->setId("Mito_2");
  mito2->setName("Mito 2");
  mito2->setOutside(compartment->getId());

  Compartment* mito3=model->createCompartment();
  mito3->setId("Mito_3");
  mito3->setName("Mito 3");
  mito3->setOutside(compartment->getId());


  // create the Species

  // Malate
  Species* malate_cyt=model->createSpecies();
  malate_cyt->setId("malate_cyt");
  malate_cyt->setName("Malate");
  malate_cyt->setCompartment(compartment->getId());

  Species* malate_mito1=model->createSpecies();
  malate_mito1->setId("malate_mito1");
  malate_mito1->setCompartment(mito1->getId());
  malate_mito1->setName("Malate");

  Species* malate_mito2=model->createSpecies();
  malate_mito2->setId("malate_mito2");
  malate_mito2->setCompartment(mito2->getId());
  malate_mito2->setName("Malate");

  Species* malate_mito3=model->createSpecies();
  malate_mito3->setId("malate_mito3");
  malate_mito3->setCompartment(mito3->getId());
  malate_mito3->setName("Malate");


  // Oxaloacetate
  Species* oxaloacetate_cyt=model->createSpecies();
  oxaloacetate_cyt->setId("oxaloacetate_cyt");
  oxaloacetate_cyt->setName("Oxaloacetate");
  oxaloacetate_cyt->setCompartment(compartment->getId());

  Species* oxaloacetate_mito1=model->createSpecies();
  oxaloacetate_mito1->setId("oxaloacetate_mito1");
  oxaloacetate_mito1->setCompartment(mito1->getId());
  oxaloacetate_mito1->setName("Oxaloacetate");

  Species* oxaloacetate_mito2=model->createSpecies();
  oxaloacetate_mito2->setId("oxaloacetate_mito2");
  oxaloacetate_mito2->setCompartment(mito2->getId());
  oxaloacetate_mito2->setName("Oxaloacetate");

  Species* oxaloacetate_mito3=model->createSpecies();
  oxaloacetate_mito3->setId("oxaloacetate_mito3");
  oxaloacetate_mito3->setCompartment(mito3->getId());
  oxaloacetate_mito3->setName("Oxaloacetate");


  // Aspartate
  Species* aspartate_cyt=model->createSpecies();
  aspartate_cyt->setId("aspartate_cyt");
  aspartate_cyt->setName("Aspartate");
  aspartate_cyt->setCompartment(compartment->getId());

  Species* aspartate_mito1=model->createSpecies();
  aspartate_mito1->setId("aspartate_mito1");
  aspartate_mito1->setCompartment(mito1->getId());
  aspartate_mito1->setName("Aspartate");

  Species* aspartate_mito2=model->createSpecies();
  aspartate_mito2->setId("aspartate_mito2");
  aspartate_mito2->setCompartment(mito2->getId());
  aspartate_mito2->setName("Aspartate");

  Species* aspartate_mito3=model->createSpecies();
  aspartate_mito3->setId("aspartate_mito3");
  aspartate_mito3->setCompartment(mito3->getId());
  aspartate_mito3->setName("Aspartate");


  // Glutamate
  Species* glutamate_cyt=model->createSpecies();
  glutamate_cyt->setId("glutamate_cyt");
  glutamate_cyt->setName("Glutamate");
  glutamate_cyt->setCompartment(compartment->getId());

  Species* glutamate_mito1=model->createSpecies();
  glutamate_mito1->setId("glutamate_mito1");
  glutamate_mito1->setCompartment(mito1->getId());
  glutamate_mito1->setName("Glutamate");

  Species* glutamate_mito2=model->createSpecies();
  glutamate_mito2->setId("glutamate_mito2");
  glutamate_mito2->setCompartment(mito2->getId());
  glutamate_mito2->setName("Glutamate");

  Species* glutamate_mito3=model->createSpecies();
  glutamate_mito3->setId("glutamate_mito3");
  glutamate_mito3->setCompartment(mito3->getId());
  glutamate_mito3->setName("Glutamate");


  // alpha-Ketoglutarate
  Species* aKetoglutarate_cyt=model->createSpecies();
  aKetoglutarate_cyt->setId("aKetoglutarate_cyt");
  aKetoglutarate_cyt->setName("alpha-Ketoglutarate");
  aKetoglutarate_cyt->setCompartment(compartment->getId());

  Species* aKetoglutarate_mito1=model->createSpecies();
  aKetoglutarate_mito1->setId("aKetoglutarate_mito1");
  aKetoglutarate_mito1->setCompartment(mito1->getId());
  aKetoglutarate_mito1->setName("alpha-Ketoglutarate");

  Species* aKetoglutarate_mito2=model->createSpecies();
  aKetoglutarate_mito2->setId("aKetoglutarate_mito2");
  aKetoglutarate_mito2->setCompartment(mito2->getId());
  aKetoglutarate_mito2->setName("alpha-Ketoglutarate");

  Species* aKetoglutarate_mito3=model->createSpecies();
  aKetoglutarate_mito3->setId("aKetoglutarate_mito3");
  aKetoglutarate_mito3->setCompartment(mito3->getId());
  aKetoglutarate_mito3->setName("alpha-Ketoglutarate");


  // protons
  Species* h_cyt=model->createSpecies();
  h_cyt->setId("h_cyt");
  h_cyt->setName("H+");
  h_cyt->setCompartment(compartment->getId());

  Species* h_mito1=model->createSpecies();
  h_mito1->setId("h_mito1");
  h_mito1->setCompartment(mito1->getId());
  h_mito1->setName("H+");

  Species* h_mito2=model->createSpecies();
  h_mito2->setId("h_mito2");
  h_mito2->setCompartment(mito2->getId());
  h_mito2->setName("H+");

  Species* h_mito3=model->createSpecies();
  h_mito3->setId("h_mito3");
  h_mito3->setCompartment(mito3->getId());
  h_mito3->setName("H+");


  // NAD+
  Species* nad_cyt=model->createSpecies();
  nad_cyt->setId("nad_cyt");
  nad_cyt->setName("NAD+");
  nad_cyt->setCompartment(compartment->getId());

  Species* nad_mito1=model->createSpecies();
  nad_mito1->setId("nad_mito1");
  nad_mito1->setCompartment(mito1->getId());
  nad_mito1->setName("NAD+");

  Species* nad_mito2=model->createSpecies();
  nad_mito2->setId("nad_mito2");
  nad_mito2->setCompartment(mito2->getId());
  nad_mito2->setName("NAD+");

  Species* nad_mito3=model->createSpecies();
  nad_mito3->setId("nad_mito3");
  nad_mito3->setCompartment(mito3->getId());
  nad_mito3->setName("NAD+");


  // NADH
  Species* nadh_cyt=model->createSpecies();
  nadh_cyt->setId("nadh_cyt");
  nadh_cyt->setName("NADH");
  nadh_cyt->setCompartment(compartment->getId());

  Species* nadh_mito1=model->createSpecies();
  nadh_mito1->setId("nadh_mito1");
  nadh_mito1->setCompartment(mito1->getId());
  nadh_mito1->setName("NADH");

  Species* nadh_mito2=model->createSpecies();
  nadh_mito2->setId("nadh_mito2");
  nadh_mito2->setCompartment(mito2->getId());
  nadh_mito2->setName("NADH");

  Species* nadh_mito3=model->createSpecies();
  nadh_mito3->setId("nadh_mito3");
  nadh_mito3->setCompartment(mito3->getId());
  nadh_mito3->setName("NADH");




  // create the Reactions

  // Cytosol

  // Malate Dehydrogenase
  Reaction* malatedh_cyt=model->createReaction();
  malatedh_cyt->setId("reaction_malatedh_cyt");
  malatedh_cyt->setName("malate dehydrogenase");
  malatedh_cyt->setReversible(false);

  SpeciesReference* sr_malate_cyt=malatedh_cyt->createReactant();
  sr_malate_cyt->setSpecies(malate_cyt->getId());
  sr_malate_cyt->setId("sr_malate_cyt");

  SpeciesReference* sr_nad_cyt=malatedh_cyt->createReactant();
  sr_nad_cyt->setSpecies(nad_cyt->getId());
  sr_nad_cyt->setId("sr_nad_cyt");

  SpeciesReference* sr_nadh_cyt=malatedh_cyt->createProduct();
  sr_nadh_cyt->setSpecies(nadh_cyt->getId());
  sr_nadh_cyt->setId("sr_nadh_cyt");

  SpeciesReference* sr_h_cyt=malatedh_cyt->createProduct();
  sr_h_cyt->setSpecies(h_cyt->getId());
  sr_h_cyt->setId("sr_h_cyt");

  SpeciesReference* sr_oxaloacetate_cyt_1=malatedh_cyt->createProduct();
  sr_oxaloacetate_cyt_1->setSpecies(oxaloacetate_cyt->getId());
  sr_oxaloacetate_cyt_1->setId("sr_oxaloacetate_cyt_1");

  //Aspartate Aminotransferase
  Reaction* aspartateat_cyt=model->createReaction();
  aspartateat_cyt->setId("reaction_aspartateat_cyt");
  aspartateat_cyt->setName("aspartate aminotransferase");
  aspartateat_cyt->setReversible(false);

  SpeciesReference* sr_oxaloacetate_cyt_2=aspartateat_cyt->createReactant();
  sr_oxaloacetate_cyt_2->setSpecies(oxaloacetate_cyt->getId());
  sr_oxaloacetate_cyt_2->setId("sr_oxaloacetate_cyt_2");

  SpeciesReference* sr_glutamate_cyt_1=aspartateat_cyt->createReactant();
  sr_glutamate_cyt_1->setSpecies(glutamate_cyt->getId());
  sr_glutamate_cyt_1->setId("sr_glutamate_cyt_1");

  SpeciesReference* sr_aspartate_cyt_1=aspartateat_cyt->createProduct();
  sr_aspartate_cyt_1->setSpecies(aspartate_cyt->getId());
  sr_aspartate_cyt_1->setId("sr_aspartate_cyt_1");

  SpeciesReference* sr_aKetoglutarate_cyt_1=aspartateat_cyt->createProduct();
  sr_aKetoglutarate_cyt_1->setSpecies(aKetoglutarate_cyt->getId());
  sr_aKetoglutarate_cyt_1->setId("sr_aKetoglutarate_cyt_1");


  // Mito 1

  // Malate Dehydrogenase
  Reaction* malatedh_mito1=model->createReaction();
  malatedh_mito1->setId("reaction_malatedh_mito1");
  malatedh_mito1->setName("malate dehydrogenase");
  malatedh_mito1->setReversible(false);

  SpeciesReference* sr_malate_mito1=malatedh_mito1->createReactant();
  sr_malate_mito1->setSpecies(malate_mito1->getId());
  sr_malate_mito1->setId("sr_malate_mito1");

  SpeciesReference* sr_nad_mito1=malatedh_mito1->createReactant();
  sr_nad_mito1->setSpecies(nad_mito1->getId());
  sr_nad_mito1->setId("sr_nad_mito1");

  SpeciesReference* sr_nadh_mito1=malatedh_mito1->createProduct();
  sr_nadh_mito1->setSpecies(nadh_mito1->getId());
  sr_nadh_mito1->setId("sr_nadh_mito1");

  SpeciesReference* sr_h_mito1=malatedh_mito1->createProduct();
  sr_h_mito1->setSpecies(h_mito1->getId());
  sr_h_mito1->setId("sr_h_mito1");

  SpeciesReference* sr_oxaloacetate_mito1_1=malatedh_mito1->createProduct();
  sr_oxaloacetate_mito1_1->setSpecies(oxaloacetate_mito1->getId());
  sr_oxaloacetate_mito1_1->setId("sr_oxaloacetate_mito1_1");

  //Aspartate Aminotransferase
  Reaction* aspartateat_mito1=model->createReaction();
  aspartateat_mito1->setId("reaction_aspartateat_mito1");
  aspartateat_mito1->setName("aspartate aminotransferase");
  aspartateat_mito1->setReversible(false);

  SpeciesReference* sr_oxaloacetate_mito1_2=aspartateat_mito1->createReactant();
  sr_oxaloacetate_mito1_2->setSpecies(oxaloacetate_mito1->getId());
  sr_oxaloacetate_mito1_2->setId("sr_oxaloacetate_mito1_2");

  SpeciesReference* sr_glutamate_mito1=aspartateat_mito1->createReactant();
  sr_glutamate_mito1->setSpecies(glutamate_mito1->getId());
  sr_glutamate_mito1->setId("sr_glutamate_mito1");

  SpeciesReference* sr_aspartate_mito1=aspartateat_mito1->createProduct();
  sr_aspartate_mito1->setSpecies(aspartate_mito1->getId());
  sr_aspartate_mito1->setId("sr_aspartate_mito1");

  SpeciesReference* sr_aKetoglutarate_mito1=aspartateat_mito1->createProduct();
  sr_aKetoglutarate_mito1->setSpecies(aKetoglutarate_mito1->getId());
  sr_aKetoglutarate_mito1->setId("sr_aKetoglutarate_mito1");


  // Mito 2

  // Malate Dehydrogenase
  Reaction* malatedh_mito2=model->createReaction();
  malatedh_mito2->setId("reaction_malatedh_mito2");
  malatedh_mito2->setName("malate dehydrogenase");
  malatedh_mito2->setReversible(false);

  SpeciesReference* sr_malate_mito2=malatedh_mito2->createReactant();
  sr_malate_mito2->setSpecies(malate_mito2->getId());
  sr_malate_mito2->setId("sr_malate_mito2");

  SpeciesReference* sr_nad_mito2=malatedh_mito2->createReactant();
  sr_nad_mito2->setSpecies(nad_mito2->getId());
  sr_nad_mito2->setId("sr_nad_mito2");

  SpeciesReference* sr_nadh_mito2=malatedh_mito2->createProduct();
  sr_nadh_mito2->setSpecies(nadh_mito2->getId());
  sr_nadh_mito2->setId("sr_nadh_mito2");

  SpeciesReference* sr_h_mito2=malatedh_mito2->createProduct();
  sr_h_mito2->setSpecies(h_mito2->getId());
  sr_h_mito2->setId("sr_h_mito2");

  SpeciesReference* sr_oxaloacetate_mito2_1=malatedh_mito2->createProduct();
  sr_oxaloacetate_mito2_1->setSpecies(oxaloacetate_mito2->getId());
  sr_oxaloacetate_mito2_1->setId("sr_oxaloacetate_mito2_1");

  //Aspartate Aminotransferase
  Reaction* aspartateat_mito2=model->createReaction();
  aspartateat_mito2->setId("reaction_aspartateat_mito2");
  aspartateat_mito2->setName("aspartate aminotransferase");
  aspartateat_mito2->setReversible(false);

  SpeciesReference* sr_oxaloacetate_mito2_2=aspartateat_mito2->createReactant();
  sr_oxaloacetate_mito2_2->setSpecies(oxaloacetate_mito2->getId());
  sr_oxaloacetate_mito2_2->setId("sr_oxaloacetate_mito2_2");

  SpeciesReference* sr_glutamate_mito2=aspartateat_mito2->createReactant();
  sr_glutamate_mito2->setSpecies(glutamate_mito2->getId());
  sr_glutamate_mito2->setId("sr_glutamate_mito2");

  SpeciesReference* sr_aspartate_mito2=aspartateat_mito2->createProduct();
  sr_aspartate_mito2->setSpecies(aspartate_mito2->getId());
  sr_aspartate_mito2->setId("sr_aspartate_mito2");

  SpeciesReference* sr_aKetoglutarate_mito2=aspartateat_mito2->createProduct();
  sr_aKetoglutarate_mito2->setSpecies(aKetoglutarate_mito2->getId());
  sr_aKetoglutarate_mito2->setId("sr_aKetoglutarate_mito2");


  // Mito 3

  // Malate Dehydrogenase
  Reaction* malatedh_mito3=model->createReaction();
  malatedh_mito3->setId("reaction_malatedh_mito3");
  malatedh_mito3->setName("malate dehydrogenase");
  malatedh_mito3->setReversible(false);

  SpeciesReference* sr_malate_mito3=malatedh_mito3->createReactant();
  sr_malate_mito3->setSpecies(malate_mito3->getId());
  sr_malate_mito3->setId("sr_malate_mito3");

  SpeciesReference* sr_nad_mito3=malatedh_mito3->createReactant();
  sr_nad_mito3->setSpecies(nad_mito3->getId());
  sr_nad_mito3->setId("sr_nad_mito3");

  SpeciesReference* sr_nadh_mito3=malatedh_mito3->createProduct();
  sr_nadh_mito3->setSpecies(nadh_mito3->getId());
  sr_nadh_mito3->setId("sr_nadh_mito3");

  SpeciesReference* sr_h_mito3=malatedh_mito3->createProduct();
  sr_h_mito3->setSpecies(h_mito3->getId());
  sr_h_mito3->setId("sr_h_mito3");

  SpeciesReference* sr_oxaloacetate_mito3_1=malatedh_mito3->createProduct();
  sr_oxaloacetate_mito3_1->setSpecies(oxaloacetate_mito3->getId());
  sr_oxaloacetate_mito3_1->setId("sr_oxaloacetate_mito3_1");

  //Aspartate Aminotransferase
  Reaction* aspartateat_mito3=model->createReaction();
  aspartateat_mito3->setId("reaction_aspartateat_mito3");
  aspartateat_mito3->setName("aspartate aminotransferase");
  aspartateat_mito3->setReversible(false);

  SpeciesReference* sr_oxaloacetate_mito3_2=aspartateat_mito3->createReactant();
  sr_oxaloacetate_mito3_2->setSpecies(oxaloacetate_mito3->getId());
  sr_oxaloacetate_mito3_2->setId("sr_oxaloacetate_mito3_2");


  SpeciesReference* sr_glutamate_mito3_1=aspartateat_mito3->createReactant();
  sr_glutamate_mito3_1->setSpecies(glutamate_mito3->getId());
  sr_glutamate_mito3_1->setId("sr_glutamate_mito3_1");


  SpeciesReference* sr_aspartate_mito3_1=aspartateat_mito3->createProduct();
  sr_aspartate_mito3_1->setSpecies(aspartate_mito3->getId());
  sr_aspartate_mito3_1->setId("sr_aspartate_mito3_1");


  SpeciesReference* sr_aKetoglutarate_mito3_1=aspartateat_mito3->createProduct();
  sr_aKetoglutarate_mito3_1->setSpecies(aKetoglutarate_mito3->getId());
  sr_aKetoglutarate_mito3_1->setId("sr_aKetoglutarate_mito3_1");


  // aspartate carrier

  Reaction* aspartateCarrier=model->createReaction();
  aspartateCarrier->setId("aspartateCarrier");
  aspartateCarrier->setName("aspartate carrier");
  aspartateCarrier->setReversible(true);

  SpeciesReference* sr_glutamate_mito3_2=aspartateCarrier->createReactant();
  sr_glutamate_mito3_2->setSpecies(glutamate_mito3->getId());
  sr_glutamate_mito3_2->setId("sr_glutamate_mito3_2");

  SpeciesReference* sr_aspartate_cyt_2=aspartateCarrier->createReactant();
  sr_aspartate_cyt_2->setSpecies(aspartate_cyt->getId());
  sr_aspartate_cyt_2->setId("sr_aspartate_cyt_2");

  SpeciesReference* sr_glutamate_cyt_2=aspartateCarrier->createProduct();
  sr_glutamate_cyt_2->setSpecies(glutamate_cyt->getId());
  sr_glutamate_cyt_2->setId("sr_glutamate_cyt_2");

  SpeciesReference* sr_aspartate_mito3_2=aspartateCarrier->createProduct();
  sr_aspartate_mito3_2->setSpecies(aspartate_mito3->getId());
  sr_aspartate_mito3_2->setId("sr_aspartate_mito3_2");

  // malate carrier

  Reaction* malateCarrier=model->createReaction();
  malateCarrier->setId("malateCarrier");
  malateCarrier->setName("malate carrier");
  malateCarrier->setReversible(true);

  SpeciesReference* sr_aKetoglutarate_mito3_2=malateCarrier->createReactant();
  sr_aKetoglutarate_mito3_2->setSpecies(aKetoglutarate_mito3->getId());
  sr_aKetoglutarate_mito3_2->setId("sr_aKetoglutarate_mito3_2");

  SpeciesReference* sr_malate_cyt_2=malateCarrier->createReactant();
  sr_malate_cyt_2->setSpecies(malate_cyt->getId());
  sr_malate_cyt_2->setId("sr_malate_cyt_2");

  SpeciesReference* sr_aKetoglutarate_cyt_2=malateCarrier->createProduct();
  sr_aKetoglutarate_cyt_2->setSpecies(aKetoglutarate_cyt->getId());
  sr_aKetoglutarate_cyt_2->setId("sr_aKetoglutarate_cyt_2");

  SpeciesReference* sr_malate_mito3_2=malateCarrier->createProduct();
  sr_malate_mito3_2->setSpecies(malate_mito3->getId());
  sr_malate_mito3_2->setId("sr_malate_mito3_2");


  /////////// create the Layout
  LayoutModelPlugin *plugin = (LayoutModelPlugin*)model->getPlugin("layout");
  fail_unless(plugin != NULL);
  if (plugin == NULL) return;

  Layout* layout=plugin->createLayout();

  layout->setId("Layout_1");
  Dimensions dim(&layoutns, 2320.0,1000.0);
  layout->setDimensions(&dim);


  // create the CompartmentGlyph

  CompartmentGlyph* compartmentGlyph=layout->createCompartmentGlyph();
  compartmentGlyph->setId("CompartmentGlyph_1");
  compartmentGlyph->setCompartmentId(compartment->getId());
  BoundingBox bb=BoundingBox(&layoutns, "bb_compartment",10,10,2300,980);
  compartmentGlyph->setBoundingBox(&bb);

  TextGlyph* tg=layout->createTextGlyph();
  tg->setId("TextGlyph_Hepatocyte");
  tg->setOriginOfTextId(compartment->getId());
  bb=BoundingBox(&layoutns, "bb_tg_compartment",50,870,300,72);
  tg->setBoundingBox(&bb);
  tg->setGraphicalObjectId(compartmentGlyph->getId());

  CompartmentGlyph* mito1Glyph=layout->createCompartmentGlyph();
  mito1Glyph->setId("Mito1_Glyph");
  mito1Glyph->setCompartmentId(mito1->getId());
  bb=BoundingBox(&layoutns, "bb_mito1",100,100,300,100);
  mito1Glyph->setBoundingBox(&bb);

  tg=layout->createTextGlyph();
  tg->setId("TextGlyph_mito1");
  tg->setOriginOfTextId(mito1->getId());
  bb=BoundingBox(&layoutns, "bb_tg_mito1",110,110,280,72);
  tg->setBoundingBox(&bb);
  tg->setGraphicalObjectId(mito1Glyph->getId());


  CompartmentGlyph* mito2Glyph=layout->createCompartmentGlyph();
  mito2Glyph->setId("Mito2_Glyph");
  mito2Glyph->setCompartmentId(mito2->getId());
  bb=BoundingBox(&layoutns, "bb_mito2",200,650,300,100);
  mito2Glyph->setBoundingBox(&bb);

  tg=layout->createTextGlyph();
  tg->setId("TextGlyph_mito2");
  tg->setOriginOfTextId(mito2->getId());
  bb=BoundingBox(&layoutns, "bb_tg_mito2",210,660,280,72);
  tg->setBoundingBox(&bb);
  tg->setGraphicalObjectId(mito2Glyph->getId());

  CompartmentGlyph* mito3Glyph_2=layout->createCompartmentGlyph();
  mito3Glyph_2->setId("Mito3_Glyph_2");
  mito3Glyph_2->setCompartmentId(mito3->getId());
  bb=BoundingBox(&layoutns, "bb_mito3_2",1470,30,820,536);
  mito3Glyph_2->setBoundingBox(&bb);

  tg=layout->createTextGlyph();
  tg->setId("TextGlyph_mito3_2");
  tg->setOriginOfTextId(mito3->getId());
  bb=BoundingBox(&layoutns, "bb_tg_mito3_2",1475,35,200,72);
  tg->setBoundingBox(&bb);
  tg->setGraphicalObjectId(mito3Glyph_2->getId());




  // create the SpeciesGlyphs

  // Cytosol

  // Malate cyt
  SpeciesGlyph* speciesGlyph_malate_cyt=layout->createSpeciesGlyph();
  speciesGlyph_malate_cyt->setId("SpeciesGlyph_malate_cyt");
  speciesGlyph_malate_cyt->setSpeciesId(malate_cyt->getId());
  bb=BoundingBox(&layoutns, "bb_sg_malate_cyt",580,280,240,36);
  speciesGlyph_malate_cyt->setBoundingBox(&bb);

  tg=layout->createTextGlyph();
  tg->setId("TextGlyph_malate_cyt");
  bb=BoundingBox(&layoutns, "bb_tg_malatate_cyt",590,280,220,36);
  tg->setBoundingBox(&bb);
  tg->setOriginOfTextId(malate_cyt->getId());
  tg->setGraphicalObjectId(speciesGlyph_malate_cyt->getId());

  // Oxaloacetate cyt
  SpeciesGlyph* speciesGlyph_oxaloacetate_cyt=layout->createSpeciesGlyph();
  speciesGlyph_oxaloacetate_cyt->setId("SpeciesGlyph_oxaloacetate_cyt");
  speciesGlyph_oxaloacetate_cyt->setSpeciesId(oxaloacetate_cyt->getId());
  bb=BoundingBox(&layoutns, "bb_sg_oxaloacetate_cyt",580,480,240,36);
  speciesGlyph_oxaloacetate_cyt->setBoundingBox(&bb);

  tg=layout->createTextGlyph();
  tg->setId("TextGlyph_oxaloacetate_cyt");
  bb=BoundingBox(&layoutns, "bb_tg_oxaloacetate_cyt",590,480,220,36);
  tg->setBoundingBox(&bb);
  tg->setOriginOfTextId(oxaloacetate_cyt->getId());
  tg->setGraphicalObjectId(speciesGlyph_oxaloacetate_cyt->getId());

  // Aspartate cyt
  SpeciesGlyph* speciesGlyph_aspartate_cyt=layout->createSpeciesGlyph();
  speciesGlyph_aspartate_cyt->setId("SpeciesGlyph_aspartate_cyt");
  speciesGlyph_aspartate_cyt->setSpeciesId(aspartate_cyt->getId());
  bb=BoundingBox(&layoutns, "bb_sg_aspartate_cyt",580,680,240,36);
  speciesGlyph_aspartate_cyt->setBoundingBox(&bb);

  tg=layout->createTextGlyph();
  tg->setId("TextGlyph_aspartate_cyt");
  bb=BoundingBox(&layoutns, "bb_tg_aspartate_cyt",590,680,220,36);
  tg->setBoundingBox(&bb);
  tg->setOriginOfTextId(aspartate_cyt->getId());
  tg->setGraphicalObjectId(speciesGlyph_aspartate_cyt->getId());

  // Glutamate cyt
  SpeciesGlyph* speciesGlyph_glutamate_cyt=layout->createSpeciesGlyph();
  speciesGlyph_glutamate_cyt->setId("SpeciesGlyph_glutamate_cyt");
  speciesGlyph_glutamate_cyt->setSpeciesId(glutamate_cyt->getId());
  bb=BoundingBox(&layoutns, "bb_sg_glutamate_cyt",800,610,240,36);
  speciesGlyph_glutamate_cyt->setBoundingBox(&bb);

  tg=layout->createTextGlyph();
  tg->setId("TextGlyph_glutamate_cyt");
  bb=BoundingBox(&layoutns, "bb_tg_glutamate_cyt",810,610,220,36);
  tg->setBoundingBox(&bb);
  tg->setOriginOfTextId(glutamate_cyt->getId());
  tg->setGraphicalObjectId(speciesGlyph_glutamate_cyt->getId());

  // alpha-Ketoglutarate cyt
  SpeciesGlyph* speciesGlyph_aKetoglutarate_cyt=layout->createSpeciesGlyph();
  speciesGlyph_aKetoglutarate_cyt->setId("SpeciesGlyph_aKetoglutarate_cyt");
  speciesGlyph_aKetoglutarate_cyt->setSpeciesId(aKetoglutarate_cyt->getId());
  bb=BoundingBox(&layoutns, "bb_sg_aKetoglutarate_cyt",860,500,280,36);
  speciesGlyph_aKetoglutarate_cyt->setBoundingBox(&bb);

  tg=layout->createTextGlyph();
  tg->setId("TextGlyph_aKetoglutarate_cyt");
  bb=BoundingBox(&layoutns, "bb_tg_aKetoglutarate_cyt",870,500,260,36);
  tg->setBoundingBox(&bb);
  tg->setOriginOfTextId(aKetoglutarate_cyt->getId());
  tg->setGraphicalObjectId(speciesGlyph_aKetoglutarate_cyt->getId());

  // NAD+ cyt
  SpeciesGlyph* speciesGlyph_nad_cyt=layout->createSpeciesGlyph();
  speciesGlyph_nad_cyt->setId("SpeciesGlyph_nad_cyt");
  speciesGlyph_nad_cyt->setSpeciesId(nad_cyt->getId());
  bb=BoundingBox(&layoutns, "bb_sg_nad_cyt",520,350,100,24);
  speciesGlyph_nad_cyt->setBoundingBox(&bb);

  tg=layout->createTextGlyph();
  tg->setId("TextGlyph_nad_cyt");
  bb=BoundingBox(&layoutns, "bb_tg_nad_cyt",525,350,80,24);
  tg->setBoundingBox(&bb);
  tg->setOriginOfTextId(nad_cyt->getId());
  tg->setGraphicalObjectId(speciesGlyph_nad_cyt->getId());

  // NADH cyt
  SpeciesGlyph* speciesGlyph_nadh_cyt=layout->createSpeciesGlyph();
  speciesGlyph_nadh_cyt->setId("SpeciesGlyph_nadh_cyt");
  speciesGlyph_nadh_cyt->setSpeciesId(nadh_cyt->getId());
  bb=BoundingBox(&layoutns, "bb_sg_nadh_cyt",520,430,100,24);
  speciesGlyph_nadh_cyt->setBoundingBox(&bb);

  tg=layout->createTextGlyph();
  tg->setId("TextGlyph_nadh_cyt");
  bb=BoundingBox(&layoutns, "bb_tg_nadh_cyt",525,430,80,24);
  tg->setBoundingBox(&bb);
  tg->setOriginOfTextId(nadh_cyt->getId());
  tg->setGraphicalObjectId(speciesGlyph_nadh_cyt->getId());

  // H+ cyt
  SpeciesGlyph* speciesGlyph_h_cyt=layout->createSpeciesGlyph();
  speciesGlyph_h_cyt->setId("SpeciesGlyph_h_cyt");
  speciesGlyph_h_cyt->setSpeciesId(h_cyt->getId());
  bb=BoundingBox(&layoutns, "bb_sg_h_cyt",430,430,40,24);
  speciesGlyph_h_cyt->setBoundingBox(&bb);

  tg=layout->createTextGlyph();
  tg->setId("TextGlyph_h_cyt");
  bb=BoundingBox(&layoutns, "bb_tg_h_cyt",435,430,30,24);
  tg->setBoundingBox(&bb);
  tg->setOriginOfTextId(h_cyt->getId());
  tg->setGraphicalObjectId(speciesGlyph_h_cyt->getId());


  // create the ReactionGlyphs

  ReactionGlyph* rg_malatedh_cyt=layout->createReactionGlyph();
  rg_malatedh_cyt->setId("rg_malatedh_cyt");
  rg_malatedh_cyt->setReactionId(malatedh_cyt->getId());

  Curve* curve=rg_malatedh_cyt->getCurve();
  LineSegment* ls=curve->createLineSegment();
  Point p(&layoutns, 700,381);
  ls->setStart(&p);
  p=Point(&layoutns, 700,415);
  ls->setEnd(&p);

  tg=layout->createTextGlyph();
  tg->setId("tg_rg_malaltedh_cyt");
  bb=BoundingBox(&layoutns, "bb_tg_rg_malaltedh_cyt",700,385,210,24);
  tg->setBoundingBox(&bb);
  tg->setOriginOfTextId(malatedh_cyt->getId());
  tg->setGraphicalObjectId(rg_malatedh_cyt->getId());


  ReactionGlyph* rg_aspartateat_cyt=layout->createReactionGlyph();
  rg_aspartateat_cyt->setId("rg_aspartateat_cyt");
  rg_aspartateat_cyt->setReactionId(aspartateat_cyt->getId());

  curve=rg_aspartateat_cyt->getCurve();
  ls=curve->createLineSegment();
  p=Point(&layoutns, 700,581);
  ls->setStart(&p);
  p=Point(&layoutns, 700,615);
  ls->setEnd(&p);

  tg=layout->createTextGlyph();
  tg->setId("tg_rg_aspartateat_cyt");
  bb=BoundingBox(&layoutns, "bb_tg_rg_aspartateat_cyt",440,585,260,24);
  tg->setBoundingBox(&bb);
  tg->setOriginOfTextId(aspartateat_cyt->getId());
  tg->setGraphicalObjectId(rg_aspartateat_cyt->getId());



  // add the SpeciesReferenceGlyphs

  SpeciesReferenceGlyph* srg_malate_cyt_1=rg_malatedh_cyt->createSpeciesReferenceGlyph();
  srg_malate_cyt_1->setId("srg_malate_cyt_1");
  srg_malate_cyt_1->setSpeciesGlyphId(speciesGlyph_malate_cyt->getId());
  srg_malate_cyt_1->setSpeciesReferenceId(sr_malate_cyt->getId());
  srg_malate_cyt_1->setRole(SPECIES_ROLE_SUBSTRATE);

  ls=srg_malate_cyt_1->createLineSegment();
  p=Point(&layoutns, 700,381);
  ls->setStart(&p);
  p=Point(&layoutns, 700,316);
  ls->setEnd(&p);

  SpeciesReferenceGlyph* srg_nad_cyt=rg_malatedh_cyt->createSpeciesReferenceGlyph();
  srg_nad_cyt->setId("srg_nad_cyt");
  srg_nad_cyt->setSpeciesGlyphId(speciesGlyph_nad_cyt->getId());
  srg_nad_cyt->setSpeciesReferenceId(sr_nad_cyt->getId());
  srg_nad_cyt->setRole(SPECIES_ROLE_SUBSTRATE);

  CubicBezier* cb=srg_nad_cyt->createCubicBezier();
  p=Point(&layoutns, 700,381);
  cb->setStart(&p);
  p=Point(&layoutns, 700,362);
  cb->setBasePoint1(&p);
  p=Point(&layoutns, 700,362);
  cb->setBasePoint2(&p);
  p=Point(&layoutns, 620,362);
  cb->setEnd(&p);

  SpeciesReferenceGlyph* srg_oxaloacetate_cyt_1=rg_malatedh_cyt->createSpeciesReferenceGlyph();
  srg_oxaloacetate_cyt_1->setId("srg_oxaloacetate_cyt_1");
  srg_oxaloacetate_cyt_1->setSpeciesGlyphId(speciesGlyph_oxaloacetate_cyt->getId());
  srg_oxaloacetate_cyt_1->setSpeciesReferenceId(sr_oxaloacetate_cyt_1->getId());
  srg_oxaloacetate_cyt_1->setRole(SPECIES_ROLE_PRODUCT);

  curve=srg_oxaloacetate_cyt_1->getCurve();
  ls=curve->createLineSegment();
  p=Point(&layoutns, 700,415);
  ls->setStart(&p);
  p=Point(&layoutns, 700,480);
  ls->setEnd(&p);

  SpeciesReferenceGlyph* srg_nadh_cyt=rg_malatedh_cyt->createSpeciesReferenceGlyph();
  srg_nadh_cyt->setId("srg_nadh_cyt");
  srg_nadh_cyt->setSpeciesGlyphId(speciesGlyph_nadh_cyt->getId());
  srg_nadh_cyt->setSpeciesReferenceId(sr_nadh_cyt->getId());
  srg_nadh_cyt->setRole(SPECIES_ROLE_PRODUCT);

  cb=srg_nadh_cyt->createCubicBezier();
  p=Point(&layoutns, 700,415);
  cb->setStart(&p);
  p=Point(&layoutns, 700,442);
  cb->setBasePoint1(&p);
  p=Point(&layoutns, 700,442);
  cb->setBasePoint2(&p);
  p=Point(&layoutns, 620,442);
  cb->setEnd(&p);

  SpeciesReferenceGlyph* srg_h_cyt=rg_malatedh_cyt->createSpeciesReferenceGlyph();
  srg_h_cyt->setId("srg_h_cyt");
  srg_h_cyt->setSpeciesGlyphId(speciesGlyph_h_cyt->getId());
  srg_h_cyt->setSpeciesReferenceId(sr_h_cyt->getId());
  srg_h_cyt->setRole(SPECIES_ROLE_PRODUCT);

  cb=srg_h_cyt->createCubicBezier();
  p=Point(&layoutns, 700,415); 
  cb->setStart(&p);
  p=Point(&layoutns, 570,415);
  cb->setBasePoint1(&p);
  p=Point(&layoutns, 570,415);
  cb->setBasePoint2(&p);
  p=Point(&layoutns, 470,430);
  cb->setEnd(&p);

  SpeciesReferenceGlyph* srg_oxaloacetate_cyt_2=rg_aspartateat_cyt->createSpeciesReferenceGlyph();
  srg_oxaloacetate_cyt_2->setId("srg_oxaloacetate_cyt_2");
  srg_oxaloacetate_cyt_2->setSpeciesGlyphId(speciesGlyph_oxaloacetate_cyt->getId());
  srg_oxaloacetate_cyt_2->setSpeciesReferenceId(sr_oxaloacetate_cyt_2->getId());
  srg_oxaloacetate_cyt_2->setRole(SPECIES_ROLE_SUBSTRATE);

  curve=srg_oxaloacetate_cyt_2->getCurve();
  ls=curve->createLineSegment();
  p=Point(&layoutns, 700,581);
  ls->setStart(&p);
  p=Point(&layoutns, 700,516);
  ls->setEnd(&p);

  SpeciesReferenceGlyph* srg_glutamate_cyt_1=rg_aspartateat_cyt->createSpeciesReferenceGlyph();
  srg_glutamate_cyt_1->setId("srg_glutamate_cyt_1");
  srg_glutamate_cyt_1->setSpeciesGlyphId(speciesGlyph_glutamate_cyt->getId());
  srg_glutamate_cyt_1->setSpeciesReferenceId(sr_glutamate_cyt_1->getId());
  srg_glutamate_cyt_1->setRole(SPECIES_ROLE_SUBSTRATE);

  curve=srg_glutamate_cyt_1->getCurve();
  cb=curve->createCubicBezier();
  p=Point(&layoutns, 700,581);
  cb->setStart(&p);
  p=Point(&layoutns, 750,581);
  cb->setBasePoint1(&p);
  p=Point(&layoutns, 750,628);
  cb->setBasePoint2(&p);
  p=Point(&layoutns, 800,628);
  cb->setEnd(&p);

  SpeciesReferenceGlyph* srg_aspartate_cyt_1=rg_aspartateat_cyt->createSpeciesReferenceGlyph();
  srg_aspartate_cyt_1->setId("srg_aspartate_cyt_1");
  srg_aspartate_cyt_1->setSpeciesGlyphId(speciesGlyph_aspartate_cyt->getId());
  srg_aspartate_cyt_1->setSpeciesReferenceId(sr_aspartate_cyt_1->getId());
  srg_aspartate_cyt_1->setRole(SPECIES_ROLE_PRODUCT);

  curve=srg_aspartate_cyt_1->getCurve();
  ls=curve->createLineSegment();
  p=Point(&layoutns, 700,615);
  ls->setStart(&p);
  p=Point(&layoutns, 700,680);
  ls->setEnd(&p);

  SpeciesReferenceGlyph* srg_aKetoglutarate_cyt_1=rg_aspartateat_cyt->createSpeciesReferenceGlyph();
  srg_aKetoglutarate_cyt_1->setId("srg_aKetoglutaratecyt_1");
  srg_aKetoglutarate_cyt_1->setSpeciesGlyphId(speciesGlyph_aKetoglutarate_cyt->getId());
  srg_aKetoglutarate_cyt_1->setSpeciesReferenceId(sr_aKetoglutarate_cyt_1->getId());
  srg_aKetoglutarate_cyt_1->setRole(SPECIES_ROLE_PRODUCT);

  curve=srg_aKetoglutarate_cyt_1->getCurve();
  cb=curve->createCubicBezier();
  p=Point(&layoutns, 700,615);
  cb->setStart(&p);
  p=Point(&layoutns, 790,615);
  cb->setBasePoint1(&p);
  p=Point(&layoutns, 790,515);
  cb->setBasePoint2(&p);
  p=Point(&layoutns, 860,515);
  cb->setEnd(&p);


  // Malate mito3
  SpeciesGlyph* speciesGlyph_malate_mito3=layout->createSpeciesGlyph();
  speciesGlyph_malate_mito3->setId("SpeciesGlyph_malate_mito3");
  speciesGlyph_malate_mito3->setSpeciesId(malate_mito3->getId());
  bb=BoundingBox(&layoutns, "bb_sg_malate_mito3",1850,80,240,36);
  speciesGlyph_malate_mito3->setBoundingBox(&bb);

  tg=layout->createTextGlyph();
  tg->setId("TextGlyph_malate_mito3");
  bb=BoundingBox(&layoutns, "bb_tg_malatate_mito3",1860,80,220,36);
  tg->setBoundingBox(&bb);
  tg->setOriginOfTextId(malate_mito3->getId());
  tg->setGraphicalObjectId(speciesGlyph_malate_mito3->getId());

  // Oxaloacetate mito3
  SpeciesGlyph* speciesGlyph_oxaloacetate_mito3=layout->createSpeciesGlyph();
  speciesGlyph_oxaloacetate_mito3->setId("SpeciesGlyph_oxaloacetate_mito3");
  speciesGlyph_oxaloacetate_mito3->setSpeciesId(oxaloacetate_mito3->getId());
  bb=BoundingBox(&layoutns, "bb_sg_oxaloacetate_mito3",1850,280,240,36);
  speciesGlyph_oxaloacetate_mito3->setBoundingBox(&bb);

  tg=layout->createTextGlyph();
  tg->setId("TextGlyph_oxaloacetate_mito3");
  bb=BoundingBox(&layoutns, "bb_tg_oxaloacetate_mito3",1860,280,220,36);
  tg->setBoundingBox(&bb);
  tg->setOriginOfTextId(oxaloacetate_mito3->getId());
  tg->setGraphicalObjectId(speciesGlyph_oxaloacetate_mito3->getId());

  // Aspartate mito3
  SpeciesGlyph* speciesGlyph_aspartate_mito3=layout->createSpeciesGlyph();
  speciesGlyph_aspartate_mito3->setId("SpeciesGlyph_aspartate_mito3");
  speciesGlyph_aspartate_mito3->setSpeciesId(aspartate_mito3->getId());
  bb=BoundingBox(&layoutns, "bb_sg_aspartate_mito3",1850,480,240,36);
  speciesGlyph_aspartate_mito3->setBoundingBox(&bb);

  tg=layout->createTextGlyph();
  tg->setId("TextGlyph_aspartate_mito3");
  bb=BoundingBox(&layoutns, "bb_tg_aspartate_mito3",1860,480,220,36);
  tg->setBoundingBox(&bb);
  tg->setOriginOfTextId(aspartate_mito3->getId());
  tg->setGraphicalObjectId(speciesGlyph_aspartate_mito3->getId());

  // Glutamate mito3
  SpeciesGlyph* speciesGlyph_glutamate_mito3=layout->createSpeciesGlyph();
  speciesGlyph_glutamate_mito3->setId("SpeciesGlyph_glutamate_mito3");
  speciesGlyph_glutamate_mito3->setSpeciesId(glutamate_mito3->getId());
  bb=BoundingBox(&layoutns, "bb_sg_glutamate_mito3",1550,430,240,36);
  speciesGlyph_glutamate_mito3->setBoundingBox(&bb);

  tg=layout->createTextGlyph();
  tg->setId("TextGlyph_glutamate_mito3");
  bb=BoundingBox(&layoutns, "bb_tg_glutamate_mito3",1560,430,220,36);
  tg->setBoundingBox(&bb);
  tg->setOriginOfTextId(glutamate_mito3->getId());
  tg->setGraphicalObjectId(speciesGlyph_glutamate_mito3->getId());

  // alpha-Ketoglutarate mito3
  SpeciesGlyph* speciesGlyph_aKetoglutarate_mito3=layout->createSpeciesGlyph();
  speciesGlyph_aKetoglutarate_mito3->setId("SpeciesGlyph_aKetoglutarate_mito3");
  speciesGlyph_aKetoglutarate_mito3->setSpeciesId(aKetoglutarate_mito3->getId());
  bb=BoundingBox(&layoutns, "bb_sg_aKetoglutarate_mito3",1530,300,280,36);
  speciesGlyph_aKetoglutarate_mito3->setBoundingBox(&bb);

  tg=layout->createTextGlyph();
  tg->setId("TextGlyph_aKetoglutarate_mito3");
  bb=BoundingBox(&layoutns, "bb_tg_aKetoglutarate_mito3",1540,300,260,36);
  tg->setBoundingBox(&bb);
  tg->setOriginOfTextId(aKetoglutarate_mito3->getId());
  tg->setGraphicalObjectId(speciesGlyph_aKetoglutarate_mito3->getId());

  // NAD+ mito3
  SpeciesGlyph* speciesGlyph_nad_mito3=layout->createSpeciesGlyph();
  speciesGlyph_nad_mito3->setId("SpeciesGlyph_nad_mito3");
  speciesGlyph_nad_mito3->setSpeciesId(nad_mito3->getId());
  bb=BoundingBox(&layoutns, "bb_sg_nad_mito3",2050,150,100,24);
  speciesGlyph_nad_mito3->setBoundingBox(&bb);

  tg=layout->createTextGlyph();
  tg->setId("TextGlyph_nad_mito3");
  bb=BoundingBox(&layoutns, "bb_tg_nad_mito3",2055,150,80,24);
  tg->setBoundingBox(&bb);
  tg->setOriginOfTextId(nad_mito3->getId());
  tg->setGraphicalObjectId(speciesGlyph_nad_mito3->getId());

  // NADH mito3
  SpeciesGlyph* speciesGlyph_nadh_mito3=layout->createSpeciesGlyph();
  speciesGlyph_nadh_mito3->setId("SpeciesGlyph_nadh_mito3");
  speciesGlyph_nadh_mito3->setSpeciesId(nadh_mito3->getId());
  bb=BoundingBox(&layoutns, "bb_sg_nadh_mito3",2050,230,100,24);
  speciesGlyph_nadh_mito3->setBoundingBox(&bb);

  tg=layout->createTextGlyph();
  tg->setId("TextGlyph_nadh_mito3");
  bb=BoundingBox(&layoutns, "bb_tg_nadh_mito3",2055,230,80,24);
  tg->setBoundingBox(&bb);
  tg->setOriginOfTextId(nadh_mito3->getId());
  tg->setGraphicalObjectId(speciesGlyph_nadh_mito3->getId());

  // H+ mito3
  SpeciesGlyph* speciesGlyph_h_mito3=layout->createSpeciesGlyph();
  speciesGlyph_h_mito3->setId("SpeciesGlyph_h_mito3");
  speciesGlyph_h_mito3->setSpeciesId(h_mito3->getId());
  bb=BoundingBox(&layoutns, "bb_sg_h_mito3",2200,230,40,24);
  speciesGlyph_h_mito3->setBoundingBox(&bb);

  tg=layout->createTextGlyph();
  tg->setId("TextGlyph_h_mito3");
  bb=BoundingBox(&layoutns, "bb_tg_h_mito3",2205,230,30,24);
  tg->setBoundingBox(&bb);
  tg->setOriginOfTextId(h_mito3->getId());
  tg->setGraphicalObjectId(speciesGlyph_h_mito3->getId());


  // create the ReactionGlyphs

  ReactionGlyph* rg_malatedh_mito3=layout->createReactionGlyph();
  rg_malatedh_mito3->setId("rg_malatedh_mito3");
  rg_malatedh_mito3->setReactionId(malatedh_mito3->getId());

  curve=rg_malatedh_mito3->getCurve();
  ls=curve->createLineSegment();
  p=Point(&layoutns, 1970,181);
  ls->setStart(&p);
  p=Point(&layoutns, 1970,215);
  ls->setEnd(&p);

  tg=layout->createTextGlyph();
  tg->setId("tg_rg_malatedh_mito3");
  bb=BoundingBox(&layoutns, "bb_tg_rg_malatedh_mito3",1740,185,220,24);
  tg->setBoundingBox(&bb);
  tg->setOriginOfTextId(malatedh_mito3->getId());
  tg->setGraphicalObjectId(rg_malatedh_mito3->getId());

  ReactionGlyph* rg_aspartateat_mito3=layout->createReactionGlyph();
  rg_aspartateat_mito3->setId("rg_aspartateat_mito3");
  rg_aspartateat_mito3->setReactionId(aspartateat_mito3->getId());

  curve=rg_aspartateat_mito3->getCurve();
  ls=curve->createLineSegment();
  p=Point(&layoutns, 1970,381);
  ls->setStart(&p);
  p=Point(&layoutns, 1970,415);
  ls->setEnd(&p);

  tg=layout->createTextGlyph();
  tg->setId("tg_rg_aspartateat_mito3");
  bb=BoundingBox(&layoutns, "bb_tg_rg_aspartateat_mito3",1970,385,260,24);
  tg->setBoundingBox(&bb);
  tg->setOriginOfTextId(aspartateat_mito3->getId());
  tg->setGraphicalObjectId(rg_aspartateat_mito3->getId());


  // add the SpeciesReferenceGlyphs

  SpeciesReferenceGlyph* srg_malate_mito3_1=rg_malatedh_mito3->createSpeciesReferenceGlyph();
  srg_malate_mito3_1->setId("srg_malate_mito3_1");
  srg_malate_mito3_1->setSpeciesGlyphId(speciesGlyph_malate_mito3->getId());
  srg_malate_mito3_1->setSpeciesReferenceId(sr_malate_mito3->getId());
  srg_malate_mito3_1->setRole(SPECIES_ROLE_SUBSTRATE);

  ls=srg_malate_mito3_1->createLineSegment();
  p=Point(&layoutns, 1970,181);
  ls->setStart(&p);
  p=Point(&layoutns, 1970,116);
  ls->setEnd(&p);

  SpeciesReferenceGlyph* srg_nad_mito3=rg_malatedh_mito3->createSpeciesReferenceGlyph();
  srg_nad_mito3->setId("srg_nad_mito3");
  srg_nad_mito3->setSpeciesGlyphId(speciesGlyph_nad_mito3->getId());
  srg_nad_mito3->setSpeciesReferenceId(sr_nad_mito3->getId());
  srg_nad_mito3->setRole(SPECIES_ROLE_SUBSTRATE);

  cb=srg_nad_mito3->createCubicBezier();
  p=Point(&layoutns, 1970,181);
  cb->setStart(&p);
  p=Point(&layoutns, 1970,162);
  cb->setBasePoint1(&p);
  p=Point(&layoutns, 1970,162);
  cb->setBasePoint2(&p);
  p=Point(&layoutns, 2050,162);
  cb->setEnd(&p);

  SpeciesReferenceGlyph* srg_oxaloacetate_mito3_1=rg_malatedh_mito3->createSpeciesReferenceGlyph();
  srg_oxaloacetate_mito3_1->setId("srg_oxaloacetate_mito3_1");
  srg_oxaloacetate_mito3_1->setSpeciesGlyphId(speciesGlyph_oxaloacetate_mito3->getId());
  srg_oxaloacetate_mito3_1->setSpeciesReferenceId(sr_oxaloacetate_mito3_1->getId());
  srg_oxaloacetate_mito3_1->setRole(SPECIES_ROLE_PRODUCT);

  curve=srg_oxaloacetate_mito3_1->getCurve();
  ls=curve->createLineSegment();
  p=Point(&layoutns, 1970,215);
  ls->setStart(&p);
  p=Point(&layoutns, 1970,280);
  ls->setEnd(&p);

  SpeciesReferenceGlyph* srg_nadh_mito3=rg_malatedh_mito3->createSpeciesReferenceGlyph();
  srg_nadh_mito3->setId("srg_nadh_mito3");
  srg_nadh_mito3->setSpeciesGlyphId(speciesGlyph_nadh_mito3->getId());
  srg_nadh_mito3->setSpeciesReferenceId(sr_nadh_mito3->getId());
  srg_nadh_mito3->setRole(SPECIES_ROLE_PRODUCT);

  cb=srg_nadh_mito3->createCubicBezier();
  p=Point(&layoutns, 1970,215);
  cb->setStart(&p);
  p=Point(&layoutns, 1970,242);
  cb->setBasePoint1(&p);
  p=Point(&layoutns, 1970,242);
  cb->setBasePoint2(&p);
  p=Point(&layoutns, 2050,242);
  cb->setEnd(&p);

  SpeciesReferenceGlyph* srg_h_mito3=rg_malatedh_mito3->createSpeciesReferenceGlyph();
  srg_h_mito3->setId("srg_h_mito3");
  srg_h_mito3->setSpeciesGlyphId(speciesGlyph_h_mito3->getId());
  srg_h_mito3->setSpeciesReferenceId(sr_h_mito3->getId());
  srg_h_mito3->setRole(SPECIES_ROLE_PRODUCT);

  cb=srg_h_mito3->createCubicBezier();
  p=Point(&layoutns, 1970,215);
  cb->setStart(&p);
  p=Point(&layoutns, 2100,215);
  cb->setBasePoint1(&p);
  p=Point(&layoutns, 2100,215);
  cb->setBasePoint2(&p);
  p=Point(&layoutns, 2200,230);
  cb->setEnd(&p);

  SpeciesReferenceGlyph* srg_oxaloacetate_mito3_2=rg_aspartateat_mito3->createSpeciesReferenceGlyph();
  srg_oxaloacetate_mito3_2->setId("srg_oxaloacetate_mito3_2");
  srg_oxaloacetate_mito3_2->setSpeciesGlyphId(speciesGlyph_oxaloacetate_mito3->getId());
  srg_oxaloacetate_mito3_2->setSpeciesReferenceId(sr_oxaloacetate_mito3_2->getId());
  srg_oxaloacetate_mito3_2->setRole(SPECIES_ROLE_SUBSTRATE);

  curve=srg_oxaloacetate_mito3_2->getCurve();
  ls=curve->createLineSegment();
  p=Point(&layoutns, 1970,381);
  ls->setStart(&p);
  p=Point(&layoutns, 1970,316);
  ls->setEnd(&p);

  SpeciesReferenceGlyph* srg_glutamate_mito3_1=rg_aspartateat_mito3->createSpeciesReferenceGlyph();
  srg_glutamate_mito3_1->setId("srg_glutamate_mito3_1");
  srg_glutamate_mito3_1->setSpeciesGlyphId(speciesGlyph_glutamate_mito3->getId());
  srg_glutamate_mito3_1->setSpeciesReferenceId(sr_glutamate_mito3_1->getId());
  srg_glutamate_mito3_1->setRole(SPECIES_ROLE_SUBSTRATE);

  curve=srg_glutamate_mito3_1->getCurve();
  cb=curve->createCubicBezier();
  p=Point(&layoutns, 1970,381);
  cb->setStart(&p);
  p=Point(&layoutns, 1880,381);
  cb->setBasePoint1(&p);
  p=Point(&layoutns, 1880,448);
  cb->setBasePoint2(&p);
  p=Point(&layoutns, 1790,448);
  cb->setEnd(&p);

  SpeciesReferenceGlyph* srg_aspartate_mito3_1=rg_aspartateat_mito3->createSpeciesReferenceGlyph();
  srg_aspartate_mito3_1->setId("srg_aspartate_mito3_1");
  srg_aspartate_mito3_1->setSpeciesGlyphId(speciesGlyph_aspartate_mito3->getId());
  srg_aspartate_mito3_1->setSpeciesReferenceId(sr_aspartate_mito3_1->getId());
  srg_aspartate_mito3_1->setRole(SPECIES_ROLE_PRODUCT);

  curve=srg_aspartate_mito3_1->getCurve();
  ls=curve->createLineSegment();
  p=Point(&layoutns, 1970,415);
  ls->setStart(&p);
  p=Point(&layoutns, 1970,480);
  ls->setEnd(&p);

  SpeciesReferenceGlyph* srg_aKetoglutarate_mito3_1=rg_aspartateat_mito3->createSpeciesReferenceGlyph();
  srg_aKetoglutarate_mito3_1->setId("srg_aKetoglutaratemito3_1");
  srg_aKetoglutarate_mito3_1->setSpeciesGlyphId(speciesGlyph_aKetoglutarate_mito3->getId());
  srg_aKetoglutarate_mito3_1->setSpeciesReferenceId(sr_aKetoglutarate_mito3_1->getId());
  srg_aKetoglutarate_mito3_1->setRole(SPECIES_ROLE_PRODUCT);

  curve=srg_aKetoglutarate_mito3_1->getCurve();
  cb=curve->createCubicBezier();
  p=Point(&layoutns, 1970,415);
  cb->setStart(&p);
  p=Point(&layoutns, 1880,415);
  cb->setBasePoint1(&p);
  p=Point(&layoutns, 1880,315);
  cb->setBasePoint2(&p);
  p=Point(&layoutns, 1810,315);
  cb->setEnd(&p);

  // add the transport reaction glyphs

  ReactionGlyph* rg_aspartateCarrier=layout->createReactionGlyph();
  rg_aspartateCarrier->setId("rg_aspartateCarrier");
  rg_aspartateCarrier->setReactionId(aspartateCarrier->getId());

  curve=rg_aspartateCarrier->getCurve();
  ls=curve->createLineSegment();
  p=Point(&layoutns, 1420,530);
  ls->setStart(&p);
  p=Point(&layoutns, 1360,550);
  ls->setEnd(&p);

  tg=layout->createTextGlyph();
  tg->setId("tg_rg_aspartateCarrier");
  bb=BoundingBox(&layoutns, "bb_tg_rg_aspartateCarrier",1380,500,160,24);
  tg->setBoundingBox(&bb);
  tg->setOriginOfTextId(aspartateCarrier->getId());
  tg->setGraphicalObjectId(rg_aspartateCarrier->getId());


  ReactionGlyph* rg_malateCarrier=layout->createReactionGlyph();
  rg_malateCarrier->setId("rg_malateCarrier");
  rg_malateCarrier->setReactionId(malateCarrier->getId());

  curve=rg_malateCarrier->getCurve();
  ls=curve->createLineSegment();
  p=Point(&layoutns, 1420,320);
  ls->setStart(&p);
  p=Point(&layoutns, 1360,340);
  ls->setEnd(&p);

  tg=layout->createTextGlyph();
  tg->setId("tg_rg_malateCarrier");
  bb=BoundingBox(&layoutns, "bb_tg_rg_malateCarrier",1360,330,140,24);
  tg->setBoundingBox(&bb);
  tg->setOriginOfTextId(malateCarrier->getId());
  tg->setGraphicalObjectId(rg_malateCarrier->getId());



  // add the SpeciesReferenceGlyphs for the transporters

  SpeciesReferenceGlyph* srg_aKetoglutarate_mito3_2=rg_malateCarrier->createSpeciesReferenceGlyph();
  srg_aKetoglutarate_mito3_2->setId("srg_aKetoglutarate_mito3_2");
  srg_aKetoglutarate_mito3_2->setSpeciesGlyphId(speciesGlyph_aKetoglutarate_mito3->getId());
  srg_aKetoglutarate_mito3_2->setSpeciesReferenceId(sr_aKetoglutarate_mito3_2->getId());
  srg_aKetoglutarate_mito3_2->setRole(SPECIES_ROLE_SUBSTRATE);

  curve=srg_aKetoglutarate_mito3_2->getCurve();
  ls=curve->createLineSegment();
  p=Point(&layoutns, 1420,320);
  ls->setStart(&p);
  p=Point(&layoutns, 1530,318);
  ls->setEnd(&p);


  SpeciesReferenceGlyph* srg_aKetoglutarate_cyt_2=rg_malateCarrier->createSpeciesReferenceGlyph();
  srg_aKetoglutarate_cyt_2->setId("srg_aKetoglutarate_cyt_2");
  srg_aKetoglutarate_cyt_2->setSpeciesGlyphId(speciesGlyph_aKetoglutarate_cyt->getId());
  srg_aKetoglutarate_cyt_2->setSpeciesReferenceId(sr_aKetoglutarate_cyt_2->getId());
  srg_aKetoglutarate_cyt_2->setRole(SPECIES_ROLE_PRODUCT);

  curve=srg_aKetoglutarate_cyt_2->getCurve();
  ls=curve->createLineSegment();
  p=Point(&layoutns, 1360,340);
  ls->setStart(&p);
  p=Point(&layoutns, 1140,518);
  ls->setEnd(&p);


  SpeciesReferenceGlyph* srg_malate_cyt_2=rg_malateCarrier->createSpeciesReferenceGlyph();
  srg_malate_cyt_2->setId("srg_malate_cyt_2");
  srg_malate_cyt_2->setSpeciesGlyphId(speciesGlyph_malate_cyt->getId());
  srg_malate_cyt_2->setSpeciesReferenceId(sr_malate_cyt_2->getId());
  srg_malate_cyt_2->setRole(SPECIES_ROLE_SUBSTRATE);

  curve=srg_malate_cyt_2->getCurve();
  cb=curve->createCubicBezier();
  p=Point(&layoutns, 1420,320);
  cb->setStart(&p);
  p=Point(&layoutns, 1390,250);
  cb->setBasePoint1(&p);
  p=Point(&layoutns, 1390,250);
  cb->setBasePoint2(&p);
  p=Point(&layoutns, 820,298);
  cb->setEnd(&p);


  SpeciesReferenceGlyph* srg_malate_mito3_2=rg_malateCarrier->createSpeciesReferenceGlyph();
  srg_malate_mito3_2->setId("srg_malate_mito3_2");
  srg_malate_mito3_2->setSpeciesGlyphId(speciesGlyph_malate_mito3->getId());
  srg_malate_mito3_2->setSpeciesReferenceId(sr_malate_mito3_2->getId());
  srg_malate_mito3_2->setRole(SPECIES_ROLE_PRODUCT);

  curve=srg_malate_mito3_2->getCurve();
  cb=curve->createCubicBezier();
  p=Point(&layoutns, 1360,340);
  cb->setStart(&p);
  p=Point(&layoutns, 1390,150);
  cb->setBasePoint1(&p);
  p=Point(&layoutns, 1390,150);
  cb->setBasePoint2(&p);
  p=Point(&layoutns, 1850,98);
  cb->setEnd(&p);






  SpeciesReferenceGlyph* srg_aspartate_mito3_2=rg_aspartateCarrier->createSpeciesReferenceGlyph();
  srg_aspartate_mito3_2->setId("srg_aspartate_mito3_2");
  srg_aspartate_mito3_2->setSpeciesGlyphId(speciesGlyph_aspartate_mito3->getId());
  srg_aspartate_mito3_2->setSpeciesReferenceId(sr_aspartate_mito3_2->getId());
  srg_aspartate_mito3_2->setRole(SPECIES_ROLE_SUBSTRATE);

  curve=srg_aspartate_mito3_2->getCurve();
  ls=curve->createLineSegment();
  p=Point(&layoutns, 1420,530);
  ls->setStart(&p);
  p=Point(&layoutns, 1850,498);
  ls->setEnd(&p);


  SpeciesReferenceGlyph* srg_aspartate_cyt_2=rg_aspartateCarrier->createSpeciesReferenceGlyph();
  srg_aspartate_cyt_2->setId("srg_aspartate_cyt_2");
  srg_aspartate_cyt_2->setSpeciesGlyphId(speciesGlyph_aspartate_cyt->getId());
  srg_aspartate_cyt_2->setSpeciesReferenceId(sr_aspartate_cyt_2->getId());
  srg_aspartate_cyt_2->setRole(SPECIES_ROLE_PRODUCT);

  curve=srg_aspartate_cyt_2->getCurve();
  cb=curve->createCubicBezier();
  p=Point(&layoutns, 1360,550);
  cb->setStart(&p);
  p=Point(&layoutns, 1390,698);
  cb->setBasePoint1(&p);
  p=Point(&layoutns, 1390,698);
  cb->setBasePoint2(&p);
  p=Point(&layoutns, 820,698);
  cb->setEnd(&p);


  SpeciesReferenceGlyph* srg_glutamate_cyt_2=rg_aspartateCarrier->createSpeciesReferenceGlyph();
  srg_glutamate_cyt_2->setId("srg_glutamate_cyt_2");
  srg_glutamate_cyt_2->setSpeciesGlyphId(speciesGlyph_glutamate_cyt->getId());
  srg_glutamate_cyt_2->setSpeciesReferenceId(sr_glutamate_cyt_2->getId());
  srg_glutamate_cyt_2->setRole(SPECIES_ROLE_SUBSTRATE);

  curve=srg_glutamate_cyt_2->getCurve();
  cb=curve->createCubicBezier();
  p=Point(&layoutns, 1420,530);
  cb->setStart(&p);
  p=Point(&layoutns, 1390,648);
  cb->setBasePoint1(&p);
  p=Point(&layoutns, 1390,648);
  cb->setBasePoint2(&p);
  p=Point(&layoutns, 1050,628);
  cb->setEnd(&p);


  SpeciesReferenceGlyph* srg_glutamate_mito3_2=rg_aspartateCarrier->createSpeciesReferenceGlyph();
  srg_glutamate_mito3_2->setId("srg_glutamate_mito3_2");
  srg_glutamate_mito3_2->setSpeciesGlyphId(speciesGlyph_glutamate_mito3->getId());
  srg_glutamate_mito3_2->setSpeciesReferenceId(sr_glutamate_mito3_2->getId());
  srg_glutamate_mito3_2->setRole(SPECIES_ROLE_PRODUCT);

  curve=srg_glutamate_mito3_2->getCurve();
  cb=curve->createCubicBezier();
  p=Point(&layoutns, 1360,550);
  cb->setStart(&p);
  p=Point(&layoutns, 1390,448);
  cb->setBasePoint1(&p);
  p=Point(&layoutns, 1390,448);
  cb->setBasePoint2(&p);
  p=Point(&layoutns, 1550,448);
  cb->setEnd(&p);

  // now we add some global render information
  ListOfLayouts* pListOfLayouts=plugin->getListOfLayouts();
  fail_unless(pListOfLayouts!=NULL);

  RenderListOfLayoutsPlugin* lolPlugin = (RenderListOfLayoutsPlugin*)pListOfLayouts->getPlugin("render");
  GlobalRenderInformation* pGlobalRender=lolPlugin->createGlobalRenderInformation();
  pGlobalRender->setId("wireFrame");
  pGlobalRender->setName("wireframe style");
  pGlobalRender->setProgramName("Ralph Gauges");
  pGlobalRender->setProgramVersion("1.0");
  pGlobalRender->setBackgroundColor("#FFFFFFFF");
  // color definitions
  ColorDefinition* pColorDefinition=pGlobalRender->createColorDefinition();
  pColorDefinition->setId("white");
  pColorDefinition->setColorValue("#FFFFFF");
  pColorDefinition=pGlobalRender->createColorDefinition();
  pColorDefinition->setId("black");
  pColorDefinition->setColorValue("#000000");
  // styles
  // style for compartment glyphs
  GlobalStyle* pGlobalStyle=pGlobalRender->createStyle("compartmentGlyphStyle");
  pGlobalStyle->addType("COMPARTMENTGLYPH");
  RenderGroup* pGroup=pGlobalStyle->getGroup();
  pGroup->setStroke("black");
  pGroup->setStrokeWidth(1.0);
  Rectangle* pRectangle=pGroup->createRectangle();
  pRectangle->setCoordinatesAndSize(RelAbsVector(0.0,0.0),RelAbsVector(0.0,0.0),RelAbsVector(0.0,0.0),RelAbsVector(0.0,100.0),RelAbsVector(0.0,100.0));
  pRectangle->setRadiusX(RelAbsVector(0.0,0.0));

  // style for species glyphs
  pGlobalStyle=pGlobalRender->createStyle("speciesGlyphStyle");
  pGlobalStyle->addType("SPECIESGLYPH");
  pGroup=pGlobalStyle->getGroup();
  pGroup->setStroke("black");
  pGroup->setStrokeWidth(1.0);
  pRectangle=pGroup->createRectangle();
  pRectangle->setCoordinatesAndSize(RelAbsVector(0.0,0.0),RelAbsVector(0.0,0.0),RelAbsVector(0.0,0.0),RelAbsVector(0.0,100.0),RelAbsVector(0.0,100.0));
  pRectangle->setRadiusX(RelAbsVector(0.0,0.0));
 
  // style for all other glyphs 
  pGlobalStyle=pGlobalRender->createStyle("reactionGlyphStyle");
  pGlobalStyle->addType("SPECIESREFERENCEGLYPH");
  pGlobalStyle->addType("REACTIONGLYPH");
  pGlobalStyle->addType("TEXTGLYPH");
  pGroup=pGlobalStyle->getGroup();
  pGroup->setStroke("black");
  pGroup->setStrokeWidth(1.0);
  pGroup->setFontSize(RelAbsVector(12.0,0.0));
  pGroup->setTextAnchor(Text::ANCHOR_MIDDLE);
  
  // second render information
  lolPlugin = (RenderListOfLayoutsPlugin*)pListOfLayouts->getPlugin("render");
  pGlobalRender=lolPlugin->createGlobalRenderInformation();
  pGlobalRender->setId("defaultGrayStyle");
  pGlobalRender->setName("grayscale style");
  pGlobalRender->setProgramName("Ralph Gauges");
  pGlobalRender->setProgramVersion("1.0");
  pGlobalRender->setBackgroundColor("#FFFFFFFF");
  // color definitions
  pColorDefinition=pGlobalRender->createColorDefinition();
  pColorDefinition->setId("lightGray");
  pColorDefinition->setColorValue("#CECECE");
  pColorDefinition=pGlobalRender->createColorDefinition();
  pColorDefinition->setId("white");
  pColorDefinition->setColorValue("#FFFFFF");
  pColorDefinition=pGlobalRender->createColorDefinition();
  pColorDefinition->setId("black");
  pColorDefinition->setColorValue("#000000");
  pColorDefinition=pGlobalRender->createColorDefinition();
  pColorDefinition->setId("lightGray2");
  pColorDefinition->setColorValue("#F0F0F0");
  pColorDefinition=pGlobalRender->createColorDefinition();
  pColorDefinition->setId("gray");
  pColorDefinition->setColorValue("#0B0B0B");
  // gradient definitions
  RadialGradient* pRadialGradient=pGlobalRender->createRadialGradientDefinition();
  pRadialGradient->setId("speciesGlyphGradient");
  GradientStop* pStop=pRadialGradient->createGradientStop();
  pStop->setOffset(RelAbsVector(0.0,0.0));
  pStop->setStopColor("white");   
  pStop=pRadialGradient->createGradientStop();
  pStop->setOffset(RelAbsVector(0.0,100.0));
  pStop->setStopColor("lightGray");
  // line endings
  LineEnding* pLineEnding=pGlobalRender->createLineEnding();
  pLineEnding->setId("simpleHead_black");
  p=Point(&layoutns, -8,-3);
  Dimensions d(&layoutns, 10,6);
  pLineEnding->getBoundingBox()->setPosition(&p);
  pLineEnding->getBoundingBox()->setDimensions(&d);
  pGroup=pLineEnding->getGroup();
  pGroup->setStroke("black");
  pGroup->setStrokeWidth(1.0);
  pGroup->setFillColor("black");
  Polygon* pPolygon=pGroup->createPolygon();
  RenderPoint* pR=pPolygon->createPoint();
  pR->setCoordinates(RelAbsVector(0.0,0.0),RelAbsVector(0.0,0.0)); 
  pR=pPolygon->createPoint();
  pR->setCoordinates(RelAbsVector(10.0,0.0),RelAbsVector(3.0,0.0)); 
  pR=pPolygon->createPoint();
  pR->setCoordinates(RelAbsVector(0.0,0.0),RelAbsVector(6.0,0.0)); 

  // styles
  // style for compartment glyphs
  pGlobalStyle=pGlobalRender->createStyle("compartmentGlyphStyle");
  pGlobalStyle->addType("COMPARTMENTGLYPH");
  pGroup=pGlobalStyle->getGroup();
  pGroup->setStroke("gray");
  pGroup->setStrokeWidth(1.0);
  pRectangle=pGroup->createRectangle();
  pRectangle->setCoordinatesAndSize(RelAbsVector(0.0,0.0),RelAbsVector(0.0,0.0),RelAbsVector(0.0,0.0),RelAbsVector(0.0,100.0),RelAbsVector(0.0,100.0));
  pRectangle->setRadiusX(RelAbsVector(0.0,5.0));
  pRectangle->setFillColor("lightGray2");

  // style for species glyphs
  pGlobalStyle=pGlobalRender->createStyle("speciesGlyphStyle");
  pGlobalStyle->addType("SPECIESGLYPH");
  pGroup=pGlobalStyle->getGroup();
  pGroup->setStroke("black");
  pGroup->setStrokeWidth(1.0);
  pRectangle=pGroup->createRectangle();
  pRectangle->setCoordinatesAndSize(RelAbsVector(0.0,0.0),RelAbsVector(0.0,0.0),RelAbsVector(0.0,0.0),RelAbsVector(0.0,100.0),RelAbsVector(0.0,100.0));
  pRectangle->setRadiusX(RelAbsVector(0.0,5.0));
  pRectangle->setFillColor("speciesGlyphGradient");
 
  // style for reaction and text glyphs 
  pGlobalStyle=pGlobalRender->createStyle("reactionGlyphStyle");
  pGlobalStyle->addType("REACTIONGLYPH");
  pGlobalStyle->addType("TEXTGLYPH");
  pGroup=pGlobalStyle->getGroup();
  pGroup->setStroke("black");
  pGroup->setStrokeWidth(1.0);
  pGroup->setFontSize(RelAbsVector(12.0,0.0));
  pGroup->setTextAnchor(Text::ANCHOR_MIDDLE);
  
  // style for substrate and product species reference glyphs
  pGlobalStyle=pGlobalRender->createStyle("reactantSpeciesReferenceGlyphStyle");
  pGlobalStyle->addRole("substrate");
  pGlobalStyle->addRole("sidesubstrate");
  pGlobalStyle->addRole("product");
  pGlobalStyle->addRole("sideproduct");
  pGroup=pGlobalStyle->getGroup();
  pGroup->setStroke("#000000");
  pGroup->setStrokeWidth(1.0);
 
  // style for activator species reference glyphs
  pGlobalStyle=pGlobalRender->createStyle("activatorSpeciesReferenceGlyphStyle");
  pGlobalStyle->addRole("activator");
  pGroup=pGlobalStyle->getGroup();
  pGroup->setStroke("#000000");
  pGroup->setStrokeWidth(1.0);
 
  // style for modifier species reference glyphs
  pGlobalStyle=pGlobalRender->createStyle("modifierSpeciesReferenceGlyphStyle");
  pGlobalStyle->addRole("modifier");
  pGroup=pGlobalStyle->getGroup();
  pGroup->setStroke("#000000");
  pGroup->setStrokeWidth(1.0);

  // style for inhibitor species reference glyphs
  pGlobalStyle=pGlobalRender->createStyle("inhibitorSpeciesReferenceGlyphStyle");
  pGlobalStyle->addRole("inhibitor");
  pGroup=pGlobalStyle->getGroup();
  pGroup->setStroke("#000000");
  pGroup->setStrokeWidth(1.0);
 
  // short gray style which uses the default style and just redefines some colors
  // second render information
  lolPlugin = (RenderListOfLayoutsPlugin*)pListOfLayouts->getPlugin("render");
  pGlobalRender=lolPlugin->createGlobalRenderInformation();
  pGlobalRender->setId("shortGrayStyle");
  pGlobalRender->setName("modified default style to grayscale");
  pGlobalRender->setReferenceRenderInformationId("defaultStyle");
  pGlobalRender->setProgramName("Ralph Gauges");
  pGlobalRender->setProgramVersion("1.0");
  pGlobalRender->setBackgroundColor("#FFFFFFFF");
  // color definitions
  pColorDefinition=pGlobalRender->createColorDefinition();
  pColorDefinition->setId("lightBlue");
  pColorDefinition->setColorValue("#CECECE");
  pColorDefinition=pGlobalRender->createColorDefinition();
  pColorDefinition->setId("white");
  pColorDefinition->setColorValue("#FFFFFF");
  pColorDefinition=pGlobalRender->createColorDefinition();
  pColorDefinition->setId("black");
  pColorDefinition->setColorValue("#000000");
  pColorDefinition=pGlobalRender->createColorDefinition();
  pColorDefinition->setId("red");
  pColorDefinition->setColorValue("#000000");
  pColorDefinition=pGlobalRender->createColorDefinition();
  pColorDefinition->setId("green");
  pColorDefinition->setColorValue("#000000");
  pColorDefinition=pGlobalRender->createColorDefinition();
  pColorDefinition->setId("blue");
  pColorDefinition->setColorValue("#000000");
  pColorDefinition=pGlobalRender->createColorDefinition();
  pColorDefinition->setId("lightYellow");
  pColorDefinition->setColorValue("#F0F0F0");
  pColorDefinition=pGlobalRender->createColorDefinition();
  pColorDefinition->setId("darkGreen");
  pColorDefinition->setColorValue("#0B0B0B");
     
  // render information for the default color style
  lolPlugin = (RenderListOfLayoutsPlugin*)pListOfLayouts->getPlugin("render");
  pGlobalRender=lolPlugin->createGlobalRenderInformation();
  pGlobalRender->setId("defaultStyle");
  pGlobalRender->setName("default style");
  pGlobalRender->setProgramName("Ralph Gauges");
  pGlobalRender->setProgramVersion("1.0");
  pGlobalRender->setBackgroundColor("#FFFFFFFF");
  // color definitions
  pColorDefinition=pGlobalRender->createColorDefinition();
  pColorDefinition->setId("lightBlue");
  pColorDefinition->setColorValue("#ADD8E6");
  pColorDefinition=pGlobalRender->createColorDefinition();
  pColorDefinition->setId("white");
  pColorDefinition->setColorValue("#FFFFFF");
  pColorDefinition=pGlobalRender->createColorDefinition();
  pColorDefinition->setId("black");
  pColorDefinition->setColorValue("#000000");
  pColorDefinition=pGlobalRender->createColorDefinition();
  pColorDefinition->setId("red");
  pColorDefinition->setColorValue("#FF0000");
  pColorDefinition=pGlobalRender->createColorDefinition();
  pColorDefinition->setId("green");
  pColorDefinition->setColorValue("#00FF00");
  pColorDefinition=pGlobalRender->createColorDefinition();
  pColorDefinition->setId("blue");
  pColorDefinition->setColorValue("#0000FF");
  pColorDefinition=pGlobalRender->createColorDefinition();
  pColorDefinition->setId("lightYellow");
  pColorDefinition->setColorValue("#FFFFD1");
  pColorDefinition=pGlobalRender->createColorDefinition();
  pColorDefinition->setId("darkGreen");
  pColorDefinition->setColorValue("#002000");
  // gradient definitions
  pRadialGradient=pGlobalRender->createRadialGradientDefinition();
  pRadialGradient->setId("speciesGlyphGradient");
  pStop=pRadialGradient->createGradientStop();
  pStop->setOffset(RelAbsVector(0.0,0.0));
  pStop->setStopColor("white");   
  pStop=pRadialGradient->createGradientStop();
  pStop->setOffset(RelAbsVector(0.0,100.0));
  pStop->setStopColor("lightBlue");
  // line endings
  pLineEnding=pGlobalRender->createLineEnding();
  pLineEnding->setId("simpleHead_black");
  pLineEnding->getBoundingBox()->setPosition(&p);
  pLineEnding->getBoundingBox()->setDimensions(&d);
  pGroup=pLineEnding->getGroup();
  pGroup->setStroke("black");
  pGroup->setStrokeWidth(1.0);
  pGroup->setFillColor("black");
  pPolygon=pGroup->createPolygon();
  pR=pPolygon->createPoint();
  pR->setCoordinates(RelAbsVector(0.0,0.0),RelAbsVector(0.0,0.0)); 
  pR=pPolygon->createPoint();
  pR->setCoordinates(RelAbsVector(10.0,0.0),RelAbsVector(3.0,0.0)); 
  pR=pPolygon->createPoint();
  pR->setCoordinates(RelAbsVector(0.0,0.0),RelAbsVector(6.0,0.0)); 
  pLineEnding=pGlobalRender->createLineEnding();
  pLineEnding->setId("simpleHead_red");
  pLineEnding->getBoundingBox()->setPosition(&p);
  pLineEnding->getBoundingBox()->setDimensions(&d);
  pGroup=pLineEnding->getGroup();
  pGroup->setStroke("red");
  pGroup->setStrokeWidth(1.0);
  pGroup->setFillColor("red");
  pPolygon=pGroup->createPolygon();
  pR=pPolygon->createPoint();
  pR->setCoordinates(RelAbsVector(0.0,0.0),RelAbsVector(0.0,0.0)); 
  pR=pPolygon->createPoint();
  pR->setCoordinates(RelAbsVector(10.0,0.0),RelAbsVector(3.0,0.0)); 
  pR=pPolygon->createPoint();
  pR->setCoordinates(RelAbsVector(0.0,0.0),RelAbsVector(6.0,0.0)); 
  pLineEnding=pGlobalRender->createLineEnding();
  pLineEnding->setId("simpleHead_green");
  pLineEnding->getBoundingBox()->setPosition(&p);
  pLineEnding->getBoundingBox()->setDimensions(&d);
  pGroup=pLineEnding->getGroup();
  pGroup->setStroke("green");
  pGroup->setStrokeWidth(1.0);
  pGroup->setFillColor("green");
  pPolygon=pGroup->createPolygon();
  pR=pPolygon->createPoint();
  pR->setCoordinates(RelAbsVector(0.0,0.0),RelAbsVector(0.0,0.0)); 
  pR=pPolygon->createPoint();
  pR->setCoordinates(RelAbsVector(10.0,0.0),RelAbsVector(3.0,0.0)); 
  pR=pPolygon->createPoint();
  pR->setCoordinates(RelAbsVector(0.0,0.0),RelAbsVector(6.0,0.0)); 
  pLineEnding=pGlobalRender->createLineEnding();
  pLineEnding->setId("simpleHead_blue");
  pLineEnding->getBoundingBox()->setPosition(&p);
  pLineEnding->getBoundingBox()->setDimensions(&d);
  pGroup=pLineEnding->getGroup();
  pGroup->setStroke("blue");
  pGroup->setStrokeWidth(1.0);
  pGroup->setFillColor("blue");
  pPolygon=pGroup->createPolygon();
  pR=pPolygon->createPoint();
  pR->setCoordinates(RelAbsVector(0.0,0.0),RelAbsVector(0.0,0.0)); 
  pR=pPolygon->createPoint();
  pR->setCoordinates(RelAbsVector(10.0,0.0),RelAbsVector(3.0,0.0)); 
  pR=pPolygon->createPoint();
  pR->setCoordinates(RelAbsVector(0.0,0.0),RelAbsVector(6.0,0.0)); 
  
  // styles
  // style for compartment glyphs
  pGlobalStyle=pGlobalRender->createStyle("compartmentGlyphStyle");
  pGlobalStyle->addType("COMPARTMENTGLYPH");
  pGroup=pGlobalStyle->getGroup();
  pGroup->setStroke("darkGreen");
  pGroup->setStrokeWidth(1.0);
  pRectangle=pGroup->createRectangle();
  pRectangle->setCoordinatesAndSize(RelAbsVector(0.0,0.0),RelAbsVector(0.0,0.0),RelAbsVector(0.0,0.0),RelAbsVector(0.0,100.0),RelAbsVector(0.0,100.0));
  pRectangle->setRadiusX(RelAbsVector(0.0,10.0));
  pRectangle->setRadiusY(RelAbsVector(0.0,10.0));
  pRectangle->setFillColor("lightYellow");

  // style for species glyphs
  pGlobalStyle=pGlobalRender->createStyle("speciesGlyphStyle");
  pGlobalStyle->addType("SPECIESGLYPH");
  pGroup=pGlobalStyle->getGroup();
  pGroup->setStroke("black");
  pGroup->setStrokeWidth(1.0);
  pRectangle=pGroup->createRectangle();
  pRectangle->setCoordinatesAndSize(RelAbsVector(0.0,0.0),RelAbsVector(0.0,0.0),RelAbsVector(0.0,0.0),RelAbsVector(0.0,100.0),RelAbsVector(0.0,100.0));
  pRectangle->setRadiusX(RelAbsVector(5.0,0.0));
  pRectangle->setRadiusY(RelAbsVector(0.0,50.0));
  pRectangle->setFillColor("speciesGlyphGradient");
 
  // style for reaction and text glyphs 
  pGlobalStyle=pGlobalRender->createStyle("reactionGlyphStyle");
  pGlobalStyle->addType("REACTIONGLYPH");
  pGlobalStyle->addType("TEXTGLYPH");
  pGroup=pGlobalStyle->getGroup();
  pGroup->setStroke("black");
  pGroup->setStrokeWidth(1.0);
  pGroup->setFontSize(RelAbsVector(12.0,0.0));
  pGroup->setTextAnchor(Text::ANCHOR_MIDDLE);
  
  // style for substrate and product species reference glyphs
  pGlobalStyle=pGlobalRender->createStyle("reactantSpeciesReferenceGlyphStyle");
  pGlobalStyle->addRole("substrate");
  pGlobalStyle->addRole("sidesubstrate");
  pGlobalStyle->addRole("product");
  pGlobalStyle->addRole("sideproduct");
  pGroup=pGlobalStyle->getGroup();
  pGroup->setStroke("#000000");
  pGroup->setStrokeWidth(1.0);
  pGroup->setEndHead("simpleHead_black");
 
  // style for activator species reference glyphs
  pGlobalStyle=pGlobalRender->createStyle("activatorSpeciesReferenceGlyphStyle");
  pGlobalStyle->addRole("activator");
  pGroup=pGlobalStyle->getGroup();
  pGroup->setStroke("green");
  pGroup->setStrokeWidth(1.0);
  pGroup->setEndHead("simpleHead_green");
 
  // style for modifier species reference glyphs
  pGlobalStyle=pGlobalRender->createStyle("modifierSpeciesReferenceGlyphStyle");
  pGlobalStyle->addRole("modifier");
  pGroup=pGlobalStyle->getGroup();
  pGroup->setStroke("blue");
  pGroup->setStrokeWidth(1.0);
  pGroup->setEndHead("simpleHead_blue");

  // style for inhibitor species reference glyphs
  pGlobalStyle=pGlobalRender->createStyle("inhibitorSpeciesReferenceGlyphStyle");
  pGlobalStyle->addRole("inhibitor");
  pGroup=pGlobalStyle->getGroup();
  pGroup->setStroke("red");
  pGroup->setStrokeWidth(1.0);
  pGroup->setEndHead("simpleHead_red");


  // local style that references a global style and redefines some things
  RenderLayoutPlugin* lPlugin = (RenderLayoutPlugin*)layout->getPlugin("render");
  lPlugin->getListOfLocalRenderInformation()->setVersion(2, 1);
  LocalRenderInformation* pLocalRender=lPlugin->createLocalRenderInformation();
  pLocalRender->setId("highlightGlucose");
  pLocalRender->setReferenceRenderInformationId("defaultStyle");
  pLocalRender->setProgramName("Ralph Gauges");
  pLocalRender->setProgramVersion("1.0");
  pLocalRender->setBackgroundColor("#FFFFFFFF");
  // color definitions
  pColorDefinition=pLocalRender->createColorDefinition();
  pColorDefinition->setId("lightRed");
  pColorDefinition->setColorValue("#E6ADD8");
  pColorDefinition=pLocalRender->createColorDefinition();
  pColorDefinition->setId("white");
  pColorDefinition->setColorValue("#FFFFFF");
  // gradient definitions
  pRadialGradient=pLocalRender->createRadialGradientDefinition();
  pRadialGradient->setId("highlightedSpeciesGlyphGradient");
  pStop=pRadialGradient->createGradientStop();
  pStop->setOffset(RelAbsVector(0.0,0.0));
  pStop->setStopColor("white");   
  pStop=pRadialGradient->createGradientStop();
  pStop->setOffset(RelAbsVector(0.0,100.0));
  pStop->setStopColor("lightRed");   

  // style for highligted species glyph
  LocalStyle* pLocalStyle=pLocalRender->createStyle("highlightedGlucose");
  pLocalStyle->addId("SpeciesGlyph_Glucose");
  pGroup=pLocalStyle->getGroup();
  pGroup->setStroke("black");
  pGroup->setStrokeWidth(1.0);
  pRectangle=pGroup->createRectangle();
  pRectangle->setCoordinatesAndSize(RelAbsVector(0.0,0.0),RelAbsVector(0.0,0.0),RelAbsVector(0.0,0.0),RelAbsVector(0.0,100.0),RelAbsVector(0.0,100.0));
  pRectangle->setRadiusX(RelAbsVector(5.0,0.0));
  pRectangle->setRadiusY(RelAbsVector(0.0,50.0));
  pRectangle->setFillColor("highlightedSpeciesGlyphGradient");
   
  SBMLWriter writer;
    
  //bool result=writeSBML(document,"example6.xml");
  char* writtenContent=writer.writeToString(document);

  XMLInputStream stream2(writtenContent,false);
  XMLNode node2(stream2);
  
  const XMLNode* listOfLayouts1,*listOfLayouts2;
  listOfLayouts1=&node;
  fail_unless(listOfLayouts1->getName()=="sbml");
  unsigned int i,iMax=listOfLayouts1->getNumChildren();
  for(i=0;i<iMax;++i)
  {
    if(listOfLayouts1->getChild(i).getName()=="model")
    {
        listOfLayouts1=&listOfLayouts1->getChild(i);
        break;
    }
  }
  fail_unless(listOfLayouts1->getName()=="model");
  iMax=listOfLayouts1->getNumChildren();
  for(i=0;i<iMax;++i)
  {
    if(listOfLayouts1->getChild(i).getName()=="annotation")
    {
        listOfLayouts1=&listOfLayouts1->getChild(i);
        break;
    }
  }
  fail_unless(listOfLayouts1->getName()=="annotation");
  iMax=listOfLayouts1->getNumChildren();
  for(i=0;i<iMax;++i)
  {
    if(listOfLayouts1->getChild(i).getName()=="listOfLayouts")
    {
        listOfLayouts1=&listOfLayouts1->getChild(i);
        break;
    }
  }
  fail_unless(listOfLayouts1->getName()=="listOfLayouts");
  
  listOfLayouts2=&node2;
  fail_unless(listOfLayouts2->getName()=="sbml");
  iMax=listOfLayouts2->getNumChildren();
  for(i=0;i<iMax;++i)
  {
    if(listOfLayouts2->getChild(i).getName()=="model")
    {
        listOfLayouts2=&listOfLayouts2->getChild(i);
        break;
    }
  }
  fail_unless(listOfLayouts2->getName()=="model");
  iMax=listOfLayouts2->getNumChildren();
  for(i=0;i<iMax;++i)
  {
    if(listOfLayouts2->getChild(i).getName()=="annotation")
    {
        listOfLayouts2=&listOfLayouts2->getChild(i);
        break;
    }
  }
  fail_unless(listOfLayouts2->getName()=="annotation");
  iMax=listOfLayouts2->getNumChildren();
  for(i=0;i<iMax;++i)
  {
    if(listOfLayouts2->getChild(i).getName()=="listOfLayouts")
    {
        listOfLayouts2=&listOfLayouts2->getChild(i);
        break;
    }
  }
  fail_unless(listOfLayouts2->getName()=="listOfLayouts");

  std::string l1 = listOfLayouts1->toXMLString();
  std::string l2 = listOfLayouts2->toXMLString();
  //fail_unless(l1 == l2);
  
  // until the sbml element gets a namespace, we only compare the listOfLayouts element and all its children.
  fail_unless(listOfLayouts1->equals(*listOfLayouts2));

  free(writtenContent);
  delete document;
}
END_TEST 

START_TEST (test_RenderWriting_write_L3_model_1)
{
  const char* s = \
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
"<sbml xmlns=\"http://www.sbml.org/sbml/level3/version1/core\" xmlns:layout=\"http://www.sbml.org/sbml/level3/version1/layout/version1\" xmlns:render=\"http://www.sbml.org/sbml/level3/version1/render/version1\" level=\"3\" version=\"1\" layout:required=\"false\" render:required=\"false\">"
"  <model id=\"TestModel\">"
"    <listOfCompartments>"
"      <compartment id=\"Hepatocyte\" name=\"Hepatocyte\"/>"
"      <compartment id=\"Mito_1\" name=\"Mito 1\"/>"
"      <compartment id=\"Mito_2\" name=\"Mito 2\"/>"
"      <compartment id=\"Mito_3\" name=\"Mito 3\"/>"
"    </listOfCompartments>"
"    <listOfSpecies>"
"      <species id=\"malate_cyt\" name=\"Malate\" compartment=\"Hepatocyte\"/>"
"      <species id=\"malate_mito1\" name=\"Malate\" compartment=\"Mito_1\"/>"
"      <species id=\"malate_mito2\" name=\"Malate\" compartment=\"Mito_2\"/>"
"      <species id=\"malate_mito3\" name=\"Malate\" compartment=\"Mito_3\"/>"
"      <species id=\"oxaloacetate_cyt\" name=\"Oxaloacetate\" compartment=\"Hepatocyte\"/>"
"      <species id=\"oxaloacetate_mito1\" name=\"Oxaloacetate\" compartment=\"Mito_1\"/>"
"      <species id=\"oxaloacetate_mito2\" name=\"Oxaloacetate\" compartment=\"Mito_2\"/>"
"      <species id=\"oxaloacetate_mito3\" name=\"Oxaloacetate\" compartment=\"Mito_3\"/>"
"      <species id=\"aspartate_cyt\" name=\"Aspartate\" compartment=\"Hepatocyte\"/>"
"      <species id=\"aspartate_mito1\" name=\"Aspartate\" compartment=\"Mito_1\"/>"
"      <species id=\"aspartate_mito2\" name=\"Aspartate\" compartment=\"Mito_2\"/>"
"      <species id=\"aspartate_mito3\" name=\"Aspartate\" compartment=\"Mito_3\"/>"
"      <species id=\"glutamate_cyt\" name=\"Glutamate\" compartment=\"Hepatocyte\"/>"
"      <species id=\"glutamate_mito1\" name=\"Glutamate\" compartment=\"Mito_1\"/>"
"      <species id=\"glutamate_mito2\" name=\"Glutamate\" compartment=\"Mito_2\"/>"
"      <species id=\"glutamate_mito3\" name=\"Glutamate\" compartment=\"Mito_3\"/>"
"      <species id=\"aKetoglutarate_cyt\" name=\"alpha-Ketoglutarate\" compartment=\"Hepatocyte\"/>"
"      <species id=\"aKetoglutarate_mito1\" name=\"alpha-Ketoglutarate\" compartment=\"Mito_1\"/>"
"      <species id=\"aKetoglutarate_mito2\" name=\"alpha-Ketoglutarate\" compartment=\"Mito_2\"/>"
"      <species id=\"aKetoglutarate_mito3\" name=\"alpha-Ketoglutarate\" compartment=\"Mito_3\"/>"
"      <species id=\"h_cyt\" name=\"H+\" compartment=\"Hepatocyte\"/>"
"      <species id=\"h_mito1\" name=\"H+\" compartment=\"Mito_1\"/>"
"      <species id=\"h_mito2\" name=\"H+\" compartment=\"Mito_2\"/>"
"      <species id=\"h_mito3\" name=\"H+\" compartment=\"Mito_3\"/>"
"      <species id=\"nad_cyt\" name=\"NAD+\" compartment=\"Hepatocyte\"/>"
"      <species id=\"nad_mito1\" name=\"NAD+\" compartment=\"Mito_1\"/>"
"      <species id=\"nad_mito2\" name=\"NAD+\" compartment=\"Mito_2\"/>"
"      <species id=\"nad_mito3\" name=\"NAD+\" compartment=\"Mito_3\"/>"
"      <species id=\"nadh_cyt\" name=\"NADH\" compartment=\"Hepatocyte\"/>"
"      <species id=\"nadh_mito1\" name=\"NADH\" compartment=\"Mito_1\"/>"
"      <species id=\"nadh_mito2\" name=\"NADH\" compartment=\"Mito_2\"/>"
"      <species id=\"nadh_mito3\" name=\"NADH\" compartment=\"Mito_3\"/>"
"    </listOfSpecies>"
"    <listOfReactions>"
"      <reaction id=\"reaction_malatedh_cyt\" name=\"malate dehydrogenase\" reversible=\"false\">"
"        <listOfReactants>"
"          <speciesReference id=\"sr_malate_cyt\" species=\"malate_cyt\"/>"
"          <speciesReference id=\"sr_nad_cyt\" species=\"nad_cyt\"/>"
"        </listOfReactants>"
"        <listOfProducts>"
"          <speciesReference id=\"sr_nadh_cyt\" species=\"nadh_cyt\"/>"
"          <speciesReference id=\"sr_h_cyt\" species=\"h_cyt\"/>"
"          <speciesReference id=\"sr_oxaloacetate_cyt_1\" species=\"oxaloacetate_cyt\"/>"
"        </listOfProducts>"
"      </reaction>"
"      <reaction id=\"reaction_aspartateat_cyt\" name=\"aspartate aminotransferase\" reversible=\"false\">"
"        <listOfReactants>"
"          <speciesReference id=\"sr_oxaloacetate_cyt_2\" species=\"oxaloacetate_cyt\"/>"
"          <speciesReference id=\"sr_glutamate_cyt_1\" species=\"glutamate_cyt\"/>"
"        </listOfReactants>"
"        <listOfProducts>"
"          <speciesReference id=\"sr_aspartate_cyt_1\" species=\"aspartate_cyt\"/>"
"          <speciesReference id=\"sr_aKetoglutarate_cyt_1\" species=\"aKetoglutarate_cyt\"/>"
"        </listOfProducts>"
"      </reaction>"
"      <reaction id=\"reaction_malatedh_mito1\" name=\"malate dehydrogenase\" reversible=\"false\">"
"        <listOfReactants>"
"          <speciesReference id=\"sr_malate_mito1\" species=\"malate_mito1\"/>"
"          <speciesReference id=\"sr_nad_mito1\" species=\"nad_mito1\"/>"
"        </listOfReactants>"
"        <listOfProducts>"
"          <speciesReference id=\"sr_nadh_mito1\" species=\"nadh_mito1\"/>"
"          <speciesReference id=\"sr_h_mito1\" species=\"h_mito1\"/>"
"          <speciesReference id=\"sr_oxaloacetate_mito1_1\" species=\"oxaloacetate_mito1\"/>"
"        </listOfProducts>"
"      </reaction>"
"      <reaction id=\"reaction_aspartateat_mito1\" name=\"aspartate aminotransferase\" reversible=\"false\">"
"        <listOfReactants>"
"          <speciesReference id=\"sr_oxaloacetate_mito1_2\" species=\"oxaloacetate_mito1\"/>"
"          <speciesReference id=\"sr_glutamate_mito1\" species=\"glutamate_mito1\"/>"
"        </listOfReactants>"
"        <listOfProducts>"
"          <speciesReference id=\"sr_aspartate_mito1\" species=\"aspartate_mito1\"/>"
"          <speciesReference id=\"sr_aKetoglutarate_mito1\" species=\"aKetoglutarate_mito1\"/>"
"        </listOfProducts>"
"      </reaction>"
"      <reaction id=\"reaction_malatedh_mito2\" name=\"malate dehydrogenase\" reversible=\"false\">"
"        <listOfReactants>"
"          <speciesReference id=\"sr_malate_mito2\" species=\"malate_mito2\"/>"
"          <speciesReference id=\"sr_nad_mito2\" species=\"nad_mito2\"/>"
"        </listOfReactants>"
"        <listOfProducts>"
"          <speciesReference id=\"sr_nadh_mito2\" species=\"nadh_mito2\"/>"
"          <speciesReference id=\"sr_h_mito2\" species=\"h_mito2\"/>"
"          <speciesReference id=\"sr_oxaloacetate_mito2_1\" species=\"oxaloacetate_mito2\"/>"
"        </listOfProducts>"
"      </reaction>"
"      <reaction id=\"reaction_aspartateat_mito2\" name=\"aspartate aminotransferase\" reversible=\"false\">"
"        <listOfReactants>"
"          <speciesReference id=\"sr_oxaloacetate_mito2_2\" species=\"oxaloacetate_mito2\"/>"
"          <speciesReference id=\"sr_glutamate_mito2\" species=\"glutamate_mito2\"/>"
"        </listOfReactants>"
"        <listOfProducts>"
"          <speciesReference id=\"sr_aspartate_mito2\" species=\"aspartate_mito2\"/>"
"          <speciesReference id=\"sr_aKetoglutarate_mito2\" species=\"aKetoglutarate_mito2\"/>"
"        </listOfProducts>"
"      </reaction>"
"      <reaction id=\"reaction_malatedh_mito3\" name=\"malate dehydrogenase\" reversible=\"false\">"
"        <listOfReactants>"
"          <speciesReference id=\"sr_malate_mito3\" species=\"malate_mito3\"/>"
"          <speciesReference id=\"sr_nad_mito3\" species=\"nad_mito3\"/>"
"        </listOfReactants>"
"        <listOfProducts>"
"          <speciesReference id=\"sr_nadh_mito3\" species=\"nadh_mito3\"/>"
"          <speciesReference id=\"sr_h_mito3\" species=\"h_mito3\"/>"
"          <speciesReference id=\"sr_oxaloacetate_mito3_1\" species=\"oxaloacetate_mito3\"/>"
"        </listOfProducts>"
"      </reaction>"
"      <reaction id=\"reaction_aspartateat_mito3\" name=\"aspartate aminotransferase\" reversible=\"false\">"
"        <listOfReactants>"
"          <speciesReference id=\"sr_oxaloacetate_mito3_2\" species=\"oxaloacetate_mito3\"/>"
"          <speciesReference id=\"sr_glutamate_mito3_1\" species=\"glutamate_mito3\"/>"
"        </listOfReactants>"
"        <listOfProducts>"
"          <speciesReference id=\"sr_aspartate_mito3_1\" species=\"aspartate_mito3\"/>"
"          <speciesReference id=\"sr_aKetoglutarate_mito3_1\" species=\"aKetoglutarate_mito3\"/>"
"        </listOfProducts>"
"      </reaction>"
"      <reaction id=\"aspartateCarrier\" name=\"aspartate carrier\" reversible=\"true\">"
"        <listOfReactants>"
"          <speciesReference id=\"sr_glutamate_mito3_2\" species=\"glutamate_mito3\"/>"
"          <speciesReference id=\"sr_aspartate_cyt_2\" species=\"aspartate_cyt\"/>"
"        </listOfReactants>"
"        <listOfProducts>"
"          <speciesReference id=\"sr_glutamate_cyt_2\" species=\"glutamate_cyt\"/>"
"          <speciesReference id=\"sr_aspartate_mito3_2\" species=\"aspartate_mito3\"/>"
"        </listOfProducts>"
"      </reaction>"
"      <reaction id=\"malateCarrier\" name=\"malate carrier\" reversible=\"true\">"
"        <listOfReactants>"
"          <speciesReference id=\"sr_aKetoglutarate_mito3_2\" species=\"aKetoglutarate_mito3\"/>"
"          <speciesReference id=\"sr_malate_cyt_2\" species=\"malate_cyt\"/>"
"        </listOfReactants>"
"        <listOfProducts>"
"          <speciesReference id=\"sr_aKetoglutarate_cyt_2\" species=\"aKetoglutarate_cyt\"/>"
"          <speciesReference id=\"sr_malate_mito3_2\" species=\"malate_mito3\"/>"
"        </listOfProducts>"
"      </reaction>"
"    </listOfReactions>"
"    <layout:listOfLayouts xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">"
"      <layout:layout layout:id=\"Layout_1\">"
"        <layout:dimensions layout:width=\"2320\" layout:height=\"1000\"/>"
"        <layout:listOfCompartmentGlyphs>"
"          <layout:compartmentGlyph layout:id=\"CompartmentGlyph_1\" layout:compartment=\"Hepatocyte\">"
"            <layout:boundingBox layout:id=\"bb_compartment\">"
"              <layout:position layout:x=\"10\" layout:y=\"10\"/>"
"              <layout:dimensions layout:width=\"2300\" layout:height=\"980\"/>"
"            </layout:boundingBox>"
"          </layout:compartmentGlyph>"
"          <layout:compartmentGlyph layout:id=\"Mito1_Glyph\" layout:compartment=\"Mito_1\">"
"            <layout:boundingBox layout:id=\"bb_mito1\">"
"              <layout:position layout:x=\"100\" layout:y=\"100\"/>"
"              <layout:dimensions layout:width=\"300\" layout:height=\"100\"/>"
"            </layout:boundingBox>"
"          </layout:compartmentGlyph>"
"          <layout:compartmentGlyph layout:id=\"Mito2_Glyph\" layout:compartment=\"Mito_2\">"
"            <layout:boundingBox layout:id=\"bb_mito2\">"
"              <layout:position layout:x=\"200\" layout:y=\"650\"/>"
"              <layout:dimensions layout:width=\"300\" layout:height=\"100\"/>"
"            </layout:boundingBox>"
"          </layout:compartmentGlyph>"
"          <layout:compartmentGlyph layout:id=\"Mito3_Glyph_2\" layout:compartment=\"Mito_3\">"
"            <layout:boundingBox layout:id=\"bb_mito3_2\">"
"              <layout:position layout:x=\"1470\" layout:y=\"30\"/>"
"              <layout:dimensions layout:width=\"820\" layout:height=\"536\"/>"
"            </layout:boundingBox>"
"          </layout:compartmentGlyph>"
"        </layout:listOfCompartmentGlyphs>"
"        <layout:listOfSpeciesGlyphs>"
"          <layout:speciesGlyph layout:id=\"SpeciesGlyph_malate_cyt\" layout:species=\"malate_cyt\">"
"            <layout:boundingBox layout:id=\"bb_sg_malate_cyt\">"
"              <layout:position layout:x=\"580\" layout:y=\"280\"/>"
"              <layout:dimensions layout:width=\"240\" layout:height=\"36\"/>"
"            </layout:boundingBox>"
"          </layout:speciesGlyph>"
"          <layout:speciesGlyph layout:id=\"SpeciesGlyph_oxaloacetate_cyt\" layout:species=\"oxaloacetate_cyt\">"
"            <layout:boundingBox layout:id=\"bb_sg_oxaloacetate_cyt\">"
"              <layout:position layout:x=\"580\" layout:y=\"480\"/>"
"              <layout:dimensions layout:width=\"240\" layout:height=\"36\"/>"
"            </layout:boundingBox>"
"          </layout:speciesGlyph>"
"          <layout:speciesGlyph layout:id=\"SpeciesGlyph_aspartate_cyt\" layout:species=\"aspartate_cyt\">"
"            <layout:boundingBox layout:id=\"bb_sg_aspartate_cyt\">"
"              <layout:position layout:x=\"580\" layout:y=\"680\"/>"
"              <layout:dimensions layout:width=\"240\" layout:height=\"36\"/>"
"            </layout:boundingBox>"
"          </layout:speciesGlyph>"
"          <layout:speciesGlyph layout:id=\"SpeciesGlyph_glutamate_cyt\" layout:species=\"glutamate_cyt\">"
"            <layout:boundingBox layout:id=\"bb_sg_glutamate_cyt\">"
"              <layout:position layout:x=\"800\" layout:y=\"610\"/>"
"              <layout:dimensions layout:width=\"240\" layout:height=\"36\"/>"
"            </layout:boundingBox>"
"          </layout:speciesGlyph>"
"          <layout:speciesGlyph layout:id=\"SpeciesGlyph_aKetoglutarate_cyt\" layout:species=\"aKetoglutarate_cyt\">"
"            <layout:boundingBox layout:id=\"bb_sg_aKetoglutarate_cyt\">"
"              <layout:position layout:x=\"860\" layout:y=\"500\"/>"
"              <layout:dimensions layout:width=\"280\" layout:height=\"36\"/>"
"            </layout:boundingBox>"
"          </layout:speciesGlyph>"
"          <layout:speciesGlyph layout:id=\"SpeciesGlyph_nad_cyt\" layout:species=\"nad_cyt\">"
"            <layout:boundingBox layout:id=\"bb_sg_nad_cyt\">"
"              <layout:position layout:x=\"520\" layout:y=\"350\"/>"
"              <layout:dimensions layout:width=\"100\" layout:height=\"24\"/>"
"            </layout:boundingBox>"
"          </layout:speciesGlyph>"
"          <layout:speciesGlyph layout:id=\"SpeciesGlyph_nadh_cyt\" layout:species=\"nadh_cyt\">"
"            <layout:boundingBox layout:id=\"bb_sg_nadh_cyt\">"
"              <layout:position layout:x=\"520\" layout:y=\"430\"/>"
"              <layout:dimensions layout:width=\"100\" layout:height=\"24\"/>"
"            </layout:boundingBox>"
"          </layout:speciesGlyph>"
"          <layout:speciesGlyph layout:id=\"SpeciesGlyph_h_cyt\" layout:species=\"h_cyt\">"
"            <layout:boundingBox layout:id=\"bb_sg_h_cyt\">"
"              <layout:position layout:x=\"430\" layout:y=\"430\"/>"
"              <layout:dimensions layout:width=\"40\" layout:height=\"24\"/>"
"            </layout:boundingBox>"
"          </layout:speciesGlyph>"
"          <layout:speciesGlyph layout:id=\"SpeciesGlyph_malate_mito3\" layout:species=\"malate_mito3\">"
"            <layout:boundingBox layout:id=\"bb_sg_malate_mito3\">"
"              <layout:position layout:x=\"1850\" layout:y=\"80\"/>"
"              <layout:dimensions layout:width=\"240\" layout:height=\"36\"/>"
"            </layout:boundingBox>"
"          </layout:speciesGlyph>"
"          <layout:speciesGlyph layout:id=\"SpeciesGlyph_oxaloacetate_mito3\" layout:species=\"oxaloacetate_mito3\">"
"            <layout:boundingBox layout:id=\"bb_sg_oxaloacetate_mito3\">"
"              <layout:position layout:x=\"1850\" layout:y=\"280\"/>"
"              <layout:dimensions layout:width=\"240\" layout:height=\"36\"/>"
"            </layout:boundingBox>"
"          </layout:speciesGlyph>"
"          <layout:speciesGlyph layout:id=\"SpeciesGlyph_aspartate_mito3\" layout:species=\"aspartate_mito3\">"
"            <layout:boundingBox layout:id=\"bb_sg_aspartate_mito3\">"
"              <layout:position layout:x=\"1850\" layout:y=\"480\"/>"
"              <layout:dimensions layout:width=\"240\" layout:height=\"36\"/>"
"            </layout:boundingBox>"
"          </layout:speciesGlyph>"
"          <layout:speciesGlyph layout:id=\"SpeciesGlyph_glutamate_mito3\" layout:species=\"glutamate_mito3\">"
"            <layout:boundingBox layout:id=\"bb_sg_glutamate_mito3\">"
"              <layout:position layout:x=\"1550\" layout:y=\"430\"/>"
"              <layout:dimensions layout:width=\"240\" layout:height=\"36\"/>"
"            </layout:boundingBox>"
"          </layout:speciesGlyph>"
"          <layout:speciesGlyph layout:id=\"SpeciesGlyph_aKetoglutarate_mito3\" layout:species=\"aKetoglutarate_mito3\">"
"            <layout:boundingBox layout:id=\"bb_sg_aKetoglutarate_mito3\">"
"              <layout:position layout:x=\"1530\" layout:y=\"300\"/>"
"              <layout:dimensions layout:width=\"280\" layout:height=\"36\"/>"
"            </layout:boundingBox>"
"          </layout:speciesGlyph>"
"          <layout:speciesGlyph layout:id=\"SpeciesGlyph_nad_mito3\" layout:species=\"nad_mito3\">"
"            <layout:boundingBox layout:id=\"bb_sg_nad_mito3\">"
"              <layout:position layout:x=\"2050\" layout:y=\"150\"/>"
"              <layout:dimensions layout:width=\"100\" layout:height=\"24\"/>"
"            </layout:boundingBox>"
"          </layout:speciesGlyph>"
"          <layout:speciesGlyph layout:id=\"SpeciesGlyph_nadh_mito3\" layout:species=\"nadh_mito3\">"
"            <layout:boundingBox layout:id=\"bb_sg_nadh_mito3\">"
"              <layout:position layout:x=\"2050\" layout:y=\"230\"/>"
"              <layout:dimensions layout:width=\"100\" layout:height=\"24\"/>"
"            </layout:boundingBox>"
"          </layout:speciesGlyph>"
"          <layout:speciesGlyph layout:id=\"SpeciesGlyph_h_mito3\" layout:species=\"h_mito3\">"
"            <layout:boundingBox layout:id=\"bb_sg_h_mito3\">"
"              <layout:position layout:x=\"2200\" layout:y=\"230\"/>"
"              <layout:dimensions layout:width=\"40\" layout:height=\"24\"/>"
"            </layout:boundingBox>"
"          </layout:speciesGlyph>"
"        </layout:listOfSpeciesGlyphs>"
"        <layout:listOfReactionGlyphs>"
"          <layout:reactionGlyph layout:id=\"rg_malatedh_cyt\" layout:reaction=\"reaction_malatedh_cyt\">"
"            <layout:curve>"
"              <layout:listOfCurveSegments>"
"                <layout:curveSegment xsi:type=\"LineSegment\">"
"                  <layout:start layout:x=\"700\" layout:y=\"381\"/>"
"                  <layout:end layout:x=\"700\" layout:y=\"415\"/>"
"                </layout:curveSegment>"
"              </layout:listOfCurveSegments>"
"            </layout:curve>"
"            <layout:listOfSpeciesReferenceGlyphs>"
"              <layout:speciesReferenceGlyph layout:id=\"srg_malate_cyt_1\" layout:speciesReference=\"sr_malate_cyt\" layout:speciesGlyph=\"SpeciesGlyph_malate_cyt\" layout:role=\"substrate\">"
"                <layout:curve>"
"                  <layout:listOfCurveSegments>"
"                    <layout:curveSegment xsi:type=\"LineSegment\">"
"                      <layout:start layout:x=\"700\" layout:y=\"381\"/>"
"                      <layout:end layout:x=\"700\" layout:y=\"316\"/>"
"                    </layout:curveSegment>"
"                  </layout:listOfCurveSegments>"
"                </layout:curve>"
"              </layout:speciesReferenceGlyph>"
"              <layout:speciesReferenceGlyph layout:id=\"srg_nad_cyt\" layout:speciesReference=\"sr_nad_cyt\" layout:speciesGlyph=\"SpeciesGlyph_nad_cyt\" layout:role=\"substrate\">"
"                <layout:curve>"
"                  <layout:listOfCurveSegments>"
"                    <layout:curveSegment xsi:type=\"CubicBezier\">"
"                      <layout:start layout:x=\"700\" layout:y=\"381\"/>"
"                      <layout:end layout:x=\"620\" layout:y=\"362\"/>"
"                      <layout:basePoint1 layout:x=\"700\" layout:y=\"362\"/>"
"                      <layout:basePoint2 layout:x=\"700\" layout:y=\"362\"/>"
"                    </layout:curveSegment>"
"                  </layout:listOfCurveSegments>"
"                </layout:curve>"
"              </layout:speciesReferenceGlyph>"
"              <layout:speciesReferenceGlyph layout:id=\"srg_oxaloacetate_cyt_1\" layout:speciesReference=\"sr_oxaloacetate_cyt_1\" layout:speciesGlyph=\"SpeciesGlyph_oxaloacetate_cyt\" layout:role=\"product\">"
"                <layout:curve>"
"                  <layout:listOfCurveSegments>"
"                    <layout:curveSegment xsi:type=\"LineSegment\">"
"                      <layout:start layout:x=\"700\" layout:y=\"415\"/>"
"                      <layout:end layout:x=\"700\" layout:y=\"480\"/>"
"                    </layout:curveSegment>"
"                  </layout:listOfCurveSegments>"
"                </layout:curve>"
"              </layout:speciesReferenceGlyph>"
"              <layout:speciesReferenceGlyph layout:id=\"srg_nadh_cyt\" layout:speciesReference=\"sr_nadh_cyt\" layout:speciesGlyph=\"SpeciesGlyph_nadh_cyt\" layout:role=\"product\">"
"                <layout:curve>"
"                  <layout:listOfCurveSegments>"
"                    <layout:curveSegment xsi:type=\"CubicBezier\">"
"                      <layout:start layout:x=\"700\" layout:y=\"415\"/>"
"                      <layout:end layout:x=\"620\" layout:y=\"442\"/>"
"                      <layout:basePoint1 layout:x=\"700\" layout:y=\"442\"/>"
"                      <layout:basePoint2 layout:x=\"700\" layout:y=\"442\"/>"
"                    </layout:curveSegment>"
"                  </layout:listOfCurveSegments>"
"                </layout:curve>"
"              </layout:speciesReferenceGlyph>"
"              <layout:speciesReferenceGlyph layout:id=\"srg_h_cyt\" layout:speciesReference=\"sr_h_cyt\" layout:speciesGlyph=\"SpeciesGlyph_h_cyt\" layout:role=\"product\">"
"                <layout:curve>"
"                  <layout:listOfCurveSegments>"
"                    <layout:curveSegment xsi:type=\"CubicBezier\">"
"                      <layout:start layout:x=\"700\" layout:y=\"415\"/>"
"                      <layout:end layout:x=\"470\" layout:y=\"430\"/>"
"                      <layout:basePoint1 layout:x=\"570\" layout:y=\"415\"/>"
"                      <layout:basePoint2 layout:x=\"570\" layout:y=\"415\"/>"
"                    </layout:curveSegment>"
"                  </layout:listOfCurveSegments>"
"                </layout:curve>"
"              </layout:speciesReferenceGlyph>"
"            </layout:listOfSpeciesReferenceGlyphs>"
"          </layout:reactionGlyph>"
"          <layout:reactionGlyph layout:id=\"rg_aspartateat_cyt\" layout:reaction=\"reaction_aspartateat_cyt\">"
"            <layout:curve>"
"              <layout:listOfCurveSegments>"
"                <layout:curveSegment xsi:type=\"LineSegment\">"
"                  <layout:start layout:x=\"700\" layout:y=\"581\"/>"
"                  <layout:end layout:x=\"700\" layout:y=\"615\"/>"
"                </layout:curveSegment>"
"              </layout:listOfCurveSegments>"
"            </layout:curve>"
"            <layout:listOfSpeciesReferenceGlyphs>"
"              <layout:speciesReferenceGlyph layout:id=\"srg_oxaloacetate_cyt_2\" layout:speciesReference=\"sr_oxaloacetate_cyt_2\" layout:speciesGlyph=\"SpeciesGlyph_oxaloacetate_cyt\" layout:role=\"substrate\">"
"                <layout:curve>"
"                  <layout:listOfCurveSegments>"
"                    <layout:curveSegment xsi:type=\"LineSegment\">"
"                      <layout:start layout:x=\"700\" layout:y=\"581\"/>"
"                      <layout:end layout:x=\"700\" layout:y=\"516\"/>"
"                    </layout:curveSegment>"
"                  </layout:listOfCurveSegments>"
"                </layout:curve>"
"              </layout:speciesReferenceGlyph>"
"              <layout:speciesReferenceGlyph layout:id=\"srg_glutamate_cyt_1\" layout:speciesReference=\"sr_glutamate_cyt_1\" layout:speciesGlyph=\"SpeciesGlyph_glutamate_cyt\" layout:role=\"substrate\">"
"                <layout:curve>"
"                  <layout:listOfCurveSegments>"
"                    <layout:curveSegment xsi:type=\"CubicBezier\">"
"                      <layout:start layout:x=\"700\" layout:y=\"581\"/>"
"                      <layout:end layout:x=\"800\" layout:y=\"628\"/>"
"                      <layout:basePoint1 layout:x=\"750\" layout:y=\"581\"/>"
"                      <layout:basePoint2 layout:x=\"750\" layout:y=\"628\"/>"
"                    </layout:curveSegment>"
"                  </layout:listOfCurveSegments>"
"                </layout:curve>"
"              </layout:speciesReferenceGlyph>"
"              <layout:speciesReferenceGlyph layout:id=\"srg_aspartate_cyt_1\" layout:speciesReference=\"sr_aspartate_cyt_1\" layout:speciesGlyph=\"SpeciesGlyph_aspartate_cyt\" layout:role=\"product\">"
"                <layout:curve>"
"                  <layout:listOfCurveSegments>"
"                    <layout:curveSegment xsi:type=\"LineSegment\">"
"                      <layout:start layout:x=\"700\" layout:y=\"615\"/>"
"                      <layout:end layout:x=\"700\" layout:y=\"680\"/>"
"                    </layout:curveSegment>"
"                  </layout:listOfCurveSegments>"
"                </layout:curve>"
"              </layout:speciesReferenceGlyph>"
"              <layout:speciesReferenceGlyph layout:id=\"srg_aKetoglutaratecyt_1\" layout:speciesReference=\"sr_aKetoglutarate_cyt_1\" layout:speciesGlyph=\"SpeciesGlyph_aKetoglutarate_cyt\" layout:role=\"product\">"
"                <layout:curve>"
"                  <layout:listOfCurveSegments>"
"                    <layout:curveSegment xsi:type=\"CubicBezier\">"
"                      <layout:start layout:x=\"700\" layout:y=\"615\"/>"
"                      <layout:end layout:x=\"860\" layout:y=\"515\"/>"
"                      <layout:basePoint1 layout:x=\"790\" layout:y=\"615\"/>"
"                      <layout:basePoint2 layout:x=\"790\" layout:y=\"515\"/>"
"                    </layout:curveSegment>"
"                  </layout:listOfCurveSegments>"
"                </layout:curve>"
"              </layout:speciesReferenceGlyph>"
"            </layout:listOfSpeciesReferenceGlyphs>"
"          </layout:reactionGlyph>"
"          <layout:reactionGlyph layout:id=\"rg_malatedh_mito3\" layout:reaction=\"reaction_malatedh_mito3\">"
"            <layout:curve>"
"              <layout:listOfCurveSegments>"
"                <layout:curveSegment xsi:type=\"LineSegment\">"
"                  <layout:start layout:x=\"1970\" layout:y=\"181\"/>"
"                  <layout:end layout:x=\"1970\" layout:y=\"215\"/>"
"                </layout:curveSegment>"
"              </layout:listOfCurveSegments>"
"            </layout:curve>"
"            <layout:listOfSpeciesReferenceGlyphs>"
"              <layout:speciesReferenceGlyph layout:id=\"srg_malate_mito3_1\" layout:speciesReference=\"sr_malate_mito3\" layout:speciesGlyph=\"SpeciesGlyph_malate_mito3\" layout:role=\"substrate\">"
"                <layout:curve>"
"                  <layout:listOfCurveSegments>"
"                    <layout:curveSegment xsi:type=\"LineSegment\">"
"                      <layout:start layout:x=\"1970\" layout:y=\"181\"/>"
"                      <layout:end layout:x=\"1970\" layout:y=\"116\"/>"
"                    </layout:curveSegment>"
"                  </layout:listOfCurveSegments>"
"                </layout:curve>"
"              </layout:speciesReferenceGlyph>"
"              <layout:speciesReferenceGlyph layout:id=\"srg_nad_mito3\" layout:speciesReference=\"sr_nad_mito3\" layout:speciesGlyph=\"SpeciesGlyph_nad_mito3\" layout:role=\"substrate\">"
"                <layout:curve>"
"                  <layout:listOfCurveSegments>"
"                    <layout:curveSegment xsi:type=\"CubicBezier\">"
"                      <layout:start layout:x=\"1970\" layout:y=\"181\"/>"
"                      <layout:end layout:x=\"2050\" layout:y=\"162\"/>"
"                      <layout:basePoint1 layout:x=\"1970\" layout:y=\"162\"/>"
"                      <layout:basePoint2 layout:x=\"1970\" layout:y=\"162\"/>"
"                    </layout:curveSegment>"
"                  </layout:listOfCurveSegments>"
"                </layout:curve>"
"              </layout:speciesReferenceGlyph>"
"              <layout:speciesReferenceGlyph layout:id=\"srg_oxaloacetate_mito3_1\" layout:speciesReference=\"sr_oxaloacetate_mito3_1\" layout:speciesGlyph=\"SpeciesGlyph_oxaloacetate_mito3\" layout:role=\"product\">"
"                <layout:curve>"
"                  <layout:listOfCurveSegments>"
"                    <layout:curveSegment xsi:type=\"LineSegment\">"
"                      <layout:start layout:x=\"1970\" layout:y=\"215\"/>"
"                      <layout:end layout:x=\"1970\" layout:y=\"280\"/>"
"                    </layout:curveSegment>"
"                  </layout:listOfCurveSegments>"
"                </layout:curve>"
"              </layout:speciesReferenceGlyph>"
"              <layout:speciesReferenceGlyph layout:id=\"srg_nadh_mito3\" layout:speciesReference=\"sr_nadh_mito3\" layout:speciesGlyph=\"SpeciesGlyph_nadh_mito3\" layout:role=\"product\">"
"                <layout:curve>"
"                  <layout:listOfCurveSegments>"
"                    <layout:curveSegment xsi:type=\"CubicBezier\">"
"                      <layout:start layout:x=\"1970\" layout:y=\"215\"/>"
"                      <layout:end layout:x=\"2050\" layout:y=\"242\"/>"
"                      <layout:basePoint1 layout:x=\"1970\" layout:y=\"242\"/>"
"                      <layout:basePoint2 layout:x=\"1970\" layout:y=\"242\"/>"
"                    </layout:curveSegment>"
"                  </layout:listOfCurveSegments>"
"                </layout:curve>"
"              </layout:speciesReferenceGlyph>"
"              <layout:speciesReferenceGlyph layout:id=\"srg_h_mito3\" layout:speciesReference=\"sr_h_mito3\" layout:speciesGlyph=\"SpeciesGlyph_h_mito3\" layout:role=\"product\">"
"                <layout:curve>"
"                  <layout:listOfCurveSegments>"
"                    <layout:curveSegment xsi:type=\"CubicBezier\">"
"                      <layout:start layout:x=\"1970\" layout:y=\"215\"/>"
"                      <layout:end layout:x=\"2200\" layout:y=\"230\"/>"
"                      <layout:basePoint1 layout:x=\"2100\" layout:y=\"215\"/>"
"                      <layout:basePoint2 layout:x=\"2100\" layout:y=\"215\"/>"
"                    </layout:curveSegment>"
"                  </layout:listOfCurveSegments>"
"                </layout:curve>"
"              </layout:speciesReferenceGlyph>"
"            </layout:listOfSpeciesReferenceGlyphs>"
"          </layout:reactionGlyph>"
"          <layout:reactionGlyph layout:id=\"rg_aspartateat_mito3\" layout:reaction=\"reaction_aspartateat_mito3\">"
"            <layout:curve>"
"              <layout:listOfCurveSegments>"
"                <layout:curveSegment xsi:type=\"LineSegment\">"
"                  <layout:start layout:x=\"1970\" layout:y=\"381\"/>"
"                  <layout:end layout:x=\"1970\" layout:y=\"415\"/>"
"                </layout:curveSegment>"
"              </layout:listOfCurveSegments>"
"            </layout:curve>"
"            <layout:listOfSpeciesReferenceGlyphs>"
"              <layout:speciesReferenceGlyph layout:id=\"srg_oxaloacetate_mito3_2\" layout:speciesReference=\"sr_oxaloacetate_mito3_2\" layout:speciesGlyph=\"SpeciesGlyph_oxaloacetate_mito3\" layout:role=\"substrate\">"
"                <layout:curve>"
"                  <layout:listOfCurveSegments>"
"                    <layout:curveSegment xsi:type=\"LineSegment\">"
"                      <layout:start layout:x=\"1970\" layout:y=\"381\"/>"
"                      <layout:end layout:x=\"1970\" layout:y=\"316\"/>"
"                    </layout:curveSegment>"
"                  </layout:listOfCurveSegments>"
"                </layout:curve>"
"              </layout:speciesReferenceGlyph>"
"              <layout:speciesReferenceGlyph layout:id=\"srg_glutamate_mito3_1\" layout:speciesReference=\"sr_glutamate_mito3_1\" layout:speciesGlyph=\"SpeciesGlyph_glutamate_mito3\" layout:role=\"substrate\">"
"                <layout:curve>"
"                  <layout:listOfCurveSegments>"
"                    <layout:curveSegment xsi:type=\"CubicBezier\">"
"                      <layout:start layout:x=\"1970\" layout:y=\"381\"/>"
"                      <layout:end layout:x=\"1790\" layout:y=\"448\"/>"
"                      <layout:basePoint1 layout:x=\"1880\" layout:y=\"381\"/>"
"                      <layout:basePoint2 layout:x=\"1880\" layout:y=\"448\"/>"
"                    </layout:curveSegment>"
"                  </layout:listOfCurveSegments>"
"                </layout:curve>"
"              </layout:speciesReferenceGlyph>"
"              <layout:speciesReferenceGlyph layout:id=\"srg_aspartate_mito3_1\" layout:speciesReference=\"sr_aspartate_mito3_1\" layout:speciesGlyph=\"SpeciesGlyph_aspartate_mito3\" layout:role=\"product\">"
"                <layout:curve>"
"                  <layout:listOfCurveSegments>"
"                    <layout:curveSegment xsi:type=\"LineSegment\">"
"                      <layout:start layout:x=\"1970\" layout:y=\"415\"/>"
"                      <layout:end layout:x=\"1970\" layout:y=\"480\"/>"
"                    </layout:curveSegment>"
"                  </layout:listOfCurveSegments>"
"                </layout:curve>"
"              </layout:speciesReferenceGlyph>"
"              <layout:speciesReferenceGlyph layout:id=\"srg_aKetoglutaratemito3_1\" layout:speciesReference=\"sr_aKetoglutarate_mito3_1\" layout:speciesGlyph=\"SpeciesGlyph_aKetoglutarate_mito3\" layout:role=\"product\">"
"                <layout:curve>"
"                  <layout:listOfCurveSegments>"
"                    <layout:curveSegment xsi:type=\"CubicBezier\">"
"                      <layout:start layout:x=\"1970\" layout:y=\"415\"/>"
"                      <layout:end layout:x=\"1810\" layout:y=\"315\"/>"
"                      <layout:basePoint1 layout:x=\"1880\" layout:y=\"415\"/>"
"                      <layout:basePoint2 layout:x=\"1880\" layout:y=\"315\"/>"
"                    </layout:curveSegment>"
"                  </layout:listOfCurveSegments>"
"                </layout:curve>"
"              </layout:speciesReferenceGlyph>"
"            </layout:listOfSpeciesReferenceGlyphs>"
"          </layout:reactionGlyph>"
"          <layout:reactionGlyph layout:id=\"rg_aspartateCarrier\" layout:reaction=\"aspartateCarrier\">"
"            <layout:curve>"
"              <layout:listOfCurveSegments>"
"                <layout:curveSegment xsi:type=\"LineSegment\">"
"                  <layout:start layout:x=\"1420\" layout:y=\"530\"/>"
"                  <layout:end layout:x=\"1360\" layout:y=\"550\"/>"
"                </layout:curveSegment>"
"              </layout:listOfCurveSegments>"
"            </layout:curve>"
"            <layout:listOfSpeciesReferenceGlyphs>"
"              <layout:speciesReferenceGlyph layout:id=\"srg_aspartate_mito3_2\" layout:speciesReference=\"sr_aspartate_mito3_2\" layout:speciesGlyph=\"SpeciesGlyph_aspartate_mito3\" layout:role=\"substrate\">"
"                <layout:curve>"
"                  <layout:listOfCurveSegments>"
"                    <layout:curveSegment xsi:type=\"LineSegment\">"
"                      <layout:start layout:x=\"1420\" layout:y=\"530\"/>"
"                      <layout:end layout:x=\"1850\" layout:y=\"498\"/>"
"                    </layout:curveSegment>"
"                  </layout:listOfCurveSegments>"
"                </layout:curve>"
"              </layout:speciesReferenceGlyph>"
"              <layout:speciesReferenceGlyph layout:id=\"srg_aspartate_cyt_2\" layout:speciesReference=\"sr_aspartate_cyt_2\" layout:speciesGlyph=\"SpeciesGlyph_aspartate_cyt\" layout:role=\"product\">"
"                <layout:curve>"
"                  <layout:listOfCurveSegments>"
"                    <layout:curveSegment xsi:type=\"CubicBezier\">"
"                      <layout:start layout:x=\"1360\" layout:y=\"550\"/>"
"                      <layout:end layout:x=\"820\" layout:y=\"698\"/>"
"                      <layout:basePoint1 layout:x=\"1390\" layout:y=\"698\"/>"
"                      <layout:basePoint2 layout:x=\"1390\" layout:y=\"698\"/>"
"                    </layout:curveSegment>"
"                  </layout:listOfCurveSegments>"
"                </layout:curve>"
"              </layout:speciesReferenceGlyph>"
"              <layout:speciesReferenceGlyph layout:id=\"srg_glutamate_cyt_2\" layout:speciesReference=\"sr_glutamate_cyt_2\" layout:speciesGlyph=\"SpeciesGlyph_glutamate_cyt\" layout:role=\"substrate\">"
"                <layout:curve>"
"                  <layout:listOfCurveSegments>"
"                    <layout:curveSegment xsi:type=\"CubicBezier\">"
"                      <layout:start layout:x=\"1420\" layout:y=\"530\"/>"
"                      <layout:end layout:x=\"1050\" layout:y=\"628\"/>"
"                      <layout:basePoint1 layout:x=\"1390\" layout:y=\"648\"/>"
"                      <layout:basePoint2 layout:x=\"1390\" layout:y=\"648\"/>"
"                    </layout:curveSegment>"
"                  </layout:listOfCurveSegments>"
"                </layout:curve>"
"              </layout:speciesReferenceGlyph>"
"              <layout:speciesReferenceGlyph layout:id=\"srg_glutamate_mito3_2\" layout:speciesReference=\"sr_glutamate_mito3_2\" layout:speciesGlyph=\"SpeciesGlyph_glutamate_mito3\" layout:role=\"product\">"
"                <layout:curve>"
"                  <layout:listOfCurveSegments>"
"                    <layout:curveSegment xsi:type=\"CubicBezier\">"
"                      <layout:start layout:x=\"1360\" layout:y=\"550\"/>"
"                      <layout:end layout:x=\"1550\" layout:y=\"448\"/>"
"                      <layout:basePoint1 layout:x=\"1390\" layout:y=\"448\"/>"
"                      <layout:basePoint2 layout:x=\"1390\" layout:y=\"448\"/>"
"                    </layout:curveSegment>"
"                  </layout:listOfCurveSegments>"
"                </layout:curve>"
"              </layout:speciesReferenceGlyph>"
"            </layout:listOfSpeciesReferenceGlyphs>"
"          </layout:reactionGlyph>"
"          <layout:reactionGlyph layout:id=\"rg_malateCarrier\" layout:reaction=\"malateCarrier\">"
"            <layout:curve>"
"              <layout:listOfCurveSegments>"
"                <layout:curveSegment xsi:type=\"LineSegment\">"
"                  <layout:start layout:x=\"1420\" layout:y=\"320\"/>"
"                  <layout:end layout:x=\"1360\" layout:y=\"340\"/>"
"                </layout:curveSegment>"
"              </layout:listOfCurveSegments>"
"            </layout:curve>"
"            <layout:listOfSpeciesReferenceGlyphs>"
"              <layout:speciesReferenceGlyph layout:id=\"srg_aKetoglutarate_mito3_2\" layout:speciesReference=\"sr_aKetoglutarate_mito3_2\" layout:speciesGlyph=\"SpeciesGlyph_aKetoglutarate_mito3\" layout:role=\"substrate\">"
"                <layout:curve>"
"                  <layout:listOfCurveSegments>"
"                    <layout:curveSegment xsi:type=\"LineSegment\">"
"                      <layout:start layout:x=\"1420\" layout:y=\"320\"/>"
"                      <layout:end layout:x=\"1530\" layout:y=\"318\"/>"
"                    </layout:curveSegment>"
"                  </layout:listOfCurveSegments>"
"                </layout:curve>"
"              </layout:speciesReferenceGlyph>"
"              <layout:speciesReferenceGlyph layout:id=\"srg_aKetoglutarate_cyt_2\" layout:speciesReference=\"sr_aKetoglutarate_cyt_2\" layout:speciesGlyph=\"SpeciesGlyph_aKetoglutarate_cyt\" layout:role=\"product\">"
"                <layout:curve>"
"                  <layout:listOfCurveSegments>"
"                    <layout:curveSegment xsi:type=\"LineSegment\">"
"                      <layout:start layout:x=\"1360\" layout:y=\"340\"/>"
"                      <layout:end layout:x=\"1140\" layout:y=\"518\"/>"
"                    </layout:curveSegment>"
"                  </layout:listOfCurveSegments>"
"                </layout:curve>"
"              </layout:speciesReferenceGlyph>"
"              <layout:speciesReferenceGlyph layout:id=\"srg_malate_cyt_2\" layout:speciesReference=\"sr_malate_cyt_2\" layout:speciesGlyph=\"SpeciesGlyph_malate_cyt\" layout:role=\"substrate\">"
"                <layout:curve>"
"                  <layout:listOfCurveSegments>"
"                    <layout:curveSegment xsi:type=\"CubicBezier\">"
"                      <layout:start layout:x=\"1420\" layout:y=\"320\"/>"
"                      <layout:end layout:x=\"820\" layout:y=\"298\"/>"
"                      <layout:basePoint1 layout:x=\"1390\" layout:y=\"250\"/>"
"                      <layout:basePoint2 layout:x=\"1390\" layout:y=\"250\"/>"
"                    </layout:curveSegment>"
"                  </layout:listOfCurveSegments>"
"                </layout:curve>"
"              </layout:speciesReferenceGlyph>"
"              <layout:speciesReferenceGlyph layout:id=\"srg_malate_mito3_2\" layout:speciesReference=\"sr_malate_mito3_2\" layout:speciesGlyph=\"SpeciesGlyph_malate_mito3\" layout:role=\"product\">"
"                <layout:curve>"
"                  <layout:listOfCurveSegments>"
"                    <layout:curveSegment xsi:type=\"CubicBezier\">"
"                      <layout:start layout:x=\"1360\" layout:y=\"340\"/>"
"                      <layout:end layout:x=\"1850\" layout:y=\"98\"/>"
"                      <layout:basePoint1 layout:x=\"1390\" layout:y=\"150\"/>"
"                      <layout:basePoint2 layout:x=\"1390\" layout:y=\"150\"/>"
"                    </layout:curveSegment>"
"                  </layout:listOfCurveSegments>"
"                </layout:curve>"
"              </layout:speciesReferenceGlyph>"
"            </layout:listOfSpeciesReferenceGlyphs>"
"          </layout:reactionGlyph>"
"        </layout:listOfReactionGlyphs>"
"        <layout:listOfTextGlyphs>"
"          <layout:textGlyph layout:id=\"TextGlyph_Hepatocyte\" layout:originOfText=\"Hepatocyte\" layout:graphicalObject=\"CompartmentGlyph_1\">"
"            <layout:boundingBox layout:id=\"bb_tg_compartment\">"
"              <layout:position layout:x=\"50\" layout:y=\"870\"/>"
"              <layout:dimensions layout:width=\"300\" layout:height=\"72\"/>"
"            </layout:boundingBox>"
"          </layout:textGlyph>"
"          <layout:textGlyph layout:id=\"TextGlyph_mito1\" layout:originOfText=\"Mito_1\" layout:graphicalObject=\"Mito1_Glyph\">"
"            <layout:boundingBox layout:id=\"bb_tg_mito1\">"
"              <layout:position layout:x=\"110\" layout:y=\"110\"/>"
"              <layout:dimensions layout:width=\"280\" layout:height=\"72\"/>"
"            </layout:boundingBox>"
"          </layout:textGlyph>"
"          <layout:textGlyph layout:id=\"TextGlyph_mito2\" layout:originOfText=\"Mito_2\" layout:graphicalObject=\"Mito2_Glyph\">"
"            <layout:boundingBox layout:id=\"bb_tg_mito2\">"
"              <layout:position layout:x=\"210\" layout:y=\"660\"/>"
"              <layout:dimensions layout:width=\"280\" layout:height=\"72\"/>"
"            </layout:boundingBox>"
"          </layout:textGlyph>"
"          <layout:textGlyph layout:id=\"TextGlyph_mito3_2\" layout:originOfText=\"Mito_3\" layout:graphicalObject=\"Mito3_Glyph_2\">"
"            <layout:boundingBox layout:id=\"bb_tg_mito3_2\">"
"              <layout:position layout:x=\"1475\" layout:y=\"35\"/>"
"              <layout:dimensions layout:width=\"200\" layout:height=\"72\"/>"
"            </layout:boundingBox>"
"          </layout:textGlyph>"
"          <layout:textGlyph layout:id=\"TextGlyph_malate_cyt\" layout:originOfText=\"malate_cyt\" layout:graphicalObject=\"SpeciesGlyph_malate_cyt\">"
"            <layout:boundingBox layout:id=\"bb_tg_malatate_cyt\">"
"              <layout:position layout:x=\"590\" layout:y=\"280\"/>"
"              <layout:dimensions layout:width=\"220\" layout:height=\"36\"/>"
"            </layout:boundingBox>"
"          </layout:textGlyph>"
"          <layout:textGlyph layout:id=\"TextGlyph_oxaloacetate_cyt\" layout:originOfText=\"oxaloacetate_cyt\" layout:graphicalObject=\"SpeciesGlyph_oxaloacetate_cyt\">"
"            <layout:boundingBox layout:id=\"bb_tg_oxaloacetate_cyt\">"
"              <layout:position layout:x=\"590\" layout:y=\"480\"/>"
"              <layout:dimensions layout:width=\"220\" layout:height=\"36\"/>"
"            </layout:boundingBox>"
"          </layout:textGlyph>"
"          <layout:textGlyph layout:id=\"TextGlyph_aspartate_cyt\" layout:originOfText=\"aspartate_cyt\" layout:graphicalObject=\"SpeciesGlyph_aspartate_cyt\">"
"            <layout:boundingBox layout:id=\"bb_tg_aspartate_cyt\">"
"              <layout:position layout:x=\"590\" layout:y=\"680\"/>"
"              <layout:dimensions layout:width=\"220\" layout:height=\"36\"/>"
"            </layout:boundingBox>"
"          </layout:textGlyph>"
"          <layout:textGlyph layout:id=\"TextGlyph_glutamate_cyt\" layout:originOfText=\"glutamate_cyt\" layout:graphicalObject=\"SpeciesGlyph_glutamate_cyt\">"
"            <layout:boundingBox layout:id=\"bb_tg_glutamate_cyt\">"
"              <layout:position layout:x=\"810\" layout:y=\"610\"/>"
"              <layout:dimensions layout:width=\"220\" layout:height=\"36\"/>"
"            </layout:boundingBox>"
"          </layout:textGlyph>"
"          <layout:textGlyph layout:id=\"TextGlyph_aKetoglutarate_cyt\" layout:originOfText=\"aKetoglutarate_cyt\" layout:graphicalObject=\"SpeciesGlyph_aKetoglutarate_cyt\">"
"            <layout:boundingBox layout:id=\"bb_tg_aKetoglutarate_cyt\">"
"              <layout:position layout:x=\"870\" layout:y=\"500\"/>"
"              <layout:dimensions layout:width=\"260\" layout:height=\"36\"/>"
"            </layout:boundingBox>"
"          </layout:textGlyph>"
"          <layout:textGlyph layout:id=\"TextGlyph_nad_cyt\" layout:originOfText=\"nad_cyt\" layout:graphicalObject=\"SpeciesGlyph_nad_cyt\">"
"            <layout:boundingBox layout:id=\"bb_tg_nad_cyt\">"
"              <layout:position layout:x=\"525\" layout:y=\"350\"/>"
"              <layout:dimensions layout:width=\"80\" layout:height=\"24\"/>"
"            </layout:boundingBox>"
"          </layout:textGlyph>"
"          <layout:textGlyph layout:id=\"TextGlyph_nadh_cyt\" layout:originOfText=\"nadh_cyt\" layout:graphicalObject=\"SpeciesGlyph_nadh_cyt\">"
"            <layout:boundingBox layout:id=\"bb_tg_nadh_cyt\">"
"              <layout:position layout:x=\"525\" layout:y=\"430\"/>"
"              <layout:dimensions layout:width=\"80\" layout:height=\"24\"/>"
"            </layout:boundingBox>"
"          </layout:textGlyph>"
"          <layout:textGlyph layout:id=\"TextGlyph_h_cyt\" layout:originOfText=\"h_cyt\" layout:graphicalObject=\"SpeciesGlyph_h_cyt\">"
"            <layout:boundingBox layout:id=\"bb_tg_h_cyt\">"
"              <layout:position layout:x=\"435\" layout:y=\"430\"/>"
"              <layout:dimensions layout:width=\"30\" layout:height=\"24\"/>"
"            </layout:boundingBox>"
"          </layout:textGlyph>"
"          <layout:textGlyph layout:id=\"tg_rg_malaltedh_cyt\" layout:originOfText=\"reaction_malatedh_cyt\" layout:graphicalObject=\"rg_malatedh_cyt\">"
"            <layout:boundingBox layout:id=\"bb_tg_rg_malaltedh_cyt\">"
"              <layout:position layout:x=\"700\" layout:y=\"385\"/>"
"              <layout:dimensions layout:width=\"210\" layout:height=\"24\"/>"
"            </layout:boundingBox>"
"          </layout:textGlyph>"
"          <layout:textGlyph layout:id=\"tg_rg_aspartateat_cyt\" layout:originOfText=\"reaction_aspartateat_cyt\" layout:graphicalObject=\"rg_aspartateat_cyt\">"
"            <layout:boundingBox layout:id=\"bb_tg_rg_aspartateat_cyt\">"
"              <layout:position layout:x=\"440\" layout:y=\"585\"/>"
"              <layout:dimensions layout:width=\"260\" layout:height=\"24\"/>"
"            </layout:boundingBox>"
"          </layout:textGlyph>"
"          <layout:textGlyph layout:id=\"TextGlyph_malate_mito3\" layout:originOfText=\"malate_mito3\" layout:graphicalObject=\"SpeciesGlyph_malate_mito3\">"
"            <layout:boundingBox layout:id=\"bb_tg_malatate_mito3\">"
"              <layout:position layout:x=\"1860\" layout:y=\"80\"/>"
"              <layout:dimensions layout:width=\"220\" layout:height=\"36\"/>"
"            </layout:boundingBox>"
"          </layout:textGlyph>"
"          <layout:textGlyph layout:id=\"TextGlyph_oxaloacetate_mito3\" layout:originOfText=\"oxaloacetate_mito3\" layout:graphicalObject=\"SpeciesGlyph_oxaloacetate_mito3\">"
"            <layout:boundingBox layout:id=\"bb_tg_oxaloacetate_mito3\">"
"              <layout:position layout:x=\"1860\" layout:y=\"280\"/>"
"              <layout:dimensions layout:width=\"220\" layout:height=\"36\"/>"
"            </layout:boundingBox>"
"          </layout:textGlyph>"
"          <layout:textGlyph layout:id=\"TextGlyph_aspartate_mito3\" layout:originOfText=\"aspartate_mito3\" layout:graphicalObject=\"SpeciesGlyph_aspartate_mito3\">"
"            <layout:boundingBox layout:id=\"bb_tg_aspartate_mito3\">"
"              <layout:position layout:x=\"1860\" layout:y=\"480\"/>"
"              <layout:dimensions layout:width=\"220\" layout:height=\"36\"/>"
"            </layout:boundingBox>"
"          </layout:textGlyph>"
"          <layout:textGlyph layout:id=\"TextGlyph_glutamate_mito3\" layout:originOfText=\"glutamate_mito3\" layout:graphicalObject=\"SpeciesGlyph_glutamate_mito3\">"
"            <layout:boundingBox layout:id=\"bb_tg_glutamate_mito3\">"
"              <layout:position layout:x=\"1560\" layout:y=\"430\"/>"
"              <layout:dimensions layout:width=\"220\" layout:height=\"36\"/>"
"            </layout:boundingBox>"
"          </layout:textGlyph>"
"          <layout:textGlyph layout:id=\"TextGlyph_aKetoglutarate_mito3\" layout:originOfText=\"aKetoglutarate_mito3\" layout:graphicalObject=\"SpeciesGlyph_aKetoglutarate_mito3\">"
"            <layout:boundingBox layout:id=\"bb_tg_aKetoglutarate_mito3\">"
"              <layout:position layout:x=\"1540\" layout:y=\"300\"/>"
"              <layout:dimensions layout:width=\"260\" layout:height=\"36\"/>"
"            </layout:boundingBox>"
"          </layout:textGlyph>"
"          <layout:textGlyph layout:id=\"TextGlyph_nad_mito3\" layout:originOfText=\"nad_mito3\" layout:graphicalObject=\"SpeciesGlyph_nad_mito3\">"
"            <layout:boundingBox layout:id=\"bb_tg_nad_mito3\">"
"              <layout:position layout:x=\"2055\" layout:y=\"150\"/>"
"              <layout:dimensions layout:width=\"80\" layout:height=\"24\"/>"
"            </layout:boundingBox>"
"          </layout:textGlyph>"
"          <layout:textGlyph layout:id=\"TextGlyph_nadh_mito3\" layout:originOfText=\"nadh_mito3\" layout:graphicalObject=\"SpeciesGlyph_nadh_mito3\">"
"            <layout:boundingBox layout:id=\"bb_tg_nadh_mito3\">"
"              <layout:position layout:x=\"2055\" layout:y=\"230\"/>"
"              <layout:dimensions layout:width=\"80\" layout:height=\"24\"/>"
"            </layout:boundingBox>"
"          </layout:textGlyph>"
"          <layout:textGlyph layout:id=\"TextGlyph_h_mito3\" layout:originOfText=\"h_mito3\" layout:graphicalObject=\"SpeciesGlyph_h_mito3\">"
"            <layout:boundingBox layout:id=\"bb_tg_h_mito3\">"
"              <layout:position layout:x=\"2205\" layout:y=\"230\"/>"
"              <layout:dimensions layout:width=\"30\" layout:height=\"24\"/>"
"            </layout:boundingBox>"
"          </layout:textGlyph>"
"          <layout:textGlyph layout:id=\"tg_rg_malatedh_mito3\" layout:originOfText=\"reaction_malatedh_mito3\" layout:graphicalObject=\"rg_malatedh_mito3\">"
"            <layout:boundingBox layout:id=\"bb_tg_rg_malatedh_mito3\">"
"              <layout:position layout:x=\"1740\" layout:y=\"185\"/>"
"              <layout:dimensions layout:width=\"220\" layout:height=\"24\"/>"
"            </layout:boundingBox>"
"          </layout:textGlyph>"
"          <layout:textGlyph layout:id=\"tg_rg_aspartateat_mito3\" layout:originOfText=\"reaction_aspartateat_mito3\" layout:graphicalObject=\"rg_aspartateat_mito3\">"
"            <layout:boundingBox layout:id=\"bb_tg_rg_aspartateat_mito3\">"
"              <layout:position layout:x=\"1970\" layout:y=\"385\"/>"
"              <layout:dimensions layout:width=\"260\" layout:height=\"24\"/>"
"            </layout:boundingBox>"
"          </layout:textGlyph>"
"          <layout:textGlyph layout:id=\"tg_rg_aspartateCarrier\" layout:originOfText=\"aspartateCarrier\" layout:graphicalObject=\"rg_aspartateCarrier\">"
"            <layout:boundingBox layout:id=\"bb_tg_rg_aspartateCarrier\">"
"              <layout:position layout:x=\"1380\" layout:y=\"500\"/>"
"              <layout:dimensions layout:width=\"160\" layout:height=\"24\"/>"
"            </layout:boundingBox>"
"          </layout:textGlyph>"
"          <layout:textGlyph layout:id=\"tg_rg_malateCarrier\" layout:originOfText=\"malateCarrier\" layout:graphicalObject=\"rg_malateCarrier\">"
"            <layout:boundingBox layout:id=\"bb_tg_rg_malateCarrier\">"
"              <layout:position layout:x=\"1360\" layout:y=\"330\"/>"
"              <layout:dimensions layout:width=\"140\" layout:height=\"24\"/>"
"            </layout:boundingBox>"
"          </layout:textGlyph>"
"        </layout:listOfTextGlyphs>"
"        <render:listOfRenderInformation>"
"          <render:renderInformation render:id=\"highlightGlucose\" render:referenceRenderInformation=\"defaultStyle\" render:programName=\"Ralph Gauges\" render:programVersion=\"1.0\" render:backgroundColor='#FFFFFFFF'>"
"            <render:listOfColorDefinitions>"
"              <render:colorDefinition render:id=\"lightRed\" render:value=\"#e6add8\"/>"
"              <render:colorDefinition render:id=\"white\" render:value=\"#ffffff\"/>"
"            </render:listOfColorDefinitions>"
"            <render:listOfGradientDefinitions>"
"              <render:radialGradient render:id=\"highlightedSpeciesGlyphGradient\">"
"                <render:stop render:offset=\"0\" render:stop-color=\"white\"/>"
"                <render:stop render:offset=\"100%\" render:stop-color=\"lightRed\"/>"
"              </render:radialGradient>"
"            </render:listOfGradientDefinitions>"
"            <render:listOfStyles>"
"              <render:style render:id=\"highlightedGlucose\" render:idList=\"SpeciesGlyph_Glucose\">"
"                <render:g render:stroke=\"black\" render:stroke-width=\"1\">"
"                  <render:rectangle render:fill=\"highlightedSpeciesGlyphGradient\" render:x=\"0\" render:y=\"0\" render:width=\"100%\" render:height=\"100%\" render:rx=\"5\" render:ry=\"50%\"/>"
"                </render:g>"
"              </render:style>"
"            </render:listOfStyles>"
"          </render:renderInformation>"
"        </render:listOfRenderInformation>"
"      </layout:layout>"
"      <render:listOfGlobalRenderInformation>"
"        <render:renderInformation render:id=\"wireFrame\" render:name=\"wireframe style\" render:programName=\"Ralph Gauges\" render:programVersion=\"1.0\" render:backgroundColor='#FFFFFFFF'>"
"          <render:listOfColorDefinitions>"
"            <render:colorDefinition render:id=\"white\" render:value=\"#ffffff\"/>"
"            <render:colorDefinition render:id=\"black\" render:value=\"#000000\"/>"
"          </render:listOfColorDefinitions>"
"          <render:listOfStyles>"
"            <render:style render:id=\"compartmentGlyphStyle\" render:typeList=\"COMPARTMENTGLYPH\">"
"              <render:g render:stroke=\"black\" render:stroke-width=\"1\">"
"                <render:rectangle render:x=\"0\" render:y=\"0\" render:width=\"100%\" render:height=\"100%\"/>"
"              </render:g>"
"            </render:style>"
"            <render:style render:id=\"speciesGlyphStyle\" render:typeList=\"SPECIESGLYPH\">"
"              <render:g render:stroke=\"black\" render:stroke-width=\"1\">"
"                <render:rectangle render:x=\"0\" render:y=\"0\" render:width=\"100%\" render:height=\"100%\"/>"
"              </render:g>"
"            </render:style>"
"            <render:style render:id=\"reactionGlyphStyle\" render:typeList=\"REACTIONGLYPH SPECIESREFERENCEGLYPH TEXTGLYPH\">"
"              <render:g render:stroke=\"black\" render:stroke-width=\"1\" render:font-size=\"12\" render:text-anchor=\"middle\"/>"
"            </render:style>"
"          </render:listOfStyles>"
"        </render:renderInformation>"
"        <render:renderInformation render:id=\"defaultGrayStyle\" render:name=\"grayscale style\" render:programName=\"Ralph Gauges\" render:programVersion=\"1.0\" render:backgroundColor='#FFFFFFFF'>"
"          <render:listOfColorDefinitions>"
"            <render:colorDefinition render:id=\"lightGray\" render:value=\"#cecece\"/>"
"            <render:colorDefinition render:id=\"white\" render:value=\"#ffffff\"/>"
"            <render:colorDefinition render:id=\"black\" render:value=\"#000000\"/>"
"            <render:colorDefinition render:id=\"lightGray2\" render:value=\"#f0f0f0\"/>"
"            <render:colorDefinition render:id=\"gray\" render:value=\"#0b0b0b\"/>"
"          </render:listOfColorDefinitions>"
"          <render:listOfGradientDefinitions>"
"            <render:radialGradient render:id=\"speciesGlyphGradient\">"
"              <render:stop render:offset=\"0\" render:stop-color=\"white\"/>"
"              <render:stop render:offset=\"100%\" render:stop-color=\"lightGray\"/>"
"            </render:radialGradient>"
"          </render:listOfGradientDefinitions>"
"          <render:listOfLineEndings>"
"            <render:lineEnding render:id=\"simpleHead_black\">"
"              <layout:boundingBox>"
"                <layout:position layout:x=\"-8\" layout:y=\"-3\"/>"
"                <layout:dimensions layout:width=\"10\" layout:height=\"6\"/>"
"              </layout:boundingBox>"
"              <render:g render:stroke=\"black\" render:stroke-width=\"1\" render:fill=\"black\">"
"                <render:polygon>"
"                  <render:listOfElements>"
"                    <render:element xsi:type=\"RenderPoint\" render:x=\"0\" render:y=\"0\"/>"
"                    <render:element xsi:type=\"RenderPoint\" render:x=\"10\" render:y=\"3\"/>"
"                    <render:element xsi:type=\"RenderPoint\" render:x=\"0\" render:y=\"6\"/>"
"                  </render:listOfElements>"
"                </render:polygon>"
"              </render:g>"
"            </render:lineEnding>"
"          </render:listOfLineEndings>"
"          <render:listOfStyles>"
"            <render:style render:id=\"compartmentGlyphStyle\" render:typeList=\"COMPARTMENTGLYPH\">"
"              <render:g render:stroke=\"gray\" render:stroke-width=\"1\">"
"                <render:rectangle render:fill=\"lightGray2\" render:x=\"0\" render:y=\"0\" render:width=\"100%\" render:height=\"100%\" render:rx=\"5%\"/>"
"              </render:g>"
"            </render:style>"
"            <render:style render:id=\"speciesGlyphStyle\" render:typeList=\"SPECIESGLYPH\">"
"              <render:g render:stroke=\"black\" render:stroke-width=\"1\">"
"                <render:rectangle render:fill=\"speciesGlyphGradient\" render:x=\"0\" render:y=\"0\" render:width=\"100%\" render:height=\"100%\" render:rx=\"5%\"/>"
"              </render:g>"
"            </render:style>"
"            <render:style render:id=\"reactionGlyphStyle\" render:typeList=\"REACTIONGLYPH TEXTGLYPH\">"
"              <render:g render:stroke=\"black\" render:stroke-width=\"1\" render:font-size=\"12\" render:text-anchor=\"middle\"/>"
"            </render:style>"
"            <render:style render:id=\"reactantSpeciesReferenceGlyphStyle\" render:roleList=\"product sideproduct sidesubstrate substrate\">"
"              <render:g render:stroke=\"#000000\" render:stroke-width=\"1\"/>"
"            </render:style>"
"            <render:style render:id=\"activatorSpeciesReferenceGlyphStyle\" render:roleList=\"activator\">"
"              <render:g render:stroke=\"#000000\" render:stroke-width=\"1\"/>"
"            </render:style>"
"            <render:style render:id=\"modifierSpeciesReferenceGlyphStyle\" render:roleList=\"modifier\">"
"              <render:g render:stroke=\"#000000\" render:stroke-width=\"1\"/>"
"            </render:style>"
"            <render:style render:id=\"inhibitorSpeciesReferenceGlyphStyle\" render:roleList=\"inhibitor\">"
"              <render:g render:stroke=\"#000000\" render:stroke-width=\"1\"/>"
"            </render:style>"
"          </render:listOfStyles>"
"        </render:renderInformation>"
"        <render:renderInformation render:id=\"shortGrayStyle\" render:name=\"modified default style to grayscale\" render:referenceRenderInformation=\"defaultStyle\" render:programName=\"Ralph Gauges\" render:programVersion=\"1.0\" render:backgroundColor='#FFFFFFFF'>"
"          <render:listOfColorDefinitions>"
"            <render:colorDefinition render:id=\"lightBlue\" render:value=\"#cecece\"/>"
"            <render:colorDefinition render:id=\"white\" render:value=\"#ffffff\"/>"
"            <render:colorDefinition render:id=\"black\" render:value=\"#000000\"/>"
"            <render:colorDefinition render:id=\"red\" render:value=\"#000000\"/>"
"            <render:colorDefinition render:id=\"green\" render:value=\"#000000\"/>"
"            <render:colorDefinition render:id=\"blue\" render:value=\"#000000\"/>"
"            <render:colorDefinition render:id=\"lightYellow\" render:value=\"#f0f0f0\"/>"
"            <render:colorDefinition render:id=\"darkGreen\" render:value=\"#0b0b0b\"/>"
"          </render:listOfColorDefinitions>"
"        </render:renderInformation>"
"        <render:renderInformation render:id=\"defaultStyle\" render:name=\"default style\" render:programName=\"Ralph Gauges\" render:programVersion=\"1.0\" render:backgroundColor='#FFFFFFFF'>"
"          <render:listOfColorDefinitions>"
"            <render:colorDefinition render:id=\"lightBlue\" render:value=\"#add8e6\"/>"
"            <render:colorDefinition render:id=\"white\" render:value=\"#ffffff\"/>"
"            <render:colorDefinition render:id=\"black\" render:value=\"#000000\"/>"
"            <render:colorDefinition render:id=\"red\" render:value=\"#ff0000\"/>"
"            <render:colorDefinition render:id=\"green\" render:value=\"#00ff00\"/>"
"            <render:colorDefinition render:id=\"blue\" render:value=\"#0000ff\"/>"
"            <render:colorDefinition render:id=\"lightYellow\" render:value=\"#ffffd1\"/>"
"            <render:colorDefinition render:id=\"darkGreen\" render:value=\"#002000\"/>"
"          </render:listOfColorDefinitions>"
"          <render:listOfGradientDefinitions>"
"            <render:radialGradient render:id=\"speciesGlyphGradient\">"
"              <render:stop render:offset=\"0\" render:stop-color=\"white\"/>"
"              <render:stop render:offset=\"100%\" render:stop-color=\"lightBlue\"/>"
"            </render:radialGradient>"
"          </render:listOfGradientDefinitions>"
"          <render:listOfLineEndings>"
"            <render:lineEnding render:id=\"simpleHead_black\">"
"              <layout:boundingBox>"
"                <layout:position layout:x=\"-8\" layout:y=\"-3\"/>"
"                <layout:dimensions layout:width=\"10\" layout:height=\"6\"/>"
"              </layout:boundingBox>"
"              <render:g render:stroke=\"black\" render:stroke-width=\"1\" render:fill=\"black\">"
"                <render:polygon>"
"                  <render:listOfElements>"
"                    <render:element xsi:type=\"RenderPoint\" render:x=\"0\" render:y=\"0\"/>"
"                    <render:element xsi:type=\"RenderPoint\" render:x=\"10\" render:y=\"3\"/>"
"                    <render:element xsi:type=\"RenderPoint\" render:x=\"0\" render:y=\"6\"/>"
"                  </render:listOfElements>"
"                </render:polygon>"
"              </render:g>"
"            </render:lineEnding>"
"            <render:lineEnding render:id=\"simpleHead_red\">"
"              <layout:boundingBox>"
"                <layout:position layout:x=\"-8\" layout:y=\"-3\"/>"
"                <layout:dimensions layout:width=\"10\" layout:height=\"6\"/>"
"              </layout:boundingBox>"
"              <render:g render:stroke=\"red\" render:stroke-width=\"1\" render:fill=\"red\">"
"                <render:polygon>"
"                  <render:listOfElements>"
"                    <render:element xsi:type=\"RenderPoint\" render:x=\"0\" render:y=\"0\"/>"
"                    <render:element xsi:type=\"RenderPoint\" render:x=\"10\" render:y=\"3\"/>"
"                    <render:element xsi:type=\"RenderPoint\" render:x=\"0\" render:y=\"6\"/>"
"                  </render:listOfElements>"
"                </render:polygon>"
"              </render:g>"
"            </render:lineEnding>"
"            <render:lineEnding render:id=\"simpleHead_green\">"
"              <layout:boundingBox>"
"                <layout:position layout:x=\"-8\" layout:y=\"-3\"/>"
"                <layout:dimensions layout:width=\"10\" layout:height=\"6\"/>"
"              </layout:boundingBox>"
"              <render:g render:stroke=\"green\" render:stroke-width=\"1\" render:fill=\"green\">"
"                <render:polygon>"
"                  <render:listOfElements>"
"                    <render:element xsi:type=\"RenderPoint\" render:x=\"0\" render:y=\"0\"/>"
"                    <render:element xsi:type=\"RenderPoint\" render:x=\"10\" render:y=\"3\"/>"
"                    <render:element xsi:type=\"RenderPoint\" render:x=\"0\" render:y=\"6\"/>"
"                  </render:listOfElements>"
"                </render:polygon>"
"              </render:g>"
"            </render:lineEnding>"
"            <render:lineEnding render:id=\"simpleHead_blue\">"
"              <layout:boundingBox>"
"                <layout:position layout:x=\"-8\" layout:y=\"-3\"/>"
"                <layout:dimensions layout:width=\"10\" layout:height=\"6\"/>"
"              </layout:boundingBox>"
"              <render:g render:stroke=\"blue\" render:stroke-width=\"1\" render:fill=\"blue\">"
"                <render:polygon>"
"                  <render:listOfElements>"
"                    <render:element xsi:type=\"RenderPoint\" render:x=\"0\" render:y=\"0\"/>"
"                    <render:element xsi:type=\"RenderPoint\" render:x=\"10\" render:y=\"3\"/>"
"                    <render:element xsi:type=\"RenderPoint\" render:x=\"0\" render:y=\"6\"/>"
"                  </render:listOfElements>"
"                </render:polygon>"
"              </render:g>"
"            </render:lineEnding>"
"          </render:listOfLineEndings>"
"          <render:listOfStyles>"
"            <render:style render:id=\"compartmentGlyphStyle\" render:typeList=\"COMPARTMENTGLYPH\">"
"              <render:g render:stroke=\"darkGreen\" render:stroke-width=\"1\">"
"                <render:rectangle render:fill=\"lightYellow\" render:x=\"0\" render:y=\"0\" render:width=\"100%\" render:height=\"100%\" render:rx=\"10%\" render:ry=\"10%\"/>"
"              </render:g>"
"            </render:style>"
"            <render:style render:id=\"speciesGlyphStyle\" render:typeList=\"SPECIESGLYPH\">"
"              <render:g render:stroke=\"black\" render:stroke-width=\"1\">"
"                <render:rectangle render:fill=\"speciesGlyphGradient\" render:x=\"0\" render:y=\"0\" render:width=\"100%\" render:height=\"100%\" render:rx=\"5\" render:ry=\"50%\"/>"
"              </render:g>"
"            </render:style>"
"            <render:style render:id=\"reactionGlyphStyle\" render:typeList=\"REACTIONGLYPH TEXTGLYPH\">"
"              <render:g render:stroke=\"black\" render:stroke-width=\"1\" render:font-size=\"12\" render:text-anchor=\"middle\"/>"
"            </render:style>"
"            <render:style render:id=\"reactantSpeciesReferenceGlyphStyle\" render:roleList=\"product sideproduct sidesubstrate substrate\">"
"              <render:g render:stroke=\"#000000\" render:stroke-width=\"1\" render:endHead=\"simpleHead_black\"/>"
"            </render:style>"
"            <render:style render:id=\"activatorSpeciesReferenceGlyphStyle\" render:roleList=\"activator\">"
"              <render:g render:stroke=\"green\" render:stroke-width=\"1\" render:endHead=\"simpleHead_green\"/>"
"            </render:style>"
"            <render:style render:id=\"modifierSpeciesReferenceGlyphStyle\" render:roleList=\"modifier\">"
"              <render:g render:stroke=\"blue\" render:stroke-width=\"1\" render:endHead=\"simpleHead_blue\"/>"
"            </render:style>"
"            <render:style render:id=\"inhibitorSpeciesReferenceGlyphStyle\" render:roleList=\"inhibitor\">"
"              <render:g render:stroke=\"red\" render:stroke-width=\"1\" render:endHead=\"simpleHead_red\"/>"
"            </render:style>"
"          </render:listOfStyles>"
"        </render:renderInformation>"
"      </render:listOfGlobalRenderInformation>"
"    </layout:listOfLayouts>"
"  </model>"
"</sbml>";


  XMLInputStream stream(s,false);
  XMLNode node(stream);
  // create the document

  SBMLDocument *document=new SBMLDocument(3,1);

  document->enablePackage(LayoutExtension::getXmlnsL3V1V1(), "layout", true);
  document->enablePackage(RenderExtension::getXmlnsL3V1V1(), "render", true);

  LayoutPkgNamespaces layoutns;

  document->setPackageRequired("layout", false);
  document->setPackageRequired("render", false);

  // create the Model

  Model* model=document->createModel();
  model->setId("TestModel");
  document->setModel(model);

  // create the Compartment

  Compartment* compartment=model->createCompartment();
  compartment->setId("Hepatocyte");
  compartment->setName("Hepatocyte");

  Compartment* mito1=model->createCompartment();
  mito1->setId("Mito_1");
  mito1->setName("Mito 1");
  mito1->setOutside(compartment->getId());

  Compartment* mito2=model->createCompartment();
  mito2->setId("Mito_2");
  mito2->setName("Mito 2");
  mito2->setOutside(compartment->getId());

  Compartment* mito3=model->createCompartment();
  mito3->setId("Mito_3");
  mito3->setName("Mito 3");
  mito3->setOutside(compartment->getId());


  // create the Species

  // Malate
  Species* malate_cyt=model->createSpecies();
  malate_cyt->setId("malate_cyt");
  malate_cyt->setName("Malate");
  malate_cyt->setCompartment(compartment->getId());

  Species* malate_mito1=model->createSpecies();
  malate_mito1->setId("malate_mito1");
  malate_mito1->setCompartment(mito1->getId());
  malate_mito1->setName("Malate");

  Species* malate_mito2=model->createSpecies();
  malate_mito2->setId("malate_mito2");
  malate_mito2->setCompartment(mito2->getId());
  malate_mito2->setName("Malate");

  Species* malate_mito3=model->createSpecies();
  malate_mito3->setId("malate_mito3");
  malate_mito3->setCompartment(mito3->getId());
  malate_mito3->setName("Malate");


  // Oxaloacetate
  Species* oxaloacetate_cyt=model->createSpecies();
  oxaloacetate_cyt->setId("oxaloacetate_cyt");
  oxaloacetate_cyt->setName("Oxaloacetate");
  oxaloacetate_cyt->setCompartment(compartment->getId());

  Species* oxaloacetate_mito1=model->createSpecies();
  oxaloacetate_mito1->setId("oxaloacetate_mito1");
  oxaloacetate_mito1->setCompartment(mito1->getId());
  oxaloacetate_mito1->setName("Oxaloacetate");

  Species* oxaloacetate_mito2=model->createSpecies();
  oxaloacetate_mito2->setId("oxaloacetate_mito2");
  oxaloacetate_mito2->setCompartment(mito2->getId());
  oxaloacetate_mito2->setName("Oxaloacetate");

  Species* oxaloacetate_mito3=model->createSpecies();
  oxaloacetate_mito3->setId("oxaloacetate_mito3");
  oxaloacetate_mito3->setCompartment(mito3->getId());
  oxaloacetate_mito3->setName("Oxaloacetate");


  // Aspartate
  Species* aspartate_cyt=model->createSpecies();
  aspartate_cyt->setId("aspartate_cyt");
  aspartate_cyt->setName("Aspartate");
  aspartate_cyt->setCompartment(compartment->getId());

  Species* aspartate_mito1=model->createSpecies();
  aspartate_mito1->setId("aspartate_mito1");
  aspartate_mito1->setCompartment(mito1->getId());
  aspartate_mito1->setName("Aspartate");

  Species* aspartate_mito2=model->createSpecies();
  aspartate_mito2->setId("aspartate_mito2");
  aspartate_mito2->setCompartment(mito2->getId());
  aspartate_mito2->setName("Aspartate");

  Species* aspartate_mito3=model->createSpecies();
  aspartate_mito3->setId("aspartate_mito3");
  aspartate_mito3->setCompartment(mito3->getId());
  aspartate_mito3->setName("Aspartate");


  // Glutamate
  Species* glutamate_cyt=model->createSpecies();
  glutamate_cyt->setId("glutamate_cyt");
  glutamate_cyt->setName("Glutamate");
  glutamate_cyt->setCompartment(compartment->getId());

  Species* glutamate_mito1=model->createSpecies();
  glutamate_mito1->setId("glutamate_mito1");
  glutamate_mito1->setCompartment(mito1->getId());
  glutamate_mito1->setName("Glutamate");

  Species* glutamate_mito2=model->createSpecies();
  glutamate_mito2->setId("glutamate_mito2");
  glutamate_mito2->setCompartment(mito2->getId());
  glutamate_mito2->setName("Glutamate");

  Species* glutamate_mito3=model->createSpecies();
  glutamate_mito3->setId("glutamate_mito3");
  glutamate_mito3->setCompartment(mito3->getId());
  glutamate_mito3->setName("Glutamate");


  // alpha-Ketoglutarate
  Species* aKetoglutarate_cyt=model->createSpecies();
  aKetoglutarate_cyt->setId("aKetoglutarate_cyt");
  aKetoglutarate_cyt->setName("alpha-Ketoglutarate");
  aKetoglutarate_cyt->setCompartment(compartment->getId());

  Species* aKetoglutarate_mito1=model->createSpecies();
  aKetoglutarate_mito1->setId("aKetoglutarate_mito1");
  aKetoglutarate_mito1->setCompartment(mito1->getId());
  aKetoglutarate_mito1->setName("alpha-Ketoglutarate");

  Species* aKetoglutarate_mito2=model->createSpecies();
  aKetoglutarate_mito2->setId("aKetoglutarate_mito2");
  aKetoglutarate_mito2->setCompartment(mito2->getId());
  aKetoglutarate_mito2->setName("alpha-Ketoglutarate");

  Species* aKetoglutarate_mito3=model->createSpecies();
  aKetoglutarate_mito3->setId("aKetoglutarate_mito3");
  aKetoglutarate_mito3->setCompartment(mito3->getId());
  aKetoglutarate_mito3->setName("alpha-Ketoglutarate");


  // protons
  Species* h_cyt=model->createSpecies();
  h_cyt->setId("h_cyt");
  h_cyt->setName("H+");
  h_cyt->setCompartment(compartment->getId());

  Species* h_mito1=model->createSpecies();
  h_mito1->setId("h_mito1");
  h_mito1->setCompartment(mito1->getId());
  h_mito1->setName("H+");

  Species* h_mito2=model->createSpecies();
  h_mito2->setId("h_mito2");
  h_mito2->setCompartment(mito2->getId());
  h_mito2->setName("H+");

  Species* h_mito3=model->createSpecies();
  h_mito3->setId("h_mito3");
  h_mito3->setCompartment(mito3->getId());
  h_mito3->setName("H+");


  // NAD+
  Species* nad_cyt=model->createSpecies();
  nad_cyt->setId("nad_cyt");
  nad_cyt->setName("NAD+");
  nad_cyt->setCompartment(compartment->getId());

  Species* nad_mito1=model->createSpecies();
  nad_mito1->setId("nad_mito1");
  nad_mito1->setCompartment(mito1->getId());
  nad_mito1->setName("NAD+");

  Species* nad_mito2=model->createSpecies();
  nad_mito2->setId("nad_mito2");
  nad_mito2->setCompartment(mito2->getId());
  nad_mito2->setName("NAD+");

  Species* nad_mito3=model->createSpecies();
  nad_mito3->setId("nad_mito3");
  nad_mito3->setCompartment(mito3->getId());
  nad_mito3->setName("NAD+");


  // NADH
  Species* nadh_cyt=model->createSpecies();
  nadh_cyt->setId("nadh_cyt");
  nadh_cyt->setName("NADH");
  nadh_cyt->setCompartment(compartment->getId());

  Species* nadh_mito1=model->createSpecies();
  nadh_mito1->setId("nadh_mito1");
  nadh_mito1->setCompartment(mito1->getId());
  nadh_mito1->setName("NADH");

  Species* nadh_mito2=model->createSpecies();
  nadh_mito2->setId("nadh_mito2");
  nadh_mito2->setCompartment(mito2->getId());
  nadh_mito2->setName("NADH");

  Species* nadh_mito3=model->createSpecies();
  nadh_mito3->setId("nadh_mito3");
  nadh_mito3->setCompartment(mito3->getId());
  nadh_mito3->setName("NADH");




  // create the Reactions

  // Cytosol

  // Malate Dehydrogenase
  Reaction* malatedh_cyt=model->createReaction();
  malatedh_cyt->setId("reaction_malatedh_cyt");
  malatedh_cyt->setName("malate dehydrogenase");
  malatedh_cyt->setReversible(false);

  SpeciesReference* sr_malate_cyt=malatedh_cyt->createReactant();
  sr_malate_cyt->setSpecies(malate_cyt->getId());
  sr_malate_cyt->setId("sr_malate_cyt");

  SpeciesReference* sr_nad_cyt=malatedh_cyt->createReactant();
  sr_nad_cyt->setSpecies(nad_cyt->getId());
  sr_nad_cyt->setId("sr_nad_cyt");

  SpeciesReference* sr_nadh_cyt=malatedh_cyt->createProduct();
  sr_nadh_cyt->setSpecies(nadh_cyt->getId());
  sr_nadh_cyt->setId("sr_nadh_cyt");

  SpeciesReference* sr_h_cyt=malatedh_cyt->createProduct();
  sr_h_cyt->setSpecies(h_cyt->getId());
  sr_h_cyt->setId("sr_h_cyt");

  SpeciesReference* sr_oxaloacetate_cyt_1=malatedh_cyt->createProduct();
  sr_oxaloacetate_cyt_1->setSpecies(oxaloacetate_cyt->getId());
  sr_oxaloacetate_cyt_1->setId("sr_oxaloacetate_cyt_1");

  //Aspartate Aminotransferase
  Reaction* aspartateat_cyt=model->createReaction();
  aspartateat_cyt->setId("reaction_aspartateat_cyt");
  aspartateat_cyt->setName("aspartate aminotransferase");
  aspartateat_cyt->setReversible(false);

  SpeciesReference* sr_oxaloacetate_cyt_2=aspartateat_cyt->createReactant();
  sr_oxaloacetate_cyt_2->setSpecies(oxaloacetate_cyt->getId());
  sr_oxaloacetate_cyt_2->setId("sr_oxaloacetate_cyt_2");

  SpeciesReference* sr_glutamate_cyt_1=aspartateat_cyt->createReactant();
  sr_glutamate_cyt_1->setSpecies(glutamate_cyt->getId());
  sr_glutamate_cyt_1->setId("sr_glutamate_cyt_1");

  SpeciesReference* sr_aspartate_cyt_1=aspartateat_cyt->createProduct();
  sr_aspartate_cyt_1->setSpecies(aspartate_cyt->getId());
  sr_aspartate_cyt_1->setId("sr_aspartate_cyt_1");

  SpeciesReference* sr_aKetoglutarate_cyt_1=aspartateat_cyt->createProduct();
  sr_aKetoglutarate_cyt_1->setSpecies(aKetoglutarate_cyt->getId());
  sr_aKetoglutarate_cyt_1->setId("sr_aKetoglutarate_cyt_1");


  // Mito 1

  // Malate Dehydrogenase
  Reaction* malatedh_mito1=model->createReaction();
  malatedh_mito1->setId("reaction_malatedh_mito1");
  malatedh_mito1->setName("malate dehydrogenase");
  malatedh_mito1->setReversible(false);

  SpeciesReference* sr_malate_mito1=malatedh_mito1->createReactant();
  sr_malate_mito1->setSpecies(malate_mito1->getId());
  sr_malate_mito1->setId("sr_malate_mito1");

  SpeciesReference* sr_nad_mito1=malatedh_mito1->createReactant();
  sr_nad_mito1->setSpecies(nad_mito1->getId());
  sr_nad_mito1->setId("sr_nad_mito1");

  SpeciesReference* sr_nadh_mito1=malatedh_mito1->createProduct();
  sr_nadh_mito1->setSpecies(nadh_mito1->getId());
  sr_nadh_mito1->setId("sr_nadh_mito1");

  SpeciesReference* sr_h_mito1=malatedh_mito1->createProduct();
  sr_h_mito1->setSpecies(h_mito1->getId());
  sr_h_mito1->setId("sr_h_mito1");

  SpeciesReference* sr_oxaloacetate_mito1_1=malatedh_mito1->createProduct();
  sr_oxaloacetate_mito1_1->setSpecies(oxaloacetate_mito1->getId());
  sr_oxaloacetate_mito1_1->setId("sr_oxaloacetate_mito1_1");

  //Aspartate Aminotransferase
  Reaction* aspartateat_mito1=model->createReaction();
  aspartateat_mito1->setId("reaction_aspartateat_mito1");
  aspartateat_mito1->setName("aspartate aminotransferase");
  aspartateat_mito1->setReversible(false);

  SpeciesReference* sr_oxaloacetate_mito1_2=aspartateat_mito1->createReactant();
  sr_oxaloacetate_mito1_2->setSpecies(oxaloacetate_mito1->getId());
  sr_oxaloacetate_mito1_2->setId("sr_oxaloacetate_mito1_2");

  SpeciesReference* sr_glutamate_mito1=aspartateat_mito1->createReactant();
  sr_glutamate_mito1->setSpecies(glutamate_mito1->getId());
  sr_glutamate_mito1->setId("sr_glutamate_mito1");

  SpeciesReference* sr_aspartate_mito1=aspartateat_mito1->createProduct();
  sr_aspartate_mito1->setSpecies(aspartate_mito1->getId());
  sr_aspartate_mito1->setId("sr_aspartate_mito1");

  SpeciesReference* sr_aKetoglutarate_mito1=aspartateat_mito1->createProduct();
  sr_aKetoglutarate_mito1->setSpecies(aKetoglutarate_mito1->getId());
  sr_aKetoglutarate_mito1->setId("sr_aKetoglutarate_mito1");


  // Mito 2

  // Malate Dehydrogenase
  Reaction* malatedh_mito2=model->createReaction();
  malatedh_mito2->setId("reaction_malatedh_mito2");
  malatedh_mito2->setName("malate dehydrogenase");
  malatedh_mito2->setReversible(false);

  SpeciesReference* sr_malate_mito2=malatedh_mito2->createReactant();
  sr_malate_mito2->setSpecies(malate_mito2->getId());
  sr_malate_mito2->setId("sr_malate_mito2");

  SpeciesReference* sr_nad_mito2=malatedh_mito2->createReactant();
  sr_nad_mito2->setSpecies(nad_mito2->getId());
  sr_nad_mito2->setId("sr_nad_mito2");

  SpeciesReference* sr_nadh_mito2=malatedh_mito2->createProduct();
  sr_nadh_mito2->setSpecies(nadh_mito2->getId());
  sr_nadh_mito2->setId("sr_nadh_mito2");

  SpeciesReference* sr_h_mito2=malatedh_mito2->createProduct();
  sr_h_mito2->setSpecies(h_mito2->getId());
  sr_h_mito2->setId("sr_h_mito2");

  SpeciesReference* sr_oxaloacetate_mito2_1=malatedh_mito2->createProduct();
  sr_oxaloacetate_mito2_1->setSpecies(oxaloacetate_mito2->getId());
  sr_oxaloacetate_mito2_1->setId("sr_oxaloacetate_mito2_1");

  //Aspartate Aminotransferase
  Reaction* aspartateat_mito2=model->createReaction();
  aspartateat_mito2->setId("reaction_aspartateat_mito2");
  aspartateat_mito2->setName("aspartate aminotransferase");
  aspartateat_mito2->setReversible(false);

  SpeciesReference* sr_oxaloacetate_mito2_2=aspartateat_mito2->createReactant();
  sr_oxaloacetate_mito2_2->setSpecies(oxaloacetate_mito2->getId());
  sr_oxaloacetate_mito2_2->setId("sr_oxaloacetate_mito2_2");

  SpeciesReference* sr_glutamate_mito2=aspartateat_mito2->createReactant();
  sr_glutamate_mito2->setSpecies(glutamate_mito2->getId());
  sr_glutamate_mito2->setId("sr_glutamate_mito2");

  SpeciesReference* sr_aspartate_mito2=aspartateat_mito2->createProduct();
  sr_aspartate_mito2->setSpecies(aspartate_mito2->getId());
  sr_aspartate_mito2->setId("sr_aspartate_mito2");

  SpeciesReference* sr_aKetoglutarate_mito2=aspartateat_mito2->createProduct();
  sr_aKetoglutarate_mito2->setSpecies(aKetoglutarate_mito2->getId());
  sr_aKetoglutarate_mito2->setId("sr_aKetoglutarate_mito2");


  // Mito 3

  // Malate Dehydrogenase
  Reaction* malatedh_mito3=model->createReaction();
  malatedh_mito3->setId("reaction_malatedh_mito3");
  malatedh_mito3->setName("malate dehydrogenase");
  malatedh_mito3->setReversible(false);

  SpeciesReference* sr_malate_mito3=malatedh_mito3->createReactant();
  sr_malate_mito3->setSpecies(malate_mito3->getId());
  sr_malate_mito3->setId("sr_malate_mito3");

  SpeciesReference* sr_nad_mito3=malatedh_mito3->createReactant();
  sr_nad_mito3->setSpecies(nad_mito3->getId());
  sr_nad_mito3->setId("sr_nad_mito3");

  SpeciesReference* sr_nadh_mito3=malatedh_mito3->createProduct();
  sr_nadh_mito3->setSpecies(nadh_mito3->getId());
  sr_nadh_mito3->setId("sr_nadh_mito3");

  SpeciesReference* sr_h_mito3=malatedh_mito3->createProduct();
  sr_h_mito3->setSpecies(h_mito3->getId());
  sr_h_mito3->setId("sr_h_mito3");

  SpeciesReference* sr_oxaloacetate_mito3_1=malatedh_mito3->createProduct();
  sr_oxaloacetate_mito3_1->setSpecies(oxaloacetate_mito3->getId());
  sr_oxaloacetate_mito3_1->setId("sr_oxaloacetate_mito3_1");

  //Aspartate Aminotransferase
  Reaction* aspartateat_mito3=model->createReaction();
  aspartateat_mito3->setId("reaction_aspartateat_mito3");
  aspartateat_mito3->setName("aspartate aminotransferase");
  aspartateat_mito3->setReversible(false);

  SpeciesReference* sr_oxaloacetate_mito3_2=aspartateat_mito3->createReactant();
  sr_oxaloacetate_mito3_2->setSpecies(oxaloacetate_mito3->getId());
  sr_oxaloacetate_mito3_2->setId("sr_oxaloacetate_mito3_2");


  SpeciesReference* sr_glutamate_mito3_1=aspartateat_mito3->createReactant();
  sr_glutamate_mito3_1->setSpecies(glutamate_mito3->getId());
  sr_glutamate_mito3_1->setId("sr_glutamate_mito3_1");


  SpeciesReference* sr_aspartate_mito3_1=aspartateat_mito3->createProduct();
  sr_aspartate_mito3_1->setSpecies(aspartate_mito3->getId());
  sr_aspartate_mito3_1->setId("sr_aspartate_mito3_1");


  SpeciesReference* sr_aKetoglutarate_mito3_1=aspartateat_mito3->createProduct();
  sr_aKetoglutarate_mito3_1->setSpecies(aKetoglutarate_mito3->getId());
  sr_aKetoglutarate_mito3_1->setId("sr_aKetoglutarate_mito3_1");


  // aspartate carrier

  Reaction* aspartateCarrier=model->createReaction();
  aspartateCarrier->setId("aspartateCarrier");
  aspartateCarrier->setName("aspartate carrier");
  aspartateCarrier->setReversible(true);

  SpeciesReference* sr_glutamate_mito3_2=aspartateCarrier->createReactant();
  sr_glutamate_mito3_2->setSpecies(glutamate_mito3->getId());
  sr_glutamate_mito3_2->setId("sr_glutamate_mito3_2");

  SpeciesReference* sr_aspartate_cyt_2=aspartateCarrier->createReactant();
  sr_aspartate_cyt_2->setSpecies(aspartate_cyt->getId());
  sr_aspartate_cyt_2->setId("sr_aspartate_cyt_2");

  SpeciesReference* sr_glutamate_cyt_2=aspartateCarrier->createProduct();
  sr_glutamate_cyt_2->setSpecies(glutamate_cyt->getId());
  sr_glutamate_cyt_2->setId("sr_glutamate_cyt_2");

  SpeciesReference* sr_aspartate_mito3_2=aspartateCarrier->createProduct();
  sr_aspartate_mito3_2->setSpecies(aspartate_mito3->getId());
  sr_aspartate_mito3_2->setId("sr_aspartate_mito3_2");

  // malate carrier

  Reaction* malateCarrier=model->createReaction();
  malateCarrier->setId("malateCarrier");
  malateCarrier->setName("malate carrier");
  malateCarrier->setReversible(true);

  SpeciesReference* sr_aKetoglutarate_mito3_2=malateCarrier->createReactant();
  sr_aKetoglutarate_mito3_2->setSpecies(aKetoglutarate_mito3->getId());
  sr_aKetoglutarate_mito3_2->setId("sr_aKetoglutarate_mito3_2");

  SpeciesReference* sr_malate_cyt_2=malateCarrier->createReactant();
  sr_malate_cyt_2->setSpecies(malate_cyt->getId());
  sr_malate_cyt_2->setId("sr_malate_cyt_2");

  SpeciesReference* sr_aKetoglutarate_cyt_2=malateCarrier->createProduct();
  sr_aKetoglutarate_cyt_2->setSpecies(aKetoglutarate_cyt->getId());
  sr_aKetoglutarate_cyt_2->setId("sr_aKetoglutarate_cyt_2");

  SpeciesReference* sr_malate_mito3_2=malateCarrier->createProduct();
  sr_malate_mito3_2->setSpecies(malate_mito3->getId());
  sr_malate_mito3_2->setId("sr_malate_mito3_2");


  /////////// create the Layout
  LayoutModelPlugin *plugin = (LayoutModelPlugin*)model->getPlugin("layout");
  fail_unless(plugin != NULL);
  if (plugin == NULL) return;

  Layout* layout=plugin ->createLayout();

  layout->setId("Layout_1");
  Dimensions dim(&layoutns,2320.0,1000.0);
  layout->setDimensions(&dim);


  // create the CompartmentGlyph

  CompartmentGlyph* compartmentGlyph=layout->createCompartmentGlyph();
  compartmentGlyph->setId("CompartmentGlyph_1");
  compartmentGlyph->setCompartmentId(compartment->getId());
  BoundingBox bb=BoundingBox(&layoutns, "bb_compartment",10,10,2300,980);
  compartmentGlyph->setBoundingBox(&bb);

  TextGlyph* tg=layout->createTextGlyph();
  tg->setId("TextGlyph_Hepatocyte");
  tg->setOriginOfTextId(compartment->getId());
  bb=BoundingBox(&layoutns, "bb_tg_compartment",50,870,300,72);
  tg->setBoundingBox(&bb);
  tg->setGraphicalObjectId(compartmentGlyph->getId());

  CompartmentGlyph* mito1Glyph=layout->createCompartmentGlyph();
  mito1Glyph->setId("Mito1_Glyph");
  mito1Glyph->setCompartmentId(mito1->getId());
  bb=BoundingBox(&layoutns, "bb_mito1",100,100,300,100);
  mito1Glyph->setBoundingBox(&bb);

  tg=layout->createTextGlyph();
  tg->setId("TextGlyph_mito1");
  tg->setOriginOfTextId(mito1->getId());
  bb=BoundingBox(&layoutns, "bb_tg_mito1",110,110,280,72);
  tg->setBoundingBox(&bb);
  tg->setGraphicalObjectId(mito1Glyph->getId());


  CompartmentGlyph* mito2Glyph=layout->createCompartmentGlyph();
  mito2Glyph->setId("Mito2_Glyph");
  mito2Glyph->setCompartmentId(mito2->getId());
  bb=BoundingBox(&layoutns, "bb_mito2",200,650,300,100);
  mito2Glyph->setBoundingBox(&bb);

  tg=layout->createTextGlyph();
  tg->setId("TextGlyph_mito2");
  tg->setOriginOfTextId(mito2->getId());
  bb=BoundingBox(&layoutns, "bb_tg_mito2",210,660,280,72);
  tg->setBoundingBox(&bb);
  tg->setGraphicalObjectId(mito2Glyph->getId());

  CompartmentGlyph* mito3Glyph_2=layout->createCompartmentGlyph();
  mito3Glyph_2->setId("Mito3_Glyph_2");
  mito3Glyph_2->setCompartmentId(mito3->getId());
  bb=BoundingBox(&layoutns, "bb_mito3_2",1470,30,820,536);
  mito3Glyph_2->setBoundingBox(&bb);

  tg=layout->createTextGlyph();
  tg->setId("TextGlyph_mito3_2");
  tg->setOriginOfTextId(mito3->getId());
  bb=BoundingBox(&layoutns, "bb_tg_mito3_2",1475,35,200,72);
  tg->setBoundingBox(&bb);
  tg->setGraphicalObjectId(mito3Glyph_2->getId());




  // create the SpeciesGlyphs

  // Cytosol

  // Malate cyt
  SpeciesGlyph* speciesGlyph_malate_cyt=layout->createSpeciesGlyph();
  speciesGlyph_malate_cyt->setId("SpeciesGlyph_malate_cyt");
  speciesGlyph_malate_cyt->setSpeciesId(malate_cyt->getId());
  bb=BoundingBox(&layoutns, "bb_sg_malate_cyt",580,280,240,36);
  speciesGlyph_malate_cyt->setBoundingBox(&bb);

  tg=layout->createTextGlyph();
  tg->setId("TextGlyph_malate_cyt");
  bb=BoundingBox(&layoutns, "bb_tg_malatate_cyt",590,280,220,36);
  tg->setBoundingBox(&bb);
  tg->setOriginOfTextId(malate_cyt->getId());
  tg->setGraphicalObjectId(speciesGlyph_malate_cyt->getId());

  // Oxaloacetate cyt
  SpeciesGlyph* speciesGlyph_oxaloacetate_cyt=layout->createSpeciesGlyph();
  speciesGlyph_oxaloacetate_cyt->setId("SpeciesGlyph_oxaloacetate_cyt");
  speciesGlyph_oxaloacetate_cyt->setSpeciesId(oxaloacetate_cyt->getId());
  bb=BoundingBox(&layoutns, "bb_sg_oxaloacetate_cyt",580,480,240,36);
  speciesGlyph_oxaloacetate_cyt->setBoundingBox(&bb);

  tg=layout->createTextGlyph();
  tg->setId("TextGlyph_oxaloacetate_cyt");
  bb=BoundingBox(&layoutns, "bb_tg_oxaloacetate_cyt",590,480,220,36);
  tg->setBoundingBox(&bb);
  tg->setOriginOfTextId(oxaloacetate_cyt->getId());
  tg->setGraphicalObjectId(speciesGlyph_oxaloacetate_cyt->getId());

  // Aspartate cyt
  SpeciesGlyph* speciesGlyph_aspartate_cyt=layout->createSpeciesGlyph();
  speciesGlyph_aspartate_cyt->setId("SpeciesGlyph_aspartate_cyt");
  speciesGlyph_aspartate_cyt->setSpeciesId(aspartate_cyt->getId());
  bb=BoundingBox(&layoutns, "bb_sg_aspartate_cyt",580,680,240,36);
  speciesGlyph_aspartate_cyt->setBoundingBox(&bb);

  tg=layout->createTextGlyph();
  tg->setId("TextGlyph_aspartate_cyt");
  bb=BoundingBox(&layoutns, "bb_tg_aspartate_cyt",590,680,220,36);
  tg->setBoundingBox(&bb);
  tg->setOriginOfTextId(aspartate_cyt->getId());
  tg->setGraphicalObjectId(speciesGlyph_aspartate_cyt->getId());

  // Glutamate cyt
  SpeciesGlyph* speciesGlyph_glutamate_cyt=layout->createSpeciesGlyph();
  speciesGlyph_glutamate_cyt->setId("SpeciesGlyph_glutamate_cyt");
  speciesGlyph_glutamate_cyt->setSpeciesId(glutamate_cyt->getId());
  bb=BoundingBox(&layoutns, "bb_sg_glutamate_cyt",800,610,240,36);
  speciesGlyph_glutamate_cyt->setBoundingBox(&bb);

  tg=layout->createTextGlyph();
  tg->setId("TextGlyph_glutamate_cyt");
  bb=BoundingBox(&layoutns, "bb_tg_glutamate_cyt",810,610,220,36);
  tg->setBoundingBox(&bb);
  tg->setOriginOfTextId(glutamate_cyt->getId());
  tg->setGraphicalObjectId(speciesGlyph_glutamate_cyt->getId());

  // alpha-Ketoglutarate cyt
  SpeciesGlyph* speciesGlyph_aKetoglutarate_cyt=layout->createSpeciesGlyph();
  speciesGlyph_aKetoglutarate_cyt->setId("SpeciesGlyph_aKetoglutarate_cyt");
  speciesGlyph_aKetoglutarate_cyt->setSpeciesId(aKetoglutarate_cyt->getId());
  bb=BoundingBox(&layoutns, "bb_sg_aKetoglutarate_cyt",860,500,280,36);
  speciesGlyph_aKetoglutarate_cyt->setBoundingBox(&bb);

  tg=layout->createTextGlyph();
  tg->setId("TextGlyph_aKetoglutarate_cyt");
  bb=BoundingBox(&layoutns, "bb_tg_aKetoglutarate_cyt",870,500,260,36);
  tg->setBoundingBox(&bb);
  tg->setOriginOfTextId(aKetoglutarate_cyt->getId());
  tg->setGraphicalObjectId(speciesGlyph_aKetoglutarate_cyt->getId());

  // NAD+ cyt
  SpeciesGlyph* speciesGlyph_nad_cyt=layout->createSpeciesGlyph();
  speciesGlyph_nad_cyt->setId("SpeciesGlyph_nad_cyt");
  speciesGlyph_nad_cyt->setSpeciesId(nad_cyt->getId());
  bb=BoundingBox(&layoutns, "bb_sg_nad_cyt",520,350,100,24);
  speciesGlyph_nad_cyt->setBoundingBox(&bb);

  tg=layout->createTextGlyph();
  tg->setId("TextGlyph_nad_cyt");
  bb=BoundingBox(&layoutns, "bb_tg_nad_cyt",525,350,80,24);
  tg->setBoundingBox(&bb);
  tg->setOriginOfTextId(nad_cyt->getId());
  tg->setGraphicalObjectId(speciesGlyph_nad_cyt->getId());

  // NADH cyt
  SpeciesGlyph* speciesGlyph_nadh_cyt=layout->createSpeciesGlyph();
  speciesGlyph_nadh_cyt->setId("SpeciesGlyph_nadh_cyt");
  speciesGlyph_nadh_cyt->setSpeciesId(nadh_cyt->getId());
  bb=BoundingBox(&layoutns, "bb_sg_nadh_cyt",520,430,100,24);
  speciesGlyph_nadh_cyt->setBoundingBox(&bb);

  tg=layout->createTextGlyph();
  tg->setId("TextGlyph_nadh_cyt");
  bb=BoundingBox(&layoutns, "bb_tg_nadh_cyt",525,430,80,24);
  tg->setBoundingBox(&bb);
  tg->setOriginOfTextId(nadh_cyt->getId());
  tg->setGraphicalObjectId(speciesGlyph_nadh_cyt->getId());

  // H+ cyt
  SpeciesGlyph* speciesGlyph_h_cyt=layout->createSpeciesGlyph();
  speciesGlyph_h_cyt->setId("SpeciesGlyph_h_cyt");
  speciesGlyph_h_cyt->setSpeciesId(h_cyt->getId());
  bb=BoundingBox(&layoutns, "bb_sg_h_cyt",430,430,40,24);
  speciesGlyph_h_cyt->setBoundingBox(&bb);

  tg=layout->createTextGlyph();
  tg->setId("TextGlyph_h_cyt");
  bb=BoundingBox(&layoutns, "bb_tg_h_cyt",435,430,30,24);
  tg->setBoundingBox(&bb);
  tg->setOriginOfTextId(h_cyt->getId());
  tg->setGraphicalObjectId(speciesGlyph_h_cyt->getId());


  // create the ReactionGlyphs

  ReactionGlyph* rg_malatedh_cyt=layout->createReactionGlyph();
  rg_malatedh_cyt->setId("rg_malatedh_cyt");
  rg_malatedh_cyt->setReactionId(malatedh_cyt->getId());

  Curve* curve=rg_malatedh_cyt->getCurve();
  LineSegment* ls=curve->createLineSegment();
  Point p(&layoutns, 700,381);
  ls->setStart(&p);
  p=Point(&layoutns, 700,415);
  ls->setEnd(&p);

  tg=layout->createTextGlyph();
  tg->setId("tg_rg_malaltedh_cyt");
  bb=BoundingBox(&layoutns, "bb_tg_rg_malaltedh_cyt",700,385,210,24);
  tg->setBoundingBox(&bb);
  tg->setOriginOfTextId(malatedh_cyt->getId());
  tg->setGraphicalObjectId(rg_malatedh_cyt->getId());


  ReactionGlyph* rg_aspartateat_cyt=layout->createReactionGlyph();
  rg_aspartateat_cyt->setId("rg_aspartateat_cyt");
  rg_aspartateat_cyt->setReactionId(aspartateat_cyt->getId());

  curve=rg_aspartateat_cyt->getCurve();
  ls=curve->createLineSegment();
  p=Point(&layoutns, 700,581);
  ls->setStart(&p);
  p=Point(&layoutns, 700,615);
  ls->setEnd(&p);

  tg=layout->createTextGlyph();
  tg->setId("tg_rg_aspartateat_cyt");
  bb=BoundingBox(&layoutns, "bb_tg_rg_aspartateat_cyt",440,585,260,24);
  tg->setBoundingBox(&bb);
  tg->setOriginOfTextId(aspartateat_cyt->getId());
  tg->setGraphicalObjectId(rg_aspartateat_cyt->getId());



  // add the SpeciesReferenceGlyphs

  SpeciesReferenceGlyph* srg_malate_cyt_1=rg_malatedh_cyt->createSpeciesReferenceGlyph();
  srg_malate_cyt_1->setId("srg_malate_cyt_1");
  srg_malate_cyt_1->setSpeciesGlyphId(speciesGlyph_malate_cyt->getId());
  srg_malate_cyt_1->setSpeciesReferenceId(sr_malate_cyt->getId());
  srg_malate_cyt_1->setRole(SPECIES_ROLE_SUBSTRATE);

  ls=srg_malate_cyt_1->createLineSegment();
  p=Point(&layoutns, 700,381);
  ls->setStart(&p);
  p=Point(&layoutns, 700,316);
  ls->setEnd(&p);

  SpeciesReferenceGlyph* srg_nad_cyt=rg_malatedh_cyt->createSpeciesReferenceGlyph();
  srg_nad_cyt->setId("srg_nad_cyt");
  srg_nad_cyt->setSpeciesGlyphId(speciesGlyph_nad_cyt->getId());
  srg_nad_cyt->setSpeciesReferenceId(sr_nad_cyt->getId());
  srg_nad_cyt->setRole(SPECIES_ROLE_SUBSTRATE);

  CubicBezier* cb=srg_nad_cyt->createCubicBezier();
  p=Point(&layoutns, 700,381);
  cb->setStart(&p);
  p=Point(&layoutns, 700,362);
  cb->setBasePoint1(&p);
  p=Point(&layoutns, 700,362);
  cb->setBasePoint2(&p);
  p=Point(&layoutns, 620,362);
  cb->setEnd(&p);

  SpeciesReferenceGlyph* srg_oxaloacetate_cyt_1=rg_malatedh_cyt->createSpeciesReferenceGlyph();
  srg_oxaloacetate_cyt_1->setId("srg_oxaloacetate_cyt_1");
  srg_oxaloacetate_cyt_1->setSpeciesGlyphId(speciesGlyph_oxaloacetate_cyt->getId());
  srg_oxaloacetate_cyt_1->setSpeciesReferenceId(sr_oxaloacetate_cyt_1->getId());
  srg_oxaloacetate_cyt_1->setRole(SPECIES_ROLE_PRODUCT);

  curve=srg_oxaloacetate_cyt_1->getCurve();
  ls=curve->createLineSegment();
  p=Point(&layoutns, 700,415);
  ls->setStart(&p);
  p=Point(&layoutns, 700,480);
  ls->setEnd(&p);

  SpeciesReferenceGlyph* srg_nadh_cyt=rg_malatedh_cyt->createSpeciesReferenceGlyph();
  srg_nadh_cyt->setId("srg_nadh_cyt");
  srg_nadh_cyt->setSpeciesGlyphId(speciesGlyph_nadh_cyt->getId());
  srg_nadh_cyt->setSpeciesReferenceId(sr_nadh_cyt->getId());
  srg_nadh_cyt->setRole(SPECIES_ROLE_PRODUCT);

  cb=srg_nadh_cyt->createCubicBezier();
  p=Point(&layoutns, 700,415);
  cb->setStart(&p);
  p=Point(&layoutns, 700,442);
  cb->setBasePoint1(&p);
  p=Point(&layoutns, 700,442);
  cb->setBasePoint2(&p);
  p=Point(&layoutns, 620,442);
  cb->setEnd(&p);

  SpeciesReferenceGlyph* srg_h_cyt=rg_malatedh_cyt->createSpeciesReferenceGlyph();
  srg_h_cyt->setId("srg_h_cyt");
  srg_h_cyt->setSpeciesGlyphId(speciesGlyph_h_cyt->getId());
  srg_h_cyt->setSpeciesReferenceId(sr_h_cyt->getId());
  srg_h_cyt->setRole(SPECIES_ROLE_PRODUCT);

  cb=srg_h_cyt->createCubicBezier();
  p=Point(&layoutns, 700,415); 
  cb->setStart(&p);
  p=Point(&layoutns, 570,415);
  cb->setBasePoint1(&p);
  p=Point(&layoutns, 570,415);
  cb->setBasePoint2(&p);
  p=Point(&layoutns, 470,430);
  cb->setEnd(&p);

  SpeciesReferenceGlyph* srg_oxaloacetate_cyt_2=rg_aspartateat_cyt->createSpeciesReferenceGlyph();
  srg_oxaloacetate_cyt_2->setId("srg_oxaloacetate_cyt_2");
  srg_oxaloacetate_cyt_2->setSpeciesGlyphId(speciesGlyph_oxaloacetate_cyt->getId());
  srg_oxaloacetate_cyt_2->setSpeciesReferenceId(sr_oxaloacetate_cyt_2->getId());
  srg_oxaloacetate_cyt_2->setRole(SPECIES_ROLE_SUBSTRATE);

  curve=srg_oxaloacetate_cyt_2->getCurve();
  ls=curve->createLineSegment();
  p=Point(&layoutns, 700,581);
  ls->setStart(&p);
  p=Point(&layoutns, 700,516);
  ls->setEnd(&p);

  SpeciesReferenceGlyph* srg_glutamate_cyt_1=rg_aspartateat_cyt->createSpeciesReferenceGlyph();
  srg_glutamate_cyt_1->setId("srg_glutamate_cyt_1");
  srg_glutamate_cyt_1->setSpeciesGlyphId(speciesGlyph_glutamate_cyt->getId());
  srg_glutamate_cyt_1->setSpeciesReferenceId(sr_glutamate_cyt_1->getId());
  srg_glutamate_cyt_1->setRole(SPECIES_ROLE_SUBSTRATE);

  curve=srg_glutamate_cyt_1->getCurve();
  cb=curve->createCubicBezier();
  p=Point(&layoutns, 700,581);
  cb->setStart(&p);
  p=Point(&layoutns, 750,581);
  cb->setBasePoint1(&p);
  p=Point(&layoutns, 750,628);
  cb->setBasePoint2(&p);
  p=Point(&layoutns, 800,628);
  cb->setEnd(&p);

  SpeciesReferenceGlyph* srg_aspartate_cyt_1=rg_aspartateat_cyt->createSpeciesReferenceGlyph();
  srg_aspartate_cyt_1->setId("srg_aspartate_cyt_1");
  srg_aspartate_cyt_1->setSpeciesGlyphId(speciesGlyph_aspartate_cyt->getId());
  srg_aspartate_cyt_1->setSpeciesReferenceId(sr_aspartate_cyt_1->getId());
  srg_aspartate_cyt_1->setRole(SPECIES_ROLE_PRODUCT);

  curve=srg_aspartate_cyt_1->getCurve();
  ls=curve->createLineSegment();
  p=Point(&layoutns, 700,615);
  ls->setStart(&p);
  p=Point(&layoutns, 700,680);
  ls->setEnd(&p);

  SpeciesReferenceGlyph* srg_aKetoglutarate_cyt_1=rg_aspartateat_cyt->createSpeciesReferenceGlyph();
  srg_aKetoglutarate_cyt_1->setId("srg_aKetoglutaratecyt_1");
  srg_aKetoglutarate_cyt_1->setSpeciesGlyphId(speciesGlyph_aKetoglutarate_cyt->getId());
  srg_aKetoglutarate_cyt_1->setSpeciesReferenceId(sr_aKetoglutarate_cyt_1->getId());
  srg_aKetoglutarate_cyt_1->setRole(SPECIES_ROLE_PRODUCT);

  curve=srg_aKetoglutarate_cyt_1->getCurve();
  cb=curve->createCubicBezier();
  p=Point(&layoutns, 700,615);
  cb->setStart(&p);
  p=Point(&layoutns, 790,615);
  cb->setBasePoint1(&p);
  p=Point(&layoutns, 790,515);
  cb->setBasePoint2(&p);
  p=Point(&layoutns, 860,515);
  cb->setEnd(&p);


  // Malate mito3
  SpeciesGlyph* speciesGlyph_malate_mito3=layout->createSpeciesGlyph();
  speciesGlyph_malate_mito3->setId("SpeciesGlyph_malate_mito3");
  speciesGlyph_malate_mito3->setSpeciesId(malate_mito3->getId());
  bb=BoundingBox(&layoutns, "bb_sg_malate_mito3",1850,80,240,36);
  speciesGlyph_malate_mito3->setBoundingBox(&bb);

  tg=layout->createTextGlyph();
  tg->setId("TextGlyph_malate_mito3");
  bb=BoundingBox(&layoutns, "bb_tg_malatate_mito3",1860,80,220,36);
  tg->setBoundingBox(&bb);
  tg->setOriginOfTextId(malate_mito3->getId());
  tg->setGraphicalObjectId(speciesGlyph_malate_mito3->getId());

  // Oxaloacetate mito3
  SpeciesGlyph* speciesGlyph_oxaloacetate_mito3=layout->createSpeciesGlyph();
  speciesGlyph_oxaloacetate_mito3->setId("SpeciesGlyph_oxaloacetate_mito3");
  speciesGlyph_oxaloacetate_mito3->setSpeciesId(oxaloacetate_mito3->getId());
  bb=BoundingBox(&layoutns, "bb_sg_oxaloacetate_mito3",1850,280,240,36);
  speciesGlyph_oxaloacetate_mito3->setBoundingBox(&bb);

  tg=layout->createTextGlyph();
  tg->setId("TextGlyph_oxaloacetate_mito3");
  bb=BoundingBox(&layoutns, "bb_tg_oxaloacetate_mito3",1860,280,220,36);
  tg->setBoundingBox(&bb);
  tg->setOriginOfTextId(oxaloacetate_mito3->getId());
  tg->setGraphicalObjectId(speciesGlyph_oxaloacetate_mito3->getId());

  // Aspartate mito3
  SpeciesGlyph* speciesGlyph_aspartate_mito3=layout->createSpeciesGlyph();
  speciesGlyph_aspartate_mito3->setId("SpeciesGlyph_aspartate_mito3");
  speciesGlyph_aspartate_mito3->setSpeciesId(aspartate_mito3->getId());
  bb=BoundingBox(&layoutns, "bb_sg_aspartate_mito3",1850,480,240,36);
  speciesGlyph_aspartate_mito3->setBoundingBox(&bb);

  tg=layout->createTextGlyph();
  tg->setId("TextGlyph_aspartate_mito3");
  bb=BoundingBox(&layoutns, "bb_tg_aspartate_mito3",1860,480,220,36);
  tg->setBoundingBox(&bb);
  tg->setOriginOfTextId(aspartate_mito3->getId());
  tg->setGraphicalObjectId(speciesGlyph_aspartate_mito3->getId());

  // Glutamate mito3
  SpeciesGlyph* speciesGlyph_glutamate_mito3=layout->createSpeciesGlyph();
  speciesGlyph_glutamate_mito3->setId("SpeciesGlyph_glutamate_mito3");
  speciesGlyph_glutamate_mito3->setSpeciesId(glutamate_mito3->getId());
  bb=BoundingBox(&layoutns, "bb_sg_glutamate_mito3",1550,430,240,36);
  speciesGlyph_glutamate_mito3->setBoundingBox(&bb);

  tg=layout->createTextGlyph();
  tg->setId("TextGlyph_glutamate_mito3");
  bb=BoundingBox(&layoutns, "bb_tg_glutamate_mito3",1560,430,220,36);
  tg->setBoundingBox(&bb);
  tg->setOriginOfTextId(glutamate_mito3->getId());
  tg->setGraphicalObjectId(speciesGlyph_glutamate_mito3->getId());

  // alpha-Ketoglutarate mito3
  SpeciesGlyph* speciesGlyph_aKetoglutarate_mito3=layout->createSpeciesGlyph();
  speciesGlyph_aKetoglutarate_mito3->setId("SpeciesGlyph_aKetoglutarate_mito3");
  speciesGlyph_aKetoglutarate_mito3->setSpeciesId(aKetoglutarate_mito3->getId());
  bb=BoundingBox(&layoutns, "bb_sg_aKetoglutarate_mito3",1530,300,280,36);
  speciesGlyph_aKetoglutarate_mito3->setBoundingBox(&bb);

  tg=layout->createTextGlyph();
  tg->setId("TextGlyph_aKetoglutarate_mito3");
  bb=BoundingBox(&layoutns, "bb_tg_aKetoglutarate_mito3",1540,300,260,36);
  tg->setBoundingBox(&bb);
  tg->setOriginOfTextId(aKetoglutarate_mito3->getId());
  tg->setGraphicalObjectId(speciesGlyph_aKetoglutarate_mito3->getId());

  // NAD+ mito3
  SpeciesGlyph* speciesGlyph_nad_mito3=layout->createSpeciesGlyph();
  speciesGlyph_nad_mito3->setId("SpeciesGlyph_nad_mito3");
  speciesGlyph_nad_mito3->setSpeciesId(nad_mito3->getId());
  bb=BoundingBox(&layoutns, "bb_sg_nad_mito3",2050,150,100,24);
  speciesGlyph_nad_mito3->setBoundingBox(&bb);

  tg=layout->createTextGlyph();
  tg->setId("TextGlyph_nad_mito3");
  bb=BoundingBox(&layoutns, "bb_tg_nad_mito3",2055,150,80,24);
  tg->setBoundingBox(&bb);
  tg->setOriginOfTextId(nad_mito3->getId());
  tg->setGraphicalObjectId(speciesGlyph_nad_mito3->getId());

  // NADH mito3
  SpeciesGlyph* speciesGlyph_nadh_mito3=layout->createSpeciesGlyph();
  speciesGlyph_nadh_mito3->setId("SpeciesGlyph_nadh_mito3");
  speciesGlyph_nadh_mito3->setSpeciesId(nadh_mito3->getId());
  bb=BoundingBox(&layoutns, "bb_sg_nadh_mito3",2050,230,100,24);
  speciesGlyph_nadh_mito3->setBoundingBox(&bb);

  tg=layout->createTextGlyph();
  tg->setId("TextGlyph_nadh_mito3");
  bb=BoundingBox(&layoutns, "bb_tg_nadh_mito3",2055,230,80,24);
  tg->setBoundingBox(&bb);
  tg->setOriginOfTextId(nadh_mito3->getId());
  tg->setGraphicalObjectId(speciesGlyph_nadh_mito3->getId());

  // H+ mito3
  SpeciesGlyph* speciesGlyph_h_mito3=layout->createSpeciesGlyph();
  speciesGlyph_h_mito3->setId("SpeciesGlyph_h_mito3");
  speciesGlyph_h_mito3->setSpeciesId(h_mito3->getId());
  bb=BoundingBox(&layoutns, "bb_sg_h_mito3",2200,230,40,24);
  speciesGlyph_h_mito3->setBoundingBox(&bb);

  tg=layout->createTextGlyph();
  tg->setId("TextGlyph_h_mito3");
  bb=BoundingBox(&layoutns, "bb_tg_h_mito3",2205,230,30,24);
  tg->setBoundingBox(&bb);
  tg->setOriginOfTextId(h_mito3->getId());
  tg->setGraphicalObjectId(speciesGlyph_h_mito3->getId());


  // create the ReactionGlyphs

  ReactionGlyph* rg_malatedh_mito3=layout->createReactionGlyph();
  rg_malatedh_mito3->setId("rg_malatedh_mito3");
  rg_malatedh_mito3->setReactionId(malatedh_mito3->getId());

  curve=rg_malatedh_mito3->getCurve();
  ls=curve->createLineSegment();
  p=Point(&layoutns, 1970,181);
  ls->setStart(&p);
  p=Point(&layoutns, 1970,215);
  ls->setEnd(&p);

  tg=layout->createTextGlyph();
  tg->setId("tg_rg_malatedh_mito3");
  bb=BoundingBox(&layoutns, "bb_tg_rg_malatedh_mito3",1740,185,220,24);
  tg->setBoundingBox(&bb);
  tg->setOriginOfTextId(malatedh_mito3->getId());
  tg->setGraphicalObjectId(rg_malatedh_mito3->getId());

  ReactionGlyph* rg_aspartateat_mito3=layout->createReactionGlyph();
  rg_aspartateat_mito3->setId("rg_aspartateat_mito3");
  rg_aspartateat_mito3->setReactionId(aspartateat_mito3->getId());

  curve=rg_aspartateat_mito3->getCurve();
  ls=curve->createLineSegment();
  p=Point(&layoutns, 1970,381);
  ls->setStart(&p);
  p=Point(&layoutns, 1970,415);
  ls->setEnd(&p);

  tg=layout->createTextGlyph();
  tg->setId("tg_rg_aspartateat_mito3");
  bb=BoundingBox(&layoutns, "bb_tg_rg_aspartateat_mito3",1970,385,260,24);
  tg->setBoundingBox(&bb);
  tg->setOriginOfTextId(aspartateat_mito3->getId());
  tg->setGraphicalObjectId(rg_aspartateat_mito3->getId());


  // add the SpeciesReferenceGlyphs

  SpeciesReferenceGlyph* srg_malate_mito3_1=rg_malatedh_mito3->createSpeciesReferenceGlyph();
  srg_malate_mito3_1->setId("srg_malate_mito3_1");
  srg_malate_mito3_1->setSpeciesGlyphId(speciesGlyph_malate_mito3->getId());
  srg_malate_mito3_1->setSpeciesReferenceId(sr_malate_mito3->getId());
  srg_malate_mito3_1->setRole(SPECIES_ROLE_SUBSTRATE);

  ls=srg_malate_mito3_1->createLineSegment();
  p=Point(&layoutns, 1970,181);
  ls->setStart(&p);
  p=Point(&layoutns, 1970,116);
  ls->setEnd(&p);

  SpeciesReferenceGlyph* srg_nad_mito3=rg_malatedh_mito3->createSpeciesReferenceGlyph();
  srg_nad_mito3->setId("srg_nad_mito3");
  srg_nad_mito3->setSpeciesGlyphId(speciesGlyph_nad_mito3->getId());
  srg_nad_mito3->setSpeciesReferenceId(sr_nad_mito3->getId());
  srg_nad_mito3->setRole(SPECIES_ROLE_SUBSTRATE);

  cb=srg_nad_mito3->createCubicBezier();
  p=Point(&layoutns, 1970,181);
  cb->setStart(&p);
  p=Point(&layoutns, 1970,162);
  cb->setBasePoint1(&p);
  p=Point(&layoutns, 1970,162);
  cb->setBasePoint2(&p);
  p=Point(&layoutns, 2050,162);
  cb->setEnd(&p);

  SpeciesReferenceGlyph* srg_oxaloacetate_mito3_1=rg_malatedh_mito3->createSpeciesReferenceGlyph();
  srg_oxaloacetate_mito3_1->setId("srg_oxaloacetate_mito3_1");
  srg_oxaloacetate_mito3_1->setSpeciesGlyphId(speciesGlyph_oxaloacetate_mito3->getId());
  srg_oxaloacetate_mito3_1->setSpeciesReferenceId(sr_oxaloacetate_mito3_1->getId());
  srg_oxaloacetate_mito3_1->setRole(SPECIES_ROLE_PRODUCT);

  curve=srg_oxaloacetate_mito3_1->getCurve();
  ls=curve->createLineSegment();
  p=Point(&layoutns, 1970,215);
  ls->setStart(&p);
  p=Point(&layoutns, 1970,280);
  ls->setEnd(&p);

  SpeciesReferenceGlyph* srg_nadh_mito3=rg_malatedh_mito3->createSpeciesReferenceGlyph();
  srg_nadh_mito3->setId("srg_nadh_mito3");
  srg_nadh_mito3->setSpeciesGlyphId(speciesGlyph_nadh_mito3->getId());
  srg_nadh_mito3->setSpeciesReferenceId(sr_nadh_mito3->getId());
  srg_nadh_mito3->setRole(SPECIES_ROLE_PRODUCT);

  cb=srg_nadh_mito3->createCubicBezier();
  p=Point(&layoutns, 1970,215);
  cb->setStart(&p);
  p=Point(&layoutns, 1970,242);
  cb->setBasePoint1(&p);
  p=Point(&layoutns, 1970,242);
  cb->setBasePoint2(&p);
  p=Point(&layoutns, 2050,242);
  cb->setEnd(&p);

  SpeciesReferenceGlyph* srg_h_mito3=rg_malatedh_mito3->createSpeciesReferenceGlyph();
  srg_h_mito3->setId("srg_h_mito3");
  srg_h_mito3->setSpeciesGlyphId(speciesGlyph_h_mito3->getId());
  srg_h_mito3->setSpeciesReferenceId(sr_h_mito3->getId());
  srg_h_mito3->setRole(SPECIES_ROLE_PRODUCT);

  cb=srg_h_mito3->createCubicBezier();
  p=Point(&layoutns, 1970,215);
  cb->setStart(&p);
  p=Point(&layoutns, 2100,215);
  cb->setBasePoint1(&p);
  p=Point(&layoutns, 2100,215);
  cb->setBasePoint2(&p);
  p=Point(&layoutns, 2200,230);
  cb->setEnd(&p);

  SpeciesReferenceGlyph* srg_oxaloacetate_mito3_2=rg_aspartateat_mito3->createSpeciesReferenceGlyph();
  srg_oxaloacetate_mito3_2->setId("srg_oxaloacetate_mito3_2");
  srg_oxaloacetate_mito3_2->setSpeciesGlyphId(speciesGlyph_oxaloacetate_mito3->getId());
  srg_oxaloacetate_mito3_2->setSpeciesReferenceId(sr_oxaloacetate_mito3_2->getId());
  srg_oxaloacetate_mito3_2->setRole(SPECIES_ROLE_SUBSTRATE);

  curve=srg_oxaloacetate_mito3_2->getCurve();
  ls=curve->createLineSegment();
  p=Point(&layoutns, 1970,381);
  ls->setStart(&p);
  p=Point(&layoutns, 1970,316);
  ls->setEnd(&p);

  SpeciesReferenceGlyph* srg_glutamate_mito3_1=rg_aspartateat_mito3->createSpeciesReferenceGlyph();
  srg_glutamate_mito3_1->setId("srg_glutamate_mito3_1");
  srg_glutamate_mito3_1->setSpeciesGlyphId(speciesGlyph_glutamate_mito3->getId());
  srg_glutamate_mito3_1->setSpeciesReferenceId(sr_glutamate_mito3_1->getId());
  srg_glutamate_mito3_1->setRole(SPECIES_ROLE_SUBSTRATE);

  curve=srg_glutamate_mito3_1->getCurve();
  cb=curve->createCubicBezier();
  p=Point(&layoutns, 1970,381);
  cb->setStart(&p);
  p=Point(&layoutns, 1880,381);
  cb->setBasePoint1(&p);
  p=Point(&layoutns, 1880,448);
  cb->setBasePoint2(&p);
  p=Point(&layoutns, 1790,448);
  cb->setEnd(&p);

  SpeciesReferenceGlyph* srg_aspartate_mito3_1=rg_aspartateat_mito3->createSpeciesReferenceGlyph();
  srg_aspartate_mito3_1->setId("srg_aspartate_mito3_1");
  srg_aspartate_mito3_1->setSpeciesGlyphId(speciesGlyph_aspartate_mito3->getId());
  srg_aspartate_mito3_1->setSpeciesReferenceId(sr_aspartate_mito3_1->getId());
  srg_aspartate_mito3_1->setRole(SPECIES_ROLE_PRODUCT);

  curve=srg_aspartate_mito3_1->getCurve();
  ls=curve->createLineSegment();
  p=Point(&layoutns, 1970,415);
  ls->setStart(&p);
  p=Point(&layoutns, 1970,480);
  ls->setEnd(&p);

  SpeciesReferenceGlyph* srg_aKetoglutarate_mito3_1=rg_aspartateat_mito3->createSpeciesReferenceGlyph();
  srg_aKetoglutarate_mito3_1->setId("srg_aKetoglutaratemito3_1");
  srg_aKetoglutarate_mito3_1->setSpeciesGlyphId(speciesGlyph_aKetoglutarate_mito3->getId());
  srg_aKetoglutarate_mito3_1->setSpeciesReferenceId(sr_aKetoglutarate_mito3_1->getId());
  srg_aKetoglutarate_mito3_1->setRole(SPECIES_ROLE_PRODUCT);

  curve=srg_aKetoglutarate_mito3_1->getCurve();
  cb=curve->createCubicBezier();
  p=Point(&layoutns, 1970,415);
  cb->setStart(&p);
  p=Point(&layoutns, 1880,415);
  cb->setBasePoint1(&p);
  p=Point(&layoutns, 1880,315);
  cb->setBasePoint2(&p);
  p=Point(&layoutns, 1810,315);
  cb->setEnd(&p);

  // add the transport reaction glyphs

  ReactionGlyph* rg_aspartateCarrier=layout->createReactionGlyph();
  rg_aspartateCarrier->setId("rg_aspartateCarrier");
  rg_aspartateCarrier->setReactionId(aspartateCarrier->getId());

  curve=rg_aspartateCarrier->getCurve();
  ls=curve->createLineSegment();
  p=Point(&layoutns, 1420,530);
  ls->setStart(&p);
  p=Point(&layoutns, 1360,550);
  ls->setEnd(&p);

  tg=layout->createTextGlyph();
  tg->setId("tg_rg_aspartateCarrier");
  bb=BoundingBox(&layoutns, "bb_tg_rg_aspartateCarrier",1380,500,160,24);
  tg->setBoundingBox(&bb);
  tg->setOriginOfTextId(aspartateCarrier->getId());
  tg->setGraphicalObjectId(rg_aspartateCarrier->getId());


  ReactionGlyph* rg_malateCarrier=layout->createReactionGlyph();
  rg_malateCarrier->setId("rg_malateCarrier");
  rg_malateCarrier->setReactionId(malateCarrier->getId());

  curve=rg_malateCarrier->getCurve();
  ls=curve->createLineSegment();
  p=Point(&layoutns, 1420,320);
  ls->setStart(&p);
  p=Point(&layoutns, 1360,340);
  ls->setEnd(&p);

  tg=layout->createTextGlyph();
  tg->setId("tg_rg_malateCarrier");
  bb=BoundingBox( &layoutns, "bb_tg_rg_malateCarrier",1360,330,140,24);
  tg->setBoundingBox(&bb);
  tg->setOriginOfTextId(malateCarrier->getId());
  tg->setGraphicalObjectId(rg_malateCarrier->getId());



  // add the SpeciesReferenceGlyphs for the transporters

  SpeciesReferenceGlyph* srg_aKetoglutarate_mito3_2=rg_malateCarrier->createSpeciesReferenceGlyph();
  srg_aKetoglutarate_mito3_2->setId("srg_aKetoglutarate_mito3_2");
  srg_aKetoglutarate_mito3_2->setSpeciesGlyphId(speciesGlyph_aKetoglutarate_mito3->getId());
  srg_aKetoglutarate_mito3_2->setSpeciesReferenceId(sr_aKetoglutarate_mito3_2->getId());
  srg_aKetoglutarate_mito3_2->setRole(SPECIES_ROLE_SUBSTRATE);

  curve=srg_aKetoglutarate_mito3_2->getCurve();
  ls=curve->createLineSegment();
  p=Point(&layoutns, 1420,320);
  ls->setStart(&p);
  p=Point(&layoutns, 1530,318);
  ls->setEnd(&p);


  SpeciesReferenceGlyph* srg_aKetoglutarate_cyt_2=rg_malateCarrier->createSpeciesReferenceGlyph();
  srg_aKetoglutarate_cyt_2->setId("srg_aKetoglutarate_cyt_2");
  srg_aKetoglutarate_cyt_2->setSpeciesGlyphId(speciesGlyph_aKetoglutarate_cyt->getId());
  srg_aKetoglutarate_cyt_2->setSpeciesReferenceId(sr_aKetoglutarate_cyt_2->getId());
  srg_aKetoglutarate_cyt_2->setRole(SPECIES_ROLE_PRODUCT);

  curve=srg_aKetoglutarate_cyt_2->getCurve();
  ls=curve->createLineSegment();
  p=Point(&layoutns, 1360,340);
  ls->setStart(&p);
  p=Point(&layoutns, 1140,518);
  ls->setEnd(&p);


  SpeciesReferenceGlyph* srg_malate_cyt_2=rg_malateCarrier->createSpeciesReferenceGlyph();
  srg_malate_cyt_2->setId("srg_malate_cyt_2");
  srg_malate_cyt_2->setSpeciesGlyphId(speciesGlyph_malate_cyt->getId());
  srg_malate_cyt_2->setSpeciesReferenceId(sr_malate_cyt_2->getId());
  srg_malate_cyt_2->setRole(SPECIES_ROLE_SUBSTRATE);

  curve=srg_malate_cyt_2->getCurve();
  cb=curve->createCubicBezier();
  p=Point(&layoutns, 1420,320);
  cb->setStart(&p);
  p=Point(&layoutns, 1390,250);
  cb->setBasePoint1(&p);
  p=Point(&layoutns, 1390,250);
  cb->setBasePoint2(&p);
  p=Point(&layoutns, 820,298);
  cb->setEnd(&p);


  SpeciesReferenceGlyph* srg_malate_mito3_2=rg_malateCarrier->createSpeciesReferenceGlyph();
  srg_malate_mito3_2->setId("srg_malate_mito3_2");
  srg_malate_mito3_2->setSpeciesGlyphId(speciesGlyph_malate_mito3->getId());
  srg_malate_mito3_2->setSpeciesReferenceId(sr_malate_mito3_2->getId());
  srg_malate_mito3_2->setRole(SPECIES_ROLE_PRODUCT);

  curve=srg_malate_mito3_2->getCurve();
  cb=curve->createCubicBezier();
  p=Point(&layoutns, 1360,340);
  cb->setStart(&p);
  p=Point(&layoutns, 1390,150);
  cb->setBasePoint1(&p);
  p=Point(&layoutns, 1390,150);
  cb->setBasePoint2(&p);
  p=Point(&layoutns, 1850,98);
  cb->setEnd(&p);






  SpeciesReferenceGlyph* srg_aspartate_mito3_2=rg_aspartateCarrier->createSpeciesReferenceGlyph();
  srg_aspartate_mito3_2->setId("srg_aspartate_mito3_2");
  srg_aspartate_mito3_2->setSpeciesGlyphId(speciesGlyph_aspartate_mito3->getId());
  srg_aspartate_mito3_2->setSpeciesReferenceId(sr_aspartate_mito3_2->getId());
  srg_aspartate_mito3_2->setRole(SPECIES_ROLE_SUBSTRATE);

  curve=srg_aspartate_mito3_2->getCurve();
  ls=curve->createLineSegment();
  p=Point(&layoutns, 1420,530);
  ls->setStart(&p);
  p=Point(&layoutns, 1850,498);
  ls->setEnd(&p);


  SpeciesReferenceGlyph* srg_aspartate_cyt_2=rg_aspartateCarrier->createSpeciesReferenceGlyph();
  srg_aspartate_cyt_2->setId("srg_aspartate_cyt_2");
  srg_aspartate_cyt_2->setSpeciesGlyphId(speciesGlyph_aspartate_cyt->getId());
  srg_aspartate_cyt_2->setSpeciesReferenceId(sr_aspartate_cyt_2->getId());
  srg_aspartate_cyt_2->setRole(SPECIES_ROLE_PRODUCT);

  curve=srg_aspartate_cyt_2->getCurve();
  cb=curve->createCubicBezier();
  p=Point(&layoutns, 1360,550);
  cb->setStart(&p);
  p=Point(&layoutns, 1390,698);
  cb->setBasePoint1(&p);
  p=Point(&layoutns, 1390,698);
  cb->setBasePoint2(&p);
  p=Point(&layoutns, 820,698);
  cb->setEnd(&p);


  SpeciesReferenceGlyph* srg_glutamate_cyt_2=rg_aspartateCarrier->createSpeciesReferenceGlyph();
  srg_glutamate_cyt_2->setId("srg_glutamate_cyt_2");
  srg_glutamate_cyt_2->setSpeciesGlyphId(speciesGlyph_glutamate_cyt->getId());
  srg_glutamate_cyt_2->setSpeciesReferenceId(sr_glutamate_cyt_2->getId());
  srg_glutamate_cyt_2->setRole(SPECIES_ROLE_SUBSTRATE);

  curve=srg_glutamate_cyt_2->getCurve();
  cb=curve->createCubicBezier();
  p=Point(&layoutns, 1420,530);
  cb->setStart(&p);
  p=Point(&layoutns, 1390,648);
  cb->setBasePoint1(&p);
  p=Point(&layoutns, 1390,648);
  cb->setBasePoint2(&p);
  p=Point(&layoutns, 1050,628);
  cb->setEnd(&p);


  SpeciesReferenceGlyph* srg_glutamate_mito3_2=rg_aspartateCarrier->createSpeciesReferenceGlyph();
  srg_glutamate_mito3_2->setId("srg_glutamate_mito3_2");
  srg_glutamate_mito3_2->setSpeciesGlyphId(speciesGlyph_glutamate_mito3->getId());
  srg_glutamate_mito3_2->setSpeciesReferenceId(sr_glutamate_mito3_2->getId());
  srg_glutamate_mito3_2->setRole(SPECIES_ROLE_PRODUCT);

  curve=srg_glutamate_mito3_2->getCurve();
  cb=curve->createCubicBezier();
  p=Point(&layoutns, 1360,550);
  cb->setStart(&p);
  p=Point(&layoutns, 1390,448);
  cb->setBasePoint1(&p);
  p=Point(&layoutns, 1390,448);
  cb->setBasePoint2(&p);
  p=Point(&layoutns, 1550,448);
  cb->setEnd(&p);

  // now we add some global render information
  ListOfLayouts* pListOfLayouts=plugin->getListOfLayouts();
  fail_unless(pListOfLayouts!=NULL);

  RenderListOfLayoutsPlugin* lolPlugin = (RenderListOfLayoutsPlugin* ) pListOfLayouts->getPlugin("render");
  GlobalRenderInformation* pGlobalRender=lolPlugin->createGlobalRenderInformation();
  pGlobalRender->setId("wireFrame");
  pGlobalRender->setName("wireframe style");
  pGlobalRender->setProgramName("Ralph Gauges");
  pGlobalRender->setProgramVersion("1.0");
  pGlobalRender->setBackgroundColor("#FFFFFFFF");
  // color definitions
  ColorDefinition* pColorDefinition=pGlobalRender->createColorDefinition();
  pColorDefinition->setId("white");
  pColorDefinition->setColorValue("#FFFFFF");
  pColorDefinition=pGlobalRender->createColorDefinition();
  pColorDefinition->setId("black");
  pColorDefinition->setColorValue("#000000");
  // styles
  // style for compartment glyphs
  GlobalStyle* pGlobalStyle=pGlobalRender->createStyle("compartmentGlyphStyle");
  pGlobalStyle->addType("COMPARTMENTGLYPH");
  RenderGroup* pGroup=pGlobalStyle->getGroup();
  pGroup->setStroke("black");
  pGroup->setStrokeWidth(1.0);
  Rectangle* pRectangle=pGroup->createRectangle();
  pRectangle->setCoordinatesAndSize(RelAbsVector(0.0,0.0),RelAbsVector(0.0,0.0),RelAbsVector(0.0,0.0),RelAbsVector(0.0,100.0),RelAbsVector(0.0,100.0));
  pRectangle->setRadiusX(RelAbsVector(0.0,0.0));

  // style for species glyphs
  pGlobalStyle=pGlobalRender->createStyle("speciesGlyphStyle");
  pGlobalStyle->addType("SPECIESGLYPH");
  pGroup=pGlobalStyle->getGroup();
  pGroup->setStroke("black");
  pGroup->setStrokeWidth(1.0);
  pRectangle=pGroup->createRectangle();
  pRectangle->setCoordinatesAndSize(RelAbsVector(0.0,0.0),RelAbsVector(0.0,0.0),RelAbsVector(0.0,0.0),RelAbsVector(0.0,100.0),RelAbsVector(0.0,100.0));
  pRectangle->setRadiusX(RelAbsVector(0.0,0.0));
 
  // style for all other glyphs 
  pGlobalStyle=pGlobalRender->createStyle("reactionGlyphStyle");
  pGlobalStyle->addType("SPECIESREFERENCEGLYPH");
  pGlobalStyle->addType("REACTIONGLYPH");
  pGlobalStyle->addType("TEXTGLYPH");
  pGroup=pGlobalStyle->getGroup();
  pGroup->setStroke("black");
  pGroup->setStrokeWidth(1.0);
  pGroup->setFontSize(RelAbsVector(12.0,0.0));
  pGroup->setTextAnchor(Text::ANCHOR_MIDDLE);
  
  // second render information
  pGlobalRender=lolPlugin->createGlobalRenderInformation();
  pGlobalRender->setId("defaultGrayStyle");
  pGlobalRender->setName("grayscale style");
  pGlobalRender->setProgramName("Ralph Gauges");
  pGlobalRender->setProgramVersion("1.0");
  pGlobalRender->setBackgroundColor("#FFFFFFFF");
  // color definitions
  pColorDefinition=pGlobalRender->createColorDefinition();
  pColorDefinition->setId("lightGray");
  pColorDefinition->setColorValue("#CECECE");
  pColorDefinition=pGlobalRender->createColorDefinition();
  pColorDefinition->setId("white");
  pColorDefinition->setColorValue("#FFFFFF");
  pColorDefinition=pGlobalRender->createColorDefinition();
  pColorDefinition->setId("black");
  pColorDefinition->setColorValue("#000000");
  pColorDefinition=pGlobalRender->createColorDefinition();
  pColorDefinition->setId("lightGray2");
  pColorDefinition->setColorValue("#F0F0F0");
  pColorDefinition=pGlobalRender->createColorDefinition();
  pColorDefinition->setId("gray");
  pColorDefinition->setColorValue("#0B0B0B");
  // gradient definitions
  RadialGradient* pRadialGradient=pGlobalRender->createRadialGradientDefinition();
  pRadialGradient->setId("speciesGlyphGradient");
  GradientStop* pStop=pRadialGradient->createGradientStop();
  pStop->setOffset(RelAbsVector(0.0,0.0));
  pStop->setStopColor("white");   
  pStop=pRadialGradient->createGradientStop();
  pStop->setOffset(RelAbsVector(0.0,100.0));
  pStop->setStopColor("lightGray");
  // line endings
  LineEnding* pLineEnding=pGlobalRender->createLineEnding();
  pLineEnding->setId("simpleHead_black");
  p=Point(&layoutns, -8,-3);
  Dimensions d(&layoutns, 10,6);
  pLineEnding->getBoundingBox()->setPosition(&p);
  pLineEnding->getBoundingBox()->setDimensions(&d);
  pGroup=pLineEnding->getGroup();
  pGroup->setStroke("black");
  pGroup->setStrokeWidth(1.0);
  pGroup->setFillColor("black");
  Polygon* pPolygon=pGroup->createPolygon();
  RenderPoint* pR=pPolygon->createPoint();
  pR->setCoordinates(RelAbsVector(0.0,0.0),RelAbsVector(0.0,0.0)); 
  pR=pPolygon->createPoint();
  pR->setCoordinates(RelAbsVector(10.0,0.0),RelAbsVector(3.0,0.0)); 
  pR=pPolygon->createPoint();
  pR->setCoordinates(RelAbsVector(0.0,0.0),RelAbsVector(6.0,0.0)); 

  // styles
  // style for compartment glyphs
  pGlobalStyle=pGlobalRender->createStyle("compartmentGlyphStyle");
  pGlobalStyle->addType("COMPARTMENTGLYPH");
  pGroup=pGlobalStyle->getGroup();
  pGroup->setStroke("gray");
  pGroup->setStrokeWidth(1.0);
  pRectangle=pGroup->createRectangle();
  pRectangle->setCoordinatesAndSize(RelAbsVector(0.0,0.0),RelAbsVector(0.0,0.0),RelAbsVector(0.0,0.0),RelAbsVector(0.0,100.0),RelAbsVector(0.0,100.0));
  pRectangle->setRadiusX(RelAbsVector(0.0,5.0));
  pRectangle->setFillColor("lightGray2");

  // style for species glyphs
  pGlobalStyle=pGlobalRender->createStyle("speciesGlyphStyle");
  pGlobalStyle->addType("SPECIESGLYPH");
  pGroup=pGlobalStyle->getGroup();
  pGroup->setStroke("black");
  pGroup->setStrokeWidth(1.0);
  pRectangle=pGroup->createRectangle();
  pRectangle->setCoordinatesAndSize(RelAbsVector(0.0,0.0),RelAbsVector(0.0,0.0),RelAbsVector(0.0,0.0),RelAbsVector(0.0,100.0),RelAbsVector(0.0,100.0));
  pRectangle->setRadiusX(RelAbsVector(0.0,5.0));
  pRectangle->setFillColor("speciesGlyphGradient");
 
  // style for reaction and text glyphs 
  pGlobalStyle=pGlobalRender->createStyle("reactionGlyphStyle");
  pGlobalStyle->addType("REACTIONGLYPH");
  pGlobalStyle->addType("TEXTGLYPH");
  pGroup=pGlobalStyle->getGroup();
  pGroup->setStroke("black");
  pGroup->setStrokeWidth(1.0);
  pGroup->setFontSize(RelAbsVector(12.0,0.0));
  pGroup->setTextAnchor(Text::ANCHOR_MIDDLE);
  
  // style for substrate and product species reference glyphs
  pGlobalStyle=pGlobalRender->createStyle("reactantSpeciesReferenceGlyphStyle");
  pGlobalStyle->addRole("substrate");
  pGlobalStyle->addRole("sidesubstrate");
  pGlobalStyle->addRole("product");
  pGlobalStyle->addRole("sideproduct");
  pGroup=pGlobalStyle->getGroup();
  pGroup->setStroke("#000000");
  pGroup->setStrokeWidth(1.0);
 
  // style for activator species reference glyphs
  pGlobalStyle=pGlobalRender->createStyle("activatorSpeciesReferenceGlyphStyle");
  pGlobalStyle->addRole("activator");
  pGroup=pGlobalStyle->getGroup();
  pGroup->setStroke("#000000");
  pGroup->setStrokeWidth(1.0);
 
  // style for modifier species reference glyphs
  pGlobalStyle=pGlobalRender->createStyle("modifierSpeciesReferenceGlyphStyle");
  pGlobalStyle->addRole("modifier");
  pGroup=pGlobalStyle->getGroup();
  pGroup->setStroke("#000000");
  pGroup->setStrokeWidth(1.0);

  // style for inhibitor species reference glyphs
  pGlobalStyle=pGlobalRender->createStyle("inhibitorSpeciesReferenceGlyphStyle");
  pGlobalStyle->addRole("inhibitor");
  pGroup=pGlobalStyle->getGroup();
  pGroup->setStroke("#000000");
  pGroup->setStrokeWidth(1.0);
 
  // short gray style which uses the default style and just redefines some colors
  // second render information
  pGlobalRender=lolPlugin->createGlobalRenderInformation();
  pGlobalRender->setId("shortGrayStyle");
  pGlobalRender->setName("modified default style to grayscale");
  pGlobalRender->setReferenceRenderInformationId("defaultStyle");
  pGlobalRender->setProgramName("Ralph Gauges");
  pGlobalRender->setProgramVersion("1.0");
  pGlobalRender->setBackgroundColor("#FFFFFFFF");
  // color definitions
  pColorDefinition=pGlobalRender->createColorDefinition();
  pColorDefinition->setId("lightBlue");
  pColorDefinition->setColorValue("#CECECE");
  pColorDefinition=pGlobalRender->createColorDefinition();
  pColorDefinition->setId("white");
  pColorDefinition->setColorValue("#FFFFFF");
  pColorDefinition=pGlobalRender->createColorDefinition();
  pColorDefinition->setId("black");
  pColorDefinition->setColorValue("#000000");
  pColorDefinition=pGlobalRender->createColorDefinition();
  pColorDefinition->setId("red");
  pColorDefinition->setColorValue("#000000");
  pColorDefinition=pGlobalRender->createColorDefinition();
  pColorDefinition->setId("green");
  pColorDefinition->setColorValue("#000000");
  pColorDefinition=pGlobalRender->createColorDefinition();
  pColorDefinition->setId("blue");
  pColorDefinition->setColorValue("#000000");
  pColorDefinition=pGlobalRender->createColorDefinition();
  pColorDefinition->setId("lightYellow");
  pColorDefinition->setColorValue("#F0F0F0");
  pColorDefinition=pGlobalRender->createColorDefinition();
  pColorDefinition->setId("darkGreen");
  pColorDefinition->setColorValue("#0B0B0B");
     
  // render information for the default color style
  lolPlugin = (RenderListOfLayoutsPlugin*)pListOfLayouts->getPlugin("render");
  pGlobalRender=lolPlugin->createGlobalRenderInformation();
  pGlobalRender->setId("defaultStyle");
  pGlobalRender->setName("default style");
  pGlobalRender->setProgramName("Ralph Gauges");
  pGlobalRender->setProgramVersion("1.0");
  pGlobalRender->setBackgroundColor("#FFFFFFFF");
  // color definitions
  pColorDefinition=pGlobalRender->createColorDefinition();
  pColorDefinition->setId("lightBlue");
  pColorDefinition->setColorValue("#ADD8E6");
  pColorDefinition=pGlobalRender->createColorDefinition();
  pColorDefinition->setId("white");
  pColorDefinition->setColorValue("#FFFFFF");
  pColorDefinition=pGlobalRender->createColorDefinition();
  pColorDefinition->setId("black");
  pColorDefinition->setColorValue("#000000");
  pColorDefinition=pGlobalRender->createColorDefinition();
  pColorDefinition->setId("red");
  pColorDefinition->setColorValue("#FF0000");
  pColorDefinition=pGlobalRender->createColorDefinition();
  pColorDefinition->setId("green");
  pColorDefinition->setColorValue("#00FF00");
  pColorDefinition=pGlobalRender->createColorDefinition();
  pColorDefinition->setId("blue");
  pColorDefinition->setColorValue("#0000FF");
  pColorDefinition=pGlobalRender->createColorDefinition();
  pColorDefinition->setId("lightYellow");
  pColorDefinition->setColorValue("#FFFFD1");
  pColorDefinition=pGlobalRender->createColorDefinition();
  pColorDefinition->setId("darkGreen");
  pColorDefinition->setColorValue("#002000");
  // gradient definitions
  pRadialGradient=pGlobalRender->createRadialGradientDefinition();
  pRadialGradient->setId("speciesGlyphGradient");
  pStop=pRadialGradient->createGradientStop();
  pStop->setOffset(RelAbsVector(0.0,0.0));
  pStop->setStopColor("white");   
  pStop=pRadialGradient->createGradientStop();
  pStop->setOffset(RelAbsVector(0.0,100.0));
  pStop->setStopColor("lightBlue");
  // line endings
  pLineEnding=pGlobalRender->createLineEnding();
  pLineEnding->setId("simpleHead_black");
  pLineEnding->getBoundingBox()->setPosition(&p);
  pLineEnding->getBoundingBox()->setDimensions(&d);
  pGroup=pLineEnding->getGroup();
  pGroup->setStroke("black");
  pGroup->setStrokeWidth(1.0);
  pGroup->setFillColor("black");
  pPolygon=pGroup->createPolygon();
  pR=pPolygon->createPoint();
  pR->setCoordinates(RelAbsVector(0.0,0.0),RelAbsVector(0.0,0.0)); 
  pR=pPolygon->createPoint();
  pR->setCoordinates(RelAbsVector(10.0,0.0),RelAbsVector(3.0,0.0)); 
  pR=pPolygon->createPoint();
  pR->setCoordinates(RelAbsVector(0.0,0.0),RelAbsVector(6.0,0.0)); 
  pLineEnding=pGlobalRender->createLineEnding();
  pLineEnding->setId("simpleHead_red");
  pLineEnding->getBoundingBox()->setPosition(&p);
  pLineEnding->getBoundingBox()->setDimensions(&d);
  pGroup=pLineEnding->getGroup();
  pGroup->setStroke("red");
  pGroup->setStrokeWidth(1.0);
  pGroup->setFillColor("red");
  pPolygon=pGroup->createPolygon();
  pR=pPolygon->createPoint();
  pR->setCoordinates(RelAbsVector(0.0,0.0),RelAbsVector(0.0,0.0)); 
  pR=pPolygon->createPoint();
  pR->setCoordinates(RelAbsVector(10.0,0.0),RelAbsVector(3.0,0.0)); 
  pR=pPolygon->createPoint();
  pR->setCoordinates(RelAbsVector(0.0,0.0),RelAbsVector(6.0,0.0)); 
  pLineEnding=pGlobalRender->createLineEnding();
  pLineEnding->setId("simpleHead_green");
  pLineEnding->getBoundingBox()->setPosition(&p);
  pLineEnding->getBoundingBox()->setDimensions(&d);
  pGroup=pLineEnding->getGroup();
  pGroup->setStroke("green");
  pGroup->setStrokeWidth(1.0);
  pGroup->setFillColor("green");
  pPolygon=pGroup->createPolygon();
  pR=pPolygon->createPoint();
  pR->setCoordinates(RelAbsVector(0.0,0.0),RelAbsVector(0.0,0.0)); 
  pR=pPolygon->createPoint();
  pR->setCoordinates(RelAbsVector(10.0,0.0),RelAbsVector(3.0,0.0)); 
  pR=pPolygon->createPoint();
  pR->setCoordinates(RelAbsVector(0.0,0.0),RelAbsVector(6.0,0.0)); 
  pLineEnding=pGlobalRender->createLineEnding();
  pLineEnding->setId("simpleHead_blue");
  pLineEnding->getBoundingBox()->setPosition(&p);
  pLineEnding->getBoundingBox()->setDimensions(&d);
  pGroup=pLineEnding->getGroup();
  pGroup->setStroke("blue");
  pGroup->setStrokeWidth(1.0);
  pGroup->setFillColor("blue");
  pPolygon=pGroup->createPolygon();
  pR=pPolygon->createPoint();
  pR->setCoordinates(RelAbsVector(0.0,0.0),RelAbsVector(0.0,0.0)); 
  pR=pPolygon->createPoint();
  pR->setCoordinates(RelAbsVector(10.0,0.0),RelAbsVector(3.0,0.0)); 
  pR=pPolygon->createPoint();
  pR->setCoordinates(RelAbsVector(0.0,0.0),RelAbsVector(6.0,0.0)); 
  
  // styles
  // style for compartment glyphs
  pGlobalStyle=pGlobalRender->createStyle("compartmentGlyphStyle");
  pGlobalStyle->addType("COMPARTMENTGLYPH");
  pGroup=pGlobalStyle->getGroup();
  pGroup->setStroke("darkGreen");
  pGroup->setStrokeWidth(1.0);
  pRectangle=pGroup->createRectangle();
  pRectangle->setCoordinatesAndSize(RelAbsVector(0.0,0.0),RelAbsVector(0.0,0.0),RelAbsVector(0.0,0.0),RelAbsVector(0.0,100.0),RelAbsVector(0.0,100.0));
  pRectangle->setRadiusX(RelAbsVector(0.0,10.0));
  pRectangle->setRadiusY(RelAbsVector(0.0,10.0));
  pRectangle->setFillColor("lightYellow");

  // style for species glyphs
  pGlobalStyle=pGlobalRender->createStyle("speciesGlyphStyle");
  pGlobalStyle->addType("SPECIESGLYPH");
  pGroup=pGlobalStyle->getGroup();
  pGroup->setStroke("black");
  pGroup->setStrokeWidth(1.0);
  pRectangle=pGroup->createRectangle();
  pRectangle->setCoordinatesAndSize(RelAbsVector(0.0,0.0),RelAbsVector(0.0,0.0),RelAbsVector(0.0,0.0),RelAbsVector(0.0,100.0),RelAbsVector(0.0,100.0));
  pRectangle->setRadiusX(RelAbsVector(5.0,0.0));
  pRectangle->setRadiusY(RelAbsVector(0.0,50.0));
  pRectangle->setFillColor("speciesGlyphGradient");
 
  // style for reaction and text glyphs 
  pGlobalStyle=pGlobalRender->createStyle("reactionGlyphStyle");
  pGlobalStyle->addType("REACTIONGLYPH");
  pGlobalStyle->addType("TEXTGLYPH");
  pGroup=pGlobalStyle->getGroup();
  pGroup->setStroke("black");
  pGroup->setStrokeWidth(1.0);
  pGroup->setFontSize(RelAbsVector(12.0,0.0));
  pGroup->setTextAnchor(Text::ANCHOR_MIDDLE);
  
  // style for substrate and product species reference glyphs
  pGlobalStyle=pGlobalRender->createStyle("reactantSpeciesReferenceGlyphStyle");
  pGlobalStyle->addRole("substrate");
  pGlobalStyle->addRole("sidesubstrate");
  pGlobalStyle->addRole("product");
  pGlobalStyle->addRole("sideproduct");
  pGroup=pGlobalStyle->getGroup();
  pGroup->setStroke("#000000");
  pGroup->setStrokeWidth(1.0);
  pGroup->setEndHead("simpleHead_black");
 
  // style for activator species reference glyphs
  pGlobalStyle=pGlobalRender->createStyle("activatorSpeciesReferenceGlyphStyle");
  pGlobalStyle->addRole("activator");
  pGroup=pGlobalStyle->getGroup();
  pGroup->setStroke("green");
  pGroup->setStrokeWidth(1.0);
  pGroup->setEndHead("simpleHead_green");
 
  // style for modifier species reference glyphs
  pGlobalStyle=pGlobalRender->createStyle("modifierSpeciesReferenceGlyphStyle");
  pGlobalStyle->addRole("modifier");
  pGroup=pGlobalStyle->getGroup();
  pGroup->setStroke("blue");
  pGroup->setStrokeWidth(1.0);
  pGroup->setEndHead("simpleHead_blue");

  // style for inhibitor species reference glyphs
  pGlobalStyle=pGlobalRender->createStyle("inhibitorSpeciesReferenceGlyphStyle");
  pGlobalStyle->addRole("inhibitor");
  pGroup=pGlobalStyle->getGroup();
  pGroup->setStroke("red");
  pGroup->setStrokeWidth(1.0);
  pGroup->setEndHead("simpleHead_red");


  // local style that references a global style and redefines some things
  RenderLayoutPlugin* rPlugin=(RenderLayoutPlugin*)layout->getPlugin("render");
  LocalRenderInformation* pLocalRender=rPlugin->createLocalRenderInformation();
  pLocalRender->setId("highlightGlucose");
  pLocalRender->setReferenceRenderInformationId("defaultStyle");
  pLocalRender->setProgramName("Ralph Gauges");
  pLocalRender->setProgramVersion("1.0");
  pLocalRender->setBackgroundColor("#FFFFFFFF");
  // color definitions
  pColorDefinition=pLocalRender->createColorDefinition();
  pColorDefinition->setId("lightRed");
  pColorDefinition->setColorValue("#E6ADD8");
  pColorDefinition=pLocalRender->createColorDefinition();
  pColorDefinition->setId("white");
  pColorDefinition->setColorValue("#FFFFFF");
  // gradient definitions
  pRadialGradient=pLocalRender->createRadialGradientDefinition();
  pRadialGradient->setId("highlightedSpeciesGlyphGradient");
  pStop=pRadialGradient->createGradientStop();
  pStop->setOffset(RelAbsVector(0.0,0.0));
  pStop->setStopColor("white");   
  pStop=pRadialGradient->createGradientStop();
  pStop->setOffset(RelAbsVector(0.0,100.0));
  pStop->setStopColor("lightRed");   

  // style for highligted species glyph
  LocalStyle* pLocalStyle=pLocalRender->createStyle("highlightedGlucose");
  pLocalStyle->addId("SpeciesGlyph_Glucose");
  pGroup=pLocalStyle->getGroup();
  pGroup->setStroke("black");
  pGroup->setStrokeWidth(1.0);
  pRectangle=pGroup->createRectangle();
  pRectangle->setCoordinatesAndSize(RelAbsVector(0.0,0.0),RelAbsVector(0.0,0.0),RelAbsVector(0.0,0.0),RelAbsVector(0.0,100.0),RelAbsVector(0.0,100.0));
  pRectangle->setRadiusX(RelAbsVector(5.0,0.0));
  pRectangle->setRadiusY(RelAbsVector(0.0,50.0));
  pRectangle->setFillColor("highlightedSpeciesGlyphGradient");
   
  SBMLWriter writer;
    
  //bool result=writeSBML(document,"example6.xml");
  char* writtenContent=writer.writeToString(document);

  XMLInputStream stream2(writtenContent,false);
  XMLNode node2(stream2);
  free(writtenContent);
  // we need to check the namespaces on node2
  // it must contain a namespace for the layout and one for the 
  // render extension
  // It must also contain a required attribute for each of the two packages
  const XMLNamespaces* pXMLNamespaces=&node.getNamespaces();
  int index=pXMLNamespaces->getIndex("http://www.sbml.org/sbml/level3/version1/layout/version1");
  fail_unless(index != -1);
  std::string prefix=pXMLNamespaces->getPrefix(index);
  fail_unless(!prefix.empty());
  const XMLAttributes* pXMLAttributes=&node2.getAttributes();
  fail_unless(pXMLAttributes->getLength() > 0);
  int i;
  for(i=0;i<pXMLAttributes->getLength();++i)
  {
      if(pXMLAttributes->getName(i) == "required" && pXMLAttributes->getPrefix(i) == prefix)
      {
        break; 
      }
  }
  fail_unless(i < pXMLAttributes->getLength());
  // same check for the render extension
  index=pXMLNamespaces->getIndex("http://www.sbml.org/sbml/level3/version1/render/version1");
  fail_unless(index != -1);
  prefix=pXMLNamespaces->getPrefix(index);
  fail_unless(!prefix.empty());
  pXMLAttributes=&node2.getAttributes();
  fail_unless(pXMLAttributes->getLength() > 0);
  for(i=0;i<pXMLAttributes->getLength();++i)
  {
      if(pXMLAttributes->getName(i) == "required" && pXMLAttributes->getPrefix(i) == prefix)
      {
        break; 
      }
  }
  fail_unless(i < pXMLAttributes->getLength());

  const XMLNode* listOfLayouts1,*listOfLayouts2;
  listOfLayouts1=&node;
  fail_unless(listOfLayouts1->getName()=="sbml");
  int iMax=listOfLayouts1->getNumChildren();
  for(i=0;i<iMax;++i)
  {
    if(listOfLayouts1->getChild(i).getName()=="model")
    {
        listOfLayouts1=&listOfLayouts1->getChild(i);
        break;
    }
  }
  fail_unless(listOfLayouts1->getName()=="model");
  iMax=listOfLayouts1->getNumChildren();
  for(i=0;i<iMax;++i)
  {  
    if(listOfLayouts1->getChild(i).getName()=="listOfLayouts")
    {
        listOfLayouts1=&listOfLayouts1->getChild(i);
        break;
    }
  }
  fail_unless(listOfLayouts1->getName()=="listOfLayouts");
  
  listOfLayouts2=&node2;
  fail_unless(listOfLayouts2->getName()=="sbml");
  iMax=listOfLayouts2->getNumChildren();
  for(i=0;i<iMax;++i)
  {
    if(listOfLayouts2->getChild(i).getName()=="model")
    {
        listOfLayouts2=&listOfLayouts2->getChild(i);
        break;
    }
  }
  fail_unless(listOfLayouts2->getName()=="model");
  iMax=listOfLayouts2->getNumChildren();
  for(i=0;i<iMax;++i)
  {
    // make sure no annotation (with layout) has been written  
    fail_unless(listOfLayouts2->getChild(i).getName() != "annotation");
    if(listOfLayouts2->getChild(i).getName()=="listOfLayouts")
    {
        listOfLayouts2=&listOfLayouts2->getChild(i);
        break;
    }
  }
  fail_unless(listOfLayouts2->getName()=="listOfLayouts");


  //std::string l1 = listOfLayouts1->toXMLString();
  //std::string l2 = listOfLayouts2->toXMLString();
  //std::string docString = writeSBMLToString(document);

  // until the sbml element gets a namespace, we only compare the listOfLayouts element and all its children.
  fail_unless(listOfLayouts1->equals(*listOfLayouts2));

  delete document;
}
END_TEST 

START_TEST (test_RenderWriting_write_L3_model_2)
{
  const char* s = \
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<sbml xmlns=\"http://www.sbml.org/sbml/level3/version1/core\" level=\"3\" version=\"1\"\n"
    "      xmlns:layout=\"http://www.sbml.org/sbml/level3/version1/layout/version1\"\n"
    "      xmlns:render=\"http://www.sbml.org/sbml/level3/version1/render/version1\"\n"
    "      layout:required=\"false\"\n"
    "      render:required=\"false\">\n"
    "  <model id=\"TestModel\">\n"
    "    <layout:listOfLayouts xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">\n"
    "      <layout:layout id=\"TextTest\">\n"
    "        <layout:dimensions width=\"510\" height=\"106\"/>\n"
    "        <layout:listOfAdditionalGraphicalObjects>\n"
    "          <layout:graphicalObject id=\"graphical_object_1\">\n"
    "            <layout:boundingBox>\n"
    "              <layout:position x=\"5\" y=\"5\"/>\n"
    "              <layout:dimensions width=\"500\" height=\"40\"/>\n"
    "            </layout:boundingBox>\n"
    "          </layout:graphicalObject>\n"
    "          <layout:graphicalObject id=\"graphical_object_2\">\n"
    "            <layout:boundingBox>\n"
    "              <layout:position x=\"5\" y=\"53\"/>\n"
    "              <layout:dimensions width=\"500\" height=\"40\"/>\n"
    "            </layout:boundingBox>\n"
    "          </layout:graphicalObject>\n"
    "        </layout:listOfAdditionalGraphicalObjects>\n"
    "        <listOfRenderInformation>\n"
    "          <renderInformation id=\"TextRenderInfo\">\n"
    "            <listOfStyles>\n"
    "              <style id=\"style_for_graphical_object_1\" idList=\"graphical_object_1\">\n"
    "                <g stroke=\"#000000\" stroke-width=\"1\" font-size=\"20\" font-family=\"sans\" text-anchor=\"start\">\n"
    "                  <rectangle fill=\"#aaaaff\" x=\"0\" y=\"0\" width=\"100%\" height=\"100%\"/>\n"
    "                  <text x=\"0\" y=\"0\">ABCDEFGHIJKLMNOPQRSTUVWXYZ</text>\n"
    "                </g>\n"
    "              </style>\n"
    "              <style id=\"style_for_graphical_object_2\" idList=\"graphical_object_2\">\n"
    "                <g stroke=\"#000000\" stroke-width=\"1\" font-size=\"20\" font-family=\"sans\" text-anchor=\"start\">\n"
    "                  <rectangle fill=\"#aaaaff\" x=\"0\" y=\"0\" width=\"100%\" height=\"100%\"/>\n"
    "                  <text x=\"0\" y=\"0\">abcdefghijklmnopqrstuvwxyz</text>\n"
    "                </g>\n"
    "              </style>\n"
    "            </listOfStyles>\n"
    "          </renderInformation>\n"
    "        </listOfRenderInformation>\n"
    "      </layout:layout>\n"
    "    </layout:listOfLayouts>\n"
    "  </model>\n"
    "</sbml>";

  XMLInputStream stream(s,false);
  XMLNode node(stream);
  // create the document

  SBMLDocument *document=new SBMLDocument(3,1);
  
  // enable layout and render
  document->enablePackage(LayoutExtension::getXmlnsL3V1V1(), "layout", true);
  document->enablePackage(RenderExtension::getXmlnsL3V1V1(), "render", true);

  // create the Model

  Model* pModel=document->createModel();
  pModel->setId("TestModel");
  document->setModel(pModel);

  // now we create a new layout
  LayoutModelPlugin* mPlugin=(LayoutModelPlugin*)pModel->getPlugin("layout");
  Layout* pLayout=mPlugin->createLayout();
  pLayout->setId("TextTest");
  double y=5.0,x=5.0;
  // draw each text in a 500x40 box
  const double WIDTH=500.0;
  double HEIGHT=40.0;
  std::ostringstream os;
  unsigned int object_index=1;
  const std::string TEXT="ABCDEFGHIJKLMNOPQRSTUVWXYZ";
  const std::string text="abcdefghijklmnopqrstuvwxyz";
  // the dimensions of the whole layout will be
  Dimensions dim(layoutns,WIDTH+10,2*HEIGHT*1.2+10);
  pLayout->setDimensions(&dim);
  // create the render information
  RenderLayoutPlugin* rPlugin=(RenderLayoutPlugin*)pLayout->getPlugin("render");
  LocalRenderInformation* pRenderInfo=rPlugin->createLocalRenderInformation();
  pRenderInfo->setId("TextRenderInfo");
  Dimensions d(layoutns,WIDTH,HEIGHT);
  // create a graphical object with the correct bounding box
  // left alligned capital letters
  os.str("");
  os << "graphical_object_" << object_index++;
  GraphicalObject* pObject=pLayout->createAdditionalGraphicalObject();
  pObject->setId(os.str());
  // we set the bound
  BoundingBox bb;
  Point p(layoutns,x,y);
  bb.setPosition(&p);
  bb.setDimensions(&d);
  pObject->setBoundingBox(&bb);
  // we create a style for the object
  LocalStyle* pStyle=pRenderInfo->createStyle("style_for_"+os.str());
  pStyle->addId(pObject->getId());
  RenderGroup* pGroup=pStyle->getGroup();
  pGroup->setStroke("#000000");
  pGroup->setStrokeWidth(1.0);
  pGroup->setFontFamily("sans");
  pGroup->setFontSize(20.0);
  pGroup->setTextAnchor(Text::ANCHOR_START);
  // add a rectangle to the group
  Rectangle* pRectangle=pGroup->createRectangle();
  pRectangle->setCoordinates(RelAbsVector(0.0,0.0),RelAbsVector(0.0,0.0),RelAbsVector(0.0,0.0));
  pRectangle->setSize(RelAbsVector(0.0,100.0),RelAbsVector(0.0,100.0));
  pRectangle->setFillColor("#aaaaff");
  // add a text element to the group
  Text* pText=pGroup->createText();
  pText->setText(TEXT);
  y+=HEIGHT*1.2;
  // create a graphical object with the correct bounding box
  // left alligned lowercase letters
  os.str("");
  os << "graphical_object_" << object_index++;
  pObject=pLayout->createAdditionalGraphicalObject();
  pObject->setId(os.str());
  // we set the bound
  p=Point(layoutns,x,y);
  bb.setPosition(&p);
  bb.setDimensions(&d);
  pObject->setBoundingBox(&bb);
  // we create a style for the object
  pStyle=pRenderInfo->createStyle("style_for_"+os.str());
  pStyle->addId(pObject->getId());
  pGroup=pStyle->getGroup();
  pGroup->setStroke("#000000");
  pGroup->setStrokeWidth(1.0);
  pGroup->setFontFamily("sans");
  pGroup->setFontSize(20.0);
  pGroup->setTextAnchor(Text::ANCHOR_START);
  // add a rectangle to the group
  pRectangle=pGroup->createRectangle();
  pRectangle->setCoordinates(RelAbsVector(0.0,0.0),RelAbsVector(0.0,0.0),RelAbsVector(0.0,0.0));
  pRectangle->setSize(RelAbsVector(0.0,100.0),RelAbsVector(0.0,100.0));
  pRectangle->setFillColor("#aaaaff");
  // add a text element to the group
  pText=pGroup->createText();
  pText->setText(text);

  // write the document
  std::string writtenContent=writeSBMLToStdString(document);
  fail_unless(!writtenContent.empty());
  // now we read the written content 
  SBMLReader reader;
  SBMLDocument* pDocument2=reader.readSBMLFromString(writtenContent);
  fail_unless(pDocument2 != NULL);
  const Model *pModel2=pDocument2->getModel();
  LayoutModelPlugin* plugin = (LayoutModelPlugin*)pModel2->getPlugin("layout");
  fail_unless(pModel2 != NULL);
  fail_unless(plugin->getListOfLayouts()->size() == 1);
  const Layout* pLayout2=plugin->getLayout(0);
  fail_unless(pLayout2 != NULL);
  fail_unless(pLayout->getListOfAdditionalGraphicalObjects()->size() == 2);
  // there should be one piece of render information
  RenderLayoutPlugin* rPlugin1 = (RenderLayoutPlugin*) pLayout->getPlugin("render");
  RenderLayoutPlugin* rPlugin2 = (RenderLayoutPlugin*) pLayout2->getPlugin("render");
  fail_unless(rPlugin1->getListOfLocalRenderInformation()->size() == 1);
  fail_unless(rPlugin2 != NULL);
  if (rPlugin2 == NULL) return;

  const LocalRenderInformation* pLocalRenderInfo2=rPlugin2->getRenderInformation(0);
  fail_unless(pLocalRenderInfo2 != NULL);
  if (pLocalRenderInfo2 == NULL) return;

  fail_unless(pLocalRenderInfo2->getId() == "TextRenderInfo");
  fail_unless(pLocalRenderInfo2->getNumColorDefinitions() == 0);
  fail_unless(pLocalRenderInfo2->getNumGradientDefinitions() == 0);
  fail_unless(pLocalRenderInfo2->getNumLineEndings() == 0);
  fail_unless(pLocalRenderInfo2->getNumStyles() == 2);
  const LocalStyle* pStyle2_1=pLocalRenderInfo2->getStyle(0),*pStyle2_2=NULL;
  fail_unless(pStyle2_1 != NULL);
  if(pStyle2_1->getId() == "style_for_graphical_object_2")
  {
      pStyle2_1=pLocalRenderInfo2->getStyle(1);
  }
  else
  {
    pStyle2_2=pLocalRenderInfo2->getStyle(1);
    fail_unless(pStyle2_2->getId() == "style_for_graphical_object_2");
  }
  fail_unless(pStyle2_2 != NULL);
  fail_unless(pStyle2_2->getId() == "style_for_graphical_object_2");
  fail_unless(pStyle2_1->getId() == "style_for_graphical_object_1");
  // check both id lists
  fail_unless(pStyle2_1->getIdList().size() == 1);
  fail_unless(pStyle2_1->isInIdList("graphical_object_1"));
  fail_unless(pStyle2_2->getIdList().size() == 1);
  fail_unless(pStyle2_2->isInIdList("graphical_object_2"));
  //
  // check both groups and their attributes
  const RenderGroup* pGroup2_1 = pStyle2_1->getGroup();
  fail_unless(pGroup2_1 != NULL);
  fail_unless(pGroup2_1->isSetStroke());
  fail_unless(pGroup2_1->getStroke() == "#000000");
  fail_unless(pGroup2_1->isSetStrokeWidth());
  fail_unless(fabs(pGroup2_1->getStrokeWidth() - 1.0) < 1e-12);
  fail_unless(pGroup2_1->isSetFontSize());
  fail_unless(pGroup2_1->getFontSize() == RelAbsVector(20.0,0.0));
  fail_unless(pGroup2_1->isSetFontFamily());
  fail_unless(pGroup2_1->getFontFamily() == "sans");
  fail_unless(pGroup2_1->isSetTextAnchor());
  fail_unless(pGroup2_1->getTextAnchor() == Text::ANCHOR_START);
  const RenderGroup* pGroup2_2 = pStyle2_2->getGroup();
  fail_unless(pGroup2_2 != NULL);
  fail_unless(pGroup2_2->isSetStroke());
  fail_unless(pGroup2_2->getStroke() == "#000000");
  fail_unless(pGroup2_2->isSetStrokeWidth());
  fail_unless(fabs(pGroup2_2->getStrokeWidth() -1.0) < 1e-12);
  fail_unless(pGroup2_2->isSetFontSize());
  fail_unless(pGroup2_2->getFontSize() == RelAbsVector(20.0,0.0));
  fail_unless(pGroup2_2->isSetFontFamily());
  fail_unless(pGroup2_2->getFontFamily() == "sans");
  fail_unless(pGroup2_2->isSetTextAnchor());
  fail_unless(pGroup2_2->getTextAnchor() == Text::ANCHOR_START);
  //
  // check the elements of both groups and their attributes
  fail_unless(pGroup2_1->getNumElements() == 2);
  const Rectangle* pRectangle2_1 = dynamic_cast<const Rectangle*>(pGroup->getElement(0));
  fail_unless(pRectangle2_1 != NULL);
  fail_unless(pRectangle2_1->isSetFillColor());
  fail_unless(pRectangle2_1->getFillColor() == "#aaaaff");
  fail_unless(pRectangle2_1->getX() == RelAbsVector(0.0,0.0));
  fail_unless(pRectangle2_1->getY() == RelAbsVector(0.0,0.0));
  fail_unless(pRectangle2_1->getWidth() == RelAbsVector(0.0,100.0));
  fail_unless(pRectangle2_1->getHeight() == RelAbsVector(0.0,100.0));
  const Text* pText2_1=dynamic_cast<const Text*>(pGroup2_1->getElement(1));
  fail_unless(pText2_1 != NULL);
  fail_unless(pText2_1->getX() == RelAbsVector(0.0,0.0));
  fail_unless(pText2_1->getY() == RelAbsVector(0.0,0.0));
  fail_unless(pText2_1->getText() == TEXT);
  fail_unless(pGroup2_2->getNumElements() == 2);
  const Rectangle* pRectangle2_2 = dynamic_cast<const Rectangle*>(pGroup->getElement(0));
  fail_unless(pRectangle2_2 != NULL);
  fail_unless(pRectangle2_2->isSetFillColor());
  fail_unless(pRectangle2_2->getFillColor() == "#aaaaff");
  fail_unless(pRectangle2_2->getX() == RelAbsVector(0.0,0.0));
  fail_unless(pRectangle2_2->getY() == RelAbsVector(0.0,0.0));
  fail_unless(pRectangle2_2->getWidth() == RelAbsVector(0.0,100.0));
  fail_unless(pRectangle2_2->getHeight() == RelAbsVector(0.0,100.0));
  const Text* pText2_2=dynamic_cast<const Text*>(pGroup2_2->getElement(1));
  fail_unless(pText2_2 != NULL);
  fail_unless(pText2_2->getX() == RelAbsVector(0.0,0.0));
  fail_unless(pText2_2->getY() == RelAbsVector(0.0,0.0));
  fail_unless(pText2_2->getText() == text);
  delete document;
  delete pDocument2;
}
END_TEST


Suite *
create_suite_RenderWriting (void)
{
  Suite *suite = suite_create("RenderWriting");
  TCase *tcase = tcase_create("RenderWriting");


  tcase_add_checked_fixture( tcase,
                             RenderWriting_setup,
                             RenderWriting_teardown );

  tcase_add_test( tcase, test_RenderWriting_write_l2_ojectrole                );
  tcase_add_test( tcase, test_RenderWriting_write_model_1                );
  tcase_add_test( tcase, test_RenderWriting_write_L3_model_1             );
  tcase_add_test( tcase, test_RenderWriting_write_L3_model_2             );

  suite_add_tcase(suite, tcase);

  return suite;
}



END_C_DECLS
