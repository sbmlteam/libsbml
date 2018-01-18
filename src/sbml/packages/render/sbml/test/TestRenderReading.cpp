#include <sbml/common/common.h>
#include <sbml/common/extern.h>

#include <sbml/SBMLReader.h>
#include <sbml/SBMLDocument.h>
#include <sbml/Model.h>
#include <sbml/ModifierSpeciesReference.h>
#include <sbml/packages/layout/sbml/Layout.h>
#include <sbml/packages/layout/extension/LayoutModelPlugin.h>
#include <sbml/packages/render/extension/RenderExtension.h>
#include <sbml/packages/render/extension/RenderLayoutPlugin.h>
#include <sbml/packages/render/extension/RenderGraphicalObjectPlugin.h>
#include <sbml/packages/render/extension/RenderListOfLayoutsPlugin.h>
#include <sbml/packages/render/sbml/GlobalRenderInformation.h>
#include <sbml/packages/render/sbml/LocalRenderInformation.h>
#include <sbml/packages/render/sbml/ColorDefinition.h>
#include <sbml/packages/render/sbml/LocalStyle.h>
#include <sbml/packages/render/sbml/GlobalStyle.h>
#include <sbml/packages/render/sbml/Rectangle.h>
#include <sbml/packages/render/sbml/Polygon.h>
#include <sbml/packages/render/sbml/Text.h>
#include <sbml/packages/render/sbml/RenderPoint.h>
#include <sbml/packages/layout/sbml/LineSegment.h>
#include <sbml/packages/layout/sbml/CubicBezier.h>
#include <sbml/conversion/ConversionProperties.h>

#include <check.h>
#include <limits>

#include <string>
#include <map>

using namespace std;

LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS

extern char *TestDataDirectory;

const char *TEST_L2_MODEL=
"<?xml version='1.0' encoding='utf-8'?>"
"<!--Created on: 2/3/2013 12:43:36 PM-->"
"<sbml xmlns='http://www.sbml.org/sbml/level2/version4' level='2' version='4'>"
"	<model metaid='COPASI1' id='Model_1' name='New Model'>"
"		<annotation>"
"			<listOfLayouts xmlns='http://projects.eml.org/bcb/sbml/level2' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"				<layout id='layout_1' xmlns='http://projects.eml.org/bcb/sbml/level2'>"
"					<!--Created by SBW SBML LayoutViewer/Manipulator-->"
"					<annotation xmlns='http://www.sbml.org/sbml/level2/version4'>"
"						<customAnnotation xmlns='http://secret/annotation/package' importantAttribute='value' />"
"						<listOfRenderInformation xmlns='http://projects.eml.org/bcb/sbml/render/level2'>"
"							<renderInformation id='SBGN_default' programName='SBGN Default style' programVersion='1.0'>"
"								<listOfColorDefinitions>"
"									<colorDefinition id='black' value='#000000' />"
"									<colorDefinition id='white' value='#ffffff' />"
"									<colorDefinition id='transparent' value='#ffffff00' />"
"									<colorDefinition id='EmptySetOutline' value='#808080' />"
"									<colorDefinition id='EmptySetGradientStart' value='#ffffff' />"
"									<colorDefinition id='EmptySetGradientEnd' value='#d3d3d3' />"
"									<colorDefinition id='CompartmentBorder' value='#666666' />"
"									<colorDefinition id='CompartmentGradientStart' value='#CCCCCC' />"
"									<colorDefinition id='CompartmentGradientEnd' value='#CCCCFF' />"
"									<colorDefinition id='CloneMarkerColor' value='#ffa500' />"
"									<colorDefinition id='EPNGradientStart' value='#ffffff' />"
"									<colorDefinition id='EPNGradientEnd' value='#c0c0c0' />"
"								</listOfColorDefinitions>"
"								<listOfGradientDefinitions>"
"									<linearGradient x1='0%' y1='0%' z1='0%' x2='100%' y2='100%' z2='100%' id='EPNBackgroundGradient' spreadMethod='reflect'>"
"										<stop offset='0%' stop-color='EPNGradientStart' />"
"										<stop offset='100%' stop-color='EPNGradientEnd' />"
"									</linearGradient>"
"									<linearGradient x1='50%' y1='0%' z1='0%' x2='50%' y2='100%' z2='100%' id='cloneMarker' spreadMethod='pad'>"
"										<stop offset='0.0' stop-color='transparent' />"
"										<stop offset='0.75' stop-color='transparent' />"
"										<stop offset='0.76' stop-color='CloneMarkerColor' />"
"										<stop offset='1.0' stop-color='CloneMarkerColor' />"
"									</linearGradient>"
"									<linearGradient x1='0%' y1='0%' z1='0%' x2='100%' y2='100%' z2='100%' id='EmptySetGradient' spreadMethod='pad'>"
"										<stop offset='0%' stop-color='EmptySetGradientStart' />"
"										<stop offset='100%' stop-color='EmptySetGradientEnd' />"
"									</linearGradient>"
"									<linearGradient x1='0%' y1='0%' z1='0%' x2='100%' y2='100%' z2='100%' id='CompartmentGradient' spreadMethod='pad'>"
"										<stop offset='0%' stop-color='CompartmentGradientStart' />"
"										<stop offset='100%' stop-color='CompartmentGradientEnd' />"
"									</linearGradient>"
"								</listOfGradientDefinitions>"
"								<listOfStyles>"
"									<style roleList='defaultText'>"
"										<g stroke='black' stroke-width='1' font-family='Verdana' font-size='10' font-style='normal' font-weight='normal' text-anchor='middle' />"
"									</style>"
"									<style roleList='substrate sidesubstrate'>"
"										<g stroke='black' stroke-width='2.0' />"
"									</style>"
"									<style roleList='SBO-0000167'>"
"										<g stroke='black' stroke-width='2.0' fill='white' endHead='process' />"
"									</style>"
"									<style roleList='SBO-0000177'>"
"										<g stroke='black' stroke-width='2.0' endHead='association' />"
"									</style>"
"									<style roleList='inhibitor inhibition SBO-0000169'>"
"										<g stroke='black' stroke-width='2.0' endHead='inhibitor' />"
"									</style>"
"									<style roleList='absolute_inhibitor absolute_inhibition SBO-0000169'>"
"										<g stroke='black' stroke-width='2.0' endHead='absolute_inhibitor' />"
"									</style>"
"									<style roleList='necessary_stimulation SBO-0000171'>"
"										<g stroke='black' stroke-width='2.0' endHead='necessary_stimulation' />"
"									</style>"
"									<style roleList='modifier SBO-0000168'>"
"										<g stroke='black' stroke-width='2.0' fill='white' endHead='modifier' />"
"									</style>"
"									<style roleList='catalysis activator SBO-0000172'>"
"										<g stroke='black' stroke-width='2.0' fill='white' endHead='activator' />"
"									</style>"
"									<style roleList='modulation SBO-0000168'>"
"										<g stroke='black' stroke-width='2.0' fill='white' endHead='modulation' />"
"									</style>"
"									<style roleList='stimulation SBO-0000170'>"
"										<g stroke='black' stroke-width='2.0' fill='white' endHead='stimulation' />"
"									</style>"
"									<style roleList='SBO-0000180'>"
"										<g stroke='black' stroke-width='2.0' endHead='disassociation' />"
"									</style>"
"									<style roleList='product sideproduct' typeList='product sideproduct'>"
"										<g stroke='black' stroke-width='2.0' endHead='product' />"
"									</style>"
"									<style roleList='SBO-0000354'>"
"										<g stroke='black' stroke-width='2' fill='EPNBackgroundGradient'>"
"											<polygon stroke='black' stroke-width='2' fill='EPNBackgroundGradient'>"
"												<listOfCurveSegments>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='0%' y='80%' />"
"														<end x='0%' y='0%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='0%' y='0%' />"
"														<end x='100%' y='0%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='100%' y='0%' />"
"														<end x='100%' y='80%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='CubicBezier' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='100%' y='80%' />"
"														<end x='80%' y='100%' />"
"														<basePoint1 x='100%' y='100%' />"
"														<basePoint2 x='100%' y='100%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='80%' y='100%' />"
"														<end x='20%' y='100%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='CubicBezier' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='20%' y='100%' />"
"														<end x='0%' y='80%' />"
"														<basePoint1 x='0%' y='100%' />"
"														<basePoint2 x='0%' y='100%' />"
"													</curveSegment>"
"												</listOfCurveSegments>"
"											</polygon>"
"										</g>"
"									</style>"
"									<style roleList='SBO-0000354-multimer'>"
"										<g stroke='black' stroke-width='2' fill='EPNBackgroundGradient'>"
"											<polygon stroke='black' stroke-width='2' fill='EPNBackgroundGradient'>"
"												<listOfCurveSegments>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='5%' y='80%' />"
"														<end x='5%' y='5%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='5%' y='5%' />"
"														<end x='100%' y='5%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='100%' y='5%' />"
"														<end x='100%' y='80%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='CubicBezier' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='100%' y='80%' />"
"														<end x='80%' y='100%' />"
"														<basePoint1 x='100%' y='100%' />"
"														<basePoint2 x='100%' y='100%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='80%' y='100%' />"
"														<end x='20%' y='100%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='CubicBezier' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='20%' y='100%' />"
"														<end x='5%' y='80%' />"
"														<basePoint1 x='5%' y='100%' />"
"														<basePoint2 x='5%' y='100%' />"
"													</curveSegment>"
"												</listOfCurveSegments>"
"											</polygon>"
"											<polygon stroke='black' stroke-width='2' fill='EPNBackgroundGradient'>"
"												<listOfCurveSegments>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='0%' y='80%' />"
"														<end x='0%' y='0%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='0%' y='0%' />"
"														<end x='95%' y='0%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='95%' y='0%' />"
"														<end x='95%' y='80%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='CubicBezier' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='95%' y='80%' />"
"														<end x='80%' y='95%' />"
"														<basePoint1 x='95%' y='95%' />"
"														<basePoint2 x='95%' y='95%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='80%' y='95%' />"
"														<end x='20%' y='95%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='CubicBezier' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='20%' y='95%' />"
"														<end x='0%' y='80%' />"
"														<basePoint1 x='0%' y='95%' />"
"														<basePoint2 x='0%' y='95%' />"
"													</curveSegment>"
"												</listOfCurveSegments>"
"											</polygon>"
"										</g>"
"									</style>"
"									<style roleList='SBO-0000354-clone'>"
"										<g stroke='black' stroke-width='2' fill='EPNBackgroundGradient'>"
"											<polygon stroke='black' stroke-width='2' fill='EPNBackgroundGradient'>"
"												<listOfCurveSegments>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='0%' y='80%' />"
"														<end x='0%' y='0%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='0%' y='0%' />"
"														<end x='100%' y='0%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='100%' y='0%' />"
"														<end x='100%' y='80%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='CubicBezier' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='100%' y='80%' />"
"														<end x='80%' y='100%' />"
"														<basePoint1 x='100%' y='100%' />"
"														<basePoint2 x='100%' y='100%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='80%' y='100%' />"
"														<end x='20%' y='100%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='CubicBezier' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='20%' y='100%' />"
"														<end x='0%' y='80%' />"
"														<basePoint1 x='0%' y='100%' />"
"														<basePoint2 x='0%' y='100%' />"
"													</curveSegment>"
"												</listOfCurveSegments>"
"											</polygon>"
"											<polygon fill='cloneMarker'>"
"												<listOfCurveSegments>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='0%' y='80%' />"
"														<end x='0%' y='0%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='0%' y='0%' />"
"														<end x='100%' y='0%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='100%' y='0%' />"
"														<end x='100%' y='80%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='CubicBezier' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='100%' y='80%' />"
"														<end x='80%' y='100%' />"
"														<basePoint1 x='100%' y='100%' />"
"														<basePoint2 x='100%' y='100%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='80%' y='100%' />"
"														<end x='20%' y='100%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='CubicBezier' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='20%' y='100%' />"
"														<end x='0%' y='80%' />"
"														<basePoint1 x='0%' y='100%' />"
"														<basePoint2 x='0%' y='100%' />"
"													</curveSegment>"
"												</listOfCurveSegments>"
"											</polygon>"
"										</g>"
"									</style>"
"									<style roleList='SBO-0000354-multimer-clone'>"
"										<g stroke='black' stroke-width='2' fill='EPNBackgroundGradient'>"
"											<polygon stroke='black' stroke-width='2' fill='EPNBackgroundGradient'>"
"												<listOfCurveSegments>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='5%' y='80%' />"
"														<end x='5%' y='5%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='5%' y='5%' />"
"														<end x='100%' y='5%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='100%' y='5%' />"
"														<end x='100%' y='80%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='CubicBezier' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='100%' y='80%' />"
"														<end x='80%' y='100%' />"
"														<basePoint1 x='100%' y='100%' />"
"														<basePoint2 x='100%' y='100%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='80%' y='100%' />"
"														<end x='20%' y='100%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='CubicBezier' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='20%' y='100%' />"
"														<end x='5%' y='80%' />"
"														<basePoint1 x='5%' y='100%' />"
"														<basePoint2 x='5%' y='100%' />"
"													</curveSegment>"
"												</listOfCurveSegments>"
"											</polygon>"
"											<polygon fill='cloneMarker'>"
"												<listOfCurveSegments>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='5%' y='80%' />"
"														<end x='5%' y='5%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='5%' y='5%' />"
"														<end x='100%' y='5%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='100%' y='5%' />"
"														<end x='100%' y='80%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='CubicBezier' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='100%' y='80%' />"
"														<end x='80%' y='100%' />"
"														<basePoint1 x='100%' y='100%' />"
"														<basePoint2 x='100%' y='100%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='80%' y='100%' />"
"														<end x='20%' y='100%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='CubicBezier' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='20%' y='100%' />"
"														<end x='5%' y='80%' />"
"														<basePoint1 x='5%' y='100%' />"
"														<basePoint2 x='5%' y='100%' />"
"													</curveSegment>"
"												</listOfCurveSegments>"
"											</polygon>"
"											<polygon stroke='black' stroke-width='2' fill='EPNBackgroundGradient'>"
"												<listOfCurveSegments>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='0%' y='80%' />"
"														<end x='0%' y='0%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='0%' y='0%' />"
"														<end x='95%' y='0%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='95%' y='0%' />"
"														<end x='95%' y='80%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='CubicBezier' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='95%' y='80%' />"
"														<end x='80%' y='95%' />"
"														<basePoint1 x='95%' y='95%' />"
"														<basePoint2 x='95%' y='95%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='80%' y='95%' />"
"														<end x='20%' y='95%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='CubicBezier' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='20%' y='95%' />"
"														<end x='0%' y='80%' />"
"														<basePoint1 x='0%' y='95%' />"
"														<basePoint2 x='0%' y='95%' />"
"													</curveSegment>"
"												</listOfCurveSegments>"
"											</polygon>"
"											<polygon fill='cloneMarker'>"
"												<listOfCurveSegments>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='0%' y='80%' />"
"														<end x='0%' y='0%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='0%' y='0%' />"
"														<end x='95%' y='0%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='95%' y='0%' />"
"														<end x='95%' y='80%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='CubicBezier' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='95%' y='80%' />"
"														<end x='80%' y='95%' />"
"														<basePoint1 x='95%' y='95%' />"
"														<basePoint2 x='95%' y='95%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='80%' y='95%' />"
"														<end x='20%' y='95%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='CubicBezier' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='20%' y='95%' />"
"														<end x='0%' y='80%' />"
"														<basePoint1 x='0%' y='95%' />"
"														<basePoint2 x='0%' y='95%' />"
"													</curveSegment>"
"												</listOfCurveSegments>"
"											</polygon>"
"										</g>"
"									</style>"
"									<style roleList='SBO-0000245'>"
"										<g stroke='black' stroke-width='2' fill='EPNBackgroundGradient'>"
"											<rectangle x='0%' y='0%' width='100%' height='100%' rx='10%' />"
"										</g>"
"									</style>"
"									<style roleList='SBO-0000245-multimer'>"
"										<g stroke='black' stroke-width='2' fill='EPNBackgroundGradient'>"
"											<rectangle x='5%' y='5%' width='95%' height='95%' rx='10%' />"
"											<rectangle x='0%' y='0%' width='95%' height='95%' rx='10%' />"
"										</g>"
"									</style>"
"									<style roleList='SBO-0000245-clone'>"
"										<g stroke='black' stroke-width='2' fill='EPNBackgroundGradient'>"
"											<rectangle x='0%' y='0%' width='100%' height='100%' rx='10%' />"
"											<rectangle fill='cloneMarker' x='0%' y='0%' width='100%' height='100%' rx='10%' />"
"										</g>"
"									</style>"
"									<style roleList='SBO-0000245-multimer-clone'>"
"										<g stroke='black' stroke-width='2' fill='EPNBackgroundGradient'>"
"											<rectangle x='5%' y='5%' width='95%' height='95%' rx='10%' />"
"											<rectangle fill='cloneMarker' x='5%' y='5%' width='95%' height='95%' rx='10%' />"
"											<rectangle x='0%' y='0%' width='95%' height='95%' rx='10%' />"
"											<rectangle fill='cloneMarker' x='0%' y='0%' width='95%' height='95%' rx='10%' />"
"										</g>"
"									</style>"
"									<style roleList='SBO-0000285 NO-SBO'>"
"										<g stroke='black' stroke-width='2' fill='EPNBackgroundGradient'>"
"											<ellipse cx='50%' cy='50%' cz='0.0' rx='50%' ry='50%' />"
"										</g>"
"									</style>"
"									<style roleList='SBO-0000285-multimer NO-SBO-multimer'>"
"										<g stroke='black' stroke-width='2' fill='EPNBackgroundGradient'>"
"											<ellipse cx='45%' cy='45%' cz='0.0' rx='45%' ry='45%' />"
"											<ellipse cx='55%' cy='55%' cz='0.0' rx='45%' ry='45%' />"
"										</g>"
"									</style>"
"									<style roleList='SBO-0000285-clone NO-SBO-clone'>"
"										<g stroke='black' stroke-width='2' fill='EPNBackgroundGradient'>"
"											<ellipse cx='50%' cy='50%' cz='0.0' rx='50%' ry='50%' />"
"											<ellipse fill='cloneMarker' cx='50%' cy='50%' cz='0.0' rx='50%' ry='50%' />"
"										</g>"
"									</style>"
"									<style roleList='SBO-0000285-multimer-clone NO-SBO-multimer-clone'>"
"										<g stroke='black' stroke-width='2' fill='EPNBackgroundGradient'>"
"											<ellipse cx='45%' cy='45%' cz='0.0' rx='45%' ry='45%' />"
"											<ellipse fill='cloneMarker' cx='45%' cy='45%' cz='0.0' rx='45%' ry='45%' />"
"											<ellipse cx='55%' cy='55%' cz='0.0' rx='45%' ry='45%' />"
"											<ellipse fill='cloneMarker' cx='55%' cy='55%' cz='0.0' rx='45%' ry='45%' />"
"										</g>"
"									</style>"
"									<style roleList='SBO-0000247'>"
"										<g stroke='black' stroke-width='2' fill='EPNBackgroundGradient'>"
"											<ellipse cx='50%' cy='50%' cz='0.0' rx='50%' ry='50%' />"
"										</g>"
"									</style>"
"									<style roleList='SBO-0000247-multimer'>"
"										<g stroke='black' stroke-width='2' fill='EPNBackgroundGradient'>"
"											<ellipse cx='55%' cy='55%' cz='0.0' rx='45%' ry='45%' />"
"											<ellipse cx='45%' cy='45%' cz='0.0' rx='45%' ry='45%' />"
"										</g>"
"									</style>"
"									<style roleList='SBO-0000247-clone'>"
"										<g stroke='black' stroke-width='2' fill='EPNBackgroundGradient'>"
"											<ellipse cx='50%' cy='50%' cz='0.0' rx='50%' ry='50%' />"
"											<ellipse fill='cloneMarker' cx='50%' cy='50%' cz='0.0' rx='50%' ry='50%' />"
"										</g>"
"									</style>"
"									<style roleList='SBO-0000247-multimer-clone'>"
"										<g stroke='black' stroke-width='2' fill='EPNBackgroundGradient'>"
"											<ellipse cx='55%' cy='55%' cz='0.0' rx='45%' ry='45%' />"
"											<ellipse fill='cloneMarker' cx='55%' cy='55%' cz='0.0' rx='45%' ry='45%' />"
"											<ellipse cx='45%' cy='45%' cz='0.0' rx='45%' ry='45%' />"
"											<ellipse fill='cloneMarker' cx='45%' cy='45%' cz='0.0' rx='45%' ry='45%' />"
"										</g>"
"									</style>"
"									<style roleList='SBO-0000291-clone SBO-0000291'>"
"										<g stroke='EmptySetOutline' stroke-width='1.25' fill='EmptySetGradient'>"
"											<curve>"
"												<listOfCurveSegments>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='0%' y='100%' />"
"														<end x='100%' y='0%' />"
"													</curveSegment>"
"												</listOfCurveSegments>"
"											</curve>"
"											<curve>"
"												<listOfCurveSegments>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='0%' y='100%' />"
"														<end x='100%' y='0%' />"
"													</curveSegment>"
"												</listOfCurveSegments>"
"											</curve>"
"											<curve>"
"												<listOfCurveSegments>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='0%' y='100%' />"
"														<end x='100%' y='0%' />"
"													</curveSegment>"
"												</listOfCurveSegments>"
"											</curve>"
"											<ellipse cx='50%' cy='50%' cz='0.0' rx='50%' ry='50%' />"
"											<g>"
"												<curve>"
"													<listOfCurveSegments>"
"														<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"															<start x='0%' y='100%' />"
"															<end x='100%' y='0%' />"
"														</curveSegment>"
"													</listOfCurveSegments>"
"												</curve>"
"											</g>"
"										</g>"
"									</style>"
"									<style roleList='SBO-0000253'>"
"										<g stroke='black' stroke-width='2' fill='EPNBackgroundGradient'>"
"											<polygon stroke='black' stroke-width='2' fill='EPNBackgroundGradient'>"
"												<listOfCurveSegments>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='0%' y='10%' />"
"														<end x='10%' y='0%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='10%' y='0%' />"
"														<end x='90%' y='0%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='90%' y='0%' />"
"														<end x='100%' y='10%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='100%' y='10%' />"
"														<end x='100%' y='90%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='100%' y='90%' />"
"														<end x='90%' y='100%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='90%' y='100%' />"
"														<end x='10%' y='100%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='10%' y='100%' />"
"														<end x='0%' y='90%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='0%' y='90%' />"
"														<end x='0%' y='10%' />"
"													</curveSegment>"
"												</listOfCurveSegments>"
"											</polygon>"
"										</g>"
"									</style>"
"									<style roleList='SBO-0000253-multimer'>"
"										<g stroke='black' stroke-width='2' fill='EPNBackgroundGradient'>"
"											<polygon stroke='black' stroke-width='2' fill='EPNBackgroundGradient'>"
"												<listOfCurveSegments>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='5%' y='15%' />"
"														<end x='15%' y='5%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='15%' y='5%' />"
"														<end x='90%' y='5%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='90%' y='5%' />"
"														<end x='100%' y='15%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='100%' y='15%' />"
"														<end x='100%' y='90%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='100%' y='90%' />"
"														<end x='90%' y='100%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='90%' y='100%' />"
"														<end x='15%' y='100%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='15%' y='100%' />"
"														<end x='5%' y='90%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='5%' y='90%' />"
"														<end x='5%' y='15%' />"
"													</curveSegment>"
"												</listOfCurveSegments>"
"											</polygon>"
"											<polygon stroke='black' stroke-width='2' fill='EPNBackgroundGradient'>"
"												<listOfCurveSegments>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='0%' y='10%' />"
"														<end x='10%' y='0%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='10%' y='0%' />"
"														<end x='85%' y='0%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='85%' y='0%' />"
"														<end x='95%' y='10%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='95%' y='10%' />"
"														<end x='95%' y='85%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='95%' y='85%' />"
"														<end x='85%' y='95%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='85%' y='95%' />"
"														<end x='10%' y='95%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='10%' y='95%' />"
"														<end x='0%' y='85%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='0%' y='85%' />"
"														<end x='0%' y='10%' />"
"													</curveSegment>"
"												</listOfCurveSegments>"
"											</polygon>"
"										</g>"
"									</style>"
"									<style roleList='SBO-0000253-clone'>"
"										<g stroke='black' stroke-width='2' fill='EPNBackgroundGradient'>"
"											<polygon stroke='black' stroke-width='2' fill='EPNBackgroundGradient'>"
"												<listOfCurveSegments>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='0%' y='10%' />"
"														<end x='10%' y='0%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='10%' y='0%' />"
"														<end x='90%' y='0%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='90%' y='0%' />"
"														<end x='100%' y='10%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='100%' y='10%' />"
"														<end x='100%' y='90%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='100%' y='90%' />"
"														<end x='90%' y='100%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='90%' y='100%' />"
"														<end x='10%' y='100%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='10%' y='100%' />"
"														<end x='0%' y='90%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='0%' y='90%' />"
"														<end x='0%' y='10%' />"
"													</curveSegment>"
"												</listOfCurveSegments>"
"											</polygon>"
"											<polygon fill='cloneMarker'>"
"												<listOfCurveSegments>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='0%' y='10%' />"
"														<end x='10%' y='0%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='10%' y='0%' />"
"														<end x='90%' y='0%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='90%' y='0%' />"
"														<end x='100%' y='10%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='100%' y='10%' />"
"														<end x='100%' y='90%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='100%' y='90%' />"
"														<end x='90%' y='100%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='90%' y='100%' />"
"														<end x='10%' y='100%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='10%' y='100%' />"
"														<end x='0%' y='90%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='0%' y='90%' />"
"														<end x='0%' y='10%' />"
"													</curveSegment>"
"												</listOfCurveSegments>"
"											</polygon>"
"										</g>"
"									</style>"
"									<style roleList='SBO-0000253-multimer-clone'>"
"										<g stroke='black' stroke-width='2' fill='EPNBackgroundGradient'>"
"											<polygon stroke='black' stroke-width='2' fill='EPNBackgroundGradient'>"
"												<listOfCurveSegments>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='5%' y='15%' />"
"														<end x='15%' y='5%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='15%' y='5%' />"
"														<end x='90%' y='5%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='90%' y='5%' />"
"														<end x='100%' y='15%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='100%' y='15%' />"
"														<end x='100%' y='90%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='100%' y='90%' />"
"														<end x='90%' y='100%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='90%' y='100%' />"
"														<end x='15%' y='100%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='15%' y='100%' />"
"														<end x='5%' y='90%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='5%' y='90%' />"
"														<end x='5%' y='15%' />"
"													</curveSegment>"
"												</listOfCurveSegments>"
"											</polygon>"
"											<polygon fill='cloneMarker'>"
"												<listOfCurveSegments>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='5%' y='15%' />"
"														<end x='15%' y='5%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='15%' y='5%' />"
"														<end x='90%' y='5%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='90%' y='5%' />"
"														<end x='100%' y='15%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='100%' y='15%' />"
"														<end x='100%' y='90%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='100%' y='90%' />"
"														<end x='90%' y='100%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='90%' y='100%' />"
"														<end x='15%' y='100%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='15%' y='100%' />"
"														<end x='5%' y='90%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='5%' y='90%' />"
"														<end x='5%' y='15%' />"
"													</curveSegment>"
"												</listOfCurveSegments>"
"											</polygon>"
"											<polygon stroke='black' stroke-width='2' fill='EPNBackgroundGradient'>"
"												<listOfCurveSegments>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='0%' y='10%' />"
"														<end x='10%' y='0%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='10%' y='0%' />"
"														<end x='85%' y='0%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='85%' y='0%' />"
"														<end x='95%' y='10%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='95%' y='10%' />"
"														<end x='95%' y='85%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='95%' y='85%' />"
"														<end x='85%' y='95%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='85%' y='95%' />"
"														<end x='10%' y='95%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='10%' y='95%' />"
"														<end x='0%' y='85%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='0%' y='85%' />"
"														<end x='0%' y='10%' />"
"													</curveSegment>"
"												</listOfCurveSegments>"
"											</polygon>"
"											<polygon fill='cloneMarker'>"
"												<listOfCurveSegments>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='0%' y='10%' />"
"														<end x='10%' y='0%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='10%' y='0%' />"
"														<end x='85%' y='0%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='85%' y='0%' />"
"														<end x='95%' y='10%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='95%' y='10%' />"
"														<end x='95%' y='85%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='95%' y='85%' />"
"														<end x='85%' y='95%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='85%' y='95%' />"
"														<end x='10%' y='95%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='10%' y='95%' />"
"														<end x='0%' y='85%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='0%' y='85%' />"
"														<end x='0%' y='10%' />"
"													</curveSegment>"
"												</listOfCurveSegments>"
"											</polygon>"
"										</g>"
"									</style>"
"									<style roleList='SBO-0000405'>"
"										<g stroke='black' stroke-width='2' fill='EPNBackgroundGradient'>"
"											<polygon stroke='black' stroke-width='2' fill='EPNBackgroundGradient'>"
"												<listOfCurveSegments>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='0%' y='0%' />"
"														<end x='100%' y='0%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='100%' y='0%' />"
"														<end x='80%' y='50%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='80%' y='50%' />"
"														<end x='100%' y='100%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='100%' y='100%' />"
"														<end x='0%' y='100%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='0%' y='100%' />"
"														<end x='20%' y='50%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='20%' y='50%' />"
"														<end x='0%' y='0%' />"
"													</curveSegment>"
"												</listOfCurveSegments>"
"											</polygon>"
"										</g>"
"									</style>"
"									<style roleList='SBO-0000405-clone'>"
"										<g stroke='black' stroke-width='2' fill='EPNBackgroundGradient'>"
"											<polygon stroke='black' stroke-width='2' fill='EPNBackgroundGradient'>"
"												<listOfCurveSegments>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='0%' y='0%' />"
"														<end x='100%' y='0%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='100%' y='0%' />"
"														<end x='80%' y='50%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='80%' y='50%' />"
"														<end x='100%' y='100%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='100%' y='100%' />"
"														<end x='0%' y='100%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='0%' y='100%' />"
"														<end x='20%' y='50%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='20%' y='50%' />"
"														<end x='0%' y='0%' />"
"													</curveSegment>"
"												</listOfCurveSegments>"
"											</polygon>"
"											<polygon fill='cloneMarker'>"
"												<listOfCurveSegments>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='0%' y='0%' />"
"														<end x='100%' y='0%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='100%' y='0%' />"
"														<end x='80%' y='50%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='80%' y='50%' />"
"														<end x='100%' y='100%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='100%' y='100%' />"
"														<end x='0%' y='100%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='0%' y='100%' />"
"														<end x='20%' y='50%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='20%' y='50%' />"
"														<end x='0%' y='0%' />"
"													</curveSegment>"
"												</listOfCurveSegments>"
"											</polygon>"
"										</g>"
"									</style>"
"									<style roleList='Tag'>"
"										<g stroke='black' stroke-width='2' fill='EPNBackgroundGradient'>"
"											<polygon stroke='black' stroke-width='2' fill='EPNBackgroundGradient'>"
"												<listOfCurveSegments>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='0%' y='0%' />"
"														<end x='75%' y='0%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='75%' y='0%' />"
"														<end x='100%' y='50%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='100%' y='50%' />"
"														<end x='75%' y='100%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='75%' y='100%' />"
"														<end x='0%' y='100%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='0%' y='100%' />"
"														<end x='0%' y='0%' />"
"													</curveSegment>"
"												</listOfCurveSegments>"
"											</polygon>"
"										</g>"
"									</style>"
"									<style roleList='Tag-right'>"
"										<g stroke='black' stroke-width='2' fill='EPNBackgroundGradient'>"
"											<polygon stroke='black' stroke-width='2' fill='EPNBackgroundGradient'>"
"												<listOfCurveSegments>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='0%' y='0%' />"
"														<end x='75%' y='0%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='75%' y='0%' />"
"														<end x='100%' y='50%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='100%' y='50%' />"
"														<end x='75%' y='100%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='75%' y='100%' />"
"														<end x='0%' y='100%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='0%' y='100%' />"
"														<end x='0%' y='0%' />"
"													</curveSegment>"
"												</listOfCurveSegments>"
"											</polygon>"
"										</g>"
"									</style>"
"									<style roleList='Tag-up'>"
"										<g stroke='black' stroke-width='2' fill='EPNBackgroundGradient'>"
"											<polygon stroke='black' stroke-width='2' fill='EPNBackgroundGradient'>"
"												<listOfCurveSegments>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='0%' y='25%' />"
"														<end x='50%' y='0%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='50%' y='0%' />"
"														<end x='100%' y='25%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='100%' y='25%' />"
"														<end x='100%' y='100%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='100%' y='100%' />"
"														<end x='0%' y='100%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='0%' y='100%' />"
"														<end x='0%' y='25%' />"
"													</curveSegment>"
"												</listOfCurveSegments>"
"											</polygon>"
"										</g>"
"									</style>"
"									<style roleList='Tag-down'>"
"										<g stroke='black' stroke-width='2' fill='EPNBackgroundGradient'>"
"											<polygon stroke='black' stroke-width='2' fill='EPNBackgroundGradient'>"
"												<listOfCurveSegments>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='0%' y='0%' />"
"														<end x='100%' y='0%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='100%' y='0%' />"
"														<end x='100%' y='75%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='100%' y='75%' />"
"														<end x='50%' y='100%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='50%' y='100%' />"
"														<end x='0%' y='75%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='0%' y='75%' />"
"														<end x='0%' y='0%' />"
"													</curveSegment>"
"												</listOfCurveSegments>"
"											</polygon>"
"										</g>"
"									</style>"
"									<style roleList='Tag-left'>"
"										<g stroke='black' stroke-width='2' fill='EPNBackgroundGradient'>"
"											<polygon stroke='black' stroke-width='2' fill='EPNBackgroundGradient'>"
"												<listOfCurveSegments>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='25%' y='0%' />"
"														<end x='100%' y='0%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='100%' y='0%' />"
"														<end x='100%' y='100%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='100%' y='100%' />"
"														<end x='25%' y='100%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='25%' y='100%' />"
"														<end x='0%' y='50%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='0%' y='50%' />"
"														<end x='25%' y='0%' />"
"													</curveSegment>"
"												</listOfCurveSegments>"
"											</polygon>"
"										</g>"
"									</style>"
"									<style roleList='SBO-0000358'>"
"										<g stroke='black' stroke-width='2' fill='EPNBackgroundGradient'>"
"											<polygon stroke='black' stroke-width='2' fill='EPNBackgroundGradient'>"
"												<listOfCurveSegments>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='20%' y='0%' />"
"														<end x='80%' y='0%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='80%' y='0%' />"
"														<end x='100%' y='50%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='100%' y='50%' />"
"														<end x='80%' y='100%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='80%' y='100%' />"
"														<end x='20%' y='100%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='20%' y='100%' />"
"														<end x='0%' y='50%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='0%' y='50%' />"
"														<end x='20%' y='0%' />"
"													</curveSegment>"
"												</listOfCurveSegments>"
"											</polygon>"
"										</g>"
"									</style>"
"									<style roleList='SBO-0000358-clone'>"
"										<g stroke='black' stroke-width='2' fill='EPNBackgroundGradient'>"
"											<polygon stroke='black' stroke-width='2' fill='EPNBackgroundGradient'>"
"												<listOfCurveSegments>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='20%' y='0%' />"
"														<end x='80%' y='0%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='80%' y='0%' />"
"														<end x='100%' y='50%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='100%' y='50%' />"
"														<end x='80%' y='100%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='80%' y='100%' />"
"														<end x='20%' y='100%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='20%' y='100%' />"
"														<end x='0%' y='50%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='0%' y='50%' />"
"														<end x='20%' y='0%' />"
"													</curveSegment>"
"												</listOfCurveSegments>"
"											</polygon>"
"											<polygon fill='cloneMarker'>"
"												<listOfCurveSegments>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='20%' y='0%' />"
"														<end x='80%' y='0%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='80%' y='0%' />"
"														<end x='100%' y='50%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='100%' y='50%' />"
"														<end x='80%' y='100%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='80%' y='100%' />"
"														<end x='20%' y='100%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='20%' y='100%' />"
"														<end x='0%' y='50%' />"
"													</curveSegment>"
"													<curveSegment xsi:type='LineSegment' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>"
"														<start x='0%' y='50%' />"
"														<end x='20%' y='0%' />"
"													</curveSegment>"
"												</listOfCurveSegments>"
"											</polygon>"
"										</g>"
"									</style>"
"									<style roleList='SBO-0000289'>"
"										<g stroke='CompartmentBorder' stroke-width='7' fill='CompartmentGradient'>"
"											<rectangle x='0' y='0' width='100%' height='100%' rx='10%' ry='10%' />"
"										</g>"
"									</style>"
"								</listOfStyles>"
"							</renderInformation>"
"						</listOfRenderInformation>"
"					</annotation>"
"					<dimensions width='594.498005126953' height='62.677000213623' />"
"					<listOfSpeciesGlyphs>"
"						<speciesGlyph id='layout_glyph_1' render:objectRole='SBO-0000285' species='species_1' xmlns:render='http://projects.eml.org/bcb/sbml/render/level2'>"
"							<boundingBox>"
"								<position x='226.449' y='12.1385' />"
"								<dimensions width='141.6' height='38.4' />"
"							</boundingBox>"
"						</speciesGlyph>"
"					</listOfSpeciesGlyphs>"
"					<listOfTextGlyphs>"
"						<textGlyph id='layout_glyph_2' render:objectRole='defaultText' graphicalObject='layout_glyph_1' originOfText='species_1' xmlns:render='http://projects.eml.org/bcb/sbml/render/level2'>"
"							<boundingBox>"
"								<position x='238.249' y='15.3385' />"
"								<dimensions width='118' height='32' />"
"							</boundingBox>"
"						</textGlyph>"
"					</listOfTextGlyphs>"
"				</layout>"
"			</listOfLayouts>"
"		</annotation>"
"		<listOfCompartments>"
"			<compartment id='compartment_1' name='compartment' spatialDimensions='3' size='1' constant='true' />"
"		</listOfCompartments>"
"		<listOfSpecies>"
"			<species id='species_1' name='species_1' compartment='compartment_1' initialConcentration='1' hasOnlySubstanceUnits='false' boundaryCondition='false' constant='false' />"
"		</listOfSpecies>"
"	</model>"
"</sbml>";


const char* MODEL_1=
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
"<sbml xmlns=\"http://www.sbml.org/sbml/level2\" level=\"2\" version=\"1\">\n"
"  <model id=\"TestModel_with_modifiers\">\n"
"    <annotation>\n"
"      <listOfLayouts xmlns=\"http://projects.eml.org/bcb/sbml/level2\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">\n"
"          <annotation>\n"
"            <listOfGlobalRenderInformation xmlns=\"http://projects.eml.org/bcb/sbml/render/level2\">\n"
"              <renderInformation id=\"wireFrame\" name=\"wireframe style\" programName=\"Ralph Gauges\" programVersion=\"1.0\">\n"
"                <listOfColorDefinitions>\n"
"                  <colorDefinition id=\"white\" value=\"#FFFFFF\"/>\n"
"                  <colorDefinition id=\"black\" value=\"#000000\"/>\n"
"                </listOfColorDefinitions>\n"
"                <listOfStyles>\n"
"                  <style id=\"compartmentGlyphStyle\" typeList=\"COMPARTMENTGLYPH\">\n"
"                    <g stroke=\"black\" stroke-width=\"1.0\">\n"
"                      <rectangle x=\"0%\" y=\"0%\" width=\"100%\" height=\"100%\" rx=\"0%\" fill=\"none\"/>\n"
"                    </g>\n"
"                  </style>  \n"
"                  <style id=\"speciesGlyphStyle\" typeList=\"SPECIESGLYPH\">\n"
"                    <g stroke=\"black\" stroke-width=\"1.0\">\n"
"                      <rectangle x=\"0%\" y=\"0%\" width=\"100%\" height=\"100%\" rx=\"0%\" fill=\"none\"/>\n"
"                    </g>\n"
"                  </style>\n"
"                  <style id=\"reactionGlyphStyle\" typeList=\"SPECIESREFERENCEGLYPH REACTIONGLYPH TEXTGLYPH\">\n"
"                    <g stroke=\"black\" stroke-width=\"1.0\" font-size=\"12\" text-anchor=\"middle\"/>\n"
"                  </style>\n"
"                </listOfStyles>\n"
"              </renderInformation>\n"
"              <renderInformation id=\"defaultGrayStyle\" name=\"grayscale style\" programName=\"Ralph Gauges\" programVersion=\"1.0\">\n"
"                <listOfColorDefinitions>\n"
"                  <colorDefinition id=\"lightGray\" value=\"#CECECE\"/>\n"
"                  <colorDefinition id=\"white\" value=\"#FFFFFF\"/>\n"
"                  <colorDefinition id=\"black\" value=\"#000000\"/>\n"
"                  <colorDefinition id=\"lightGray2\" value=\"#F0F0F0\"/>\n"
"                  <colorDefinition id=\"gray\" value=\"#0B0B0B\"/>\n"
"                </listOfColorDefinitions>\n"
"                <listOfGradientDefinitions>\n"
"                  <radialGradient id=\"speciesGlyphGradient\">\n"
"                    <stop offset=\"0%\" stop-color=\"white\"/>\n"
"                    <stop offset=\"100%\" stop-color=\"lightGray\"/>\n"
"                  </radialGradient>\n"
"                </listOfGradientDefinitions>\n"
"                <listOfLineEndings>\n"
"                 <lineEnding id=\"simpleHead_black\">\n"
"                   <boundingBox xmlns=\"http://projects.eml.org/bcb/sbml/level2\">\n"
"                     <position x=\"-8\" y=\"-3\"/>\n"
"                     <dimensions width=\"10\" height=\"6\"/>\n"
"                   </boundingBox>\n"
"                   <g stroke=\"black\" stroke-width=\"1.0\" fill=\"black\">\n"
"                     <polygon>\n"
"                       <listOfElements>\n"
"                         <element xsi:type=\"RenderPoint\" x=\"0\" y=\"0\"/>\n"
"                         <element xsi:type=\"RenderPoint\" x=\"10\" y=\"3\"/>\n"
"                         <element xsi:type=\"RenderPoint\" x=\"0\" y=\"6\"/>\n"
"                       </listOfElements>\n"
"                     </polygon>\n"
"                   </g>\n"
"                 </lineEnding>\n"
"                </listOfLineEndings>\n"
"                <listOfStyles>\n"
"                  <style id=\"compartmentGlyphStyle\" typeList=\"COMPARTMENTGLYPH\">\n"
"                    <g stroke=\"gray\" stroke-width=\"1.0\">\n"
"                      <rectangle x=\"0%\" y=\"0%\" width=\"100%\" height=\"100%\" rx=\"5%\" fill=\"lightGray2\"/>\n"
"                    </g>\n"
"                  </style>  \n"
"                  <style id=\"speciesGlyphStyle\" typeList=\"SPECIESGLYPH\">\n"
"                    <g stroke=\"black\" stroke-width=\"1.0\">\n"
"                      <rectangle x=\"0%\" y=\"0%\" width=\"100%\" height=\"100%\" rx=\"5%\" fill=\"speciesGlyphGradient\"/>\n"
"                    </g>\n"
"                  </style>\n"
"                  <style id=\"reactionGlyphStyle\" typeList=\"REACTIONGLYPH TEXTGLYPH\">\n"
"                    <g stroke=\"black\" stroke-width=\"1.0\" font-size=\"12\" text-anchor=\"middle\"/>\n"
"                  </style>\n"
"                  <style id=\"speciesReferenceGlyphStyle\" roleList=\"substrate sidesubstrate product sideproduct\">\n"
"                    <g stroke=\"#000000\" stroke-width=\"1.0\"/>\n"
"                  </style>\n"
"                  <style id=\"activatorSpeciesReferenceGlyphStyle\" roleList=\"activator\">\n"
"                    <g stroke=\"#000000\" stroke-width=\"1.0\"/>\n"
"                  </style>\n"
"                  <style id=\"modifierSpeciesReferenceGlyphStyle\" roleList=\"modifier\">\n"
"                    <g stroke=\"#000000\" stroke-width=\"1.0\"/>\n"
"                  </style>\n"
"                  <style id=\"inhibitorSpeciesReferenceGlyphStyle\" roleList=\"inhibitor\">\n"
"                    <g stroke=\"#000000\" stroke-width=\"1.0\"/>\n"
"                  </style>\n"
"                </listOfStyles>\n"
"              </renderInformation>\n"
"              <!-- references defaultStyle but redefines the colors to make a grayscale image instead of a colored one -->\n"
"              <renderInformation id=\"shortGrayStyle\" name=\"modified default style to grayscale\" referenceRenderInformation=\"defaultStyle\" programName=\"Ralph Gauges\" programVersion=\"1.0\">\n"
"                <listOfColorDefinitions>\n"
"                  <colorDefinition id=\"lightBlue\" value=\"#CECECE\"/>\n"
"                  <colorDefinition id=\"white\" value=\"#FFFFFF\"/>\n"
"                  <colorDefinition id=\"black\" value=\"#000000\"/>\n"
"                  <colorDefinition id=\"red\" value=\"#000000\"/>\n"
"                  <colorDefinition id=\"green\" value=\"#000000\"/>\n"
"                  <colorDefinition id=\"blue\" value=\"#000000\"/>\n"
"                  <colorDefinition id=\"lightYellow\" value=\"#F0F0F0\"/>\n"
"                  <colorDefinition id=\"darkGreen\" value=\"#0B0B0B\"/>\n"
"                </listOfColorDefinitions>\n"
"              </renderInformation>\n"
"              <!-- Normal color style similar to the old XSLT stylesheet -->\n"
"              <renderInformation id=\"defaultStyle\" name=\"default style\" programName=\"Ralph Gauges\" programVersion=\"1.0\">\n"
"                <listOfColorDefinitions>\n"
"                  <colorDefinition id=\"lightBlue\" value=\"#ADD8E6\"/>\n"
"                  <colorDefinition id=\"white\" value=\"#FFFFFF\"/>\n"
"                  <colorDefinition id=\"black\" value=\"#000000\"/>\n"
"                  <colorDefinition id=\"red\" value=\"#FF0000\"/>\n"
"                  <colorDefinition id=\"green\" value=\"#00FF00\"/>\n"
"                  <colorDefinition id=\"blue\" value=\"#0000FF\"/>\n"
"                  <colorDefinition id=\"lightYellow\" value=\"#FFFFD1\"/>\n"
"                  <colorDefinition id=\"darkGreen\" value=\"#002000\"/>\n"
"                </listOfColorDefinitions>\n"
"                <listOfGradientDefinitions>\n"
"                  <radialGradient id=\"speciesGlyphGradient\">\n"
"                    <stop offset=\"0%\" stop-color=\"white\"/>\n"
"                    <stop offset=\"100%\" stop-color=\"lightBlue\"/>\n"
"                  </radialGradient>\n"
"                </listOfGradientDefinitions>\n"
"                <listOfLineEndings>\n"
"                 <lineEnding id=\"simpleHead_black\">\n"
"                   <boundingBox xmlns=\"http://projects.eml.org/bcb/sbml/level2\">\n"
"                     <position x=\"-8\" y=\"-3\"/>\n"
"                     <dimensions width=\"10\" height=\"6\"/>\n"
"                   </boundingBox>\n"
"                   <g stroke=\"black\" stroke-width=\"1.0\" fill=\"black\">\n"
"                     <polygon>\n"
"                       <listOfElements>\n"
"                         <element xsi:type=\"RenderPoint\" x=\"0\" y=\"0\"/>\n"
"                         <element xsi:type=\"RenderPoint\" x=\"10\" y=\"3\"/>\n"
"                         <element xsi:type=\"RenderPoint\" x=\"0\" y=\"6\"/>\n"
"                       </listOfElements>\n"
"                     </polygon>\n"
"                   </g>\n"
"                 </lineEnding>\n"
"                 <lineEnding id=\"simpleHead_red\">\n"
"                   <boundingBox xmlns=\"http://projects.eml.org/bcb/sbml/level2\">\n"
"                     <position x=\"-8\" y=\"-3\"/>\n"
"                     <dimensions width=\"10\" height=\"6\"/>\n"
"                   </boundingBox>\n"
"                   <g stroke=\"red\" stroke-width=\"1.0\" fill=\"red\">\n"
"                     <polygon>\n"
"                       <listOfElements>\n"
"                         <element xsi:type=\"RenderPoint\" x=\"0\" y=\"0\"/>\n"
"                         <element xsi:type=\"RenderPoint\" x=\"10\" y=\"3\"/>\n"
"                         <element xsi:type=\"RenderPoint\" x=\"0\" y=\"6\"/>\n"
"                       </listOfElements>\n"
"                     </polygon>\n"
"                   </g>\n"
"                 </lineEnding>\n"
"                 <lineEnding id=\"simpleHead_green\">\n"
"                   <boundingBox xmlns=\"http://projects.eml.org/bcb/sbml/level2\">\n"
"                     <position x=\"-8\" y=\"-3\"/>\n"
"                     <dimensions width=\"10\" height=\"6\"/>\n"
"                   </boundingBox>\n"
"                   <g stroke=\"green\" stroke-width=\"1.0\" fill=\"green\">\n"
"                     <polygon>\n"
"                       <listOfElements>\n"
"                         <element xsi:type=\"RenderPoint\" x=\"0\" y=\"0\"/>\n"
"                         <element xsi:type=\"RenderPoint\" x=\"10\" y=\"3\"/>\n"
"                         <element xsi:type=\"RenderPoint\" x=\"0\" y=\"6\"/>\n"
"                       </listOfElements>\n"
"                     </polygon>\n"
"                   </g>\n"
"                 </lineEnding>\n"
"                 <lineEnding id=\"simpleHead_blue\">\n"
"                   <boundingBox xmlns=\"http://projects.eml.org/bcb/sbml/level2\">\n"
"                     <position x=\"-8\" y=\"-3\"/>\n"
"                     <dimensions width=\"10\" height=\"6\"/>\n"
"                   </boundingBox>\n"
"                   <g stroke=\"blue\" stroke-width=\"1.0\" fill=\"blue\">\n"
"                     <polygon>\n"
"                       <listOfElements>\n"
"                         <element xsi:type=\"RenderPoint\" x=\"0\" y=\"0\"/>\n"
"                         <element xsi:type=\"RenderPoint\" x=\"10\" y=\"3\"/>\n"
"                         <element xsi:type=\"RenderPoint\" x=\"0\" y=\"6\"/>\n"
"                       </listOfElements>\n"
"                     </polygon>\n"
"                   </g>\n"
"                 </lineEnding>\n"
"                </listOfLineEndings>\n"
"                <listOfStyles>\n"
"                  <style id=\"compartmentGlyphStyle\" typeList=\"COMPARTMENTGLYPH\">\n"
"                    <g stroke=\"darkGreen\" stroke-width=\"1.0\">\n"
"                      <rectangle x=\"0%\" y=\"0%\" width=\"100%\" height=\"100%\" rx=\"10%\" ry=\"10%\" fill=\"lightYellow\"/>\n"
"                    </g>\n"
"                  </style>  \n"
"                  <style id=\"speciesGlyphStyle\" typeList=\"SPECIESGLYPH\">\n"
"                    <g stroke=\"black\" stroke-width=\"1.0\">\n"
"                      <rectangle x=\"0%\" y=\"0%\" width=\"100%\" height=\"100%\" rx=\"5\" ry=\"50%\" fill=\"speciesGlyphGradient\"/>\n"
"                    </g>\n"
"                  </style>\n"
"                  <style id=\"reactionGlyphStyle\" typeList=\"REACTIONGLYPH TEXTGLYPH\">\n"
"                    <g stroke=\"black\" stroke-width=\"1.0\" font-size=\"12\" text-anchor=\"middle\"/>\n"
"                  </style>\n"
"                  <style id=\"speciesReferenceGlyphStyle\" roleList=\"substrate sidesubstrate product sideproduct\">\n"
"                    <g stroke=\"#000000\" stroke-width=\"1.0\" endHead=\"simpleHead_black\"/>\n"
"                  </style>\n"
"                  <style id=\"activatorSpeciesReferenceGlyphStyle\" roleList=\"activator\">\n"
"                    <g stroke=\"green\" stroke-width=\"1.0\" endHead=\"simpleHead_green\"/>\n"
"                  </style>\n"
"                  <style id=\"modifierSpeciesReferenceGlyphStyle\" roleList=\"modifier\">\n"
"                    <g stroke=\"blue\" stroke-width=\"1.0\" endHead=\"simpleHead_blue\"/>\n"
"                  </style>\n"
"                  <style id=\"inhibitorSpeciesReferenceGlyphStyle\" roleList=\"inhibitor\">\n"
"                    <g stroke=\"red\" stroke-width=\"1.0\" endHead=\"simpleHead_red\"/>\n"
"                  </style>\n"
"                </listOfStyles>\n"
"              </renderInformation>\n"
"            </listOfGlobalRenderInformation>\n"
"          </annotation>\n"
"          <layout id=\"Layout_1\">\n"
"          <annotation>\n"
"            <listOfRenderInformation xmlns=\"http://projects.eml.org/bcb/sbml/render/level2\">\n"
"              <renderInformation id=\"highlightGlucose\" referenceRenderInformation=\"defaultStyle\" programName=\"Ralph Gauges\" programVersion=\"1.0\">\n"
"                <listOfColorDefinitions>\n"
"                  <colorDefinition id=\"lightRed\" value=\"#E6ADD8\"/>\n"
"                  <colorDefinition id=\"white\" value=\"#FFFFFF\"/>\n"
"                </listOfColorDefinitions>\n"
"                <listOfGradientDefinitions>\n"
"                  <radialGradient id=\"highlightedSpeciesGlyphGradient\">\n"
"                    <stop offset=\"0%\" stop-color=\"white\"/>\n"
"                    <stop offset=\"100%\" stop-color=\"lightRed\"/>\n"
"                  </radialGradient>\n"
"                </listOfGradientDefinitions>\n"
"                <listOfStyles>\n"
"                  <style id=\"highlightedGlucose\" idList=\"SpeciesGlyph_Glucose\">\n"
"                    <g stroke=\"black\" stroke-width=\"1.0\">\n"
"                      <rectangle x=\"0%\" y=\"0%\" width=\"100%\" height=\"100%\" rx=\"5\" ry=\"50%\" fill=\"highlightedSpeciesGlyphGradient\"/>\n"
"                    </g>\n"
"                  </style>\n"
"                </listOfStyles>\n"
"              </renderInformation>\n"
"            </listOfRenderInformation>\n"
"          </annotation>\n"
"          <dimensions width=\"400\" height=\"230\"/>\n"
"          <listOfCompartmentGlyphs>\n"
"              <compartmentGlyph id=\"CompartmentGlyph_1\" compartment=\"Yeast\">\n"
"              <boundingBox id=\"bb1\">\n"
"                  <position x=\"5\" y=\"5\"/>\n"
"                  <dimensions width=\"390\" height=\"220\"/>\n"
"              </boundingBox>\n"
"              </compartmentGlyph>\n"
"          </listOfCompartmentGlyphs>\n"
"          <listOfSpeciesGlyphs>\n"
"              <speciesGlyph id=\"SpeciesGlyph_Glucose\" species=\"Glucose\">\n"
"              <boundingBox id=\"bb2\">\n"
"                  <position x=\"105\" y=\"20\"/>\n"
"                  <dimensions width=\"130\" height=\"20\"/>\n"
"              </boundingBox>\n"
"              </speciesGlyph>\n"
"              <speciesGlyph id=\"SpeciesGlyph_G6P\" species=\"Glucose_6_phosphate\">\n"
"              <boundingBox id=\"bb5\">\n"
"                  <position x=\"50\" y=\"190\"/>\n"
"                  <dimensions width=\"270\" height=\"20\"/>\n"
"              </boundingBox>\n"
"              </speciesGlyph>\n"
"              <speciesGlyph id=\"SpeciesGlyph_ATP\" species=\"ATP\">\n"
"              <boundingBox id=\"bb3\">\n"
"                  <position x=\"270\" y=\"70\"/>\n"
"                  <dimensions width=\"80\" height=\"20\"/>\n"
"              </boundingBox>\n"
"              </speciesGlyph>\n"
"              <speciesGlyph id=\"glyph_ADP\" species=\"ADP\">\n"
"              <boundingBox id=\"bb4\">\n"
"                  <position x=\"270\" y=\"140\"/>\n"
"                  <dimensions width=\"80\" height=\"20\"/>\n"
"              </boundingBox>\n"
"              </speciesGlyph>\n"
"              <speciesGlyph id=\"SpeciesGlyph_Pi\" species=\"Pi\">\n"
"              <boundingBox id=\"bb6\">\n"
"                  <position x=\"50\" y=\"100\"/>\n"
"                  <dimensions width=\"60\" height=\"20\"/>\n"
"              </boundingBox>\n"
"              </speciesGlyph>\n"
"          </listOfSpeciesGlyphs>\n"
"          <listOfReactionGlyphs>\n"
"              <reactionGlyph id=\"glyph_Hexokinase\" reaction=\"Hexokinase\">\n"
"              <curve>\n"
"                  <listOfCurveSegments>\n"
"                  <curveSegment xsi:type=\"LineSegment\">\n"
"                      <start x=\"170\" y=\"100\"/>\n"
"                      <end x=\"170\" y=\"130\"/>\n"
"                  </curveSegment>\n"
"                  </listOfCurveSegments>\n"
"              </curve>\n"
"              <listOfSpeciesReferenceGlyphs>\n"
"                  <speciesReferenceGlyph id=\"SpeciesReferenceGlyph_Glucose\" speciesReference=\"SpeciesReference_Glucose\" speciesGlyph=\"SpeciesGlyph_Glucose\" role=\"substrate\" render:objectRole=\"substrate\"  xmlns:render='http://projects.eml.org/bcb/sbml/render/level2'>\n"
"                  <curve>\n"
"                      <listOfCurveSegments>\n"
"                      <curveSegment xsi:type=\"LineSegment\">\n"
"                          <start x=\"170\" y=\"100\"/>\n"
"                          <end x=\"170\" y=\"50\"/>\n"
"                      </curveSegment>\n"
"                      </listOfCurveSegments>\n"
"                  </curve>\n"
"                  </speciesReferenceGlyph>\n"
"                  <speciesReferenceGlyph id=\"SpeciesReferenceGlyph_ATP\" speciesReference=\"SpeciesReference_ATP\" speciesGlyph=\"SpeciesGlyph_ATP\" role=\"sidesubstrate\" render:objectRole=\"sidesubstrate\"  xmlns:render='http://projects.eml.org/bcb/sbml/render/level2'>\n"
"                  <curve>\n"
"                      <listOfCurveSegments>\n"
"                      <curveSegment xsi:type=\"CubicBezier\">\n"
"                          <start x=\"170\" y=\"100\"/>\n"
"                          <end x=\"260\" y=\"80\"/>\n"
"                          <basePoint1 x=\"170\" y=\"80\"/>\n"
"                          <basePoint2 x=\"170\" y=\"80\"/>\n"
"                      </curveSegment>\n"
"                      </listOfCurveSegments>\n"
"                  </curve>\n"
"                  </speciesReferenceGlyph>\n"
"                  <speciesReferenceGlyph id=\"SpeciesReferenceGlyph_G6P_1\" speciesReference=\"SpeciesReference_G6P\" speciesGlyph=\"SpeciesGlyph_G6P\" role=\"product\" render:objectRole=\"product\"  xmlns:render='http://projects.eml.org/bcb/sbml/render/level2'>\n"
"                  <curve>\n"
"                      <listOfCurveSegments>\n"
"                      <curveSegment xsi:type=\"LineSegment\">\n"
"                          <start x=\"170\" y=\"130\"/>\n"
"                          <end x=\"170\" y=\"180\"/>\n"
"                      </curveSegment>\n"
"                      </listOfCurveSegments>\n"
"                  </curve>\n"
"                  </speciesReferenceGlyph>\n"
"                  <speciesReferenceGlyph id=\"SpeciesReferenceGlyph_ADP\" speciesReference=\"SpeciesReference_ADP\" speciesGlyph=\"glyph_ADP\" role=\"sideproduct\" render:objectRole=\"sideproduct\"  xmlns:render='http://projects.eml.org/bcb/sbml/render/level2'>\n"
"                  <curve>\n"
"                      <listOfCurveSegments>\n"
"                      <curveSegment xsi:type=\"CubicBezier\">\n"
"                          <start x=\"170\" y=\"130\"/>\n"
"                          <end x=\"260\" y=\"150\"/>\n"
"                          <basePoint1 x=\"170\" y=\"150\"/>\n"
"                          <basePoint2 x=\"170\" y=\"150\"/>\n"
"                      </curveSegment>\n"
"                      </listOfCurveSegments>\n"
"                  </curve>\n"
"                  </speciesReferenceGlyph>\n"
"                  <speciesReferenceGlyph id=\"SpeciesReferenceGlyph_G6P_2\" speciesReference=\"ModifierSpeciesReference_G6P\" speciesGlyph=\"SpeciesGlyph_G6P\" role=\"inhibitor\" render:objectRole=\"inhibitor\"  xmlns:render='http://projects.eml.org/bcb/sbml/render/level2'>\n"
"                  <curve>\n"
"                      <listOfCurveSegments>\n"
"                      <curveSegment xsi:type=\"CubicBezier\">\n"
"                          <start x=\"45\" y=\"200\"/>\n"
"                          <end x=\"165\" y=\"120\"/>\n"
"                          <basePoint1 x=\"0\" y=\"200\"/>\n"
"                          <basePoint2 x=\"0\" y=\"120\"/>\n"
"                      </curveSegment>\n"
"                      </listOfCurveSegments>\n"
"                  </curve>\n"
"                  </speciesReferenceGlyph>\n"
"                  <speciesReferenceGlyph id=\"SpeciesReferenceGlyph_PI\" speciesReference=\"ModifierSpeciesReference_Pi\" speciesGlyph=\"SpeciesGlyph_Pi\" role=\"activator\" render:objectRole=\"activator\"  xmlns:render='http://projects.eml.org/bcb/sbml/render/level2'>\n"
"                  <curve>\n"
"                      <listOfCurveSegments>\n"
"                        <curveSegment xsi:type=\"CubicBezier\">\n"
"                          <start x=\"115\" y=\"110\"/>\n"
"                          <end x=\"165\" y=\"110\"/>\n"
"                          <basePoint1 x=\"140\" y=\"110\"/>\n"
"                          <basePoint2 x=\"140\" y=\"110\"/>\n"
"                        </curveSegment>\n"
"                      </listOfCurveSegments>\n"
"                  </curve>\n"
"                  </speciesReferenceGlyph>\n"
"              </listOfSpeciesReferenceGlyphs>\n"
"              </reactionGlyph>\n"
"          </listOfReactionGlyphs>\n"
"          <listOfTextGlyphs>\n"
"              <textGlyph id=\"TextGlyph_Glucose\" graphicalObject=\"SpeciesGlyph_Glucose\" originOfText=\"Glucose\">\n"
"              <boundingBox id=\"bbA\">\n"
"                  <position x=\"115\" y=\"20\"/>\n"
"                  <dimensions width=\"110\" height=\"20\"/>\n"
"              </boundingBox>\n"
"              </textGlyph>\n"
"              <textGlyph id=\"TextGlyph_G6P\" graphicalObject=\"SpeciesGlyph_G6P\" originOfText=\"Glucose_6_phosphate\">\n"
"              <boundingBox id=\"bbD\">\n"
"                  <position x=\"60\" y=\"190\"/>\n"
"                  <dimensions width=\"250\" height=\"20\"/>\n"
"              </boundingBox>\n"
"              </textGlyph>\n"
"              <textGlyph id=\"TextGlyph_ATP\" graphicalObject=\"SpeciesGlyph_ATP\" originOfText=\"ATP\">\n"
"              <boundingBox id=\"bbB\">\n"
"                  <position x=\"280\" y=\"70\"/>\n"
"                  <dimensions width=\"60\" height=\"20\"/>\n"
"              </boundingBox>\n"
"              </textGlyph>\n"
"              <textGlyph id=\"TextGlyph_ADP\" graphicalObject=\"glyph_ADP\" originOfText=\"ADP\">\n"
"              <boundingBox id=\"bbC\">\n"
"                  <position x=\"280\" y=\"140\"/>\n"
"                  <dimensions width=\"60\" height=\"20\"/>\n"
"              </boundingBox>\n"
"              </textGlyph>\n"
"              <textGlyph id=\"TextGlyph_PI\" graphicalObject=\"SpeciesGlyph_Pi\" originOfText=\"Pi\">\n"
"              <boundingBox id=\"bbE\">\n"
"                  <position x=\"60\" y=\"100\"/>\n"
"                  <dimensions width=\"40\" height=\"20\"/>\n"
"              </boundingBox>\n"
"              </textGlyph>\n"
"          </listOfTextGlyphs>\n"
"          </layout>\n"
"      </listOfLayouts>\n"
"    </annotation>\n"
"    <listOfCompartments>\n"
"      <compartment id=\"Yeast\"/>\n"
"    </listOfCompartments>\n"
"    <listOfSpecies>\n"
"      <species id=\"Glucose\" compartment=\"Yeast\"/>\n"
"      <species id=\"Glucose_6_phosphate\" compartment=\"Yeast\"/>\n"
"      <species id=\"ATP\" compartment=\"Yeast\"/>\n"
"      <species id=\"ADP\" compartment=\"Yeast\"/>\n"
"      <species id=\"Pi\" compartment=\"Yeast\"/>\n"
"    </listOfSpecies>\n"
"    <listOfReactions>\n"
"      <reaction id=\"Hexokinase\" reversible=\"false\">\n"
"        <listOfReactants>\n"
"          <speciesReference species=\"Glucose\">\n"
"            <annotation>\n"
"  <layoutId xmlns=\"http://projects.eml.org/bcb/sbml/level2\" id=\"SpeciesReference_Glucose\"/>\n"
"</annotation>\n"
"          </speciesReference>\n"
"          <speciesReference species=\"ATP\">\n"
"            <annotation>\n"
"  <layoutId xmlns=\"http://projects.eml.org/bcb/sbml/level2\" id=\"SpeciesReference_ATP\"/>\n"
"</annotation>\n"
"          </speciesReference>\n"
"        </listOfReactants>\n"
"        <listOfProducts>\n"
"          <speciesReference species=\"Glucose_6_phosphate\">\n"
"            <annotation>\n"
"  <layoutId xmlns=\"http://projects.eml.org/bcb/sbml/level2\" id=\"SpeciesReference_G6P\"/>\n"
"</annotation>\n"
"          </speciesReference>\n"
"          <speciesReference species=\"ADP\">\n"
"            <annotation>\n"
"  <layoutId xmlns=\"http://projects.eml.org/bcb/sbml/level2\" id=\"SpeciesReference_ADP\"/>\n"
"</annotation>\n"
"          </speciesReference>\n"
"        </listOfProducts>\n"
"        <listOfModifiers>\n"
"          <modifierSpeciesReference species=\"Glucose_6_phosphate\">\n"
"            <annotation>\n"
"  <layoutId xmlns=\"http://projects.eml.org/bcb/sbml/level2\" id=\"ModifierSpeciesReference_G6P\"/>\n"
"</annotation>\n"
"          </modifierSpeciesReference>\n"
"          <modifierSpeciesReference species=\"Pi\">\n"
"            <annotation>\n"
"  <layoutId xmlns=\"http://projects.eml.org/bcb/sbml/level2\" id=\"ModifierSpeciesReference_Pi\"/>\n"
"</annotation>\n"
"          </modifierSpeciesReference>\n"
"        </listOfModifiers>\n"
"      </reaction>\n"
"    </listOfReactions>\n"
"  </model>\n"
"</sbml>\n";

// L3 Model with layout and render information written in the correct way.
const char* MODEL_2 = \
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
"<sbml xmlns=\"http://www.sbml.org/sbml/level3/version1/core\" level=\"3\" version=\"1\"\n"
"    xmlns:layout=\"http://www.sbml.org/sbml/level3/version1/layout/version1\"\n"
"    xmlns:render=\"http://www.sbml.org/sbml/level3/version1/render/version1\"\n"
"    layout:required=\"false\" render:required=\"false\">\n"
"  <model id=\"TestModel_with_modifiers\">\n"
"    <listOfCompartments>\n"
"      <compartment id=\"Yeast\" constant=\"true\"/>\n"
"    </listOfCompartments>\n"
"    <listOfSpecies>\n"
"      <species id=\"Glucose\" compartment=\"Yeast\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\"/>\n"
"      <species id=\"Glucose_6_phosphate\" compartment=\"Yeast\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\"/>\n"
"      <species id=\"ATP\" compartment=\"Yeast\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\"/>\n"
"      <species id=\"ADP\" compartment=\"Yeast\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\"/>\n"
"      <species id=\"Pi\" compartment=\"Yeast\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\"/>\n"
"    </listOfSpecies>\n"
"    <listOfReactions>\n"
"      <reaction id=\"Hexokinase\" reversible=\"false\" fast=\"false\">\n"
"        <listOfReactants>\n"
"          <speciesReference id=\"SpeciesReference_Glucose\" species=\"Glucose\" constant=\"false\"/>\n"
"          <speciesReference id=\"SpeciesReference_ATP\" species=\"ATP\" constant=\"false\"/>\n"
"        </listOfReactants>\n"
"        <listOfProducts>\n"
"          <speciesReference id=\"SpeciesReference_G6P\" species=\"Glucose_6_phosphate\" constant=\"false\"/>\n"
"          <speciesReference id=\"SpeciesReference_ADP\" species=\"ADP\" constant=\"false\"/>\n"
"        </listOfProducts>\n"
"        <listOfModifiers>\n"
"          <modifierSpeciesReference id=\"ModifierSpeciesReference_G6P\" species=\"Glucose_6_phosphate\"/>\n"
"          <modifierSpeciesReference id=\"ModifierSpeciesReference_Pi\" species=\"Pi\"/>\n"
"        </listOfModifiers>\n"
"      </reaction>\n"
"    </listOfReactions>\n"
"    <layout:listOfLayouts xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">\n"
"      <layout:layout layout:id=\"Layout_1\">\n"
"        <layout:dimensions layout:width=\"400\" layout:height=\"230\"/>\n"
"        <layout:listOfCompartmentGlyphs>\n"
"          <layout:compartmentGlyph layout:id=\"CompartmentGlyph_1\" layout:compartment=\"Yeast\">\n"
"            <layout:boundingBox layout:id=\"bb1\">\n"
"              <layout:position layout:x=\"5\" layout:y=\"5\"/>\n"
"              <layout:dimensions layout:width=\"390\" layout:height=\"220\"/>\n"
"            </layout:boundingBox>\n"
"          </layout:compartmentGlyph>\n"
"        </layout:listOfCompartmentGlyphs>\n"
"        <layout:listOfSpeciesGlyphs>\n"
"          <layout:speciesGlyph layout:id=\"SpeciesGlyph_Glucose\" layout:species=\"Glucose\">\n"
"            <layout:boundingBox layout:id=\"bb2\">\n"
"              <layout:position layout:x=\"105\" layout:y=\"20\"/>\n"
"              <layout:dimensions layout:width=\"130\" layout:height=\"20\"/>\n"
"            </layout:boundingBox>\n"
"          </layout:speciesGlyph>\n"
"          <layout:speciesGlyph layout:id=\"SpeciesGlyph_G6P\" layout:species=\"Glucose_6_phosphate\">\n"
"            <layout:boundingBox layout:id=\"bb5\">\n"
"              <layout:position layout:x=\"50\" layout:y=\"190\"/>\n"
"              <layout:dimensions layout:width=\"270\" layout:height=\"20\"/>\n"
"            </layout:boundingBox>\n"
"          </layout:speciesGlyph>\n"
"          <layout:speciesGlyph layout:id=\"SpeciesGlyph_ATP\" layout:species=\"ATP\">\n"
"            <layout:boundingBox layout:id=\"bb3\">\n"
"              <layout:position layout:x=\"270\" layout:y=\"70\"/>\n"
"              <layout:dimensions layout:width=\"80\" layout:height=\"20\"/>\n"
"            </layout:boundingBox>\n"
"          </layout:speciesGlyph>\n"
"          <layout:speciesGlyph layout:id=\"glyph_ADP\" layout:species=\"ADP\">\n"
"            <layout:boundingBox layout:id=\"bb4\">\n"
"              <layout:position layout:x=\"270\" layout:y=\"140\"/>\n"
"              <layout:dimensions layout:width=\"80\" layout:height=\"20\"/>\n"
"            </layout:boundingBox>\n"
"          </layout:speciesGlyph>\n"
"          <layout:speciesGlyph layout:id=\"SpeciesGlyph_Pi\" layout:species=\"Pi\">\n"
"            <layout:boundingBox layout:id=\"bb6\">\n"
"              <layout:position layout:x=\"50\" layout:y=\"100\"/>\n"
"              <layout:dimensions layout:width=\"60\" layout:height=\"20\"/>\n"
"            </layout:boundingBox>\n"
"          </layout:speciesGlyph>\n"
"        </layout:listOfSpeciesGlyphs>\n"
"        <layout:listOfReactionGlyphs>\n"
"          <layout:reactionGlyph layout:id=\"glyph_Hexokinase\" layout:reaction=\"Hexokinase\">\n"
"            <layout:curve>\n"
"              <layout:listOfCurveSegments>\n"
"                <layout:curveSegment xsi:type=\"LineSegment\">\n"
"                  <layout:start layout:x=\"170\" layout:y=\"100\"/>\n"
"                  <layout:end layout:x=\"170\" layout:y=\"130\"/>\n"
"                </layout:curveSegment>\n"
"              </layout:listOfCurveSegments>\n"
"            </layout:curve>\n"
"            <layout:listOfSpeciesReferenceGlyphs>\n"
"              <layout:speciesReferenceGlyph layout:id=\"SpeciesReferenceGlyph_Glucose\" layout:speciesReference=\"SpeciesReference_Glucose\" layout:speciesGlyph=\"SpeciesGlyph_Glucose\" layout:role=\"substrate\">\n"
"                <layout:curve>\n"
"                  <layout:listOfCurveSegments>\n"
"                    <layout:curveSegment xsi:type=\"LineSegment\">\n"
"                      <layout:start layout:x=\"170\" layout:y=\"100\"/>\n"
"                      <layout:end layout:x=\"170\" layout:y=\"50\"/>\n"
"                    </layout:curveSegment>\n"
"                  </layout:listOfCurveSegments>\n"
"                </layout:curve>\n"
"              </layout:speciesReferenceGlyph>\n"
"              <layout:speciesReferenceGlyph layout:id=\"SpeciesReferenceGlyph_ATP\" layout:speciesReference=\"SpeciesReference_ATP\" layout:speciesGlyph=\"SpeciesGlyph_ATP\" layout:role=\"sidesubstrate\">\n"
"                <layout:curve>\n"
"                  <layout:listOfCurveSegments>\n"
"                    <layout:curveSegment xsi:type=\"CubicBezier\">\n"
"                      <layout:start layout:x=\"170\" layout:y=\"100\"/>\n"
"                      <layout:end layout:x=\"260\" layout:y=\"80\"/>\n"
"                      <layout:basePoint1 layout:x=\"170\" layout:y=\"80\"/>\n"
"                      <layout:basePoint2 layout:x=\"170\" layout:y=\"80\"/>\n"
"                    </layout:curveSegment>\n"
"                  </layout:listOfCurveSegments>\n"
"                </layout:curve>\n"
"              </layout:speciesReferenceGlyph>\n"
"              <layout:speciesReferenceGlyph layout:id=\"SpeciesReferenceGlyph_G6P_1\" layout:speciesReference=\"SpeciesReference_G6P\" layout:speciesGlyph=\"SpeciesGlyph_G6P\" layout:role=\"product\">\n"
"                <layout:curve>\n"
"                  <layout:listOfCurveSegments>\n"
"                    <layout:curveSegment xsi:type=\"LineSegment\">\n"
"                      <layout:start layout:x=\"170\" layout:y=\"130\"/>\n"
"                      <layout:end layout:x=\"170\" layout:y=\"180\"/>\n"
"                    </layout:curveSegment>\n"
"                  </layout:listOfCurveSegments>\n"
"                </layout:curve>\n"
"              </layout:speciesReferenceGlyph>\n"
"              <layout:speciesReferenceGlyph layout:id=\"SpeciesReferenceGlyph_ADP\" layout:speciesReference=\"SpeciesReference_ADP\" layout:speciesGlyph=\"glyph_ADP\" layout:role=\"sideproduct\">\n"
"                <layout:curve>\n"
"                  <layout:listOfCurveSegments>\n"
"                    <layout:curveSegment xsi:type=\"CubicBezier\">\n"
"                      <layout:start layout:x=\"170\" layout:y=\"130\"/>\n"
"                      <layout:end layout:x=\"260\" layout:y=\"150\"/>\n"
"                      <layout:basePoint1 layout:x=\"170\" layout:y=\"150\"/>\n"
"                      <layout:basePoint2 layout:x=\"170\" layout:y=\"150\"/>\n"
"                    </layout:curveSegment>\n"
"                  </layout:listOfCurveSegments>\n"
"                </layout:curve>\n"
"              </layout:speciesReferenceGlyph>\n"
"              <layout:speciesReferenceGlyph layout:id=\"SpeciesReferenceGlyph_G6P_2\" layout:speciesReference=\"ModifierSpeciesReference_G6P\" layout:speciesGlyph=\"SpeciesGlyph_G6P\" layout:role=\"inhibitor\">\n"
"                <layout:curve>\n"
"                  <layout:listOfCurveSegments>\n"
"                    <layout:curveSegment xsi:type=\"CubicBezier\">\n"
"                      <layout:start layout:x=\"45\" layout:y=\"200\"/>\n"
"                      <layout:end layout:x=\"165\" layout:y=\"120\"/>\n"
"                      <layout:basePoint1 layout:x=\"0\" layout:y=\"200\"/>\n"
"                      <layout:basePoint2 layout:x=\"0\" layout:y=\"120\"/>\n"
"                    </layout:curveSegment>\n"
"                  </layout:listOfCurveSegments>\n"
"                </layout:curve>\n"
"              </layout:speciesReferenceGlyph>\n"
"              <layout:speciesReferenceGlyph layout:id=\"SpeciesReferenceGlyph_PI\" layout:speciesReference=\"ModifierSpeciesReference_Pi\" layout:speciesGlyph=\"SpeciesGlyph_Pi\" layout:role=\"activator\">\n"
"                <layout:curve>\n"
"                  <layout:listOfCurveSegments>\n"
"                    <layout:curveSegment xsi:type=\"CubicBezier\">\n"
"                      <layout:start layout:x=\"115\" layout:y=\"110\"/>\n"
"                      <layout:end layout:x=\"165\" layout:y=\"110\"/>\n"
"                      <layout:basePoint1 layout:x=\"140\" layout:y=\"110\"/>\n"
"                      <layout:basePoint2 layout:x=\"140\" layout:y=\"110\"/>\n"
"                    </layout:curveSegment>\n"
"                  </layout:listOfCurveSegments>\n"
"                </layout:curve>\n"
"              </layout:speciesReferenceGlyph>\n"
"            </layout:listOfSpeciesReferenceGlyphs>\n"
"          </layout:reactionGlyph>\n"
"        </layout:listOfReactionGlyphs>\n"
"        <layout:listOfTextGlyphs>\n"
"          <layout:textGlyph layout:id=\"TextGlyph_Glucose\" layout:originOfText=\"Glucose\" layout:graphicalObject=\"SpeciesGlyph_Glucose\">\n"
"            <layout:boundingBox layout:id=\"bbA\">\n"
"              <layout:position layout:x=\"115\" layout:y=\"20\"/>\n"
"              <layout:dimensions layout:width=\"110\" layout:height=\"20\"/>\n"
"            </layout:boundingBox>\n"
"          </layout:textGlyph>\n"
"          <layout:textGlyph layout:id=\"TextGlyph_G6P\" layout:originOfText=\"Glucose_6_phosphate\" layout:graphicalObject=\"SpeciesGlyph_G6P\">\n"
"            <layout:boundingBox layout:id=\"bbD\">\n"
"              <layout:position layout:x=\"60\" layout:y=\"190\"/>\n"
"              <layout:dimensions layout:width=\"250\" layout:height=\"20\"/>\n"
"            </layout:boundingBox>\n"
"          </layout:textGlyph>\n"
"          <layout:textGlyph layout:id=\"TextGlyph_ATP\" layout:originOfText=\"ATP\" layout:graphicalObject=\"SpeciesGlyph_ATP\">\n"
"            <layout:boundingBox layout:id=\"bbB\">\n"
"              <layout:position layout:x=\"280\" layout:y=\"70\"/>\n"
"              <layout:dimensions layout:width=\"60\" layout:height=\"20\"/>\n"
"            </layout:boundingBox>\n"
"          </layout:textGlyph>\n"
"          <layout:textGlyph layout:id=\"TextGlyph_ADP\" layout:originOfText=\"ADP\" layout:graphicalObject=\"glyph_ADP\">\n"
"            <layout:boundingBox layout:id=\"bbC\">\n"
"              <layout:position layout:x=\"280\" layout:y=\"140\"/>\n"
"              <layout:dimensions layout:width=\"60\" layout:height=\"20\"/>\n"
"            </layout:boundingBox>\n"
"          </layout:textGlyph>\n"
"          <layout:textGlyph layout:id=\"TextGlyph_PI\" layout:originOfText=\"Pi\" layout:graphicalObject=\"SpeciesGlyph_Pi\">\n"
"            <layout:boundingBox layout:id=\"bbE\">\n"
"              <layout:position layout:x=\"60\" layout:y=\"100\"/>\n"
"              <layout:dimensions layout:width=\"40\" layout:height=\"20\"/>\n"
"            </layout:boundingBox>\n"
"          </layout:textGlyph>\n"
"        </layout:listOfTextGlyphs>\n"
"        <render:listOfRenderInformation>\n"
"          <render:renderInformation render:id=\"highlightGlucose\" render:referenceRenderInformation=\"defaultStyle\" render:programName=\"Ralph Gauges\" render:programVersion=\"1.0\">\n"
"            <render:listOfColorDefinitions>\n"
"              <render:colorDefinition render:id=\"lightRed\" render:value=\"#E6ADD8\"/>\n"
"              <render:colorDefinition render:id=\"white\"    render:value=\"#FFFFFF\"/>\n"
"            </render:listOfColorDefinitions>\n"
"            <render:listOfGradientDefinitions>\n"
"              <render:radialGradient render:id=\"highlightedSpeciesGlyphGradient\">\n"
"                <render:stop render:offset=\"0%\"   render:stop-color=\"white\"/>\n"
"                <render:stop render:offset=\"100%\" render:stop-color=\"lightRed\"/>\n"
"              </render:radialGradient>\n"
"            </render:listOfGradientDefinitions>\n"
"            <render:listOfStyles>\n"
"              <render:style render:id=\"highlightedGlucose\" render:idList=\"SpeciesGlyph_Glucose\">\n"
"                <render:g render:stroke=\"black\" render:stroke-width=\"1.0\">\n"
"                  <render:rectangle render:x=\"0%\" render:y=\"0%\" render:width=\"100%\" render:height=\"100%\" render:rx=\"5\" render:ry=\"50%\" render:fill=\"highlightedSpeciesGlyphGradient\"/>\n"
"                </render:g>\n"
"              </render:style>\n"
"            </render:listOfStyles>\n"
"          </render:renderInformation>\n"
"        </render:listOfRenderInformation>\n"
"      </layout:layout>\n"
"      <render:listOfGlobalRenderInformation>\n"
"        <render:renderInformation render:id=\"wireFrame\" render:name=\"wireframe style\" render:programName=\"Ralph Gauges\" render:programVersion=\"1.0\">\n"
"          <render:listOfColorDefinitions>\n"
"            <render:colorDefinition render:id=\"white\" render:value=\"#FFFFFF\"/>\n"
"            <render:colorDefinition render:id=\"black\" render:value=\"#000000\"/>\n"
"          </render:listOfColorDefinitions>\n"
"          <render:listOfStyles>\n"
"            <render:style render:id=\"compartmentGlyphStyle\" render:typeList=\"COMPARTMENTGLYPH\">\n"
"              <render:g render:stroke=\"black\" render:stroke-width=\"1.0\">\n"
"                <render:rectangle render:x=\"0%\" render:y=\"0%\" render:width=\"100%\" render:height=\"100%\" render:rx=\"0%\" render:fill=\"none\"/>\n"
"              </render:g>\n"
"            </render:style>  \n"
"            <render:style render:id=\"speciesGlyphStyle\" render:typeList=\"SPECIESGLYPH\">\n"
"              <render:g render:stroke=\"black\" render:stroke-width=\"1.0\">\n"
"                <render:rectangle render:x=\"0%\" render:y=\"0%\" render:width=\"100%\" render:height=\"100%\" render:rx=\"0%\" render:fill=\"none\"/>\n"
"              </render:g>\n"
"            </render:style>\n"
"            <render:style render:id=\"reactionGlyphStyle\" render:typeList=\"SPECIESREFERENCEGLYPH REACTIONGLYPH TEXTGLYPH\">\n"
"              <render:g render:stroke=\"black\" render:stroke-width=\"1.0\" render:font-size=\"12\" render:text-anchor=\"middle\"/>\n"
"            </render:style>\n"
"          </render:listOfStyles>\n"
"        </render:renderInformation>\n"
"        <render:renderInformation render:id=\"defaultGrayStyle\" render:name=\"grayscale style\" render:programName=\"Ralph Gauges\" render:programVersion=\"1.0\">\n"
"          <render:listOfColorDefinitions>\n"
"            <render:colorDefinition render:id=\"lightGray\" render:value=\"#CECECE\"/>\n"
"            <render:colorDefinition render:id=\"white\" render:value=\"#FFFFFF\"/>\n"
"            <render:colorDefinition render:id=\"black\" render:value=\"#000000\"/>\n"
"            <render:colorDefinition render:id=\"lightGray2\" render:value=\"#F0F0F0\"/>\n"
"            <render:colorDefinition render:id=\"gray\" render:value=\"#0B0B0B\"/>\n"
"          </render:listOfColorDefinitions>\n"
"          <render:listOfGradientDefinitions>\n"
"            <render:radialGradient render:id=\"speciesGlyphGradient\">\n"
"              <render:stop render:offset=\"0%\" render:stop-color=\"white\"/>\n"
"              <render:stop render:offset=\"100%\" render:stop-color=\"lightGray\"/>\n"
"            </render:radialGradient>\n"
"          </render:listOfGradientDefinitions>\n"
"          <render:listOfLineEndings>\n"
"           <render:lineEnding render:id=\"simpleHead_black\">\n"
"             <layout:boundingBox>\n"
"               <layout:position layout:x=\"-8\" layout:y=\"-3\"/>\n"
"               <layout:dimensions layout:width=\"10\" layout:height=\"6\"/>\n"
"             </layout:boundingBox>\n"
"             <render:g render:stroke=\"black\" render:stroke-width=\"1.0\" render:fill=\"black\">\n"
"               <render:polygon>\n"
"                 <render:listOfElements>\n"
"                   <render:element xsi:type=\"RenderPoint\" render:x=\"0\" render:y=\"0\"/>\n"
"                   <render:element xsi:type=\"RenderPoint\" render:x=\"10\" render:y=\"3\"/>\n"
"                   <render:element xsi:type=\"RenderPoint\" render:x=\"0\" render:y=\"6\"/>\n"
"                 </render:listOfElements>\n"
"               </render:polygon>\n"
"             </render:g>\n"
"           </render:lineEnding>\n"
"          </render:listOfLineEndings>\n"
"          <render:listOfStyles>\n"
"            <render:style render:id=\"compartmentGlyphStyle\" render:typeList=\"COMPARTMENTGLYPH\">\n"
"              <render:g render:stroke=\"gray\" render:stroke-width=\"1.0\">\n"
"                <render:rectangle render:x=\"0%\" render:y=\"0%\" render:width=\"100%\" render:height=\"100%\" render:rx=\"5%\" render:fill=\"lightGray2\"/>\n"
"              </render:g>\n"
"            </render:style>  \n"
"            <render:style render:id=\"speciesGlyphStyle\" render:typeList=\"SPECIESGLYPH\">\n"
"              <render:g render:stroke=\"black\" render:stroke-width=\"1.0\">\n"
"                <render:rectangle render:x=\"0%\" render:y=\"0%\" render:width=\"100%\" render:height=\"100%\" render:rx=\"5%\" render:fill=\"speciesGlyphGradient\"/>\n"
"              </render:g>\n"
"            </render:style>\n"
"            <render:style render:id=\"reactionGlyphStyle\" render:typeList=\"REACTIONGLYPH TEXTGLYPH\">\n"
"              <render:g render:stroke=\"black\" render:stroke-width=\"1.0\" render:font-size=\"12\" render:text-anchor=\"middle\"/>\n"
"            </render:style>\n"
"            <render:style render:id=\"speciesReferenceGlyphStyle\" render:roleList=\"substrate sidesubstrate product sideproduct\">\n"
"              <render:g render:stroke=\"#000000\" render:stroke-width=\"1.0\"/>\n"
"            </render:style>\n"
"            <render:style render:id=\"activatorSpeciesReferenceGlyphStyle\" render:roleList=\"activator\">\n"
"              <render:g render:stroke=\"#000000\" render:stroke-width=\"1.0\"/>\n"
"            </render:style>\n"
"            <render:style render:id=\"modifierSpeciesReferenceGlyphStyle\" render:roleList=\"modifier\">\n"
"              <render:g render:stroke=\"#000000\" render:stroke-width=\"1.0\"/>\n"
"            </render:style>\n"
"            <render:style render:id=\"inhibitorSpeciesReferenceGlyphStyle\" render:roleList=\"inhibitor\">\n"
"              <render:g render:stroke=\"#000000\" render:stroke-width=\"1.0\"/>\n"
"            </render:style>\n"
"          </render:listOfStyles>\n"
"        </render:renderInformation>\n"
"        <!-- references defaultStyle but redefines the colors to make a grayscale image instead of a colored one -->\n"
"        <render:renderInformation render:id=\"shortGrayStyle\" render:name=\"modified default style to grayscale\" render:referenceRenderInformation=\"defaultStyle\" render:programName=\"Ralph Gauges\" render:programVersion=\"1.0\">\n"
"          <render:listOfColorDefinitions>\n"
"            <render:colorDefinition render:id=\"lightBlue\" render:value=\"#CECECE\"/>\n"
"            <render:colorDefinition render:id=\"white\" render:value=\"#FFFFFF\"/>\n"
"            <render:colorDefinition render:id=\"black\" render:value=\"#000000\"/>\n"
"            <render:colorDefinition render:id=\"red\" render:value=\"#000000\"/>\n"
"            <render:colorDefinition render:id=\"green\" render:value=\"#000000\"/>\n"
"            <render:colorDefinition render:id=\"blue\" render:value=\"#000000\"/>\n"
"            <render:colorDefinition render:id=\"lightYellow\" render:value=\"#F0F0F0\"/>\n"
"            <render:colorDefinition render:id=\"darkGreen\" render:value=\"#0B0B0B\"/>\n"
"          </render:listOfColorDefinitions>\n"
"        </render:renderInformation>\n"
"        <!-- Normal color style similar to the old XSLT stylesheet -->\n"
"        <render:renderInformation render:id=\"defaultStyle\" render:name=\"default style\" render:programName=\"Ralph Gauges\" render:programVersion=\"1.0\">\n"
"          <render:listOfColorDefinitions>\n"
"            <render:colorDefinition render:id=\"lightBlue\" render:value=\"#ADD8E6\"/>\n"
"            <render:colorDefinition render:id=\"white\" render:value=\"#FFFFFF\"/>\n"
"            <render:colorDefinition render:id=\"black\" render:value=\"#000000\"/>\n"
"            <render:colorDefinition render:id=\"red\" render:value=\"#FF0000\"/>\n"
"            <render:colorDefinition render:id=\"green\" render:value=\"#00FF00\"/>\n"
"            <render:colorDefinition render:id=\"blue\" render:value=\"#0000FF\"/>\n"
"            <render:colorDefinition render:id=\"lightYellow\" render:value=\"#FFFFD1\"/>\n"
"            <render:colorDefinition render:id=\"darkGreen\" render:value=\"#002000\"/>\n"
"          </render:listOfColorDefinitions>\n"
"          <render:listOfGradientDefinitions>\n"
"            <render:radialGradient render:id=\"speciesGlyphGradient\">\n"
"              <render:stop render:offset=\"0%\" render:stop-color=\"white\"/>\n"
"              <render:stop render:offset=\"100%\" render:stop-color=\"lightBlue\"/>\n"
"            </render:radialGradient>\n"
"          </render:listOfGradientDefinitions>\n"
"          <render:listOfLineEndings>\n"
"           <render:lineEnding render:id=\"simpleHead_black\">\n"
"             <layout:boundingBox>\n"
"               <layout:position layout:x=\"-8\" layout:y=\"-3\"/>\n"
"               <layout:dimensions layout:width=\"10\" layout:height=\"6\"/>\n"
"             </layout:boundingBox>\n"
"             <render:g render:stroke=\"black\" render:stroke-width=\"1.0\" render:fill=\"black\">\n"
"               <render:polygon>\n"
"                 <render:listOfElements>\n"
"                   <render:element xsi:type=\"RenderPoint\" render:x=\"0\" render:y=\"0\"/>\n"
"                   <render:element xsi:type=\"RenderPoint\" render:x=\"10\" render:y=\"3\"/>\n"
"                   <render:element xsi:type=\"RenderPoint\" render:x=\"0\" render:y=\"6\"/>\n"
"                 </render:listOfElements>\n"
"               </render:polygon>\n"
"             </render:g>\n"
"           </render:lineEnding>\n"
"           <render:lineEnding render:id=\"simpleHead_red\">\n"
"             <layout:boundingBox>\n"
"               <layout:position layout:x=\"-8\" layout:y=\"-3\"/>\n"
"               <layout:dimensions layout:width=\"10\" layout:height=\"6\"/>\n"
"             </layout:boundingBox>\n"
"             <render:g stroke=\"red\" render:stroke-width=\"1.0\" render:fill=\"red\">\n"
"               <render:polygon>\n"
"                 <render:listOfElements>\n"
"                   <render:element xsi:type=\"RenderPoint\" render:x=\"0\" render:y=\"0\"/>\n"
"                   <render:element xsi:type=\"RenderPoint\" render:x=\"10\" render:y=\"3\"/>\n"
"                   <render:element xsi:type=\"RenderPoint\" render:x=\"0\" render:y=\"6\"/>\n"
"                 </render:listOfElements>\n"
"               </render:polygon>\n"
"             </render:g>\n"
"           </render:lineEnding>\n"
"           <render:lineEnding render:id=\"simpleHead_green\">\n"
"             <layout:boundingBox>\n"
"               <layout:position layout:x=\"-8\" layout:y=\"-3\"/>\n"
"               <layout:dimensions layout:width=\"10\" layout:height=\"6\"/>\n"
"             </layout:boundingBox>\n"
"             <render:g render:stroke=\"green\" render:stroke-width=\"1.0\" render:fill=\"green\">\n"
"               <render:polygon>\n"
"                 <render:listOfElements>\n"
"                   <render:element xsi:type=\"RenderPoint\" render:x=\"0\" render:y=\"0\"/>\n"
"                   <render:element xsi:type=\"RenderPoint\" render:x=\"10\" render:y=\"3\"/>\n"
"                   <render:element xsi:type=\"RenderPoint\" render:x=\"0\" render:y=\"6\"/>\n"
"                 </render:listOfElements>\n"
"               </render:polygon>\n"
"             </render:g>\n"
"           </render:lineEnding>\n"
"           <render:lineEnding render:id=\"simpleHead_blue\">\n"
"             <layout:boundingBox>\n"
"               <layout:position layout:x=\"-8\" layout:y=\"-3\"/>\n"
"               <layout:dimensions layout:width=\"10\" layout:height=\"6\"/>\n"
"             </layout:boundingBox>\n"
"             <render:g render:stroke=\"blue\" render:stroke-width=\"1.0\" render:fill=\"blue\">\n"
"               <render:polygon>\n"
"                 <render:listOfElements>\n"
"                   <render:element xsi:type=\"RenderPoint\" render:x=\"0\" render:y=\"0\"/>\n"
"                   <render:element xsi:type=\"RenderPoint\" render:x=\"10\" render:y=\"3\"/>\n"
"                   <render:element xsi:type=\"RenderPoint\" render:x=\"0\" render:y=\"6\"/>\n"
"                 </render:listOfElements>\n"
"               </render:polygon>\n"
"             </render:g>\n"
"           </render:lineEnding>\n"
"          </render:listOfLineEndings>\n"
"          <render:listOfStyles>\n"
"            <render:style render:id=\"compartmentGlyphStyle\" render:typeList=\"COMPARTMENTGLYPH\">\n"
"              <render:g render:stroke=\"darkGreen\" render:stroke-width=\"1.0\">\n"
"                <render:rectangle render:x=\"0%\" render:y=\"0%\" render:width=\"100%\" render:height=\"100%\" render:rx=\"10%\" render:ry=\"10%\" render:fill=\"lightYellow\"/>\n"
"              </render:g>\n"
"            </render:style>  \n"
"            <render:style render:id=\"speciesGlyphStyle\" render:typeList=\"SPECIESGLYPH\">\n"
"              <render:g render:stroke=\"black\" render:stroke-width=\"1.0\">\n"
"                <render:rectangle render:x=\"0%\" render:y=\"0%\" render:width=\"100%\" render:height=\"100%\" render:rx=\"5\" render:ry=\"50%\" render:fill=\"speciesGlyphGradient\"/>\n"
"              </render:g>\n"
"            </render:style>\n"
"            <render:style render:id=\"reactionGlyphStyle\" render:typeList=\"REACTIONGLYPH TEXTGLYPH\">\n"
"              <render:g render:stroke=\"black\" render:stroke-width=\"1.0\" render:font-size=\"12\" render:text-anchor=\"middle\"/>\n"
"            </render:style>\n"
"            <render:style render:id=\"speciesReferenceGlyphStyle\" render:roleList=\"substrate sidesubstrate product sideproduct\">\n"
"              <render:g render:stroke=\"#000000\" render:stroke-width=\"1.0\" render:endHead=\"simpleHead_black\"/>\n"
"            </render:style>\n"
"            <render:style render:id=\"activatorSpeciesReferenceGlyphStyle\" render:roleList=\"activator\">\n"
"              <render:g render:stroke=\"green\" render:stroke-width=\"1.0\" render:endHead=\"simpleHead_green\"/>\n"
"            </render:style>\n"
"            <render:style render:id=\"modifierSpeciesReferenceGlyphStyle\" render:roleList=\"modifier\">\n"
"              <render:g render:stroke=\"blue\" render:stroke-width=\"1.0\" render:endHead=\"simpleHead_blue\"/>\n"
"            </render:style>\n"
"            <render:style render:id=\"inhibitorSpeciesReferenceGlyphStyle\" render:roleList=\"inhibitor\">\n"
"              <render:g render:stroke=\"red\" render:stroke-width=\"1.0\" render:endHead=\"simpleHead_red\"/>\n"
"            </render:style>\n"
"          </render:listOfStyles>\n"
"        </render:renderInformation>\n"
"      </render:listOfGlobalRenderInformation>\n"
"    </layout:listOfLayouts>\n"
"  </model>\n"
"</sbml>\n";

// Level 3 model with render information as an annotation
// This render annotation should be ignored when reading the document.
const char* MODEL_3 = \
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
"<sbml xmlns=\"http://www.sbml.org/sbml/level3/version1/core\" level=\"3\" version=\"1\"\n"
"    xmlns:layout=\"http://www.sbml.org/sbml/level3/version1/layout/version1\"\n"
"    layout:required=\"false\">\n"
"  <model id=\"TestModel_with_modifiers\">\n"
"    <listOfCompartments>\n"
"      <compartment id=\"Yeast\" constant=\"true\"/>\n"
"    </listOfCompartments>\n"
"    <listOfSpecies>\n"
"      <species id=\"Glucose\" compartment=\"Yeast\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\"/>\n"
"      <species id=\"Glucose_6_phosphate\" compartment=\"Yeast\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\"/>\n"
"      <species id=\"ATP\" compartment=\"Yeast\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\"/>\n"
"      <species id=\"ADP\" compartment=\"Yeast\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\"/>\n"
"      <species id=\"Pi\" compartment=\"Yeast\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\"/>\n"
"    </listOfSpecies>\n"
"    <listOfReactions>\n"
"      <reaction id=\"Hexokinase\" reversible=\"false\" fast=\"false\">\n"
"        <listOfReactants>\n"
"          <speciesReference id=\"SpeciesReference_Glucose\" species=\"Glucose\" constant=\"false\"/>\n"
"          <speciesReference id=\"SpeciesReference_ATP\" species=\"ATP\" constant=\"false\"/>\n"
"        </listOfReactants>\n"
"        <listOfProducts>\n"
"          <speciesReference id=\"SpeciesReference_G6P\" species=\"Glucose_6_phosphate\" constant=\"false\"/>\n"
"          <speciesReference id=\"SpeciesReference_ADP\" species=\"ADP\" constant=\"false\"/>\n"
"        </listOfProducts>\n"
"        <listOfModifiers>\n"
"          <modifierSpeciesReference id=\"ModifierSpeciesReference_G6P\" species=\"Glucose_6_phosphate\"/>\n"
"          <modifierSpeciesReference id=\"ModifierSpeciesReference_Pi\" species=\"Pi\"/>\n"
"        </listOfModifiers>\n"
"      </reaction>\n"
"    </listOfReactions>\n"
"    <layout:listOfLayouts xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">\n"
"      <annotation>\n"
"        <listOfGlobalRenderInformation xmlns=\"http://projects.eml.org/bcb/sbml/render/level2\">\n"
"          <renderInformation id=\"wireFrame\" name=\"wireframe style\" programName=\"Ralph Gauges\" programVersion=\"1.0\">\n"
"            <listOfColorDefinitions>\n"
"              <colorDefinition id=\"white\" value=\"#FFFFFF\"/>\n"
"              <colorDefinition id=\"black\" value=\"#000000\"/>\n"
"            </listOfColorDefinitions>\n"
"            <listOfStyles>\n"
"              <style id=\"compartmentGlyphStyle\" typeList=\"COMPARTMENTGLYPH\">\n"
"                <g stroke=\"black\" stroke-width=\"1.0\">\n"
"                  <rectangle x=\"0%\" y=\"0%\" width=\"100%\" height=\"100%\" rx=\"0%\" fill=\"none\"/>\n"
"                </g>\n"
"              </style>  \n"
"              <style id=\"speciesGlyphStyle\" typeList=\"SPECIESGLYPH\">\n"
"                <g stroke=\"black\" stroke-width=\"1.0\">\n"
"                  <rectangle x=\"0%\" y=\"0%\" width=\"100%\" height=\"100%\" rx=\"0%\" fill=\"none\"/>\n"
"                </g>\n"
"              </style>\n"
"              <style id=\"reactionGlyphStyle\" typeList=\"SPECIESREFERENCEGLYPH REACTIONGLYPH TEXTGLYPH\">\n"
"                <g stroke=\"black\" stroke-width=\"1.0\" font-size=\"12\" text-anchor=\"middle\"/>\n"
"              </style>\n"
"            </listOfStyles>\n"
"          </renderInformation>\n"
"          <renderInformation id=\"defaultGrayStyle\" name=\"grayscale style\" programName=\"Ralph Gauges\" programVersion=\"1.0\">\n"
"            <listOfColorDefinitions>\n"
"              <colorDefinition id=\"lightGray\" value=\"#CECECE\"/>\n"
"              <colorDefinition id=\"white\" value=\"#FFFFFF\"/>\n"
"              <colorDefinition id=\"black\" value=\"#000000\"/>\n"
"              <colorDefinition id=\"lightGray2\" value=\"#F0F0F0\"/>\n"
"              <colorDefinition id=\"gray\" value=\"#0B0B0B\"/>\n"
"            </listOfColorDefinitions>\n"
"            <listOfGradientDefinitions>\n"
"              <radialGradient id=\"speciesGlyphGradient\">\n"
"                <stop offset=\"0%\" stop-color=\"white\"/>\n"
"                <stop offset=\"100%\" stop-color=\"lightGray\"/>\n"
"              </radialGradient>\n"
"            </listOfGradientDefinitions>\n"
"            <listOfLineEndings>\n"
"             <lineEnding id=\"simpleHead_black\">\n"
"               <boundingBox xmlns=\"http://projects.eml.org/bcb/sbml/level2\">\n"
"                 <position x=\"-8\" y=\"-3\"/>\n"
"                 <dimensions width=\"10\" height=\"6\"/>\n"
"               </boundingBox>\n"
"               <g stroke=\"black\" stroke-width=\"1.0\" fill=\"black\">\n"
"                 <polygon>\n"
"                   <listOfElements>\n"
"                     <element xsi:type=\"RenderPoint\" x=\"0\" y=\"0\"/>\n"
"                     <element xsi:type=\"RenderPoint\" x=\"10\" y=\"3\"/>\n"
"                     <element xsi:type=\"RenderPoint\" x=\"0\" y=\"6\"/>\n"
"                   </listOfElements>\n"
"                 </polygon>\n"
"               </g>\n"
"             </lineEnding>\n"
"            </listOfLineEndings>\n"
"            <listOfStyles>\n"
"              <style id=\"compartmentGlyphStyle\" typeList=\"COMPARTMENTGLYPH\">\n"
"                <g stroke=\"gray\" stroke-width=\"1.0\">\n"
"                  <rectangle x=\"0%\" y=\"0%\" width=\"100%\" height=\"100%\" rx=\"5%\" fill=\"lightGray2\"/>\n"
"                </g>\n"
"              </style>  \n"
"              <style id=\"speciesGlyphStyle\" typeList=\"SPECIESGLYPH\">\n"
"                <g stroke=\"black\" stroke-width=\"1.0\">\n"
"                  <rectangle x=\"0%\" y=\"0%\" width=\"100%\" height=\"100%\" rx=\"5%\" fill=\"speciesGlyphGradient\"/>\n"
"                </g>\n"
"              </style>\n"
"              <style id=\"reactionGlyphStyle\" typeList=\"REACTIONGLYPH TEXTGLYPH\">\n"
"                <g stroke=\"black\" stroke-width=\"1.0\" font-size=\"12\" text-anchor=\"middle\"/>\n"
"              </style>\n"
"              <style id=\"speciesReferenceGlyphStyle\" roleList=\"substrate sidesubstrate product sideproduct\">\n"
"                <g stroke=\"#000000\" stroke-width=\"1.0\"/>\n"
"              </style>\n"
"              <style id=\"activatorSpeciesReferenceGlyphStyle\" roleList=\"activator\">\n"
"                <g stroke=\"#000000\" stroke-width=\"1.0\"/>\n"
"              </style>\n"
"              <style id=\"modifierSpeciesReferenceGlyphStyle\" roleList=\"modifier\">\n"
"                <g stroke=\"#000000\" stroke-width=\"1.0\"/>\n"
"              </style>\n"
"              <style id=\"inhibitorSpeciesReferenceGlyphStyle\" roleList=\"inhibitor\">\n"
"                <g stroke=\"#000000\" stroke-width=\"1.0\"/>\n"
"              </style>\n"
"            </listOfStyles>\n"
"          </renderInformation>\n"
"          <!-- references defaultStyle but redefines the colors to make a grayscale image instead of a colored one -->\n"
"          <renderInformation id=\"shortGrayStyle\" name=\"modified default style to grayscale\" referenceRenderInformation=\"defaultStyle\" programName=\"Ralph Gauges\" programVersion=\"1.0\">\n"
"            <listOfColorDefinitions>\n"
"              <colorDefinition id=\"lightBlue\" value=\"#CECECE\"/>\n"
"              <colorDefinition id=\"white\" value=\"#FFFFFF\"/>\n"
"              <colorDefinition id=\"black\" value=\"#000000\"/>\n"
"              <colorDefinition id=\"red\" value=\"#000000\"/>\n"
"              <colorDefinition id=\"green\" value=\"#000000\"/>\n"
"              <colorDefinition id=\"blue\" value=\"#000000\"/>\n"
"              <colorDefinition id=\"lightYellow\" value=\"#F0F0F0\"/>\n"
"              <colorDefinition id=\"darkGreen\" value=\"#0B0B0B\"/>\n"
"            </listOfColorDefinitions>\n"
"          </renderInformation>\n"
"          <!-- Normal color style similar to the old XSLT stylesheet -->\n"
"          <renderInformation id=\"defaultStyle\" name=\"default style\" programName=\"Ralph Gauges\" programVersion=\"1.0\">\n"
"            <listOfColorDefinitions>\n"
"              <colorDefinition id=\"lightBlue\" value=\"#ADD8E6\"/>\n"
"              <colorDefinition id=\"white\" value=\"#FFFFFF\"/>\n"
"              <colorDefinition id=\"black\" value=\"#000000\"/>\n"
"              <colorDefinition id=\"red\" value=\"#FF0000\"/>\n"
"              <colorDefinition id=\"green\" value=\"#00FF00\"/>\n"
"              <colorDefinition id=\"blue\" value=\"#0000FF\"/>\n"
"              <colorDefinition id=\"lightYellow\" value=\"#FFFFD1\"/>\n"
"              <colorDefinition id=\"darkGreen\" value=\"#002000\"/>\n"
"            </listOfColorDefinitions>\n"
"            <listOfGradientDefinitions>\n"
"              <radialGradient id=\"speciesGlyphGradient\">\n"
"                <stop offset=\"0%\" stop-color=\"white\"/>\n"
"                <stop offset=\"100%\" stop-color=\"lightBlue\"/>\n"
"              </radialGradient>\n"
"            </listOfGradientDefinitions>\n"
"            <listOfLineEndings>\n"
"             <lineEnding id=\"simpleHead_black\">\n"
"               <boundingBox xmlns=\"http://projects.eml.org/bcb/sbml/level2\">\n"
"                 <position x=\"-8\" y=\"-3\"/>\n"
"                 <dimensions width=\"10\" height=\"6\"/>\n"
"               </boundingBox>\n"
"               <g stroke=\"black\" stroke-width=\"1.0\" fill=\"black\">\n"
"                 <polygon>\n"
"                   <listOfElements>\n"
"                     <element xsi:type=\"RenderPoint\" x=\"0\" y=\"0\"/>\n"
"                     <element xsi:type=\"RenderPoint\" x=\"10\" y=\"3\"/>\n"
"                     <element xsi:type=\"RenderPoint\" x=\"0\" y=\"6\"/>\n"
"                   </listOfElements>\n"
"                 </polygon>\n"
"               </g>\n"
"             </lineEnding>\n"
"             <lineEnding id=\"simpleHead_red\">\n"
"               <boundingBox xmlns=\"http://projects.eml.org/bcb/sbml/level2\">\n"
"                 <position x=\"-8\" y=\"-3\"/>\n"
"                 <dimensions width=\"10\" height=\"6\"/>\n"
"               </boundingBox>\n"
"               <g stroke=\"red\" stroke-width=\"1.0\" fill=\"red\">\n"
"                 <polygon>\n"
"                   <listOfElements>\n"
"                     <element xsi:type=\"RenderPoint\" x=\"0\" y=\"0\"/>\n"
"                     <element xsi:type=\"RenderPoint\" x=\"10\" y=\"3\"/>\n"
"                     <element xsi:type=\"RenderPoint\" x=\"0\" y=\"6\"/>\n"
"                   </listOfElements>\n"
"                 </polygon>\n"
"               </g>\n"
"             </lineEnding>\n"
"             <lineEnding id=\"simpleHead_green\">\n"
"               <boundingBox xmlns=\"http://projects.eml.org/bcb/sbml/level2\">\n"
"                 <position x=\"-8\" y=\"-3\"/>\n"
"                 <dimensions width=\"10\" height=\"6\"/>\n"
"               </boundingBox>\n"
"               <g stroke=\"green\" stroke-width=\"1.0\" fill=\"green\">\n"
"                 <polygon>\n"
"                   <listOfElements>\n"
"                     <element xsi:type=\"RenderPoint\" x=\"0\" y=\"0\"/>\n"
"                     <element xsi:type=\"RenderPoint\" x=\"10\" y=\"3\"/>\n"
"                     <element xsi:type=\"RenderPoint\" x=\"0\" y=\"6\"/>\n"
"                   </listOfElements>\n"
"                 </polygon>\n"
"               </g>\n"
"             </lineEnding>\n"
"             <lineEnding id=\"simpleHead_blue\">\n"
"               <boundingBox xmlns=\"http://projects.eml.org/bcb/sbml/level2\">\n"
"                 <position x=\"-8\" y=\"-3\"/>\n"
"                 <dimensions width=\"10\" height=\"6\"/>\n"
"               </boundingBox>\n"
"               <g stroke=\"blue\" stroke-width=\"1.0\" fill=\"blue\">\n"
"                 <polygon>\n"
"                   <listOfElements>\n"
"                     <element xsi:type=\"RenderPoint\" x=\"0\" y=\"0\"/>\n"
"                     <element xsi:type=\"RenderPoint\" x=\"10\" y=\"3\"/>\n"
"                     <element xsi:type=\"RenderPoint\" x=\"0\" y=\"6\"/>\n"
"                   </listOfElements>\n"
"                 </polygon>\n"
"               </g>\n"
"             </lineEnding>\n"
"            </listOfLineEndings>\n"
"            <listOfStyles>\n"
"              <style id=\"compartmentGlyphStyle\" typeList=\"COMPARTMENTGLYPH\">\n"
"                <g stroke=\"darkGreen\" stroke-width=\"1.0\">\n"
"                  <rectangle x=\"0%\" y=\"0%\" width=\"100%\" height=\"100%\" rx=\"10%\" ry=\"10%\" fill=\"lightYellow\"/>\n"
"                </g>\n"
"              </style>  \n"
"              <style id=\"speciesGlyphStyle\" typeList=\"SPECIESGLYPH\">\n"
"                <g stroke=\"black\" stroke-width=\"1.0\">\n"
"                  <rectangle x=\"0%\" y=\"0%\" width=\"100%\" height=\"100%\" rx=\"5\" ry=\"50%\" fill=\"speciesGlyphGradient\"/>\n"
"                </g>\n"
"              </style>\n"
"              <style id=\"reactionGlyphStyle\" typeList=\"REACTIONGLYPH TEXTGLYPH\">\n"
"                <g stroke=\"black\" stroke-width=\"1.0\" font-size=\"12\" text-anchor=\"middle\"/>\n"
"              </style>\n"
"              <style id=\"speciesReferenceGlyphStyle\" roleList=\"substrate sidesubstrate product sideproduct\">\n"
"                <g stroke=\"#000000\" stroke-width=\"1.0\" endHead=\"simpleHead_black\"/>\n"
"              </style>\n"
"              <style id=\"activatorSpeciesReferenceGlyphStyle\" roleList=\"activator\">\n"
"                <g stroke=\"green\" stroke-width=\"1.0\" endHead=\"simpleHead_green\"/>\n"
"              </style>\n"
"              <style id=\"modifierSpeciesReferenceGlyphStyle\" roleList=\"modifier\">\n"
"                <g stroke=\"blue\" stroke-width=\"1.0\" endHead=\"simpleHead_blue\"/>\n"
"              </style>\n"
"              <style id=\"inhibitorSpeciesReferenceGlyphStyle\" roleList=\"inhibitor\">\n"
"                <g stroke=\"red\" stroke-width=\"1.0\" endHead=\"simpleHead_red\"/>\n"
"              </style>\n"
"            </listOfStyles>\n"
"          </renderInformation>\n"
"        </listOfGlobalRenderInformation>\n"
"      </annotation>\n"
"      <layout:layout id=\"Layout_1\">\n"
"        <annotation>\n"
"          <listOfRenderInformation xmlns=\"http://projects.eml.org/bcb/sbml/render/level2\">\n"
"            <renderInformation id=\"highlightGlucose\" referenceRenderInformation=\"defaultStyle\" programName=\"Ralph Gauges\" programVersion=\"1.0\">\n"
"              <listOfColorDefinitions>\n"
"                <colorDefinition id=\"lightRed\" value=\"#E6ADD8\"/>\n"
"                <colorDefinition id=\"white\" value=\"#FFFFFF\"/>\n"
"              </listOfColorDefinitions>\n"
"              <listOfGradientDefinitions>\n"
"                <radialGradient id=\"highlightedSpeciesGlyphGradient\">\n"
"                  <stop offset=\"0%\" stop-color=\"white\"/>\n"
"                  <stop offset=\"100%\" stop-color=\"lightRed\"/>\n"
"                </radialGradient>\n"
"              </listOfGradientDefinitions>\n"
"              <listOfStyles>\n"
"                <style id=\"highlightedGlucose\" idList=\"SpeciesGlyph_Glucose\">\n"
"                  <g stroke=\"black\" stroke-width=\"1.0\">\n"
"                    <rectangle x=\"0%\" y=\"0%\" width=\"100%\" height=\"100%\" rx=\"5\" ry=\"50%\" fill=\"highlightedSpeciesGlyphGradient\"/>\n"
"                  </g>\n"
"                </style>\n"
"              </listOfStyles>\n"
"            </renderInformation>\n"
"          </listOfRenderInformation>\n"
"        </annotation>\n"
"        <layout:dimensions width=\"400\" height=\"230\"/>\n"
"        <layout:listOfCompartmentGlyphs>\n"
"          <layout:compartmentGlyph id=\"CompartmentGlyph_1\" compartment=\"Yeast\">\n"
"            <layout:boundingBox id=\"bb1\">\n"
"              <layout:position x=\"5\" y=\"5\"/>\n"
"              <layout:dimensions width=\"390\" height=\"220\"/>\n"
"            </layout:boundingBox>\n"
"          </layout:compartmentGlyph>\n"
"        </layout:listOfCompartmentGlyphs>\n"
"        <layout:listOfSpeciesGlyphs>\n"
"          <layout:speciesGlyph id=\"SpeciesGlyph_Glucose\" species=\"Glucose\">\n"
"            <layout:boundingBox id=\"bb2\">\n"
"              <layout:position x=\"105\" y=\"20\"/>\n"
"              <layout:dimensions width=\"130\" height=\"20\"/>\n"
"            </layout:boundingBox>\n"
"          </layout:speciesGlyph>\n"
"          <layout:speciesGlyph id=\"SpeciesGlyph_G6P\" species=\"Glucose_6_phosphate\">\n"
"            <layout:boundingBox id=\"bb5\">\n"
"              <layout:position x=\"50\" y=\"190\"/>\n"
"              <layout:dimensions width=\"270\" height=\"20\"/>\n"
"            </layout:boundingBox>\n"
"          </layout:speciesGlyph>\n"
"          <layout:speciesGlyph id=\"SpeciesGlyph_ATP\" species=\"ATP\">\n"
"            <layout:boundingBox id=\"bb3\">\n"
"              <layout:position x=\"270\" y=\"70\"/>\n"
"              <layout:dimensions width=\"80\" height=\"20\"/>\n"
"            </layout:boundingBox>\n"
"          </layout:speciesGlyph>\n"
"          <layout:speciesGlyph id=\"glyph_ADP\" species=\"ADP\">\n"
"            <layout:boundingBox id=\"bb4\">\n"
"              <layout:position x=\"270\" y=\"140\"/>\n"
"              <layout:dimensions width=\"80\" height=\"20\"/>\n"
"            </layout:boundingBox>\n"
"          </layout:speciesGlyph>\n"
"          <layout:speciesGlyph id=\"SpeciesGlyph_Pi\" species=\"Pi\">\n"
"            <layout:boundingBox id=\"bb6\">\n"
"              <layout:position x=\"50\" y=\"100\"/>\n"
"              <layout:dimensions width=\"60\" height=\"20\"/>\n"
"            </layout:boundingBox>\n"
"          </layout:speciesGlyph>\n"
"        </layout:listOfSpeciesGlyphs>\n"
"        <layout:listOfReactionGlyphs>\n"
"          <layout:reactionGlyph id=\"glyph_Hexokinase\" reaction=\"Hexokinase\">\n"
"            <layout:curve>\n"
"              <layout:listOfCurveSegments>\n"
"                <layout:curveSegment xsi:type=\"LineSegment\">\n"
"                  <layout:start x=\"170\" y=\"100\"/>\n"
"                  <layout:end x=\"170\" y=\"130\"/>\n"
"                </layout:curveSegment>\n"
"              </layout:listOfCurveSegments>\n"
"            </layout:curve>\n"
"            <layout:listOfSpeciesReferenceGlyphs>\n"
"              <layout:speciesReferenceGlyph id=\"SpeciesReferenceGlyph_Glucose\" speciesReference=\"SpeciesReference_Glucose\" speciesGlyph=\"SpeciesGlyph_Glucose\" role=\"substrate\">\n"
"                <layout:curve>\n"
"                  <layout:listOfCurveSegments>\n"
"                    <layout:curveSegment xsi:type=\"LineSegment\">\n"
"                      <layout:start x=\"170\" y=\"100\"/>\n"
"                      <layout:end x=\"170\" y=\"50\"/>\n"
"                    </layout:curveSegment>\n"
"                  </layout:listOfCurveSegments>\n"
"                </layout:curve>\n"
"              </layout:speciesReferenceGlyph>\n"
"              <layout:speciesReferenceGlyph id=\"SpeciesReferenceGlyph_ATP\" speciesReference=\"SpeciesReference_ATP\" speciesGlyph=\"SpeciesGlyph_ATP\" role=\"sidesubstrate\">\n"
"                <layout:curve>\n"
"                  <layout:listOfCurveSegments>\n"
"                    <layout:curveSegment xsi:type=\"CubicBezier\">\n"
"                      <layout:start x=\"170\" y=\"100\"/>\n"
"                      <layout:end x=\"260\" y=\"80\"/>\n"
"                      <layout:basePoint1 x=\"170\" y=\"80\"/>\n"
"                      <layout:basePoint2 x=\"170\" y=\"80\"/>\n"
"                    </layout:curveSegment>\n"
"                  </layout:listOfCurveSegments>\n"
"                </layout:curve>\n"
"              </layout:speciesReferenceGlyph>\n"
"              <layout:speciesReferenceGlyph id=\"SpeciesReferenceGlyph_G6P_1\" speciesReference=\"SpeciesReference_G6P\" speciesGlyph=\"SpeciesGlyph_G6P\" role=\"product\">\n"
"                <layout:curve>\n"
"                  <layout:listOfCurveSegments>\n"
"                    <layout:curveSegment xsi:type=\"LineSegment\">\n"
"                      <layout:start x=\"170\" y=\"130\"/>\n"
"                      <layout:end x=\"170\" y=\"180\"/>\n"
"                    </layout:curveSegment>\n"
"                  </layout:listOfCurveSegments>\n"
"                </layout:curve>\n"
"              </layout:speciesReferenceGlyph>\n"
"              <layout:speciesReferenceGlyph id=\"SpeciesReferenceGlyph_ADP\" speciesReference=\"SpeciesReference_ADP\" speciesGlyph=\"glyph_ADP\" role=\"sideproduct\">\n"
"                <layout:curve>\n"
"                  <layout:listOfCurveSegments>\n"
"                    <layout:curveSegment xsi:type=\"CubicBezier\">\n"
"                      <layout:start x=\"170\" y=\"130\"/>\n"
"                      <layout:end x=\"260\" y=\"150\"/>\n"
"                      <layout:basePoint1 x=\"170\" y=\"150\"/>\n"
"                      <layout:basePoint2 x=\"170\" y=\"150\"/>\n"
"                    </layout:curveSegment>\n"
"                  </layout:listOfCurveSegments>\n"
"                </layout:curve>\n"
"              </layout:speciesReferenceGlyph>\n"
"              <layout:speciesReferenceGlyph id=\"SpeciesReferenceGlyph_G6P_2\" speciesReference=\"ModifierSpeciesReference_G6P\" speciesGlyph=\"SpeciesGlyph_G6P\" role=\"inhibitor\">\n"
"                <layout:curve>\n"
"                  <layout:listOfCurveSegments>\n"
"                    <layout:curveSegment xsi:type=\"CubicBezier\">\n"
"                      <layout:start x=\"45\" y=\"200\"/>\n"
"                      <layout:end x=\"165\" y=\"120\"/>\n"
"                      <layout:basePoint1 x=\"0\" y=\"200\"/>\n"
"                      <layout:basePoint2 x=\"0\" y=\"120\"/>\n"
"                    </layout:curveSegment>\n"
"                  </layout:listOfCurveSegments>\n"
"                </layout:curve>\n"
"              </layout:speciesReferenceGlyph>\n"
"              <layout:speciesReferenceGlyph id=\"SpeciesReferenceGlyph_PI\" speciesReference=\"ModifierSpeciesReference_Pi\" speciesGlyph=\"SpeciesGlyph_Pi\" role=\"activator\">\n"
"                <layout:curve>\n"
"                  <layout:listOfCurveSegments>\n"
"                    <layout:curveSegment xsi:type=\"CubicBezier\">\n"
"                      <layout:start x=\"115\" y=\"110\"/>\n"
"                      <layout:end x=\"165\" y=\"110\"/>\n"
"                      <layout:basePoint1 x=\"140\" y=\"110\"/>\n"
"                      <layout:basePoint2 x=\"140\" y=\"110\"/>\n"
"                    </layout:curveSegment>\n"
"                  </layout:listOfCurveSegments>\n"
"                </layout:curve>\n"
"              </layout:speciesReferenceGlyph>\n"
"            </layout:listOfSpeciesReferenceGlyphs>\n"
"          </layout:reactionGlyph>\n"
"        </layout:listOfReactionGlyphs>\n"
"        <layout:listOfTextGlyphs>\n"
"          <layout:textGlyph id=\"TextGlyph_Glucose\" originOfText=\"Glucose\" graphicalObject=\"SpeciesGlyph_Glucose\">\n"
"            <layout:boundingBox id=\"bbA\">\n"
"              <layout:position x=\"115\" y=\"20\"/>\n"
"              <layout:dimensions width=\"110\" height=\"20\"/>\n"
"            </layout:boundingBox>\n"
"          </layout:textGlyph>\n"
"          <layout:textGlyph id=\"TextGlyph_G6P\" originOfText=\"Glucose_6_phosphate\" graphicalObject=\"SpeciesGlyph_G6P\">\n"
"            <layout:boundingBox id=\"bbD\">\n"
"              <layout:position x=\"60\" y=\"190\"/>\n"
"              <layout:dimensions width=\"250\" height=\"20\"/>\n"
"            </layout:boundingBox>\n"
"          </layout:textGlyph>\n"
"          <layout:textGlyph id=\"TextGlyph_ATP\" originOfText=\"ATP\" graphicalObject=\"SpeciesGlyph_ATP\">\n"
"            <layout:boundingBox id=\"bbB\">\n"
"              <layout:position x=\"280\" y=\"70\"/>\n"
"              <layout:dimensions width=\"60\" height=\"20\"/>\n"
"            </layout:boundingBox>\n"
"          </layout:textGlyph>\n"
"          <layout:textGlyph id=\"TextGlyph_ADP\" originOfText=\"ADP\" graphicalObject=\"glyph_ADP\">\n"
"            <layout:boundingBox id=\"bbC\">\n"
"              <layout:position x=\"280\" y=\"140\"/>\n"
"              <layout:dimensions width=\"60\" height=\"20\"/>\n"
"            </layout:boundingBox>\n"
"          </layout:textGlyph>\n"
"          <layout:textGlyph id=\"TextGlyph_PI\" originOfText=\"Pi\" graphicalObject=\"SpeciesGlyph_Pi\">\n"
"            <layout:boundingBox id=\"bbE\">\n"
"              <layout:position x=\"60\" y=\"100\"/>\n"
"              <layout:dimensions width=\"40\" height=\"20\"/>\n"
"            </layout:boundingBox>\n"
"          </layout:textGlyph>\n"
"        </layout:listOfTextGlyphs>\n"
"      </layout:layout>\n"
"    </layout:listOfLayouts>\n"
"  </model>\n"
"</sbml>\n";


void
RenderReading_setup (void)
{
}

void 
RenderReading_teardown (void)
{
}

START_TEST (test_RenderReading_read_model_1)
{
    const unsigned int level = 2;
    const unsigned int version = 1;
    SBMLReader reader;
    SBMLDocument* pDocument=reader.readSBMLFromString(MODEL_1);
    fail_unless(pDocument!=NULL);
    fail_unless(pDocument->getLevel() == level);
    fail_unless(pDocument->getVersion() == version);

    fail_unless(pDocument->getNumErrors()==0);
    fail_unless(pDocument->getModel()!=NULL);
    fail_unless(pDocument->getModel()->getLevel() == level);
    fail_unless(pDocument->getModel()->getVersion() == version);

    fail_unless(pDocument->getModel()->getListOfCompartments()->size()==1);
    fail_unless(pDocument->getModel()->getListOfSpecies()->size()==5);
    fail_unless(pDocument->getModel()->getListOfReactions()->size()==1);
    LayoutModelPlugin* plugin = (LayoutModelPlugin*)pDocument->getModel()->getPlugin("layout");

    fail_unless(plugin->getListOfLayouts()->size()==1);
    
    plugin = (LayoutModelPlugin*)pDocument->getModel()->getPlugin("layout");

    const Layout* pLayout=plugin->getLayout(0);
    // Do some sanity checks on the layout
    fail_unless(pLayout!=NULL);
    fail_unless(pLayout->getLevel() == level);
    fail_unless(pLayout->getVersion() == version);

    const Dimensions* pDimensions=pLayout->getDimensions();
    fail_unless(pDimensions!=NULL);
    fail_unless(fabs((pDimensions->getWidth()-400.0)/400.0) < 1e-12);
    fail_unless(fabs((pDimensions->getHeight()-230.0)/230.0) < 1e-12);
    fail_unless(pLayout->getListOfCompartmentGlyphs()->size()==1);
    fail_unless(pLayout->getListOfSpeciesGlyphs()->size()==5);
    fail_unless(pLayout->getListOfReactionGlyphs()->size()==1);
    fail_unless(pLayout->getListOfTextGlyphs()->size()==5);


    RenderListOfLayoutsPlugin* lolPlugin = (RenderListOfLayoutsPlugin*)plugin->getListOfLayouts()->getPlugin("render");
    fail_unless(lolPlugin !=NULL);
    if (lolPlugin == NULL) return;
    fail_unless(lolPlugin->getListOfGlobalRenderInformation()->size()==4);
    
    
    // check the four global render information objects
    std::map<std::string,const GlobalRenderInformation*> globalRenderInformationMap;
    const GlobalRenderInformation* pGlobal=NULL;
    unsigned int i;
    for(i=0;i<4;++i)
    {
         pGlobal = lolPlugin->getRenderInformation(i);
         fail_unless(pGlobal!=NULL);
         if (pGlobal == NULL) return;
         fail_unless(pGlobal->getLevel() == level);
         fail_unless(pGlobal->getVersion() == version);

         fail_unless(pGlobal->isSetId()==true);
         globalRenderInformationMap.insert(std::pair<std::string,const GlobalRenderInformation*>(pGlobal->getId(),pGlobal));
    }
    fail_unless(globalRenderInformationMap.size()==4);

    
    // check the global render information called wireFrame
    std::map<std::string,const GlobalRenderInformation*>::const_iterator pos=globalRenderInformationMap.find("wireFrame");
    fail_unless(pos!=globalRenderInformationMap.end());
    pGlobal=pos->second;
    fail_unless(pGlobal->isSetName());
    fail_unless(pGlobal->getName()=="wireframe style");
    fail_unless(pGlobal->getProgramName()=="Ralph Gauges");
    fail_unless(pGlobal->getProgramVersion()=="1.0");
    fail_unless(pGlobal->getListOfColorDefinitions()->size()==2);
    std::map<std::string,const SBase*> objectMap;
    const ColorDefinition* pColor=NULL;
    for(i=0;i<2;++i)
    {
        pColor=pGlobal->getColorDefinition(i);
        fail_unless(pColor!=NULL);
        fail_unless(pColor->getLevel() == level);
        fail_unless(pColor->getVersion() == version);

        fail_unless(pColor->isSetId());
        objectMap.insert(std::pair<std::string,const SBase*>(pColor->getId(),pColor));
    }
    fail_unless(objectMap.size()==2);
    // test the two colors
    std::map<std::string,const SBase*>::const_iterator pos2=objectMap.find("white");
    fail_unless(pos2!=objectMap.end());
    pColor=dynamic_cast<const ColorDefinition*>(pos2->second);
    fail_unless(pColor!=NULL);
    fail_unless(pColor->getRed()==255);
    fail_unless(pColor->getGreen()==255);
    fail_unless(pColor->getBlue()==255);
    fail_unless(pColor->getAlpha()==255);
    pos2=objectMap.find("black");
    fail_unless(pos2!=objectMap.end());
    pColor=dynamic_cast<const ColorDefinition*>(pos2->second);
    fail_unless(pColor!=NULL);
    fail_unless(pColor->getRed()==0);
    fail_unless(pColor->getGreen()==0);
    fail_unless(pColor->getBlue()==0);
    fail_unless(pColor->getAlpha()==255);
    objectMap.clear();
    // check the styles
    fail_unless(pGlobal->getListOfStyles()->size()==3);
    const GlobalStyle* pGlobalStyle=NULL;
    for(i=0;i<3;++i)
    {
        pGlobalStyle=dynamic_cast<const GlobalStyle*>(pGlobal->getStyle(i));
        fail_unless(pGlobalStyle!=NULL);
        fail_unless(pGlobalStyle->getLevel() == level);
        fail_unless(pGlobalStyle->getVersion() == version);

        fail_unless(pGlobalStyle->isSetId());
        objectMap.insert(std::pair<std::string,const SBase*>(pGlobalStyle->getId(),pGlobalStyle));
    }
    fail_unless(objectMap.size()==3);
    // test the three styles
    pos2=objectMap.find("compartmentGlyphStyle");
    fail_unless(pos2!=objectMap.end());
    pGlobalStyle=dynamic_cast<const GlobalStyle*>(pos2->second);
    fail_unless(pGlobalStyle!=NULL);
    fail_unless(pGlobalStyle->getNumTypes()==1);
    fail_unless(pGlobalStyle->isInTypeList("COMPARTMENTGLYPH")==true);
    const RenderGroup* pGroup=pGlobalStyle->getGroup();
    fail_unless(pGroup->getLevel() == level);
    fail_unless(pGroup->getVersion() == version);

    fail_unless(pGroup->getStroke()=="black");
    fail_unless(fabs(pGroup->getStrokeWidth()-1.0) < 1e-12);
    fail_unless(pGroup->getListOfElements()->size()==1);
    // the one element is a rectangle
    const Rectangle* pRectangle=dynamic_cast<const Rectangle*>(pGroup->getElement(0));
    fail_unless(pRectangle!=NULL);
    fail_unless(pRectangle->getLevel() == level);
    fail_unless(pRectangle->getVersion() == version);

    fail_unless(pRectangle->getFillColor()=="none");
    RelAbsVector v=pRectangle->getX();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs(v.getRelativeValue()) < 1e-12); 
    v=pRectangle->getY();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs(v.getRelativeValue()) < 1e-12); 
    v=pRectangle->getRadiusX();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs(v.getRelativeValue()) < 1e-12); 
    v=pRectangle->getRadiusY();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs(v.getRelativeValue()) < 1e-12); 
    v=pRectangle->getWidth();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs((v.getRelativeValue()-100.0)/100.0) < 1e-12); 
    v=pRectangle->getHeight();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs((v.getRelativeValue()-100.0)/100.0) < 1e-12); 
    
    pos2=objectMap.find("speciesGlyphStyle");
    fail_unless(pos2!=objectMap.end());
    pGlobalStyle=dynamic_cast<const GlobalStyle*>(pos2->second);
    fail_unless(pGlobalStyle!=NULL);
    fail_unless(pGlobalStyle->getLevel() == level);
    fail_unless(pGlobalStyle->getVersion() == version);

    fail_unless(pGlobalStyle->getNumTypes()==1);
    fail_unless(pGlobalStyle->isInTypeList("SPECIESGLYPH")==true);
    pGroup=pGlobalStyle->getGroup();
    fail_unless(pGroup->getLevel() == level);
    fail_unless(pGroup->getVersion() == version);

    fail_unless(pGroup->getStroke()=="black");
    fail_unless(fabs(pGroup->getStrokeWidth()-1.0) < 1e-12);
    fail_unless(pGroup->getListOfElements()->size()==1);
    // the one element is a rectangle
    pRectangle=dynamic_cast<const Rectangle*>(pGroup->getElement(0));
    fail_unless(pRectangle!=NULL);
    fail_unless(pRectangle->getLevel() == level);
    fail_unless(pRectangle->getVersion() == version);

    fail_unless(pRectangle->getFillColor()=="none");
    v=pRectangle->getX();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs(v.getRelativeValue()) < 1e-12); 
    v=pRectangle->getY();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs(v.getRelativeValue()) < 1e-12); 
    v=pRectangle->getRadiusX();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs(v.getRelativeValue()) < 1e-12); 
    v=pRectangle->getRadiusY();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs(v.getRelativeValue()) < 1e-12); 
    v=pRectangle->getWidth();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs((v.getRelativeValue()-100.0)/100.0) < 1e-12); 
    v=pRectangle->getHeight();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs((v.getRelativeValue()-100.0)/100.0) < 1e-12); 

    pos2=objectMap.find("reactionGlyphStyle");
    fail_unless(pos2!=objectMap.end());
    pGlobalStyle=dynamic_cast<const GlobalStyle*>(pos2->second);
    fail_unless(pGlobalStyle!=NULL);
    fail_unless(pGlobalStyle->getLevel() == level);
    fail_unless(pGlobalStyle->getVersion() == version);

    fail_unless(pGlobalStyle->getNumTypes()==3);
    fail_unless(pGlobalStyle->isInTypeList("REACTIONGLYPH")==true);
    fail_unless(pGlobalStyle->isInTypeList("SPECIESREFERENCEGLYPH")==true);
    fail_unless(pGlobalStyle->isInTypeList("TEXTGLYPH")==true);
    pGroup=pGlobalStyle->getGroup();
    fail_unless(pGroup->getLevel() == level);
    fail_unless(pGroup->getVersion() == version);

    fail_unless(pGroup->getStroke()=="black");
    fail_unless(fabs(pGroup->getStrokeWidth()-1.0) < 1e-12);
    fail_unless(fabs((pGroup->getFontSize().getAbsoluteValue()-12.0)/12.0) < 1e-12);
    fail_unless(fabs(pGroup->getFontSize().getRelativeValue()) < 1e-12);
    fail_unless(pGroup->getTextAnchor()==Text::ANCHOR_MIDDLE);
    fail_unless(pGroup->getListOfElements()->size()==0);
    objectMap.clear();

    // check the global render information called defaultGrayStyle
    pos=globalRenderInformationMap.find("defaultGrayStyle");
    fail_unless(pos!=globalRenderInformationMap.end());
    pGlobal=pos->second;
    fail_unless(pGlobal->getLevel() == level);
    fail_unless(pGlobal->getVersion() == version);

    fail_unless(pGlobal->isSetName());
    fail_unless(pGlobal->getName()=="grayscale style");
    fail_unless(pGlobal->getProgramName()=="Ralph Gauges");
    fail_unless(pGlobal->getProgramVersion()=="1.0");
    fail_unless(pGlobal->getListOfColorDefinitions()->size()==5);
    for(i=0;i<5;++i)
    {
        pColor=pGlobal->getColorDefinition(i);
        fail_unless(pColor!=NULL);
        fail_unless(pColor->getLevel() == level);
        fail_unless(pColor->getVersion() == version);

        fail_unless(pColor->isSetId());
        objectMap.insert(std::pair<std::string,const SBase*>(pColor->getId(),pColor));
    }
    fail_unless(objectMap.size()==5);
    // test the five colors
    pos2=objectMap.find("white");
    fail_unless(pos2!=objectMap.end());
    pColor=dynamic_cast<const ColorDefinition*>(pos2->second);
    fail_unless(pColor!=NULL);
    fail_unless(pColor->getRed()==255);
    fail_unless(pColor->getGreen()==255);
    fail_unless(pColor->getBlue()==255);
    fail_unless(pColor->getAlpha()==255);
    pos2=objectMap.find("black");
    fail_unless(pos2!=objectMap.end());
    pColor=dynamic_cast<const ColorDefinition*>(pos2->second);
    fail_unless(pColor!=NULL);
    fail_unless(pColor->getRed()==0);
    fail_unless(pColor->getGreen()==0);
    fail_unless(pColor->getBlue()==0);
    fail_unless(pColor->getAlpha()==255);
    pos2=objectMap.find("lightGray");
    fail_unless(pos2!=objectMap.end());
    pColor=dynamic_cast<const ColorDefinition*>(pos2->second);
    fail_unless(pColor!=NULL);
    fail_unless(pColor->getRed()==0xCE);
    fail_unless(pColor->getGreen()==0xCE);
    fail_unless(pColor->getBlue()==0xCE);
    fail_unless(pColor->getAlpha()==255);
    pos2=objectMap.find("lightGray2");
    fail_unless(pos2!=objectMap.end());
    pColor=dynamic_cast<const ColorDefinition*>(pos2->second);
    fail_unless(pColor!=NULL);
    fail_unless(pColor->getRed()==0xF0);
    fail_unless(pColor->getGreen()==0xF0);
    fail_unless(pColor->getBlue()==0xF0);
    fail_unless(pColor->getAlpha()==255);
    pos2=objectMap.find("gray");
    fail_unless(pos2!=objectMap.end());
    pColor=dynamic_cast<const ColorDefinition*>(pos2->second);
    fail_unless(pColor!=NULL);
    fail_unless(pColor->getRed()==0x0B);
    fail_unless(pColor->getGreen()==0x0B);
    fail_unless(pColor->getBlue()==0x0B);
    fail_unless(pColor->getAlpha()==255);
    objectMap.clear();
    // check the gradient definition
    fail_unless(pGlobal->getListOfGradientDefinitions()->size()==1);
    const GradientBase* pGradientBase=pGlobal->getGradientDefinition(0);
    fail_unless(pGradientBase!=NULL);
    fail_unless(pGradientBase->getLevel() == level);
    fail_unless(pGradientBase->getVersion() == version);

    fail_unless(pGradientBase->isSetId());
    fail_unless(pGradientBase->getId()=="speciesGlyphGradient");
    const RadialGradient* pRadialGradient=dynamic_cast<const RadialGradient*>(pGradientBase);
    fail_unless(pRadialGradient!=NULL);
    // all values are default
    v=pRadialGradient->getCenterX();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs((v.getRelativeValue()-50.0)/50.0) < 1e-12);
    v=pRadialGradient->getCenterY();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs((v.getRelativeValue()-50.0)/50.0) < 1e-12);
    v=pRadialGradient->getCenterZ();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs((v.getRelativeValue()-50.0)/50.0) < 1e-12);
    v=pRadialGradient->getRadius();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs((v.getRelativeValue()-50.0)/50.0) < 1e-12);
    // all F parameters must be the same as the center
    v=pRadialGradient->getFocalPointX();
    fail_unless(v==pRadialGradient->getCenterX());
    v=pRadialGradient->getFocalPointY();
    fail_unless(v==pRadialGradient->getCenterY());
    v=pRadialGradient->getFocalPointZ();
    fail_unless(v==pRadialGradient->getCenterZ());
    fail_unless(pGradientBase->getListOfGradientStops()->size()==2);
    const GradientStop* pGradientStop=pGradientBase->getGradientStop(0);
    fail_unless(pGradientStop!=NULL);
    fail_unless(pGradientStop->getLevel() == level);
    fail_unless(pGradientStop->getVersion() == version);

    fail_unless(pGradientStop->getStopColor()=="white");
    v=pGradientStop->getOffset();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    pGradientStop=pGradientBase->getGradientStop(1);
    fail_unless(pGradientStop!=NULL);
    fail_unless(pGradientStop->getLevel() == level);
    fail_unless(pGradientStop->getVersion() == version);

    fail_unless(pGradientStop->getStopColor()=="lightGray");
    v=pGradientStop->getOffset();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs((v.getRelativeValue()-100.0)/100.0) < 1e-12);
    // check the line endings 
    fail_unless(pGlobal->getListOfLineEndings()->size()==1); 
    const LineEnding* pLineEnding=NULL;
    for(i=0;i<1;++i)
    {
        pLineEnding=pGlobal->getLineEnding(i);
        fail_unless(pLineEnding!=NULL);
        fail_unless(pLineEnding->getLevel() == level);
        fail_unless(pLineEnding->getVersion() == version);

        fail_unless(pLineEnding->isSetId());
        objectMap.insert(std::pair<std::string,const SBase*>(pLineEnding->getId(),pLineEnding));
    }
    fail_unless(objectMap.size()==1);
    // lineEnding simpleHead_black
    pos2=objectMap.find("simpleHead_black");
    fail_unless(pos2!=objectMap.end());
    const SBase* pSBase=pos2->second;
    pLineEnding=dynamic_cast<const LineEnding*>(pSBase);
    const BoundingBox* pBoundingBox=pLineEnding->getBoundingBox();
    fail_unless(pBoundingBox!=NULL);
    fail_unless(fabs((pBoundingBox->getPosition()->x()-(-8.0))/-8.0) < 1e-12);
    fail_unless(fabs((pBoundingBox->getPosition()->y()-(-3.0))/-3.0) < 1e-12);
    fail_unless(fabs((pBoundingBox->getDimensions()->getWidth()-10.0)/10.0) < 1e-12);
    fail_unless(fabs((pBoundingBox->getDimensions()->getHeight()-6.0)/6.0) < 1e-12);
    pGroup=pLineEnding->getGroup();
    fail_unless(pGroup!=NULL);
    fail_unless(pGroup->getLevel() == level);
    fail_unless(pGroup->getVersion() == version);

    fail_unless(pGroup->getStroke()=="black");
    fail_unless(fabs(pGroup->getStrokeWidth()-1.0) < 1e-12);
    fail_unless(pGroup->getFillColor()=="black");
    fail_unless(pGroup->getListOfElements()->size()==1);
    const Polygon* pPolygon=dynamic_cast<const Polygon*>(pGroup->getElement(0));
    fail_unless(pPolygon!=NULL);
    fail_unless(pPolygon->getLevel() == level);
    fail_unless(pPolygon->getVersion() == version);

    fail_unless(pPolygon->getListOfElements()->size()==3);
    const RenderPoint* pElement=pPolygon->getElement(0);
    fail_unless(pElement->getLevel() == level);
    fail_unless(pElement->getVersion() == version);

    fail_unless(pElement!=NULL);
    fail_unless(pElement->getTypeCode()==SBML_RENDER_POINT);
    v=pElement->x();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->y();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->z();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    pElement=pPolygon->getElement(1);
    fail_unless(pElement!=NULL);
    fail_unless(pElement->getTypeCode()==SBML_RENDER_POINT);
    v=pElement->x();
    fail_unless(fabs((v.getAbsoluteValue()-10.0)/10.0) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->y();
    fail_unless(fabs((v.getAbsoluteValue()-3.0)/3.0) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->z();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    pElement=pPolygon->getElement(2);
    fail_unless(pElement!=NULL);
    fail_unless(pElement->getLevel() == level);
    fail_unless(pElement->getVersion() == version);

    fail_unless(pElement->getTypeCode()==SBML_RENDER_POINT);
    v=pElement->x();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->y();
    fail_unless(fabs((v.getAbsoluteValue()-6.0)/6.0) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->z();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);

    objectMap.clear();
    // check the styles
    fail_unless(pGlobal->getListOfStyles()->size()==7);
    for(i=0;i<7;++i)
    {
        pGlobalStyle=dynamic_cast<const GlobalStyle*>(pGlobal->getStyle(i));
        fail_unless(pGlobalStyle!=NULL);
        fail_unless(pGlobalStyle->getLevel() == level);
        fail_unless(pGlobalStyle->getVersion() == version);

        fail_unless(pGlobalStyle->isSetId());
        objectMap.insert(std::pair<std::string,const SBase*>(pGlobalStyle->getId(),pGlobalStyle));
    }
    fail_unless(objectMap.size()==7);
    // test the three styles
    pos2=objectMap.find("compartmentGlyphStyle");
    fail_unless(pos2!=objectMap.end());
    pGlobalStyle=dynamic_cast<const GlobalStyle*>(pos2->second);
    fail_unless(pGlobalStyle!=NULL);
    fail_unless(pGlobalStyle->getNumTypes()==1);
    fail_unless(pGlobalStyle->isInTypeList("COMPARTMENTGLYPH")==true);
    pGroup=pGlobalStyle->getGroup();
    fail_unless(pGroup->getLevel() == level);
    fail_unless(pGroup->getVersion() == version);

    fail_unless(pGroup->getStroke()=="gray");
    fail_unless(fabs(pGroup->getStrokeWidth()-1.0) < 1e-12);
    fail_unless(pGroup->getListOfElements()->size()==1);
    // the one element is a rectangle
    pRectangle=dynamic_cast<const Rectangle*>(pGroup->getElement(0));
    fail_unless(pRectangle!=NULL);
    fail_unless(pRectangle->getLevel() == level);
    fail_unless(pRectangle->getVersion() == version);

    fail_unless(pRectangle->getFillColor()=="lightGray2");
    v=pRectangle->getX();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs(v.getRelativeValue()) < 1e-12); 
    v=pRectangle->getY();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs(v.getRelativeValue()) < 1e-12); 
    v=pRectangle->getRadiusX();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs((v.getRelativeValue()-5.0)/5.0) < 1e-12); 
    v=pRectangle->getRadiusY();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs(((v.getRelativeValue()-5.0)/5.0)) < 1e-12); 
    v=pRectangle->getWidth();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs((v.getRelativeValue()-100.0)/100.0) < 1e-12); 
    v=pRectangle->getHeight();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs((v.getRelativeValue()-100.0)/100.0) < 1e-12); 
    
    pos2=objectMap.find("speciesGlyphStyle");
    fail_unless(pos2!=objectMap.end());
    pGlobalStyle=dynamic_cast<const GlobalStyle*>(pos2->second);
    fail_unless(pGlobalStyle!=NULL);
    fail_unless(pGlobalStyle->getLevel() == level);
    fail_unless(pGlobalStyle->getVersion() == version);

    fail_unless(pGlobalStyle->getNumTypes()==1);
    fail_unless(pGlobalStyle->isInTypeList("SPECIESGLYPH")==true);
    pGroup=pGlobalStyle->getGroup();
    fail_unless(pGroup->getLevel() == level);
    fail_unless(pGroup->getVersion() == version);

    fail_unless(pGroup->getStroke()=="black");
    fail_unless(fabs(pGroup->getStrokeWidth()-1.0) < 1e-12);
    fail_unless(pGroup->getListOfElements()->size()==1);
    // the one element is a rectangle
    pRectangle=dynamic_cast<const Rectangle*>(pGroup->getElement(0));
    fail_unless(pRectangle!=NULL);
    fail_unless(pRectangle->getLevel() == level);
    fail_unless(pRectangle->getVersion() == version);

    fail_unless(pRectangle->getFillColor()=="speciesGlyphGradient");
    v=pRectangle->getX();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs(v.getRelativeValue()) < 1e-12); 
    v=pRectangle->getY();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs(v.getRelativeValue()) < 1e-12); 
    v=pRectangle->getRadiusX();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs((v.getRelativeValue()-5.0)/5.0) < 1e-12); 
    v=pRectangle->getRadiusY();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs((v.getRelativeValue()-5.0)/5.0) < 1e-12); 
    v=pRectangle->getWidth();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs((v.getRelativeValue()-100.0)/100.0) < 1e-12); 
    v=pRectangle->getHeight();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs((v.getRelativeValue()-100.0)/100.0) < 1e-12); 

    pos2=objectMap.find("reactionGlyphStyle");
    fail_unless(pos2!=objectMap.end());
    pGlobalStyle=dynamic_cast<const GlobalStyle*>(pos2->second);
    fail_unless(pGlobalStyle!=NULL);
    fail_unless(pGlobalStyle->getLevel() == level);
    fail_unless(pGlobalStyle->getVersion() == version);

    fail_unless(pGlobalStyle->getNumTypes()==2);
    fail_unless(pGlobalStyle->isInTypeList("REACTIONGLYPH")==true);
    fail_unless(pGlobalStyle->isInTypeList("TEXTGLYPH")==true);
    pGroup=pGlobalStyle->getGroup();
    fail_unless(pGroup->getLevel() == level);
    fail_unless(pGroup->getVersion() == version);

    fail_unless(pGroup->getStroke()=="black");
    fail_unless(fabs(pGroup->getStrokeWidth()-1.0) < 1e-12);
    fail_unless(fabs((pGroup->getFontSize().getAbsoluteValue()-12.0)/12.0) < 1e-12);
    fail_unless(fabs(pGroup->getFontSize().getRelativeValue()) < 1e-12);
    fail_unless(pGroup->getTextAnchor()==Text::ANCHOR_MIDDLE);
    fail_unless(pGroup->getListOfElements()->size()==0);
    
    pos2=objectMap.find("speciesReferenceGlyphStyle");
    fail_unless(pos2!=objectMap.end());
    pGlobalStyle=dynamic_cast<const GlobalStyle*>(pos2->second);
    fail_unless(pGlobalStyle!=NULL);
    fail_unless(pGlobalStyle->getLevel() == level);
    fail_unless(pGlobalStyle->getVersion() == version);

    fail_unless(pGlobalStyle->getNumTypes()==0);
    fail_unless(pGlobalStyle->getNumRoles()==4);
    fail_unless(pGlobalStyle->isInRoleList("substrate")==true);
    fail_unless(pGlobalStyle->isInRoleList("sidesubstrate")==true);
    fail_unless(pGlobalStyle->isInRoleList("product")==true);
    fail_unless(pGlobalStyle->isInRoleList("sideproduct")==true);
    pGroup=pGlobalStyle->getGroup();
    fail_unless(pGroup->getLevel() == level);
    fail_unless(pGroup->getVersion() == version);

    fail_unless(pGroup->getStroke()=="#000000");
    fail_unless(fabs(pGroup->getStrokeWidth()-1.0) < 1e-12);
    fail_unless(pGroup->getListOfElements()->size()==0);
    pos2=objectMap.find("activatorSpeciesReferenceGlyphStyle");
    fail_unless(pos2!=objectMap.end());
    pGlobalStyle=dynamic_cast<const GlobalStyle*>(pos2->second);
    fail_unless(pGlobalStyle!=NULL);
    fail_unless(pGlobalStyle->getLevel() == level);
    fail_unless(pGlobalStyle->getVersion() == version);

    fail_unless(pGlobalStyle->getNumTypes()==0);
    fail_unless(pGlobalStyle->getNumRoles()==1);
    fail_unless(pGlobalStyle->isInRoleList("activator")==true);
    pGroup=pGlobalStyle->getGroup();
    fail_unless(pGroup->getLevel() == level);
    fail_unless(pGroup->getVersion() == version);

    fail_unless(pGroup->getStroke()=="#000000");
    fail_unless(fabs(pGroup->getStrokeWidth()-1.0) < 1e-12);
    fail_unless(pGroup->getListOfElements()->size()==0);
    pos2=objectMap.find("inhibitorSpeciesReferenceGlyphStyle");
    fail_unless(pos2!=objectMap.end());
    pGlobalStyle=dynamic_cast<const GlobalStyle*>(pos2->second);
    fail_unless(pGlobalStyle!=NULL);
    fail_unless(pGlobalStyle->getLevel() == level);
    fail_unless(pGlobalStyle->getVersion() == version);

    fail_unless(pGlobalStyle->getNumTypes()==0);
    fail_unless(pGlobalStyle->getNumRoles()==1);
    fail_unless(pGlobalStyle->isInRoleList("inhibitor")==true);
    pGroup=pGlobalStyle->getGroup();
    fail_unless(pGroup->getLevel() == level);
    fail_unless(pGroup->getVersion() == version);

    fail_unless(pGroup->getStroke()=="#000000");
    fail_unless(fabs(pGroup->getStrokeWidth()-1.0) < 1e-12);
    fail_unless(pGroup->getListOfElements()->size()==0);
    pos2=objectMap.find("modifierSpeciesReferenceGlyphStyle");
    fail_unless(pos2!=objectMap.end());
    pGlobalStyle=dynamic_cast<const GlobalStyle*>(pos2->second);
    fail_unless(pGlobalStyle!=NULL);
    fail_unless(pGlobalStyle->getLevel() == level);
    fail_unless(pGlobalStyle->getVersion() == version);

    fail_unless(pGlobalStyle->getNumTypes()==0);
    fail_unless(pGlobalStyle->getNumRoles()==1);
    fail_unless(pGlobalStyle->isInRoleList("modifier")==true);
    pGroup=pGlobalStyle->getGroup();
    fail_unless(pGroup->getLevel() == level);
    fail_unless(pGroup->getVersion() == version);

    fail_unless(pGroup->getStroke()=="#000000");
    fail_unless(fabs(pGroup->getStrokeWidth()-1.0) < 1e-12);
    fail_unless(pGroup->getListOfElements()->size()==0);
    objectMap.clear();

    // check the global render information called shortGrayStyle
    pos=globalRenderInformationMap.find("shortGrayStyle");
    fail_unless(pos!=globalRenderInformationMap.end());
    pGlobal=pos->second;
    fail_unless(pGlobal->getLevel() == level);
    fail_unless(pGlobal->getVersion() == version);

    fail_unless(pGlobal->isSetName());
    fail_unless(pGlobal->getName()=="modified default style to grayscale");
    fail_unless(pGlobal->getReferenceRenderInformationId()=="defaultStyle");
    fail_unless(pGlobal->getProgramName()=="Ralph Gauges");
    fail_unless(pGlobal->getProgramVersion()=="1.0");
    fail_unless(pGlobal->getListOfColorDefinitions()->size()==8);
    for(i=0;i<8;++i)
    {
        pColor=pGlobal->getColorDefinition(i);
        fail_unless(pColor!=NULL);
        fail_unless(pColor->getLevel() == level);
        fail_unless(pColor->getVersion() == version);

        fail_unless(pColor->isSetId());
        objectMap.insert(std::pair<std::string,const SBase*>(pColor->getId(),pColor));
    }
    fail_unless(objectMap.size()==8);
    // test the eight colors
    pos2=objectMap.find("white");
    fail_unless(pos2!=objectMap.end());
    pColor=dynamic_cast<const ColorDefinition*>(pos2->second);
    fail_unless(pColor!=NULL);
    fail_unless(pColor->getRed()==255);
    fail_unless(pColor->getGreen()==255);
    fail_unless(pColor->getBlue()==255);
    fail_unless(pColor->getAlpha()==255);
    pos2=objectMap.find("black");
    fail_unless(pos2!=objectMap.end());
    pColor=dynamic_cast<const ColorDefinition*>(pos2->second);
    fail_unless(pColor!=NULL);
    fail_unless(pColor->getRed()==0);
    fail_unless(pColor->getGreen()==0);
    fail_unless(pColor->getBlue()==0);
    fail_unless(pColor->getAlpha()==255);
    pos2=objectMap.find("lightBlue");
    fail_unless(pos2!=objectMap.end());
    pColor=dynamic_cast<const ColorDefinition*>(pos2->second);
    fail_unless(pColor!=NULL);
    fail_unless(pColor->getRed()==0xCE);
    fail_unless(pColor->getGreen()==0xCE);
    fail_unless(pColor->getBlue()==0xCE);
    fail_unless(pColor->getAlpha()==255);
    pos2=objectMap.find("red");
    fail_unless(pos2!=objectMap.end());
    pColor=dynamic_cast<const ColorDefinition*>(pos2->second);
    fail_unless(pColor!=NULL);
    fail_unless(pColor->getRed()==0);
    fail_unless(pColor->getGreen()==0);
    fail_unless(pColor->getBlue()==0);
    fail_unless(pColor->getAlpha()==255);
    pos2=objectMap.find("green");
    fail_unless(pos2!=objectMap.end());
    pColor=dynamic_cast<const ColorDefinition*>(pos2->second);
    fail_unless(pColor!=NULL);
    fail_unless(pColor->getRed()==0);
    fail_unless(pColor->getGreen()==0);
    fail_unless(pColor->getBlue()==0);
    fail_unless(pColor->getAlpha()==255);
    pos2=objectMap.find("blue");
    fail_unless(pos2!=objectMap.end());
    pColor=dynamic_cast<const ColorDefinition*>(pos2->second);
    fail_unless(pColor!=NULL);
    fail_unless(pColor->getRed()==0);
    fail_unless(pColor->getGreen()==0);
    fail_unless(pColor->getBlue()==0);
    fail_unless(pColor->getAlpha()==255);
    pos2=objectMap.find("lightYellow");
    fail_unless(pos2!=objectMap.end());
    pColor=dynamic_cast<const ColorDefinition*>(pos2->second);
    fail_unless(pColor!=NULL);
    fail_unless(pColor->getRed()==0xF0);
    fail_unless(pColor->getGreen()==0xF0);
    fail_unless(pColor->getBlue()==0xF0);
    fail_unless(pColor->getAlpha()==255);
    pos2=objectMap.find("darkGreen");
    fail_unless(pos2!=objectMap.end());
    pColor=dynamic_cast<const ColorDefinition*>(pos2->second);
    fail_unless(pColor!=NULL);
    fail_unless(pColor->getRed()==0x0B);
    fail_unless(pColor->getGreen()==0x0B);
    fail_unless(pColor->getBlue()==0x0B);
    fail_unless(pColor->getAlpha()==255);
    objectMap.clear();
    
    // check the global render information called defaultStyle
    pos=globalRenderInformationMap.find("defaultStyle");
    fail_unless(pos!=globalRenderInformationMap.end());
    pGlobal=pos->second;
    fail_unless(pGlobal->getLevel() == level);
    fail_unless(pGlobal->getVersion() == version);

    fail_unless(pGlobal->isSetName());
    fail_unless(pGlobal->getName()=="default style");
    fail_unless(pGlobal->getProgramName()=="Ralph Gauges");
    fail_unless(pGlobal->getProgramVersion()=="1.0");
    fail_unless(pGlobal->getListOfColorDefinitions()->size()==8);
    for(i=0;i<8;++i)
    {
        pColor=pGlobal->getColorDefinition(i);
        fail_unless(pColor!=NULL);
        fail_unless(pColor->getLevel() == level);
        fail_unless(pColor->getVersion() == version);

        fail_unless(pColor->isSetId());
        objectMap.insert(std::pair<std::string,const SBase*>(pColor->getId(),pColor));
    }
    fail_unless(objectMap.size()==8);
    // test the eight colors
    pos2=objectMap.find("white");
    fail_unless(pos2!=objectMap.end());
    pColor=dynamic_cast<const ColorDefinition*>(pos2->second);
    fail_unless(pColor!=NULL);
    fail_unless(pColor->getRed()==255);
    fail_unless(pColor->getGreen()==255);
    fail_unless(pColor->getBlue()==255);
    fail_unless(pColor->getAlpha()==255);
    pos2=objectMap.find("black");
    fail_unless(pos2!=objectMap.end());
    pColor=dynamic_cast<const ColorDefinition*>(pos2->second);
    fail_unless(pColor!=NULL);
    fail_unless(pColor->getRed()==0);
    fail_unless(pColor->getGreen()==0);
    fail_unless(pColor->getBlue()==0);
    fail_unless(pColor->getAlpha()==255);
    pos2=objectMap.find("lightBlue");
    fail_unless(pos2!=objectMap.end());
    pColor=dynamic_cast<const ColorDefinition*>(pos2->second);
    fail_unless(pColor!=NULL);
    fail_unless(pColor->getRed()==0xAD);
    fail_unless(pColor->getGreen()==0xD8);
    fail_unless(pColor->getBlue()==0xE6);
    fail_unless(pColor->getAlpha()==255);
    pos2=objectMap.find("red");
    fail_unless(pos2!=objectMap.end());
    pColor=dynamic_cast<const ColorDefinition*>(pos2->second);
    fail_unless(pColor!=NULL);
    fail_unless(pColor->getRed()==0xFF);
    fail_unless(pColor->getGreen()==0x00);
    fail_unless(pColor->getBlue()==0x00);
    fail_unless(pColor->getAlpha()==255);
    pos2=objectMap.find("green");
    fail_unless(pos2!=objectMap.end());
    pColor=dynamic_cast<const ColorDefinition*>(pos2->second);
    fail_unless(pColor!=NULL);
    fail_unless(pColor->getRed()==0x00);
    fail_unless(pColor->getGreen()==0xFF);
    fail_unless(pColor->getBlue()==0x00);
    fail_unless(pColor->getAlpha()==255);
    pos2=objectMap.find("blue");
    fail_unless(pos2!=objectMap.end());
    pColor=dynamic_cast<const ColorDefinition*>(pos2->second);
    fail_unless(pColor!=NULL);
    fail_unless(pColor->getRed()==0x00);
    fail_unless(pColor->getGreen()==0x00);
    fail_unless(pColor->getBlue()==0xFF);
    fail_unless(pColor->getAlpha()==255);
    pos2=objectMap.find("lightYellow");
    fail_unless(pos2!=objectMap.end());
    pColor=dynamic_cast<const ColorDefinition*>(pos2->second);
    fail_unless(pColor!=NULL);
    fail_unless(pColor->getRed()==0xFF);
    fail_unless(pColor->getGreen()==0xFF);
    fail_unless(pColor->getBlue()==0xD1);
    fail_unless(pColor->getAlpha()==255);
    pos2=objectMap.find("darkGreen");
    fail_unless(pos2!=objectMap.end());
    pColor=dynamic_cast<const ColorDefinition*>(pos2->second);
    fail_unless(pColor!=NULL);
    fail_unless(pColor->getRed()==0x00);
    fail_unless(pColor->getGreen()==0x20);
    fail_unless(pColor->getBlue()==0x00);
    fail_unless(pColor->getAlpha()==255);
    objectMap.clear();
    // check the gradient definition
    fail_unless(pGlobal->getListOfGradientDefinitions()->size()==1);
    pGradientBase=pGlobal->getGradientDefinition(0);
    fail_unless(pGradientBase!=NULL);
    fail_unless(pGradientBase->getLevel() == level);
    fail_unless(pGradientBase->getVersion() == version);

    fail_unless(pGradientBase->isSetId());
    fail_unless(pGradientBase->getId()=="speciesGlyphGradient");
    pRadialGradient=dynamic_cast<const RadialGradient*>(pGradientBase);
    fail_unless(pRadialGradient!=NULL);
    // all values are default
    v=pRadialGradient->getCenterX();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs((v.getRelativeValue()-50.0)/50.0) < 1e-12);
    v=pRadialGradient->getCenterY();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs((v.getRelativeValue()-50.0)/50.0) < 1e-12);
    v=pRadialGradient->getCenterZ();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs((v.getRelativeValue()-50.0)/50.0) < 1e-12);
    v=pRadialGradient->getRadius();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs((v.getRelativeValue()-50.0)/50.0) < 1e-12);
    // all F parameter must be the same as the center point
    v=pRadialGradient->getFocalPointX();
    fail_unless(v==pRadialGradient->getCenterX());
    v=pRadialGradient->getFocalPointY();
    fail_unless(v==pRadialGradient->getCenterY());
    v=pRadialGradient->getFocalPointZ();
    fail_unless(v==pRadialGradient->getCenterZ());
    fail_unless(pGradientBase->getListOfGradientStops()->size()==2);
    pGradientStop=pGradientBase->getGradientStop(0);
    fail_unless(pGradientStop!=NULL);
    fail_unless(pGradientStop->getLevel() == level);
    fail_unless(pGradientStop->getVersion() == version);

    fail_unless(pGradientStop->getStopColor()=="white");
    v=pGradientStop->getOffset();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    pGradientStop=pGradientBase->getGradientStop(1);
    fail_unless(pGradientStop!=NULL);
    fail_unless(pGradientStop->getLevel() == level);
    fail_unless(pGradientStop->getVersion() == version);

    fail_unless(pGradientStop->getStopColor()=="lightBlue");
    v=pGradientStop->getOffset();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs((v.getRelativeValue()-100.0)/100.0) < 1e-12);
    // check the line endings 
    fail_unless(pGlobal->getListOfLineEndings()->size()==4); 
    pLineEnding=NULL;
    for(i=0;i<4;++i)
    {
        pLineEnding=pGlobal->getLineEnding(i);
        fail_unless(pLineEnding!=NULL);
        fail_unless(pLineEnding->getLevel() == level);
        fail_unless(pLineEnding->getVersion() == version);

        fail_unless(pLineEnding->isSetId());
        objectMap.insert(std::pair<std::string,const SBase*>(pLineEnding->getId(),pLineEnding));
    }
    fail_unless(objectMap.size()==4);
    // lineEnding simpleHead_black
    pos2=objectMap.find("simpleHead_black");
    fail_unless(pos2!=objectMap.end());
    pLineEnding=dynamic_cast<const LineEnding*>(pos2->second);
    pBoundingBox=pLineEnding->getBoundingBox();
    fail_unless(pBoundingBox!=NULL);
    fail_unless(fabs((pBoundingBox->getPosition()->x()-(-8.0))/-8.0) < 1e-12);
    fail_unless(fabs((pBoundingBox->getPosition()->y()-(-3.0))/-3.0) < 1e-12);
    fail_unless(fabs((pBoundingBox->getDimensions()->getWidth()-10.0)/10.0) < 1e-12);
    fail_unless(fabs((pBoundingBox->getDimensions()->getHeight()-6.0)/6.0) < 1e-12);
    pGroup=pLineEnding->getGroup();
    fail_unless(pGroup!=NULL);
    fail_unless(pGroup->getLevel() == level);
    fail_unless(pGroup->getVersion() == version);

    fail_unless(pGroup->getStroke()=="black");
    fail_unless(fabs(pGroup->getStrokeWidth()-1.0) < 1e-12);
    fail_unless(pGroup->getFillColor()=="black");
    fail_unless(pGroup->getListOfElements()->size()==1);
    pPolygon=dynamic_cast<const Polygon*>(pGroup->getElement(0));
    fail_unless(pPolygon!=NULL);
    fail_unless(pPolygon->getLevel() == level);
    fail_unless(pPolygon->getVersion() == version);

    fail_unless(pPolygon->getListOfElements()->size()==3);
    pElement=pPolygon->getElement(0);
    fail_unless(pElement!=NULL);
    fail_unless(pElement->getLevel() == level);
    fail_unless(pElement->getVersion() == version);

    fail_unless(pElement->getTypeCode()==SBML_RENDER_POINT);
    v=pElement->x();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->y();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->z();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    pElement=pPolygon->getElement(1);
    fail_unless(pElement!=NULL);
    fail_unless(pElement->getLevel() == level);
    fail_unless(pElement->getVersion() == version);

    fail_unless(pElement->getTypeCode()==SBML_RENDER_POINT);
    v=pElement->x();
    fail_unless(fabs((v.getAbsoluteValue()-10.0)/10.0) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->y();
    fail_unless(fabs((v.getAbsoluteValue()-3.0)/3.0) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->z();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    pElement=pPolygon->getElement(2);
    fail_unless(pElement!=NULL);
    fail_unless(pElement->getLevel() == level);
    fail_unless(pElement->getVersion() == version);

    fail_unless(pElement->getTypeCode()==SBML_RENDER_POINT);
    v=pElement->x();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->y();
    fail_unless(fabs((v.getAbsoluteValue()-6.0)/6.0) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->z();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    // lineEnding simpleHead_red
    pos2=objectMap.find("simpleHead_red");
    fail_unless(pos2!=objectMap.end());
    pLineEnding=dynamic_cast<const LineEnding*>(pos2->second);
    pBoundingBox=pLineEnding->getBoundingBox();
    fail_unless(pBoundingBox!=NULL);
    fail_unless(fabs((pBoundingBox->getPosition()->x()-(-8.0))/-8.0) < 1e-12);
    fail_unless(fabs((pBoundingBox->getPosition()->y()-(-3.0))/-3.0) < 1e-12);
    fail_unless(fabs((pBoundingBox->getDimensions()->getWidth()-10.0)/10.0) < 1e-12);
    fail_unless(fabs((pBoundingBox->getDimensions()->getHeight()-6.0)/6.0) < 1e-12);
    pGroup=pLineEnding->getGroup();
    fail_unless(pGroup!=NULL);
    fail_unless(pGroup->getLevel() == level);
    fail_unless(pGroup->getVersion() == version);

    fail_unless(pGroup->getStroke()=="red");
    fail_unless(fabs(pGroup->getStrokeWidth()-1.0) < 1e-12);
    fail_unless(pGroup->getFillColor()=="red");
    fail_unless(pGroup->getListOfElements()->size()==1);
    pPolygon=dynamic_cast<const Polygon*>(pGroup->getElement(0));
    fail_unless(pPolygon!=NULL);
    fail_unless(pPolygon->getLevel() == level);
    fail_unless(pPolygon->getVersion() == version);

    fail_unless(pPolygon->getListOfElements()->size()==3);
    pElement=pPolygon->getElement(0);
    fail_unless(pElement!=NULL);
    fail_unless(pElement->getLevel() == level);
    fail_unless(pElement->getVersion() == version);

    fail_unless(pElement->getTypeCode()==SBML_RENDER_POINT);
    v=pElement->x();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->y();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->z();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    pElement=pPolygon->getElement(1);
    fail_unless(pElement!=NULL);
    fail_unless(pElement->getLevel() == level);
    fail_unless(pElement->getVersion() == version);

    fail_unless(pElement->getTypeCode()==SBML_RENDER_POINT);
    v=pElement->x();
    fail_unless(fabs((v.getAbsoluteValue()-10.0)/10.0) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->y();
    fail_unless(fabs((v.getAbsoluteValue()-3.0)/3.0) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->z();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    pElement=pPolygon->getElement(2);
    fail_unless(pElement!=NULL);
    fail_unless(pElement->getLevel() == level);
    fail_unless(pElement->getVersion() == version);

    fail_unless(pElement->getTypeCode()==SBML_RENDER_POINT);
    v=pElement->x();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->y();
    fail_unless(fabs((v.getAbsoluteValue()-6.0)/6.0) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->z();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    // lineEnding simpleHead_green
    pos2=objectMap.find("simpleHead_green");
    fail_unless(pos2!=objectMap.end());
    pLineEnding=dynamic_cast<const LineEnding*>(pos2->second);
    pBoundingBox=pLineEnding->getBoundingBox();
    fail_unless(pBoundingBox!=NULL);
    fail_unless(fabs((pBoundingBox->getPosition()->x()-(-8.0))/-8.0) < 1e-12);
    fail_unless(fabs((pBoundingBox->getPosition()->y()-(-3.0))/-3.0) < 1e-12);
    fail_unless(fabs((pBoundingBox->getDimensions()->getWidth()-10.0)/10.0) < 1e-12);
    fail_unless(fabs((pBoundingBox->getDimensions()->getHeight()-6.0)/6.0) < 1e-12);
    pGroup=pLineEnding->getGroup();
    fail_unless(pGroup!=NULL);
    fail_unless(pGroup->getLevel() == level);
    fail_unless(pGroup->getVersion() == version);

    fail_unless(pGroup->getStroke()=="green");
    fail_unless(fabs(pGroup->getStrokeWidth()-1.0) < 1e-12);
    fail_unless(pGroup->getFillColor()=="green");
    fail_unless(pGroup->getListOfElements()->size()==1);
    pPolygon=dynamic_cast<const Polygon*>(pGroup->getElement(0));
    fail_unless(pPolygon!=NULL);
    fail_unless(pPolygon->getLevel() == level);
    fail_unless(pPolygon->getVersion() == version);

    fail_unless(pPolygon->getListOfElements()->size()==3);
    pElement=pPolygon->getElement(0);
    fail_unless(pElement!=NULL);
    fail_unless(pElement->getLevel() == level);
    fail_unless(pElement->getVersion() == version);

    fail_unless(pElement->getTypeCode()==SBML_RENDER_POINT);
    v=pElement->x();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->y();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->z();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    pElement=pPolygon->getElement(1);
    fail_unless(pElement!=NULL);
    fail_unless(pElement->getLevel() == level);
    fail_unless(pElement->getVersion() == version);

    fail_unless(pElement->getTypeCode()==SBML_RENDER_POINT);
    v=pElement->x();
    fail_unless(fabs((v.getAbsoluteValue()-10.0)/10.0) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->y();
    fail_unless(fabs((v.getAbsoluteValue()-3.0)/3.0) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->z();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    pElement=pPolygon->getElement(2);
    fail_unless(pElement!=NULL);
    fail_unless(pElement->getLevel() == level);
    fail_unless(pElement->getVersion() == version);

    fail_unless(pElement->getTypeCode()==SBML_RENDER_POINT);
    v=pElement->x();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->y();
    fail_unless(fabs((v.getAbsoluteValue()-6.0)/6.0) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->z();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    // lineEnding simpleHead_blue
    pos2=objectMap.find("simpleHead_blue");
    fail_unless(pos2!=objectMap.end());
    pLineEnding=dynamic_cast<const LineEnding*>(pos2->second);
    pBoundingBox=pLineEnding->getBoundingBox();
    fail_unless(pBoundingBox!=NULL);
    fail_unless(fabs((pBoundingBox->getPosition()->x()-(-8.0))/-8.0) < 1e-12);
    fail_unless(fabs((pBoundingBox->getPosition()->y()-(-3.0))/-3.0) < 1e-12);
    fail_unless(fabs((pBoundingBox->getDimensions()->getWidth()-10.0)/10.0) < 1e-12);
    fail_unless(fabs((pBoundingBox->getDimensions()->getHeight()-6.0)/6.0) < 1e-12);
    pGroup=pLineEnding->getGroup();
    fail_unless(pGroup!=NULL);
    fail_unless(pGroup->getLevel() == level);
    fail_unless(pGroup->getVersion() == version);

    fail_unless(pGroup->getStroke()=="blue");
    fail_unless(fabs(pGroup->getStrokeWidth()-1.0) < 1e-12);
    fail_unless(pGroup->getFillColor()=="blue");
    fail_unless(pGroup->getListOfElements()->size()==1);
    pPolygon=dynamic_cast<const Polygon*>(pGroup->getElement(0));
    fail_unless(pPolygon!=NULL);
    fail_unless(pPolygon->getLevel() == level);
    fail_unless(pPolygon->getVersion() == version);

    fail_unless(pPolygon->getListOfElements()->size()==3);
    pElement=pPolygon->getElement(0);
    fail_unless(pElement!=NULL);
    fail_unless(pElement->getLevel() == level);
    fail_unless(pElement->getVersion() == version);

    fail_unless(pElement->getTypeCode()==SBML_RENDER_POINT);
    v=pElement->x();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->y();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->z();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    pElement=pPolygon->getElement(1);
    fail_unless(pElement!=NULL);
    fail_unless(pElement->getLevel() == level);
    fail_unless(pElement->getVersion() == version);

    fail_unless(pElement->getTypeCode()==SBML_RENDER_POINT);
    v=pElement->x();
    fail_unless(fabs((v.getAbsoluteValue()-10.0)/10.0) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->y();
    fail_unless(fabs((v.getAbsoluteValue()-3.0)/3.0) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->z();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    pElement=pPolygon->getElement(2);
    fail_unless(pElement!=NULL);
    fail_unless(pElement->getLevel() == level);
    fail_unless(pElement->getVersion() == version);

    fail_unless(pElement->getTypeCode()==SBML_RENDER_POINT);
    v=pElement->x();
    fail_unless(fabs((v.getAbsoluteValue())) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->y();
    fail_unless(fabs((v.getAbsoluteValue()-6.0)/6.0) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->z();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);

    objectMap.clear();
    // check the styles
    fail_unless(pGlobal->getListOfStyles()->size()==7);
    for(i=0;i<7;++i)
    {
        pGlobalStyle=dynamic_cast<const GlobalStyle*>(pGlobal->getStyle(i));
        fail_unless(pGlobalStyle!=NULL);
        fail_unless(pGlobalStyle->getLevel() == level);
        fail_unless(pGlobalStyle->getVersion() == version);

        fail_unless(pGlobalStyle->isSetId());
        objectMap.insert(std::pair<std::string,const SBase*>(pGlobalStyle->getId(),pGlobalStyle));
    }
    fail_unless(objectMap.size()==7);
    // test the three styles
    pos2=objectMap.find("compartmentGlyphStyle");
    fail_unless(pos2!=objectMap.end());
    pGlobalStyle=dynamic_cast<const GlobalStyle*>(pos2->second);
    fail_unless(pGlobalStyle!=NULL);
    fail_unless(pGlobalStyle->getNumTypes()==1);
    fail_unless(pGlobalStyle->isInTypeList("COMPARTMENTGLYPH")==true);
    pGroup=pGlobalStyle->getGroup();
    fail_unless(pGroup->getLevel() == level);
    fail_unless(pGroup->getVersion() == version);

    fail_unless(pGroup->getStroke()=="darkGreen");
    fail_unless(fabs(pGroup->getStrokeWidth()-1.0) < 1e-12);
    fail_unless(pGroup->getListOfElements()->size()==1);
    // the one element is a rectangle
    pRectangle=dynamic_cast<const Rectangle*>(pGroup->getElement(0));
    fail_unless(pRectangle!=NULL);
    fail_unless(pRectangle->getLevel() == level);
    fail_unless(pRectangle->getVersion() == version);

    fail_unless(pRectangle->getFillColor()=="lightYellow");
    v=pRectangle->getX();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs(v.getRelativeValue()) < 1e-12); 
    v=pRectangle->getY();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs(v.getRelativeValue()) < 1e-12); 
    v=pRectangle->getRadiusX();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs((v.getRelativeValue()-10.0)/10.0) < 1e-12); 
    v=pRectangle->getRadiusY();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs((v.getRelativeValue()-10.0)/10.0) < 1e-12); 
    v=pRectangle->getWidth();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs((v.getRelativeValue()-100.0)/100.0) < 1e-12); 
    v=pRectangle->getHeight();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs((v.getRelativeValue()-100.0)/100.0) < 1e-12); 
    
    pos2=objectMap.find("speciesGlyphStyle");
    fail_unless(pos2!=objectMap.end());
    pGlobalStyle=dynamic_cast<const GlobalStyle*>(pos2->second);
    fail_unless(pGlobalStyle!=NULL);
    fail_unless(pGlobalStyle->getLevel() == level);
    fail_unless(pGlobalStyle->getVersion() == version);

    fail_unless(pGlobalStyle->getNumTypes()==1);
    fail_unless(pGlobalStyle->isInTypeList("SPECIESGLYPH")==true);
    pGroup=pGlobalStyle->getGroup();
    fail_unless(pGroup->getLevel() == level);
    fail_unless(pGroup->getVersion() == version);

    fail_unless(pGroup->getStroke()=="black");
    fail_unless(fabs(pGroup->getStrokeWidth()-1.0) < 1e-12);
    fail_unless(pGroup->getListOfElements()->size()==1);
    // the one element is a rectangle
    pRectangle=dynamic_cast<const Rectangle*>(pGroup->getElement(0));
    fail_unless(pRectangle!=NULL);
    fail_unless(pRectangle->getLevel() == level);
    fail_unless(pRectangle->getVersion() == version);

    fail_unless(pRectangle->getFillColor()=="speciesGlyphGradient");
    v=pRectangle->getX();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs(v.getRelativeValue()) < 1e-12); 
    v=pRectangle->getY();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs(v.getRelativeValue()) < 1e-12); 
    v=pRectangle->getRadiusX();
    fail_unless(fabs((v.getAbsoluteValue()-5.0)/5.0) < 1e-12); 
    fail_unless(fabs(v.getRelativeValue()) < 1e-12); 
    v=pRectangle->getRadiusY();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs((v.getRelativeValue()-50.0)/50.0) < 1e-12); 
    v=pRectangle->getWidth();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs((v.getRelativeValue()-100.0)/100.0) < 1e-12); 
    v=pRectangle->getHeight();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs((v.getRelativeValue()-100.0)/100.0) < 1e-12); 

    pos2=objectMap.find("reactionGlyphStyle");
    fail_unless(pos2!=objectMap.end());
    pGlobalStyle=dynamic_cast<const GlobalStyle*>(pos2->second);
    fail_unless(pGlobalStyle!=NULL);
    fail_unless(pGlobalStyle->getLevel() == level);
    fail_unless(pGlobalStyle->getVersion() == version);

    fail_unless(pGlobalStyle->getNumTypes()==2);
    fail_unless(pGlobalStyle->isInTypeList("REACTIONGLYPH")==true);
    fail_unless(pGlobalStyle->isInTypeList("TEXTGLYPH")==true);
    pGroup=pGlobalStyle->getGroup();
    fail_unless(pGroup->getLevel() == level);
    fail_unless(pGroup->getVersion() == version);

    fail_unless(pGroup->getStroke()=="black");
    fail_unless(fabs(pGroup->getStrokeWidth()-1.0) < 1e-12);
    fail_unless(fabs((pGroup->getFontSize().getAbsoluteValue()-12.0)/12.0) < 1e-12);
    fail_unless(fabs(pGroup->getFontSize().getRelativeValue()) < 1e-12);
    fail_unless(pGroup->getTextAnchor()==Text::ANCHOR_MIDDLE);
    fail_unless(pGroup->getListOfElements()->size()==0);
    
    pos2=objectMap.find("speciesReferenceGlyphStyle");
    fail_unless(pos2!=objectMap.end());
    pGlobalStyle=dynamic_cast<const GlobalStyle*>(pos2->second);
    fail_unless(pGlobalStyle!=NULL);
    fail_unless(pGlobalStyle->getLevel() == level);
    fail_unless(pGlobalStyle->getVersion() == version);

    fail_unless(pGlobalStyle->getNumTypes()==0);
    fail_unless(pGlobalStyle->getNumRoles()==4);
    fail_unless(pGlobalStyle->isInRoleList("substrate")==true);
    fail_unless(pGlobalStyle->isInRoleList("sidesubstrate")==true);
    fail_unless(pGlobalStyle->isInRoleList("product")==true);
    fail_unless(pGlobalStyle->isInRoleList("sideproduct")==true);
    pGroup=pGlobalStyle->getGroup();
    fail_unless(pGroup->getLevel() == level);
    fail_unless(pGroup->getVersion() == version);

    fail_unless(pGroup->getStroke()=="#000000");
    fail_unless(pGroup->getEndHead()=="simpleHead_black");
    fail_unless(fabs(pGroup->getStrokeWidth()-1.0) < 1e-12);
    fail_unless(pGroup->getListOfElements()->size()==0);
    pos2=objectMap.find("activatorSpeciesReferenceGlyphStyle");
    fail_unless(pos2!=objectMap.end());
    pGlobalStyle=dynamic_cast<const GlobalStyle*>(pos2->second);
    fail_unless(pGlobalStyle!=NULL);
    fail_unless(pGlobalStyle->getLevel() == level);
    fail_unless(pGlobalStyle->getVersion() == version);

    fail_unless(pGlobalStyle->getNumTypes()==0);
    fail_unless(pGlobalStyle->getNumRoles()==1);
    fail_unless(pGlobalStyle->isInRoleList("activator")==true);
    pGroup=pGlobalStyle->getGroup();
    fail_unless(pGroup->getLevel() == level);
    fail_unless(pGroup->getVersion() == version);

    fail_unless(pGroup->getStroke()=="green");
    fail_unless(pGroup->getEndHead()=="simpleHead_green");
    fail_unless(fabs(pGroup->getStrokeWidth()-1.0) < 1e-12);
    fail_unless(pGroup->getListOfElements()->size()==0);
    pos2=objectMap.find("inhibitorSpeciesReferenceGlyphStyle");
    fail_unless(pos2!=objectMap.end());
    pGlobalStyle=dynamic_cast<const GlobalStyle*>(pos2->second);
    fail_unless(pGlobalStyle!=NULL);
    fail_unless(pGlobalStyle->getLevel() == level);
    fail_unless(pGlobalStyle->getVersion() == version);

    fail_unless(pGlobalStyle->getNumTypes()==0);
    fail_unless(pGlobalStyle->getNumRoles()==1);
    fail_unless(pGlobalStyle->isInRoleList("inhibitor")==true);
    pGroup=pGlobalStyle->getGroup();
    fail_unless(pGroup->getLevel() == level);
    fail_unless(pGroup->getVersion() == version);

    fail_unless(pGroup->getStroke()=="red");
    fail_unless(pGroup->getEndHead()=="simpleHead_red");
    fail_unless(fabs(pGroup->getStrokeWidth()-1.0) < 1e-12);
    fail_unless(pGroup->getListOfElements()->size()==0);
    pos2=objectMap.find("modifierSpeciesReferenceGlyphStyle");
    fail_unless(pos2!=objectMap.end());
    pGlobalStyle=dynamic_cast<const GlobalStyle*>(pos2->second);
    fail_unless(pGlobalStyle!=NULL);
    fail_unless(pGlobalStyle->getLevel() == level);
    fail_unless(pGlobalStyle->getVersion() == version);

    fail_unless(pGlobalStyle->getNumTypes()==0);
    fail_unless(pGlobalStyle->getNumRoles()==1);
    fail_unless(pGlobalStyle->isInRoleList("modifier")==true);
    pGroup=pGlobalStyle->getGroup();
    fail_unless(pGroup->getLevel() == level);
    fail_unless(pGroup->getVersion() == version);

    fail_unless(pGroup->getStroke()=="blue");
    fail_unless(pGroup->getEndHead()=="simpleHead_blue");
    fail_unless(fabs(pGroup->getStrokeWidth()-1.0) < 1e-12);
    fail_unless(pGroup->getListOfElements()->size()==0);
    objectMap.clear();

    // make sure the local layout is read
    RenderLayoutPlugin* rPlugin=(RenderLayoutPlugin*)pLayout->getPlugin("render");
    fail_unless(rPlugin != NULL);
    if (rPlugin == NULL) return;
    fail_unless(rPlugin->getListOfLocalRenderInformation()->size()==1);
    // check the local layout
    const LocalRenderInformation* pLocal=rPlugin->getRenderInformation(0);
    fail_unless(pLocal!=NULL);
    fail_unless(pLocal->getLevel() == level);
    fail_unless(pLocal->getVersion() == version);

    fail_unless(!pLocal->isSetName());
    fail_unless(pLocal->isSetId());
    fail_unless(pLocal->getId()=="highlightGlucose");
    fail_unless(pLocal->getReferenceRenderInformationId()=="defaultStyle");
    fail_unless(pLocal->getProgramName()=="Ralph Gauges");
    fail_unless(pLocal->getProgramVersion()=="1.0");
    fail_unless(pLocal->getListOfColorDefinitions()->size()==2);
    pColor=NULL;
    for(i=0;i<2;++i)
    {
        pColor=pLocal->getColorDefinition(i);
        fail_unless(pColor!=NULL);
        fail_unless(pColor->getLevel() == level);
        fail_unless(pColor->getVersion() == version);

        fail_unless(pColor->isSetId());
        objectMap.insert(std::pair<std::string,const SBase*>(pColor->getId(),pColor));
    }
    fail_unless(objectMap.size()==2);
    // test the two colors
    pos2=objectMap.find("white");
    fail_unless(pos2!=objectMap.end());
    pColor=dynamic_cast<const ColorDefinition*>(pos2->second);
    fail_unless(pColor!=NULL);
    fail_unless(pColor->getRed()==255);
    fail_unless(pColor->getGreen()==255);
    fail_unless(pColor->getBlue()==255);
    fail_unless(pColor->getAlpha()==255);
    pos2=objectMap.find("lightRed");
    fail_unless(pos2!=objectMap.end());
    pColor=dynamic_cast<const ColorDefinition*>(pos2->second);
    fail_unless(pColor!=NULL);
    fail_unless(pColor->getRed()==0xE6);
    fail_unless(pColor->getGreen()==0xAD);
    fail_unless(pColor->getBlue()==0xD8);
    fail_unless(pColor->getAlpha()==255);
    objectMap.clear();
    // check the gradient definition
    fail_unless(pLocal->getListOfGradientDefinitions()->size()==1);
    pGradientBase=pLocal->getGradientDefinition(0);
    fail_unless(pGradientBase!=NULL);
    fail_unless(pGradientBase->getLevel() == level);
    fail_unless(pGradientBase->getVersion() == version);

    fail_unless(pGradientBase->isSetId());
    fail_unless(pGradientBase->getId()=="highlightedSpeciesGlyphGradient");
    pRadialGradient=dynamic_cast<const RadialGradient*>(pGradientBase);
    fail_unless(pRadialGradient!=NULL);
    // all values are default
    v=pRadialGradient->getCenterX();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs((v.getRelativeValue()-50.0)/50.0) < 1e-12);
    v=pRadialGradient->getCenterY();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs((v.getRelativeValue()-50.0)/50.0) < 1e-12);
    v=pRadialGradient->getCenterZ();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs((v.getRelativeValue()-50.0)/50.0) < 1e-12);
    v=pRadialGradient->getRadius();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs((v.getRelativeValue()-50.0)/50.0) < 1e-12);
    // all F parameter must be the same as the center point
    v=pRadialGradient->getFocalPointX();
    fail_unless(v==pRadialGradient->getCenterX());
    v=pRadialGradient->getFocalPointY();
    fail_unless(v==pRadialGradient->getCenterY());
    v=pRadialGradient->getFocalPointZ();
    fail_unless(v==pRadialGradient->getCenterZ());
    fail_unless(pGradientBase->getListOfGradientStops()->size()==2);
    pGradientStop=pGradientBase->getGradientStop(0);
    fail_unless(pGradientStop!=NULL);
    fail_unless(pGradientStop->getLevel() == level);
    fail_unless(pGradientStop->getVersion() == version);

    fail_unless(pGradientStop->getStopColor()=="white");
    v=pGradientStop->getOffset();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    pGradientStop=pGradientBase->getGradientStop(1);
    fail_unless(pGradientStop!=NULL);
    fail_unless(pGradientStop->getLevel() == level);
    fail_unless(pGradientStop->getVersion() == version);

    fail_unless(pGradientStop->getStopColor()=="lightRed");
    v=pGradientStop->getOffset();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs((v.getRelativeValue()-100.0)/100.0) < 1e-12);

    // check the one style
    fail_unless(pLocal->getListOfStyles()->size()==1);
    const LocalStyle* pLocalStyle=NULL;
    for(i=0;i<1;++i)
    {
        pLocalStyle=dynamic_cast<const LocalStyle*>(pLocal->getStyle(i));
        fail_unless(pLocalStyle!=NULL);
        fail_unless(pLocalStyle->getLevel() == level);
        fail_unless(pLocalStyle->getVersion() == version);

        fail_unless(pLocalStyle->isSetId());
        objectMap.insert(std::pair<std::string,const SBase*>(pLocalStyle->getId(),pLocalStyle));
    }
    fail_unless(objectMap.size()==1);
    // test the three styles
    pos2=objectMap.find("highlightedGlucose");
    fail_unless(pos2!=objectMap.end());
    pLocalStyle=dynamic_cast<const LocalStyle*>(pos2->second);
    fail_unless(pLocalStyle!=NULL);
    fail_unless(pLocalStyle->getLevel() == level);
    fail_unless(pLocalStyle->getVersion() == version);

    fail_unless(pLocalStyle->getNumTypes()==0);
    fail_unless(pLocalStyle->getNumIds()==1);
    fail_unless(pLocalStyle->isInIdList("SpeciesGlyph_Glucose")==true);
    pGroup=pLocalStyle->getGroup();
    fail_unless(pGroup->getLevel() == level);
    fail_unless(pGroup->getVersion() == version);

    fail_unless(pGroup->getStroke()=="black");
    fail_unless(fabs(pGroup->getStrokeWidth()-1.0) < 1e-12);
    fail_unless(pGroup->getListOfElements()->size()==1);
    // the one element is a rectangle
    pRectangle=dynamic_cast<const Rectangle*>(pGroup->getElement(0));
    fail_unless(pRectangle!=NULL);
    fail_unless(pRectangle->getLevel() == level);
    fail_unless(pRectangle->getVersion() == version);

    fail_unless(pRectangle->getFillColor()=="highlightedSpeciesGlyphGradient");
    v=pRectangle->getX();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs(v.getRelativeValue()) < 1e-12); 
    v=pRectangle->getY();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs(v.getRelativeValue()) < 1e-12); 
    v=pRectangle->getRadiusX();
    fail_unless(fabs((v.getAbsoluteValue()-5.0)/5.0) < 1e-12); 
    fail_unless(fabs(v.getRelativeValue()) < 1e-12); 
    v=pRectangle->getRadiusY();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs((v.getRelativeValue()-50.0)/50.0) < 1e-12); 
    v=pRectangle->getWidth();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs((v.getRelativeValue()-100.0)/100.0) < 1e-12); 
    v=pRectangle->getHeight();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs((v.getRelativeValue()-100.0)/100.0) < 1e-12); 

    delete pDocument;

}
END_TEST 

START_TEST (test_RenderReading_read_L3_model_1)
{
    const unsigned int level = 3;
    const unsigned int version = 1;
    SBMLReader reader;
    SBMLDocument* pDocument=reader.readSBMLFromString(MODEL_2);
    fail_unless(pDocument!=NULL);
    fail_unless(pDocument->getLevel() == level);
    fail_unless(pDocument->getVersion() == version);

    fail_unless(pDocument->getNumErrors()==0);

    pDocument->printErrors();

    fail_unless(pDocument->getLevel() == 3);
    fail_unless(pDocument->getVersion() == 1);
    fail_unless(pDocument->getModel()!=NULL);
    fail_unless(pDocument->getModel()->getLevel() == level);
    fail_unless(pDocument->getModel()->getVersion() == version);

    fail_unless(pDocument->getModel()->getListOfCompartments()->size()==1);
    fail_unless(pDocument->getModel()->getListOfSpecies()->size()==5);
    fail_unless(pDocument->getModel()->getListOfReactions()->size()==1);
    LayoutModelPlugin *plugin = (LayoutModelPlugin*)pDocument->getModel()->getPlugin("layout");
    fail_unless(plugin->getListOfLayouts()->size()==1);
    const Layout* pLayout=plugin->getLayout(0);
    // Do some sanity checks on the layout
    fail_unless(pLayout!=NULL);
    fail_unless(pLayout->getLevel() == level);
    fail_unless(pLayout->getVersion() == version);

    const Dimensions* pDimensions=pLayout->getDimensions();
    fail_unless(pDimensions!=NULL);
    fail_unless(fabs((pDimensions->getWidth()-400.0)/400.0) < 1e-12);
    fail_unless(fabs((pDimensions->getHeight()-230.0)/230.0) < 1e-12);
    fail_unless(pLayout->getListOfCompartmentGlyphs()->size()==1);
    fail_unless(pLayout->getListOfSpeciesGlyphs()->size()==5);
    fail_unless(pLayout->getListOfReactionGlyphs()->size()==1);
    fail_unless(pLayout->getListOfTextGlyphs()->size()==5);

    RenderListOfLayoutsPlugin *lolPlugin = (RenderListOfLayoutsPlugin *)plugin->getListOfLayouts()->getPlugin("render");
    fail_unless(lolPlugin != NULL);
    if (lolPlugin == NULL) return;
    fail_unless(lolPlugin->getListOfGlobalRenderInformation()->size()==4);
    
    
    // check the four global render information objects
    std::map<std::string,const GlobalRenderInformation*> globalRenderInformationMap;
    const GlobalRenderInformation* pGlobal=NULL;
    unsigned int i;
    std::string id;
    for(i=0;i<4;++i)
    {
         pGlobal=lolPlugin->getRenderInformation(i);
         fail_unless(pGlobal!=NULL);
         if (pGlobal == NULL) return;
         fail_unless(pGlobal->getLevel() == level);
         fail_unless(pGlobal->getVersion() == version);

         fail_unless(pGlobal->isSetId()==true);
         id=pGlobal->getId();
         fail_unless(!id.empty());
         globalRenderInformationMap.insert(std::pair<std::string,const GlobalRenderInformation*>(id,pGlobal));
    }
    fail_unless(globalRenderInformationMap.size()==4);

    
    // check the global render information called wireFrame
    std::map<std::string,const GlobalRenderInformation*>::const_iterator pos=globalRenderInformationMap.find("wireFrame");
    fail_unless(pos!=globalRenderInformationMap.end());
    pGlobal=pos->second;
    fail_unless(pGlobal->isSetName());
    fail_unless(pGlobal->getName()=="wireframe style");
    fail_unless(pGlobal->getProgramName()=="Ralph Gauges");
    fail_unless(pGlobal->getProgramVersion()=="1.0");
    fail_unless(pGlobal->getListOfColorDefinitions()->size()==2);
    std::map<std::string,const SBase*> objectMap;
    const ColorDefinition* pColor=NULL;
    for(i=0;i<2;++i)
    {
        pColor=pGlobal->getColorDefinition(i);
        fail_unless(pColor!=NULL);
        fail_unless(pColor->getLevel() == level);
        fail_unless(pColor->getVersion() == version);

        fail_unless(pColor->isSetId());
        objectMap.insert(std::pair<std::string,const SBase*>(pColor->getId(),pColor));
    }
    fail_unless(objectMap.size()==2);
    // test the two colors
    std::map<std::string,const SBase*>::const_iterator pos2=objectMap.find("white");
    fail_unless(pos2!=objectMap.end());
    pColor=dynamic_cast<const ColorDefinition*>(pos2->second);
    fail_unless(pColor!=NULL);
    fail_unless(pColor->getRed()==255);
    fail_unless(pColor->getGreen()==255);
    fail_unless(pColor->getBlue()==255);
    fail_unless(pColor->getAlpha()==255);
    pos2=objectMap.find("black");
    fail_unless(pos2!=objectMap.end());
    pColor=dynamic_cast<const ColorDefinition*>(pos2->second);
    fail_unless(pColor!=NULL);
    fail_unless(pColor->getRed()==0);
    fail_unless(pColor->getGreen()==0);
    fail_unless(pColor->getBlue()==0);
    fail_unless(pColor->getAlpha()==255);
    objectMap.clear();
    // check the styles
    fail_unless(pGlobal->getListOfStyles()->size()==3);
    const GlobalStyle* pGlobalStyle=NULL;
    for(i=0;i<3;++i)
    {
        pGlobalStyle=dynamic_cast<const GlobalStyle*>(pGlobal->getStyle(i));
        fail_unless(pGlobalStyle!=NULL);
        fail_unless(pGlobalStyle->getLevel() == level);
        fail_unless(pGlobalStyle->getVersion() == version);

        fail_unless(pGlobalStyle->isSetId());
        objectMap.insert(std::pair<std::string,const SBase*>(pGlobalStyle->getId(),pGlobalStyle));
    }
    fail_unless(objectMap.size()==3);
    // test the three styles
    pos2=objectMap.find("compartmentGlyphStyle");
    fail_unless(pos2!=objectMap.end());
    pGlobalStyle=dynamic_cast<const GlobalStyle*>(pos2->second);
    fail_unless(pGlobalStyle!=NULL);
    fail_unless(pGlobalStyle->getNumTypes()==1);
    fail_unless(pGlobalStyle->isInTypeList("COMPARTMENTGLYPH")==true);
    const RenderGroup* pGroup=pGlobalStyle->getGroup();
    fail_unless(pGroup->getLevel() == level);
    fail_unless(pGroup->getVersion() == version);

    fail_unless(pGroup->getStroke()=="black");
    fail_unless(fabs(pGroup->getStrokeWidth()-1.0) < 1e-12);
    fail_unless(pGroup->getListOfElements()->size()==1);
    // the one element is a rectangle
    const Rectangle* pRectangle=dynamic_cast<const Rectangle*>(pGroup->getElement(0));
    fail_unless(pRectangle!=NULL);
    fail_unless(pRectangle->getLevel() == level);
    fail_unless(pRectangle->getVersion() == version);

    fail_unless(pRectangle->getFillColor()=="none");
    RelAbsVector v=pRectangle->getX();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs(v.getRelativeValue()) < 1e-12); 
    v=pRectangle->getY();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs(v.getRelativeValue()) < 1e-12); 
    v=pRectangle->getRadiusX();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs(v.getRelativeValue()) < 1e-12); 
    v=pRectangle->getRadiusY();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs(v.getRelativeValue()) < 1e-12); 
    v=pRectangle->getWidth();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs((v.getRelativeValue()-100.0)/100.0) < 1e-12); 
    v=pRectangle->getHeight();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs((v.getRelativeValue()-100.0)/100.0) < 1e-12); 
    
    pos2=objectMap.find("speciesGlyphStyle");
    fail_unless(pos2!=objectMap.end());
    pGlobalStyle=dynamic_cast<const GlobalStyle*>(pos2->second);
    fail_unless(pGlobalStyle!=NULL);
    fail_unless(pGlobalStyle->getNumTypes()==1);
    fail_unless(pGlobalStyle->isInTypeList("SPECIESGLYPH")==true);
    pGroup=pGlobalStyle->getGroup();
    fail_unless(pGroup->getLevel() == level);
    fail_unless(pGroup->getVersion() == version);

    fail_unless(pGroup->getStroke()=="black");
    fail_unless(fabs(pGroup->getStrokeWidth()-1.0) < 1e-12);
    fail_unless(pGroup->getListOfElements()->size()==1);
    // the one element is a rectangle
    pRectangle=dynamic_cast<const Rectangle*>(pGroup->getElement(0));
    fail_unless(pRectangle!=NULL);
    fail_unless(pRectangle->getLevel() == level);
    fail_unless(pRectangle->getVersion() == version);

    fail_unless(pRectangle->getFillColor()=="none");
    v=pRectangle->getX();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs(v.getRelativeValue()) < 1e-12); 
    v=pRectangle->getY();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs(v.getRelativeValue()) < 1e-12); 
    v=pRectangle->getRadiusX();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs(v.getRelativeValue()) < 1e-12); 
    v=pRectangle->getRadiusY();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs(v.getRelativeValue()) < 1e-12); 
    v=pRectangle->getWidth();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs((v.getRelativeValue()-100.0)/100.0) < 1e-12); 
    v=pRectangle->getHeight();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs((v.getRelativeValue()-100.0)/100.0) < 1e-12); 

    pos2=objectMap.find("reactionGlyphStyle");
    fail_unless(pos2!=objectMap.end());
    pGlobalStyle=dynamic_cast<const GlobalStyle*>(pos2->second);
    fail_unless(pGlobalStyle!=NULL);
    fail_unless(pGlobalStyle->getNumTypes()==3);
    fail_unless(pGlobalStyle->isInTypeList("REACTIONGLYPH")==true);
    fail_unless(pGlobalStyle->isInTypeList("SPECIESREFERENCEGLYPH")==true);
    fail_unless(pGlobalStyle->isInTypeList("TEXTGLYPH")==true);
    pGroup=pGlobalStyle->getGroup();
    fail_unless(pGroup->getLevel() == level);
    fail_unless(pGroup->getVersion() == version);

    fail_unless(pGroup->getStroke()=="black");
    fail_unless(fabs(pGroup->getStrokeWidth()-1.0) < 1e-12);
    fail_unless(fabs((pGroup->getFontSize().getAbsoluteValue()-12.0)/12.0) < 1e-12);
    fail_unless(fabs(pGroup->getFontSize().getRelativeValue()) < 1e-12);
    fail_unless(pGroup->getTextAnchor()==Text::ANCHOR_MIDDLE);
    fail_unless(pGroup->getListOfElements()->size()==0);
    objectMap.clear();

    // check the global render information called defaultGrayStyle
    pos=globalRenderInformationMap.find("defaultGrayStyle");
    fail_unless(pos!=globalRenderInformationMap.end());
    pGlobal=pos->second;
    fail_unless(pGlobal->isSetName());
    fail_unless(pGlobal->getName()=="grayscale style");
    fail_unless(pGlobal->getProgramName()=="Ralph Gauges");
    fail_unless(pGlobal->getProgramVersion()=="1.0");
    fail_unless(pGlobal->getListOfColorDefinitions()->size()==5);
    for(i=0;i<5;++i)
    {
        pColor=pGlobal->getColorDefinition(i);
        fail_unless(pColor!=NULL);
        fail_unless(pColor->getLevel() == level);
        fail_unless(pColor->getVersion() == version);

        fail_unless(pColor->isSetId());
        objectMap.insert(std::pair<std::string,const SBase*>(pColor->getId(),pColor));
    }
    fail_unless(objectMap.size()==5);
    // test the five colors
    pos2=objectMap.find("white");
    fail_unless(pos2!=objectMap.end());
    pColor=dynamic_cast<const ColorDefinition*>(pos2->second);
    fail_unless(pColor!=NULL);
    fail_unless(pColor->getRed()==255);
    fail_unless(pColor->getGreen()==255);
    fail_unless(pColor->getBlue()==255);
    fail_unless(pColor->getAlpha()==255);
    pos2=objectMap.find("black");
    fail_unless(pos2!=objectMap.end());
    pColor=dynamic_cast<const ColorDefinition*>(pos2->second);
    fail_unless(pColor!=NULL);
    fail_unless(pColor->getRed()==0);
    fail_unless(pColor->getGreen()==0);
    fail_unless(pColor->getBlue()==0);
    fail_unless(pColor->getAlpha()==255);
    pos2=objectMap.find("lightGray");
    fail_unless(pos2!=objectMap.end());
    pColor=dynamic_cast<const ColorDefinition*>(pos2->second);
    fail_unless(pColor!=NULL);
    fail_unless(pColor->getRed()==0xCE);
    fail_unless(pColor->getGreen()==0xCE);
    fail_unless(pColor->getBlue()==0xCE);
    fail_unless(pColor->getAlpha()==255);
    pos2=objectMap.find("lightGray2");
    fail_unless(pos2!=objectMap.end());
    pColor=dynamic_cast<const ColorDefinition*>(pos2->second);
    fail_unless(pColor!=NULL);
    fail_unless(pColor->getRed()==0xF0);
    fail_unless(pColor->getGreen()==0xF0);
    fail_unless(pColor->getBlue()==0xF0);
    fail_unless(pColor->getAlpha()==255);
    pos2=objectMap.find("gray");
    fail_unless(pos2!=objectMap.end());
    pColor=dynamic_cast<const ColorDefinition*>(pos2->second);
    fail_unless(pColor!=NULL);
    fail_unless(pColor->getRed()==0x0B);
    fail_unless(pColor->getGreen()==0x0B);
    fail_unless(pColor->getBlue()==0x0B);
    fail_unless(pColor->getAlpha()==255);
    objectMap.clear();
    // check the gradient definition
    fail_unless(pGlobal->getListOfGradientDefinitions()->size()==1);
    const GradientBase* pGradientBase=pGlobal->getGradientDefinition(0);
    fail_unless(pGradientBase!=NULL);
    fail_unless(pGradientBase->getLevel() == level);
    fail_unless(pGradientBase->getVersion() == version);

    fail_unless(pGradientBase->isSetId());
    fail_unless(pGradientBase->getId()=="speciesGlyphGradient");
    const RadialGradient* pRadialGradient=dynamic_cast<const RadialGradient*>(pGradientBase);
    fail_unless(pRadialGradient!=NULL);
    // all values are default
    v=pRadialGradient->getCenterX();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs((v.getRelativeValue()-50.0)/50.0) < 1e-12);
    v=pRadialGradient->getCenterY();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs((v.getRelativeValue()-50.0)/50.0) < 1e-12);
    v=pRadialGradient->getCenterZ();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs((v.getRelativeValue()-50.0)/50.0) < 1e-12);
    v=pRadialGradient->getRadius();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs((v.getRelativeValue()-50.0)/50.0) < 1e-12);
    // all F parameters must be the same as the center
    v=pRadialGradient->getFocalPointX();
    fail_unless(v==pRadialGradient->getCenterX());
    v=pRadialGradient->getFocalPointY();
    fail_unless(v==pRadialGradient->getCenterY());
    v=pRadialGradient->getFocalPointZ();
    fail_unless(v==pRadialGradient->getCenterZ());
    fail_unless(pGradientBase->getListOfGradientStops()->size()==2);
    const GradientStop* pGradientStop=pGradientBase->getGradientStop(0);
    fail_unless(pGradientStop!=NULL);
    fail_unless(pGradientStop->getLevel() == level);
    fail_unless(pGradientStop->getVersion() == version);

    fail_unless(pGradientStop->getStopColor()=="white");
    v=pGradientStop->getOffset();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    pGradientStop=pGradientBase->getGradientStop(1);
    fail_unless(pGradientStop!=NULL);
    fail_unless(pGradientStop->getLevel() == level);
    fail_unless(pGradientStop->getVersion() == version);

    fail_unless(pGradientStop->getStopColor()=="lightGray");
    v=pGradientStop->getOffset();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs((v.getRelativeValue()-100.0)/100.0) < 1e-12);
    // check the line endings 
    fail_unless(pGlobal->getListOfLineEndings()->size()==1); 
    const LineEnding* pLineEnding=NULL;
    for(i=0;i<1;++i)
    {
        pLineEnding=pGlobal->getLineEnding(i);
        fail_unless(pLineEnding!=NULL);
        fail_unless(pLineEnding->getLevel() == level);
        fail_unless(pLineEnding->getVersion() == version);

        fail_unless(pLineEnding->isSetId());
        objectMap.insert(std::pair<std::string,const SBase*>(pLineEnding->getId(),pLineEnding));
    }
    fail_unless(objectMap.size()==1);
    // lineEnding simpleHead_black
    pos2=objectMap.find("simpleHead_black");
    fail_unless(pos2!=objectMap.end());
    const SBase* pSBase=pos2->second;
    pLineEnding=dynamic_cast<const LineEnding*>(pSBase);
    const BoundingBox* pBoundingBox=pLineEnding->getBoundingBox();
    fail_unless(pBoundingBox!=NULL);
    fail_unless(fabs((pBoundingBox->getPosition()->x()-(-8.0))/-8.0) < 1e-12);
    fail_unless(fabs((pBoundingBox->getPosition()->y()-(-3.0))/-3.0) < 1e-12);
    fail_unless(fabs((pBoundingBox->getDimensions()->getWidth()-10.0)/10.0) < 1e-12);
    fail_unless(fabs((pBoundingBox->getDimensions()->getHeight()-6.0)/6.0) < 1e-12);
    pGroup=pLineEnding->getGroup();
    fail_unless(pGroup!=NULL);
    fail_unless(pGroup->getLevel() == level);
    fail_unless(pGroup->getVersion() == version);

    fail_unless(pGroup->getStroke()=="black");
    fail_unless(fabs(pGroup->getStrokeWidth()-1.0) < 1e-12);
    fail_unless(pGroup->getFillColor()=="black");
    fail_unless(pGroup->getListOfElements()->size()==1);
    const Polygon* pPolygon=dynamic_cast<const Polygon*>(pGroup->getElement(0));
    fail_unless(pPolygon!=NULL);
    fail_unless(pPolygon->getLevel() == level);
    fail_unless(pPolygon->getVersion() == version);

    fail_unless(pPolygon->getListOfElements()->size()==3);
    const RenderPoint* pElement=pPolygon->getElement(0);
    fail_unless(pElement!=NULL);
    fail_unless(pElement->getLevel() == level);
    fail_unless(pElement->getVersion() == version);

    fail_unless(pElement->getTypeCode()==SBML_RENDER_POINT);
    v=pElement->x();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->y();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->z();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    pElement=pPolygon->getElement(1);
    fail_unless(pElement!=NULL);
    fail_unless(pElement->getLevel() == level);
    fail_unless(pElement->getVersion() == version);

    fail_unless(pElement->getTypeCode()==SBML_RENDER_POINT);
    v=pElement->x();
    fail_unless(fabs((v.getAbsoluteValue()-10.0)/10.0) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->y();
    fail_unless(fabs((v.getAbsoluteValue()-3.0)/3.0) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->z();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    pElement=pPolygon->getElement(2);
    fail_unless(pElement!=NULL);
    fail_unless(pElement->getLevel() == level);
    fail_unless(pElement->getVersion() == version);

    fail_unless(pElement->getTypeCode()==SBML_RENDER_POINT);
    v=pElement->x();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->y();
    fail_unless(fabs((v.getAbsoluteValue()-6.0)/6.0) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->z();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);

    objectMap.clear();
    // check the styles
    fail_unless(pGlobal->getListOfStyles()->size()==7);
    for(i=0;i<7;++i)
    {
        pGlobalStyle=dynamic_cast<const GlobalStyle*>(pGlobal->getStyle(i));
        fail_unless(pGlobalStyle!=NULL);
        fail_unless(pGlobalStyle->getLevel() == level);
        fail_unless(pGlobalStyle->getVersion() == version);

        fail_unless(pGlobalStyle->isSetId());
        objectMap.insert(std::pair<std::string,const SBase*>(pGlobalStyle->getId(),pGlobalStyle));
    }
    fail_unless(objectMap.size()==7);
    // test the three styles
    pos2=objectMap.find("compartmentGlyphStyle");
    fail_unless(pos2!=objectMap.end());
    pGlobalStyle=dynamic_cast<const GlobalStyle*>(pos2->second);
    fail_unless(pGlobalStyle!=NULL);
    fail_unless(pGlobalStyle->getNumTypes()==1);
    fail_unless(pGlobalStyle->isInTypeList("COMPARTMENTGLYPH")==true);
    pGroup=pGlobalStyle->getGroup();
    fail_unless(pGroup->getLevel() == level);
    fail_unless(pGroup->getVersion() == version);

    fail_unless(pGroup->getStroke()=="gray");
    fail_unless(fabs(pGroup->getStrokeWidth()-1.0) < 1e-12);
    fail_unless(pGroup->getListOfElements()->size()==1);
    // the one element is a rectangle
    pRectangle=dynamic_cast<const Rectangle*>(pGroup->getElement(0));
    fail_unless(pRectangle!=NULL);
    fail_unless(pRectangle->getLevel() == level);
    fail_unless(pRectangle->getVersion() == version);

    fail_unless(pRectangle->getFillColor()=="lightGray2");
    v=pRectangle->getX();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs(v.getRelativeValue()) < 1e-12); 
    v=pRectangle->getY();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs(v.getRelativeValue()) < 1e-12); 
    v=pRectangle->getRadiusX();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs((v.getRelativeValue()-5.0)/5.0) < 1e-12); 
    v=pRectangle->getRadiusY();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs(((v.getRelativeValue()-5.0)/5.0)) < 1e-12); 
    v=pRectangle->getWidth();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs((v.getRelativeValue()-100.0)/100.0) < 1e-12); 
    v=pRectangle->getHeight();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs((v.getRelativeValue()-100.0)/100.0) < 1e-12); 
    
    pos2=objectMap.find("speciesGlyphStyle");
    fail_unless(pos2!=objectMap.end());
    pGlobalStyle=dynamic_cast<const GlobalStyle*>(pos2->second);
    fail_unless(pGlobalStyle!=NULL);
    fail_unless(pGlobalStyle->getNumTypes()==1);
    fail_unless(pGlobalStyle->isInTypeList("SPECIESGLYPH")==true);
    pGroup=pGlobalStyle->getGroup();
    fail_unless(pGroup->getLevel() == level);
    fail_unless(pGroup->getVersion() == version);

    fail_unless(pGroup->getStroke()=="black");
    fail_unless(fabs(pGroup->getStrokeWidth()-1.0) < 1e-12);
    fail_unless(pGroup->getListOfElements()->size()==1);
    // the one element is a rectangle
    pRectangle=dynamic_cast<const Rectangle*>(pGroup->getElement(0));
    fail_unless(pRectangle!=NULL);
    fail_unless(pRectangle->getLevel() == level);
    fail_unless(pRectangle->getVersion() == version);

    fail_unless(pRectangle->getFillColor()=="speciesGlyphGradient");
    v=pRectangle->getX();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs(v.getRelativeValue()) < 1e-12); 
    v=pRectangle->getY();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs(v.getRelativeValue()) < 1e-12); 
    v=pRectangle->getRadiusX();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs((v.getRelativeValue()-5.0)/5.0) < 1e-12); 
    v=pRectangle->getRadiusY();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs((v.getRelativeValue()-5.0)/5.0) < 1e-12); 
    v=pRectangle->getWidth();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs((v.getRelativeValue()-100.0)/100.0) < 1e-12); 
    v=pRectangle->getHeight();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs((v.getRelativeValue()-100.0)/100.0) < 1e-12); 

    pos2=objectMap.find("reactionGlyphStyle");
    fail_unless(pos2!=objectMap.end());
    pGlobalStyle=dynamic_cast<const GlobalStyle*>(pos2->second);
    fail_unless(pGlobalStyle!=NULL);
    fail_unless(pGlobalStyle->getNumTypes()==2);
    fail_unless(pGlobalStyle->isInTypeList("REACTIONGLYPH")==true);
    fail_unless(pGlobalStyle->isInTypeList("TEXTGLYPH")==true);
    pGroup=pGlobalStyle->getGroup();
    fail_unless(pGroup->getLevel() == level);
    fail_unless(pGroup->getVersion() == version);

    fail_unless(pGroup->getStroke()=="black");
    fail_unless(fabs(pGroup->getStrokeWidth()-1.0) < 1e-12);
    fail_unless(fabs((pGroup->getFontSize().getAbsoluteValue()-12.0)/12.0) < 1e-12);
    fail_unless(fabs(pGroup->getFontSize().getRelativeValue()) < 1e-12);
    fail_unless(pGroup->getTextAnchor()==Text::ANCHOR_MIDDLE);
    fail_unless(pGroup->getListOfElements()->size()==0);
    
    pos2=objectMap.find("speciesReferenceGlyphStyle");
    fail_unless(pos2!=objectMap.end());
    pGlobalStyle=dynamic_cast<const GlobalStyle*>(pos2->second);
    fail_unless(pGlobalStyle!=NULL);
    fail_unless(pGlobalStyle->getNumTypes()==0);
    fail_unless(pGlobalStyle->getNumRoles()==4);
    fail_unless(pGlobalStyle->isInRoleList("substrate")==true);
    fail_unless(pGlobalStyle->isInRoleList("sidesubstrate")==true);
    fail_unless(pGlobalStyle->isInRoleList("product")==true);
    fail_unless(pGlobalStyle->isInRoleList("sideproduct")==true);
    pGroup=pGlobalStyle->getGroup();
    fail_unless(pGroup->getLevel() == level);
    fail_unless(pGroup->getVersion() == version);

    fail_unless(pGroup->getStroke()=="#000000");
    fail_unless(fabs(pGroup->getStrokeWidth()-1.0) < 1e-12);
    fail_unless(pGroup->getListOfElements()->size()==0);
    pos2=objectMap.find("activatorSpeciesReferenceGlyphStyle");
    fail_unless(pos2!=objectMap.end());
    pGlobalStyle=dynamic_cast<const GlobalStyle*>(pos2->second);
    fail_unless(pGlobalStyle!=NULL);
    fail_unless(pGlobalStyle->getNumTypes()==0);
    fail_unless(pGlobalStyle->getNumRoles()==1);
    fail_unless(pGlobalStyle->isInRoleList("activator")==true);
    pGroup=pGlobalStyle->getGroup();
    fail_unless(pGroup->getLevel() == level);
    fail_unless(pGroup->getVersion() == version);

    fail_unless(pGroup->getStroke()=="#000000");
    fail_unless(fabs(pGroup->getStrokeWidth()-1.0) < 1e-12);
    fail_unless(pGroup->getListOfElements()->size()==0);
    pos2=objectMap.find("inhibitorSpeciesReferenceGlyphStyle");
    fail_unless(pos2!=objectMap.end());
    pGlobalStyle=dynamic_cast<const GlobalStyle*>(pos2->second);
    fail_unless(pGlobalStyle!=NULL);
    fail_unless(pGlobalStyle->getNumTypes()==0);
    fail_unless(pGlobalStyle->getNumRoles()==1);
    fail_unless(pGlobalStyle->isInRoleList("inhibitor")==true);
    pGroup=pGlobalStyle->getGroup();
    fail_unless(pGroup->getLevel() == level);
    fail_unless(pGroup->getVersion() == version);

    fail_unless(pGroup->getStroke()=="#000000");
    fail_unless(fabs(pGroup->getStrokeWidth()-1.0) < 1e-12);
    fail_unless(pGroup->getListOfElements()->size()==0);
    pos2=objectMap.find("modifierSpeciesReferenceGlyphStyle");
    fail_unless(pos2!=objectMap.end());
    pGlobalStyle=dynamic_cast<const GlobalStyle*>(pos2->second);
    fail_unless(pGlobalStyle!=NULL);
    fail_unless(pGlobalStyle->getNumTypes()==0);
    fail_unless(pGlobalStyle->getNumRoles()==1);
    fail_unless(pGlobalStyle->isInRoleList("modifier")==true);
    pGroup=pGlobalStyle->getGroup();
    fail_unless(pGroup->getLevel() == level);
    fail_unless(pGroup->getVersion() == version);

    fail_unless(pGroup->getStroke()=="#000000");
    fail_unless(fabs(pGroup->getStrokeWidth()-1.0) < 1e-12);
    fail_unless(pGroup->getListOfElements()->size()==0);
    objectMap.clear();

    // check the global render information called shortGrayStyle
    pos=globalRenderInformationMap.find("shortGrayStyle");
    fail_unless(pos!=globalRenderInformationMap.end());
    pGlobal=pos->second;
    fail_unless(pGlobal->isSetName());
    fail_unless(pGlobal->getName()=="modified default style to grayscale");
    fail_unless(pGlobal->getReferenceRenderInformationId()=="defaultStyle");
    fail_unless(pGlobal->getProgramName()=="Ralph Gauges");
    fail_unless(pGlobal->getProgramVersion()=="1.0");
    fail_unless(pGlobal->getListOfColorDefinitions()->size()==8);
    for(i=0;i<8;++i)
    {
        pColor=pGlobal->getColorDefinition(i);
        fail_unless(pColor!=NULL);
        fail_unless(pColor->getLevel() == level);
        fail_unless(pColor->getVersion() == version);

        fail_unless(pColor->isSetId());
        objectMap.insert(std::pair<std::string,const SBase*>(pColor->getId(),pColor));
    }
    fail_unless(objectMap.size()==8);
    // test the eight colors
    pos2=objectMap.find("white");
    fail_unless(pos2!=objectMap.end());
    pColor=dynamic_cast<const ColorDefinition*>(pos2->second);
    fail_unless(pColor!=NULL);
    fail_unless(pColor->getRed()==255);
    fail_unless(pColor->getGreen()==255);
    fail_unless(pColor->getBlue()==255);
    fail_unless(pColor->getAlpha()==255);
    pos2=objectMap.find("black");
    fail_unless(pos2!=objectMap.end());
    pColor=dynamic_cast<const ColorDefinition*>(pos2->second);
    fail_unless(pColor!=NULL);
    fail_unless(pColor->getRed()==0);
    fail_unless(pColor->getGreen()==0);
    fail_unless(pColor->getBlue()==0);
    fail_unless(pColor->getAlpha()==255);
    pos2=objectMap.find("lightBlue");
    fail_unless(pos2!=objectMap.end());
    pColor=dynamic_cast<const ColorDefinition*>(pos2->second);
    fail_unless(pColor!=NULL);
    fail_unless(pColor->getRed()==0xCE);
    fail_unless(pColor->getGreen()==0xCE);
    fail_unless(pColor->getBlue()==0xCE);
    fail_unless(pColor->getAlpha()==255);
    pos2=objectMap.find("red");
    fail_unless(pos2!=objectMap.end());
    pColor=dynamic_cast<const ColorDefinition*>(pos2->second);
    fail_unless(pColor!=NULL);
    fail_unless(pColor->getRed()==0);
    fail_unless(pColor->getGreen()==0);
    fail_unless(pColor->getBlue()==0);
    fail_unless(pColor->getAlpha()==255);
    pos2=objectMap.find("green");
    fail_unless(pos2!=objectMap.end());
    pColor=dynamic_cast<const ColorDefinition*>(pos2->second);
    fail_unless(pColor!=NULL);
    fail_unless(pColor->getRed()==0);
    fail_unless(pColor->getGreen()==0);
    fail_unless(pColor->getBlue()==0);
    fail_unless(pColor->getAlpha()==255);
    pos2=objectMap.find("blue");
    fail_unless(pos2!=objectMap.end());
    pColor=dynamic_cast<const ColorDefinition*>(pos2->second);
    fail_unless(pColor!=NULL);
    fail_unless(pColor->getRed()==0);
    fail_unless(pColor->getGreen()==0);
    fail_unless(pColor->getBlue()==0);
    fail_unless(pColor->getAlpha()==255);
    pos2=objectMap.find("lightYellow");
    fail_unless(pos2!=objectMap.end());
    pColor=dynamic_cast<const ColorDefinition*>(pos2->second);
    fail_unless(pColor!=NULL);
    fail_unless(pColor->getRed()==0xF0);
    fail_unless(pColor->getGreen()==0xF0);
    fail_unless(pColor->getBlue()==0xF0);
    fail_unless(pColor->getAlpha()==255);
    pos2=objectMap.find("darkGreen");
    fail_unless(pos2!=objectMap.end());
    pColor=dynamic_cast<const ColorDefinition*>(pos2->second);
    fail_unless(pColor!=NULL);
    fail_unless(pColor->getRed()==0x0B);
    fail_unless(pColor->getGreen()==0x0B);
    fail_unless(pColor->getBlue()==0x0B);
    fail_unless(pColor->getAlpha()==255);
    objectMap.clear();
    
    // check the global render information called defaultStyle
    pos=globalRenderInformationMap.find("defaultStyle");
    fail_unless(pos!=globalRenderInformationMap.end());
    pGlobal=pos->second;
    fail_unless(pGlobal->isSetName());
    fail_unless(pGlobal->getName()=="default style");
    fail_unless(pGlobal->getProgramName()=="Ralph Gauges");
    fail_unless(pGlobal->getProgramVersion()=="1.0");
    fail_unless(pGlobal->getListOfColorDefinitions()->size()==8);
    for(i=0;i<8;++i)
    {
        pColor=pGlobal->getColorDefinition(i);
        fail_unless(pColor!=NULL);
        fail_unless(pColor->getLevel() == level);
        fail_unless(pColor->getVersion() == version);

        fail_unless(pColor->isSetId());
        objectMap.insert(std::pair<std::string,const SBase*>(pColor->getId(),pColor));
    }
    fail_unless(objectMap.size()==8);
    // test the eight colors
    pos2=objectMap.find("white");
    fail_unless(pos2!=objectMap.end());
    pColor=dynamic_cast<const ColorDefinition*>(pos2->second);
    fail_unless(pColor!=NULL);
    fail_unless(pColor->getRed()==255);
    fail_unless(pColor->getGreen()==255);
    fail_unless(pColor->getBlue()==255);
    fail_unless(pColor->getAlpha()==255);
    pos2=objectMap.find("black");
    fail_unless(pos2!=objectMap.end());
    pColor=dynamic_cast<const ColorDefinition*>(pos2->second);
    fail_unless(pColor!=NULL);
    fail_unless(pColor->getRed()==0);
    fail_unless(pColor->getGreen()==0);
    fail_unless(pColor->getBlue()==0);
    fail_unless(pColor->getAlpha()==255);
    pos2=objectMap.find("lightBlue");
    fail_unless(pos2!=objectMap.end());
    pColor=dynamic_cast<const ColorDefinition*>(pos2->second);
    fail_unless(pColor!=NULL);
    fail_unless(pColor->getRed()==0xAD);
    fail_unless(pColor->getGreen()==0xD8);
    fail_unless(pColor->getBlue()==0xE6);
    fail_unless(pColor->getAlpha()==255);
    pos2=objectMap.find("red");
    fail_unless(pos2!=objectMap.end());
    pColor=dynamic_cast<const ColorDefinition*>(pos2->second);
    fail_unless(pColor!=NULL);
    fail_unless(pColor->getRed()==0xFF);
    fail_unless(pColor->getGreen()==0x00);
    fail_unless(pColor->getBlue()==0x00);
    fail_unless(pColor->getAlpha()==255);
    pos2=objectMap.find("green");
    fail_unless(pos2!=objectMap.end());
    pColor=dynamic_cast<const ColorDefinition*>(pos2->second);
    fail_unless(pColor!=NULL);
    fail_unless(pColor->getRed()==0x00);
    fail_unless(pColor->getGreen()==0xFF);
    fail_unless(pColor->getBlue()==0x00);
    fail_unless(pColor->getAlpha()==255);
    pos2=objectMap.find("blue");
    fail_unless(pos2!=objectMap.end());
    pColor=dynamic_cast<const ColorDefinition*>(pos2->second);
    fail_unless(pColor!=NULL);
    fail_unless(pColor->getRed()==0x00);
    fail_unless(pColor->getGreen()==0x00);
    fail_unless(pColor->getBlue()==0xFF);
    fail_unless(pColor->getAlpha()==255);
    pos2=objectMap.find("lightYellow");
    fail_unless(pos2!=objectMap.end());
    pColor=dynamic_cast<const ColorDefinition*>(pos2->second);
    fail_unless(pColor!=NULL);
    fail_unless(pColor->getRed()==0xFF);
    fail_unless(pColor->getGreen()==0xFF);
    fail_unless(pColor->getBlue()==0xD1);
    fail_unless(pColor->getAlpha()==255);
    pos2=objectMap.find("darkGreen");
    fail_unless(pos2!=objectMap.end());
    pColor=dynamic_cast<const ColorDefinition*>(pos2->second);
    fail_unless(pColor!=NULL);
    fail_unless(pColor->getRed()==0x00);
    fail_unless(pColor->getGreen()==0x20);
    fail_unless(pColor->getBlue()==0x00);
    fail_unless(pColor->getAlpha()==255);
    objectMap.clear();
    // check the gradient definition
    fail_unless(pGlobal->getListOfGradientDefinitions()->size()==1);
    pGradientBase=pGlobal->getGradientDefinition(0);
    fail_unless(pGradientBase!=NULL);
    fail_unless(pGradientBase->getLevel() == level);
    fail_unless(pGradientBase->getVersion() == version);

    fail_unless(pGradientBase->isSetId());
    fail_unless(pGradientBase->getId()=="speciesGlyphGradient");
    pRadialGradient=dynamic_cast<const RadialGradient*>(pGradientBase);
    fail_unless(pRadialGradient!=NULL);
    // all values are default
    v=pRadialGradient->getCenterX();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs((v.getRelativeValue()-50.0)/50.0) < 1e-12);
    v=pRadialGradient->getCenterY();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs((v.getRelativeValue()-50.0)/50.0) < 1e-12);
    v=pRadialGradient->getCenterZ();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs((v.getRelativeValue()-50.0)/50.0) < 1e-12);
    v=pRadialGradient->getRadius();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs((v.getRelativeValue()-50.0)/50.0) < 1e-12);
    // all F parameter must be the same as the center point
    v=pRadialGradient->getFocalPointX();
    fail_unless(v==pRadialGradient->getCenterX());
    v=pRadialGradient->getFocalPointY();
    fail_unless(v==pRadialGradient->getCenterY());
    v=pRadialGradient->getFocalPointZ();
    fail_unless(v==pRadialGradient->getCenterZ());
    fail_unless(pGradientBase->getListOfGradientStops()->size()==2);
    pGradientStop=pGradientBase->getGradientStop(0);
    fail_unless(pGradientStop!=NULL);
    fail_unless(pGradientStop->getLevel() == level);
    fail_unless(pGradientStop->getVersion() == version);

    fail_unless(pGradientStop->getStopColor()=="white");
    v=pGradientStop->getOffset();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    pGradientStop=pGradientBase->getGradientStop(1);
    fail_unless(pGradientStop!=NULL);
    fail_unless(pGradientStop->getLevel() == level);
    fail_unless(pGradientStop->getVersion() == version);

    fail_unless(pGradientStop->getStopColor()=="lightBlue");
    v=pGradientStop->getOffset();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs((v.getRelativeValue()-100.0)/100.0) < 1e-12);
    // check the line endings 
    fail_unless(pGlobal->getListOfLineEndings()->size()==4); 
    pLineEnding=NULL;
    for(i=0;i<4;++i)
    {
        pLineEnding=pGlobal->getLineEnding(i);
        fail_unless(pLineEnding!=NULL);
        fail_unless(pLineEnding->getLevel() == level);
        fail_unless(pLineEnding->getVersion() == version);

        fail_unless(pLineEnding->isSetId());
        objectMap.insert(std::pair<std::string,const SBase*>(pLineEnding->getId(),pLineEnding));
    }
    fail_unless(objectMap.size()==4);
    // lineEnding simpleHead_black
    pos2=objectMap.find("simpleHead_black");
    fail_unless(pos2!=objectMap.end());
    pLineEnding=dynamic_cast<const LineEnding*>(pos2->second);
    pBoundingBox=pLineEnding->getBoundingBox();
    fail_unless(pBoundingBox!=NULL);
    fail_unless(fabs((pBoundingBox->getPosition()->x()-(-8.0))/-8.0) < 1e-12);
    fail_unless(fabs((pBoundingBox->getPosition()->y()-(-3.0))/-3.0) < 1e-12);
    fail_unless(fabs((pBoundingBox->getDimensions()->getWidth()-10.0)/10.0) < 1e-12);
    fail_unless(fabs((pBoundingBox->getDimensions()->getHeight()-6.0)/6.0) < 1e-12);
    pGroup=pLineEnding->getGroup();
    fail_unless(pGroup!=NULL);
    fail_unless(pGroup->getLevel() == level);
    fail_unless(pGroup->getVersion() == version);

    fail_unless(pGroup->getStroke()=="black");
    fail_unless(fabs(pGroup->getStrokeWidth()-1.0) < 1e-12);
    fail_unless(pGroup->getFillColor()=="black");
    fail_unless(pGroup->getListOfElements()->size()==1);
    pPolygon=dynamic_cast<const Polygon*>(pGroup->getElement(0));
    fail_unless(pPolygon!=NULL);
    fail_unless(pPolygon->getLevel() == level);
    fail_unless(pPolygon->getVersion() == version);

    fail_unless(pPolygon->getListOfElements()->size()==3);
    pElement=pPolygon->getElement(0);
    fail_unless(pElement!=NULL);
    fail_unless(pElement->getLevel() == level);
    fail_unless(pElement->getVersion() == version);

    fail_unless(pElement->getTypeCode()==SBML_RENDER_POINT);
    v=pElement->x();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->y();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->z();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    pElement=pPolygon->getElement(1);
    fail_unless(pElement!=NULL);
    fail_unless(pElement->getLevel() == level);
    fail_unless(pElement->getVersion() == version);

    fail_unless(pElement->getTypeCode()==SBML_RENDER_POINT);
    v=pElement->x();
    fail_unless(fabs((v.getAbsoluteValue()-10.0)/10.0) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->y();
    fail_unless(fabs((v.getAbsoluteValue()-3.0)/3.0) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->z();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    pElement=pPolygon->getElement(2);
    fail_unless(pElement!=NULL);
    fail_unless(pElement->getLevel() == level);
    fail_unless(pElement->getVersion() == version);

    fail_unless(pElement->getTypeCode()==SBML_RENDER_POINT);
    v=pElement->x();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->y();
    fail_unless(fabs((v.getAbsoluteValue()-6.0)/6.0) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->z();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    // lineEnding simpleHead_red
    pos2=objectMap.find("simpleHead_red");
    fail_unless(pos2!=objectMap.end());
    pLineEnding=dynamic_cast<const LineEnding*>(pos2->second);
    pBoundingBox=pLineEnding->getBoundingBox();
    fail_unless(pBoundingBox!=NULL);
    fail_unless(fabs((pBoundingBox->getPosition()->x()-(-8.0))/-8.0) < 1e-12);
    fail_unless(fabs((pBoundingBox->getPosition()->y()-(-3.0))/-3.0) < 1e-12);
    fail_unless(fabs((pBoundingBox->getDimensions()->getWidth()-10.0)/10.0) < 1e-12);
    fail_unless(fabs((pBoundingBox->getDimensions()->getHeight()-6.0)/6.0) < 1e-12);
    pGroup=pLineEnding->getGroup();
    fail_unless(pGroup!=NULL);
    fail_unless(pGroup->getLevel() == level);
    fail_unless(pGroup->getVersion() == version);

    fail_unless(pGroup->getStroke()=="red");
    fail_unless(fabs(pGroup->getStrokeWidth()-1.0) < 1e-12);
    fail_unless(pGroup->getFillColor()=="red");
    fail_unless(pGroup->getListOfElements()->size()==1);
    pPolygon=dynamic_cast<const Polygon*>(pGroup->getElement(0));
    fail_unless(pPolygon!=NULL);
    fail_unless(pPolygon->getLevel() == level);
    fail_unless(pPolygon->getVersion() == version);

    fail_unless(pPolygon->getListOfElements()->size()==3);
    pElement=pPolygon->getElement(0);
    fail_unless(pElement!=NULL);
    fail_unless(pElement->getLevel() == level);
    fail_unless(pElement->getVersion() == version);

    fail_unless(pElement->getTypeCode()==SBML_RENDER_POINT);
    v=pElement->x();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->y();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->z();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    pElement=pPolygon->getElement(1);
    fail_unless(pElement!=NULL);
    fail_unless(pElement->getLevel() == level);
    fail_unless(pElement->getVersion() == version);

    fail_unless(pElement->getTypeCode()==SBML_RENDER_POINT);
    v=pElement->x();
    fail_unless(fabs((v.getAbsoluteValue()-10.0)/10.0) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->y();
    fail_unless(fabs((v.getAbsoluteValue()-3.0)/3.0) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->z();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    pElement=pPolygon->getElement(2);
    fail_unless(pElement!=NULL);
    fail_unless(pElement->getLevel() == level);
    fail_unless(pElement->getVersion() == version);

    fail_unless(pElement->getTypeCode()==SBML_RENDER_POINT);
    v=pElement->x();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->y();
    fail_unless(fabs((v.getAbsoluteValue()-6.0)/6.0) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->z();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    // lineEnding simpleHead_green
    pos2=objectMap.find("simpleHead_green");
    fail_unless(pos2!=objectMap.end());
    pLineEnding=dynamic_cast<const LineEnding*>(pos2->second);
    pBoundingBox=pLineEnding->getBoundingBox();
    fail_unless(pBoundingBox!=NULL);
    fail_unless(fabs((pBoundingBox->getPosition()->x()-(-8.0))/-8.0) < 1e-12);
    fail_unless(fabs((pBoundingBox->getPosition()->y()-(-3.0))/-3.0) < 1e-12);
    fail_unless(fabs((pBoundingBox->getDimensions()->getWidth()-10.0)/10.0) < 1e-12);
    fail_unless(fabs((pBoundingBox->getDimensions()->getHeight()-6.0)/6.0) < 1e-12);
    pGroup=pLineEnding->getGroup();
    fail_unless(pGroup!=NULL);
    fail_unless(pGroup->getLevel() == level);
    fail_unless(pGroup->getVersion() == version);

    fail_unless(pGroup->getStroke()=="green");
    fail_unless(fabs(pGroup->getStrokeWidth()-1.0) < 1e-12);
    fail_unless(pGroup->getFillColor()=="green");
    fail_unless(pGroup->getListOfElements()->size()==1);
    pPolygon=dynamic_cast<const Polygon*>(pGroup->getElement(0));
    fail_unless(pPolygon!=NULL);
    fail_unless(pPolygon->getLevel() == level);
    fail_unless(pPolygon->getVersion() == version);

    fail_unless(pPolygon->getListOfElements()->size()==3);
    pElement=pPolygon->getElement(0);
    fail_unless(pElement!=NULL);
    fail_unless(pElement->getLevel() == level);
    fail_unless(pElement->getVersion() == version);

    fail_unless(pElement->getTypeCode()==SBML_RENDER_POINT);
    v=pElement->x();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->y();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->z();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    pElement=pPolygon->getElement(1);
    fail_unless(pElement!=NULL);
    fail_unless(pElement->getLevel() == level);
    fail_unless(pElement->getVersion() == version);

    fail_unless(pElement->getTypeCode()==SBML_RENDER_POINT);
    v=pElement->x();
    fail_unless(fabs((v.getAbsoluteValue()-10.0)/10.0) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->y();
    fail_unless(fabs((v.getAbsoluteValue()-3.0)/3.0) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->z();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    pElement=pPolygon->getElement(2);
    fail_unless(pElement!=NULL);
    fail_unless(pElement->getLevel() == level);
    fail_unless(pElement->getVersion() == version);

    fail_unless(pElement->getTypeCode()==SBML_RENDER_POINT);
    v=pElement->x();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->y();
    fail_unless(fabs((v.getAbsoluteValue()-6.0)/6.0) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->z();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    // lineEnding simpleHead_blue
    pos2=objectMap.find("simpleHead_blue");
    fail_unless(pos2!=objectMap.end());
    pLineEnding=dynamic_cast<const LineEnding*>(pos2->second);
    pBoundingBox=pLineEnding->getBoundingBox();
    fail_unless(pBoundingBox!=NULL);
    fail_unless(fabs((pBoundingBox->getPosition()->x()-(-8.0))/-8.0) < 1e-12);
    fail_unless(fabs((pBoundingBox->getPosition()->y()-(-3.0))/-3.0) < 1e-12);
    fail_unless(fabs((pBoundingBox->getDimensions()->getWidth()-10.0)/10.0) < 1e-12);
    fail_unless(fabs((pBoundingBox->getDimensions()->getHeight()-6.0)/6.0) < 1e-12);
    pGroup=pLineEnding->getGroup();
    fail_unless(pGroup!=NULL);
    fail_unless(pGroup->getLevel() == level);
    fail_unless(pGroup->getVersion() == version);

    fail_unless(pGroup->getStroke()=="blue");
    fail_unless(fabs(pGroup->getStrokeWidth()-1.0) < 1e-12);
    fail_unless(pGroup->getFillColor()=="blue");
    fail_unless(pGroup->getListOfElements()->size()==1);
    pPolygon=dynamic_cast<const Polygon*>(pGroup->getElement(0));
    fail_unless(pPolygon!=NULL);
    fail_unless(pPolygon->getLevel() == level);
    fail_unless(pPolygon->getVersion() == version);

    fail_unless(pPolygon->getListOfElements()->size()==3);
    pElement=pPolygon->getElement(0);
    fail_unless(pElement!=NULL);
    fail_unless(pElement->getLevel() == level);
    fail_unless(pElement->getVersion() == version);

    fail_unless(pElement->getTypeCode()==SBML_RENDER_POINT);
    v=pElement->x();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->y();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->z();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    pElement=pPolygon->getElement(1);
    fail_unless(pElement!=NULL);
    fail_unless(pElement->getLevel() == level);
    fail_unless(pElement->getVersion() == version);

    fail_unless(pElement->getTypeCode()==SBML_RENDER_POINT);
    v=pElement->x();
    fail_unless(fabs((v.getAbsoluteValue()-10.0)/10.0) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->y();
    fail_unless(fabs((v.getAbsoluteValue()-3.0)/3.0) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->z();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    pElement=pPolygon->getElement(2);
    fail_unless(pElement!=NULL);
    fail_unless(pElement->getLevel() == level);
    fail_unless(pElement->getVersion() == version);

    fail_unless(pElement->getTypeCode()==SBML_RENDER_POINT);
    v=pElement->x();
    fail_unless(fabs((v.getAbsoluteValue())) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->y();
    fail_unless(fabs((v.getAbsoluteValue()-6.0)/6.0) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    v=pElement->z();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);

    objectMap.clear();
    // check the styles
    fail_unless(pGlobal->getListOfStyles()->size()==7);
    for(i=0;i<7;++i)
    {
        pGlobalStyle=dynamic_cast<const GlobalStyle*>(pGlobal->getStyle(i));
        fail_unless(pGlobalStyle!=NULL);
        fail_unless(pGlobalStyle->getLevel() == level);
        fail_unless(pGlobalStyle->getVersion() == version);

        fail_unless(pGlobalStyle->isSetId());
        objectMap.insert(std::pair<std::string,const SBase*>(pGlobalStyle->getId(),pGlobalStyle));
    }
    fail_unless(objectMap.size()==7);
    // test the three styles
    pos2=objectMap.find("compartmentGlyphStyle");
    fail_unless(pos2!=objectMap.end());
    pGlobalStyle=dynamic_cast<const GlobalStyle*>(pos2->second);
    fail_unless(pGlobalStyle!=NULL);
    fail_unless(pGlobalStyle->getNumTypes()==1);
    fail_unless(pGlobalStyle->isInTypeList("COMPARTMENTGLYPH")==true);
    pGroup=pGlobalStyle->getGroup();
    fail_unless(pGroup->getLevel() == level);
    fail_unless(pGroup->getVersion() == version);

    fail_unless(pGroup->getStroke()=="darkGreen");
    fail_unless(fabs(pGroup->getStrokeWidth()-1.0) < 1e-12);
    fail_unless(pGroup->getListOfElements()->size()==1);
    // the one element is a rectangle
    pRectangle=dynamic_cast<const Rectangle*>(pGroup->getElement(0));
    fail_unless(pRectangle!=NULL);
    fail_unless(pRectangle->getLevel() == level);
    fail_unless(pRectangle->getVersion() == version);

    fail_unless(pRectangle->getFillColor()=="lightYellow");
    v=pRectangle->getX();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs(v.getRelativeValue()) < 1e-12); 
    v=pRectangle->getY();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs(v.getRelativeValue()) < 1e-12); 
    v=pRectangle->getRadiusX();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs((v.getRelativeValue()-10.0)/10.0) < 1e-12); 
    v=pRectangle->getRadiusY();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs((v.getRelativeValue()-10.0)/10.0) < 1e-12); 
    v=pRectangle->getWidth();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs((v.getRelativeValue()-100.0)/100.0) < 1e-12); 
    v=pRectangle->getHeight();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs((v.getRelativeValue()-100.0)/100.0) < 1e-12); 
    
    pos2=objectMap.find("speciesGlyphStyle");
    fail_unless(pos2!=objectMap.end());
    pGlobalStyle=dynamic_cast<const GlobalStyle*>(pos2->second);
    fail_unless(pGlobalStyle!=NULL);
    fail_unless(pGlobalStyle->getNumTypes()==1);
    fail_unless(pGlobalStyle->isInTypeList("SPECIESGLYPH")==true);
    pGroup=pGlobalStyle->getGroup();
    fail_unless(pGroup->getLevel() == level);
    fail_unless(pGroup->getVersion() == version);

    fail_unless(pGroup->getStroke()=="black");
    fail_unless(fabs(pGroup->getStrokeWidth()-1.0) < 1e-12);
    fail_unless(pGroup->getListOfElements()->size()==1);
    // the one element is a rectangle
    pRectangle=dynamic_cast<const Rectangle*>(pGroup->getElement(0));
    fail_unless(pRectangle!=NULL);
    fail_unless(pRectangle->getLevel() == level);
    fail_unless(pRectangle->getVersion() == version);

    fail_unless(pRectangle->getFillColor()=="speciesGlyphGradient");
    v=pRectangle->getX();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs(v.getRelativeValue()) < 1e-12); 
    v=pRectangle->getY();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs(v.getRelativeValue()) < 1e-12); 
    v=pRectangle->getRadiusX();
    fail_unless(fabs((v.getAbsoluteValue()-5.0)/5.0) < 1e-12); 
    fail_unless(fabs(v.getRelativeValue()) < 1e-12); 
    v=pRectangle->getRadiusY();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs((v.getRelativeValue()-50.0)/50.0) < 1e-12); 
    v=pRectangle->getWidth();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs((v.getRelativeValue()-100.0)/100.0) < 1e-12); 
    v=pRectangle->getHeight();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs((v.getRelativeValue()-100.0)/100.0) < 1e-12); 

    pos2=objectMap.find("reactionGlyphStyle");
    fail_unless(pos2!=objectMap.end());
    pGlobalStyle=dynamic_cast<const GlobalStyle*>(pos2->second);
    fail_unless(pGlobalStyle!=NULL);
    fail_unless(pGlobalStyle->getNumTypes()==2);
    fail_unless(pGlobalStyle->isInTypeList("REACTIONGLYPH")==true);
    fail_unless(pGlobalStyle->isInTypeList("TEXTGLYPH")==true);
    pGroup=pGlobalStyle->getGroup();
    fail_unless(pGroup->getLevel() == level);
    fail_unless(pGroup->getVersion() == version);

    fail_unless(pGroup->getStroke()=="black");
    fail_unless(fabs(pGroup->getStrokeWidth()-1.0) < 1e-12);
    fail_unless(fabs((pGroup->getFontSize().getAbsoluteValue()-12.0)/12.0) < 1e-12);
    fail_unless(fabs(pGroup->getFontSize().getRelativeValue()) < 1e-12);
    fail_unless(pGroup->getTextAnchor()==Text::ANCHOR_MIDDLE);
    fail_unless(pGroup->getListOfElements()->size()==0);
    
    pos2=objectMap.find("speciesReferenceGlyphStyle");
    fail_unless(pos2!=objectMap.end());
    pGlobalStyle=dynamic_cast<const GlobalStyle*>(pos2->second);
    fail_unless(pGlobalStyle!=NULL);
    fail_unless(pGlobalStyle->getNumTypes()==0);
    fail_unless(pGlobalStyle->getNumRoles()==4);
    fail_unless(pGlobalStyle->isInRoleList("substrate")==true);
    fail_unless(pGlobalStyle->isInRoleList("sidesubstrate")==true);
    fail_unless(pGlobalStyle->isInRoleList("product")==true);
    fail_unless(pGlobalStyle->isInRoleList("sideproduct")==true);
    pGroup=pGlobalStyle->getGroup();
    fail_unless(pGroup->getLevel() == level);
    fail_unless(pGroup->getVersion() == version);

    fail_unless(pGroup->getStroke()=="#000000");
    fail_unless(pGroup->getEndHead()=="simpleHead_black");
    fail_unless(fabs(pGroup->getStrokeWidth()-1.0) < 1e-12);
    fail_unless(pGroup->getListOfElements()->size()==0);
    pos2=objectMap.find("activatorSpeciesReferenceGlyphStyle");
    fail_unless(pos2!=objectMap.end());
    pGlobalStyle=dynamic_cast<const GlobalStyle*>(pos2->second);
    fail_unless(pGlobalStyle!=NULL);
    fail_unless(pGlobalStyle->getNumTypes()==0);
    fail_unless(pGlobalStyle->getNumRoles()==1);
    fail_unless(pGlobalStyle->isInRoleList("activator")==true);
    pGroup=pGlobalStyle->getGroup();
    fail_unless(pGroup->getLevel() == level);
    fail_unless(pGroup->getVersion() == version);

    fail_unless(pGroup->getStroke()=="green");
    fail_unless(pGroup->getEndHead()=="simpleHead_green");
    fail_unless(fabs(pGroup->getStrokeWidth()-1.0) < 1e-12);
    fail_unless(pGroup->getListOfElements()->size()==0);
    pos2=objectMap.find("inhibitorSpeciesReferenceGlyphStyle");
    fail_unless(pos2!=objectMap.end());
    pGlobalStyle=dynamic_cast<const GlobalStyle*>(pos2->second);
    fail_unless(pGlobalStyle!=NULL);
    fail_unless(pGlobalStyle->getNumTypes()==0);
    fail_unless(pGlobalStyle->getNumRoles()==1);
    fail_unless(pGlobalStyle->isInRoleList("inhibitor")==true);
    pGroup=pGlobalStyle->getGroup();
    fail_unless(pGroup->getLevel() == level);
    fail_unless(pGroup->getVersion() == version);

    fail_unless(pGroup->getStroke()=="red");
    fail_unless(pGroup->getEndHead()=="simpleHead_red");
    fail_unless(fabs(pGroup->getStrokeWidth()-1.0) < 1e-12);
    fail_unless(pGroup->getListOfElements()->size()==0);
    pos2=objectMap.find("modifierSpeciesReferenceGlyphStyle");
    fail_unless(pos2!=objectMap.end());
    pGlobalStyle=dynamic_cast<const GlobalStyle*>(pos2->second);
    fail_unless(pGlobalStyle!=NULL);
    fail_unless(pGlobalStyle->getNumTypes()==0);
    fail_unless(pGlobalStyle->getNumRoles()==1);
    fail_unless(pGlobalStyle->isInRoleList("modifier")==true);
    pGroup=pGlobalStyle->getGroup();
    fail_unless(pGroup->getLevel() == level);
    fail_unless(pGroup->getVersion() == version);

    fail_unless(pGroup->getStroke()=="blue");
    fail_unless(pGroup->getEndHead()=="simpleHead_blue");
    fail_unless(fabs(pGroup->getStrokeWidth()-1.0) < 1e-12);
    fail_unless(pGroup->getListOfElements()->size()==0);
    objectMap.clear();

    // make sure the local layout is read
    RenderLayoutPlugin* rPlugin=(RenderLayoutPlugin*)pLayout->getPlugin("render");
    fail_unless(rPlugin->getListOfLocalRenderInformation()->size()==1);
    // check the local layout
    const LocalRenderInformation* pLocal=rPlugin->getRenderInformation(0);
    fail_unless(pLocal!=NULL);
    fail_unless(pLocal->getLevel() == level);
    fail_unless(pLocal->getVersion() == version);

    fail_unless(!pLocal->isSetName());
    fail_unless(pLocal->isSetId());
    fail_unless(pLocal->getId()=="highlightGlucose");
    fail_unless(pLocal->getReferenceRenderInformationId()=="defaultStyle");
    fail_unless(pLocal->getProgramName()=="Ralph Gauges");
    fail_unless(pLocal->getProgramVersion()=="1.0");
    fail_unless(pLocal->getListOfColorDefinitions()->size()==2);
    pColor=NULL;
    for(i=0;i<2;++i)
    {
        pColor=pLocal->getColorDefinition(i);
        fail_unless(pColor!=NULL);
        fail_unless(pColor->getLevel() == level);
        fail_unless(pColor->getVersion() == version);

        fail_unless(pColor->isSetId());
        objectMap.insert(std::pair<std::string,const SBase*>(pColor->getId(),pColor));
    }
    fail_unless(objectMap.size()==2);
    // test the two colors
    pos2=objectMap.find("white");
    fail_unless(pos2!=objectMap.end());
    pColor=dynamic_cast<const ColorDefinition*>(pos2->second);
    fail_unless(pColor!=NULL);
    fail_unless(pColor->getRed()==255);
    fail_unless(pColor->getGreen()==255);
    fail_unless(pColor->getBlue()==255);
    fail_unless(pColor->getAlpha()==255);
    pos2=objectMap.find("lightRed");
    fail_unless(pos2!=objectMap.end());
    pColor=dynamic_cast<const ColorDefinition*>(pos2->second);
    fail_unless(pColor!=NULL);
    fail_unless(pColor->getRed()==0xE6);
    fail_unless(pColor->getGreen()==0xAD);
    fail_unless(pColor->getBlue()==0xD8);
    fail_unless(pColor->getAlpha()==255);
    objectMap.clear();
    // check the gradient definition
    fail_unless(pLocal->getListOfGradientDefinitions()->size()==1);
    pGradientBase=pLocal->getGradientDefinition(0);
    fail_unless(pGradientBase!=NULL);
    fail_unless(pGradientBase->getLevel() == level);
    fail_unless(pGradientBase->getVersion() == version);

    fail_unless(pGradientBase->isSetId());
    fail_unless(pGradientBase->getId()=="highlightedSpeciesGlyphGradient");
    pRadialGradient=dynamic_cast<const RadialGradient*>(pGradientBase);
    fail_unless(pRadialGradient!=NULL);
    // all values are default
    v=pRadialGradient->getCenterX();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs((v.getRelativeValue()-50.0)/50.0) < 1e-12);
    v=pRadialGradient->getCenterY();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs((v.getRelativeValue()-50.0)/50.0) < 1e-12);
    v=pRadialGradient->getCenterZ();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs((v.getRelativeValue()-50.0)/50.0) < 1e-12);
    v=pRadialGradient->getRadius();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs((v.getRelativeValue()-50.0)/50.0) < 1e-12);
    // all F parameter must be the same as the center point
    v=pRadialGradient->getFocalPointX();
    fail_unless(v==pRadialGradient->getCenterX());
    v=pRadialGradient->getFocalPointY();
    fail_unless(v==pRadialGradient->getCenterY());
    v=pRadialGradient->getFocalPointZ();
    fail_unless(v==pRadialGradient->getCenterZ());
    fail_unless(pGradientBase->getListOfGradientStops()->size()==2);
    pGradientStop=pGradientBase->getGradientStop(0);
    fail_unless(pGradientStop!=NULL);
    fail_unless(pGradientStop->getLevel() == level);
    fail_unless(pGradientStop->getVersion() == version);

    fail_unless(pGradientStop->getStopColor()=="white");
    v=pGradientStop->getOffset();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs(v.getRelativeValue()) < 1e-12);
    pGradientStop=pGradientBase->getGradientStop(1);
    fail_unless(pGradientStop!=NULL);
    fail_unless(pGradientStop->getLevel() == level);
    fail_unless(pGradientStop->getVersion() == version);

    fail_unless(pGradientStop->getStopColor()=="lightRed");
    v=pGradientStop->getOffset();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12);
    fail_unless(fabs((v.getRelativeValue()-100.0)/100.0) < 1e-12);

    // check the one style
    fail_unless(pLocal->getListOfStyles()->size()==1);
    const LocalStyle* pLocalStyle=NULL;
    for(i=0;i<1;++i)
    {
        pLocalStyle=dynamic_cast<const LocalStyle*>(pLocal->getStyle(i));
        fail_unless(pLocalStyle!=NULL);
        fail_unless(pLocalStyle->getLevel() == level);
        fail_unless(pLocalStyle->getVersion() == version);

        fail_unless(pLocalStyle->isSetId());
        objectMap.insert(std::pair<std::string,const SBase*>(pLocalStyle->getId(),pLocalStyle));
    }
    fail_unless(objectMap.size()==1);
    // test the three styles
    pos2=objectMap.find("highlightedGlucose");
    fail_unless(pos2!=objectMap.end());
    pLocalStyle=dynamic_cast<const LocalStyle*>(pos2->second);
    fail_unless(pLocalStyle!=NULL);
    fail_unless(pLocalStyle->getNumTypes()==0);
    fail_unless(pLocalStyle->getNumIds()==1);
    fail_unless(pLocalStyle->isInIdList("SpeciesGlyph_Glucose")==true);
    pGroup=pLocalStyle->getGroup();
    fail_unless(pGroup->getLevel() == level);
    fail_unless(pGroup->getVersion() == version);

    fail_unless(pGroup->getStroke()=="black");
    fail_unless(fabs(pGroup->getStrokeWidth()-1.0) < 1e-12);
    fail_unless(pGroup->getListOfElements()->size()==1);
    // the one element is a rectangle
    pRectangle=dynamic_cast<const Rectangle*>(pGroup->getElement(0));
    fail_unless(pRectangle!=NULL);
    fail_unless(pRectangle->getLevel() == level);
    fail_unless(pRectangle->getVersion() == version);

    fail_unless(pRectangle->getFillColor()=="highlightedSpeciesGlyphGradient");
    v=pRectangle->getX();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs(v.getRelativeValue()) < 1e-12); 
    v=pRectangle->getY();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs(v.getRelativeValue()) < 1e-12); 
    v=pRectangle->getRadiusX();
    fail_unless(fabs((v.getAbsoluteValue()-5.0)/5.0) < 1e-12); 
    fail_unless(fabs(v.getRelativeValue()) < 1e-12); 
    v=pRectangle->getRadiusY();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs((v.getRelativeValue()-50.0)/50.0) < 1e-12); 
    v=pRectangle->getWidth();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs((v.getRelativeValue()-100.0)/100.0) < 1e-12); 
    v=pRectangle->getHeight();
    fail_unless(fabs(v.getAbsoluteValue()) < 1e-12); 
    fail_unless(fabs((v.getRelativeValue()-100.0)/100.0) < 1e-12); 

    delete pDocument;

}
END_TEST 


// checks reading L3 documents with render information in an annotation to the layout
START_TEST (test_RenderReading_read_L3_model_annotation)
{
  SBMLReader reader=SBMLReader(); 

  SBMLDocument* D = reader.readSBMLFromString(MODEL_3);

  //SBMLWriter writer;

  //writer.writeSBML(D,"example_level3.xml");
      	
  fail_unless(D!=NULL);

  fail_unless(D->getLevel() == 3);
  fail_unless(D->getVersion() == 1);

  Model* model=D->getModel();

  fail_unless(model!=NULL);

  // test compartments
  fail_unless(model->getNumCompartments()==1);

  Compartment* c=model->getCompartment(0);

  fail_unless(c!=NULL);

  fail_unless(c->isSetId());

  fail_unless(c->getId()=="Yeast");

  fail_unless(!c->isSetName());

  // test species
  fail_unless(model->getNumSpecies()==5);

  Species* species=model->getSpecies(0);

  fail_unless(species!=NULL);

  fail_unless(species->getCompartment()=="Yeast");

  fail_unless(species->isSetId());

  fail_unless(species->getId()=="Glucose");

  fail_unless(!species->isSetName());

  species=model->getSpecies(1);

  fail_unless(species!=NULL);

  fail_unless(species->getCompartment()=="Yeast");

  fail_unless(species->isSetId());

  fail_unless(species->getId()=="Glucose_6_phosphate");

  fail_unless(!species->isSetName());

  species=model->getSpecies(2);

  fail_unless(species!=NULL);

  fail_unless(species->getCompartment()=="Yeast");

  fail_unless(species->isSetId());

  fail_unless(species->getId()=="ATP");

  fail_unless(!species->isSetName());

  species=model->getSpecies(3);

  fail_unless(species!=NULL);

  fail_unless(species->getCompartment()=="Yeast");

  fail_unless(species->isSetId());

  fail_unless(species->getId()=="ADP");

  fail_unless(!species->isSetName());

  species=model->getSpecies(4);

  fail_unless(species!=NULL);

  fail_unless(species->getCompartment()=="Yeast");

  fail_unless(species->isSetId());

  fail_unless(species->getId()=="Pi");

  fail_unless(!species->isSetName());

  // test reactions
  fail_unless(model->getNumReactions()==1);

  Reaction* r=model->getReaction(0);

  fail_unless(r!=NULL);

  fail_unless(r->isSetId());

  fail_unless(r->getId()=="Hexokinase");

  fail_unless(!r->isSetName());

  fail_unless(r->getNumReactants()==2);

  SpeciesReference* sr=r->getReactant(0);

  fail_unless(sr!=NULL);

  fail_unless(sr->isSetSpecies());

  fail_unless(sr->getSpecies()=="Glucose");

  fail_unless(sr->isSetId());

  fail_unless(sr->getId()=="SpeciesReference_Glucose");

  sr=r->getReactant(1);

  fail_unless(sr!=NULL);

  fail_unless(sr->isSetSpecies());

  fail_unless(sr->getSpecies()=="ATP");

  fail_unless(sr->isSetId());

  fail_unless(sr->getId()=="SpeciesReference_ATP");
  
  fail_unless(r->getNumProducts()==2);

  sr=r->getProduct(0);

  fail_unless(sr!=NULL);

  fail_unless(sr->isSetSpecies());

  fail_unless(sr->getSpecies()=="Glucose_6_phosphate");

  fail_unless(sr->isSetId());

  fail_unless(sr->getId()=="SpeciesReference_G6P");

  sr=r->getProduct(1);

  fail_unless(sr!=NULL);

  fail_unless(sr->isSetSpecies());

  fail_unless(sr->getSpecies()=="ADP");

  fail_unless(sr->isSetId());

  fail_unless(sr->getId()=="SpeciesReference_ADP");
  
  
  fail_unless(r->getNumModifiers()==2);

  ModifierSpeciesReference* msr=r->getModifier(0);

  fail_unless(msr!=NULL);

  fail_unless(msr->isSetSpecies());

  fail_unless(msr->getSpecies()=="Glucose_6_phosphate");

  fail_unless(msr->isSetId());

  fail_unless(msr->getId()=="ModifierSpeciesReference_G6P");
  
  msr=r->getModifier(1);

  fail_unless(msr!=NULL);

  fail_unless(msr->isSetSpecies());

  fail_unless(msr->getSpecies()=="Pi");

  fail_unless(msr->isSetId());

  fail_unless(msr->getId()=="ModifierSpeciesReference_Pi");
 
  // test layout
  LayoutModelPlugin *plugin = (LayoutModelPlugin*)model->getPlugin("layout");
  fail_unless(plugin->getListOfLayouts()->size()==1);

  Layout* l=plugin->getLayout(0);

  fail_unless(l!=NULL);

  fail_unless(l->isSetId());

  fail_unless(l->getId()=="Layout_1");
  
  Dimensions* dimensions=l->getDimensions();

  fail_unless(dimensions!=NULL);  
  
  fail_unless(dimensions->getWidth()==400.0);  
  
  fail_unless(dimensions->getHeight()==230.0);  
  
  fail_unless(dimensions->getDepth()==0.0);  

  // CompartmentGlyphs
  fail_unless(l->getNumCompartmentGlyphs()==1);

  CompartmentGlyph* cg=l->getCompartmentGlyph(0);

  fail_unless(cg!=NULL);

  fail_unless(cg->isSetId());

  fail_unless(cg->getId()=="CompartmentGlyph_1");

  fail_unless(cg->isSetCompartmentId());

  fail_unless(cg->getCompartmentId()=="Yeast");
 
  BoundingBox* bb=cg->getBoundingBox();

  fail_unless(bb!=NULL);

  fail_unless(bb->isSetId());

  fail_unless(bb->getId()=="bb1");

  Point* position=bb->getPosition();

  fail_unless(position->getXOffset()==5.0);
  
  fail_unless(position->getYOffset()==5.0);
  
  fail_unless(position->getZOffset()==0.0);
  
  fail_unless(bb!=NULL);

  dimensions=bb->getDimensions();

  fail_unless(dimensions!=NULL);

  fail_unless(dimensions->getWidth()==390.0);
  
  fail_unless(dimensions->getHeight()==220.0);
  
  fail_unless(dimensions->getDepth()==0.0);


  
  
  // SpeciesGlyphs
  fail_unless(l->getNumSpeciesGlyphs()==5);

  SpeciesGlyph* sg=l->getSpeciesGlyph(0);

  fail_unless(sg!=NULL);

  fail_unless(sg->isSetId());

  fail_unless(sg->getId()=="SpeciesGlyph_Glucose");

  fail_unless(sg->isSetSpeciesId());

  fail_unless(sg->getSpeciesId()=="Glucose");
      
  bb=sg->getBoundingBox();

  fail_unless(bb!=NULL);

  fail_unless(bb->isSetId());

  fail_unless(bb->getId()=="bb2");

  position=bb->getPosition();

  fail_unless(position->getXOffset()==105.0);
  
  fail_unless(position->getYOffset()==20.0);
  
  fail_unless(position->getZOffset()==0.0);
  
  dimensions=bb->getDimensions();

  fail_unless(dimensions!=NULL);

  fail_unless(dimensions->getWidth()==130.0);
  
  fail_unless(dimensions->getHeight()==20.0);
  
  fail_unless(dimensions->getDepth()==0.0);

  sg=l->getSpeciesGlyph(1);

  fail_unless(sg!=NULL);

  fail_unless(sg->isSetId());

  fail_unless(sg->getId()=="SpeciesGlyph_G6P");

  fail_unless(sg->isSetSpeciesId());

  fail_unless(sg->getSpeciesId()=="Glucose_6_phosphate");
      
  bb=sg->getBoundingBox();

  fail_unless(bb!=NULL);

  fail_unless(bb->isSetId());

  fail_unless(bb->getId()=="bb5");

  position=bb->getPosition();

  fail_unless(position->getXOffset()==50.0);
  
  fail_unless(position->getYOffset()==190.0);
  
  fail_unless(position->getZOffset()==0.0);
  
  fail_unless(bb!=NULL);

  dimensions=bb->getDimensions();

  fail_unless(dimensions!=NULL);

  fail_unless(dimensions->getWidth()==270.0);
  
  fail_unless(dimensions->getHeight()==20.0);
  
  fail_unless(dimensions->getDepth()==0.0);

  sg=l->getSpeciesGlyph(2);

  fail_unless(sg!=NULL);

  fail_unless(sg->isSetId());

  fail_unless(sg->getId()=="SpeciesGlyph_ATP");

  fail_unless(sg->isSetSpeciesId());

  fail_unless(sg->getSpeciesId()=="ATP");
      
  bb=sg->getBoundingBox();

  fail_unless(bb->isSetId());

  fail_unless(bb->getId()=="bb3");

  fail_unless(bb!=NULL);

  position=bb->getPosition();

  fail_unless(position->getXOffset()==270.0);
  
  fail_unless(position->getYOffset()==70.0);
  
  fail_unless(position->getZOffset()==0.0);
  
  fail_unless(bb!=NULL);

  dimensions=bb->getDimensions();

  fail_unless(dimensions!=NULL);

  fail_unless(dimensions->getWidth()==80.0);
  
  fail_unless(dimensions->getHeight()==20.0);
  
  fail_unless(dimensions->getDepth()==0.0);

  sg=l->getSpeciesGlyph(3);

  fail_unless(sg!=NULL);

  fail_unless(sg->isSetId());

  fail_unless(sg->getId()=="glyph_ADP");

  fail_unless(sg->isSetSpeciesId());

  fail_unless(sg->getSpeciesId()=="ADP");
      
  bb=sg->getBoundingBox();

  fail_unless(bb!=NULL);

  fail_unless(bb->isSetId());

  fail_unless(bb->getId()=="bb4");

  position=bb->getPosition();

  fail_unless(position->getXOffset()==270.0);
  
  fail_unless(position->getYOffset()==140.0);
  
  fail_unless(position->getZOffset()==0.0);
  
  fail_unless(bb!=NULL);

  dimensions=bb->getDimensions();

  fail_unless(dimensions!=NULL);

  fail_unless(dimensions->getWidth()==80.0);
  
  fail_unless(dimensions->getHeight()==20.0);
  
  fail_unless(dimensions->getDepth()==0.0);

  sg=l->getSpeciesGlyph(4);

  fail_unless(sg!=NULL);

  fail_unless(sg->isSetId());

  fail_unless(sg->getId()=="SpeciesGlyph_Pi");

  fail_unless(sg->isSetSpeciesId());

  fail_unless(sg->getSpeciesId()=="Pi");
      
  bb=sg->getBoundingBox();

  fail_unless(bb!=NULL);

  fail_unless(bb->isSetId());

  fail_unless(bb->getId()=="bb6");

  position=bb->getPosition();

  fail_unless(position->getXOffset()==50.0);
  
  fail_unless(position->getYOffset()==100.0);
  
  fail_unless(position->getZOffset()==0.0);
  
  fail_unless(bb!=NULL);

  dimensions=bb->getDimensions();

  fail_unless(dimensions!=NULL);

  fail_unless(dimensions->getWidth()==60.0);
  
  fail_unless(dimensions->getHeight()==20.0);
  
  fail_unless(dimensions->getDepth()==0.0);

  
  // ReactionGlyphs  
  fail_unless(l->getNumReactionGlyphs()==1);

  ReactionGlyph* rg=l->getReactionGlyph(0);

  fail_unless(rg!=NULL);

  fail_unless(rg->isSetId());

  fail_unless(rg->getId()=="glyph_Hexokinase");

  fail_unless(rg->isSetReactionId());

  fail_unless(rg->getReactionId()=="Hexokinase");

  fail_unless(rg->isSetCurve());

  Curve* curve=rg->getCurve();

  fail_unless(curve!=NULL);

  fail_unless(curve->getNumCurveSegments()==1);

  LineSegment* ls=curve->getCurveSegment(0);

  fail_unless(ls!=NULL);

  fail_unless(ls->getTypeCode()==SBML_LAYOUT_LINESEGMENT);

  const Point* p=ls->getStart();

  fail_unless(p!=NULL);

  fail_unless(p->getXOffset()==170.0);
  
  fail_unless(p->getYOffset()==100.0);
  
  fail_unless(p->getZOffset()==0.0);

  p=ls->getEnd();
  
  fail_unless(p->getXOffset()==170.0);
  
  fail_unless(p->getYOffset()==130.0);
  
  fail_unless(p->getZOffset()==0.0);

  fail_unless(rg->getNumSpeciesReferenceGlyphs()==6);

  SpeciesReferenceGlyph* srg=rg->getSpeciesReferenceGlyph(0);

  fail_unless(srg!=NULL);

  fail_unless(srg->isSetId());

  fail_unless(srg->getId()=="SpeciesReferenceGlyph_Glucose");

  fail_unless(srg->isSetSpeciesReferenceId());

  fail_unless(srg->getSpeciesReferenceId()=="SpeciesReference_Glucose");

  fail_unless(srg->isSetSpeciesGlyphId());

  fail_unless(srg->getSpeciesGlyphId()=="SpeciesGlyph_Glucose");

  fail_unless(srg->getRole()==SPECIES_ROLE_SUBSTRATE);

  fail_unless(srg->isSetCurve());

  curve=srg->getCurve();

  fail_unless(curve!=NULL);

  fail_unless(curve->getNumCurveSegments()==1);

  ls=curve->getCurveSegment(0);

  fail_unless(ls!=NULL);

  fail_unless(ls->getTypeCode()==SBML_LAYOUT_LINESEGMENT);

  p=ls->getStart();

  fail_unless(p!=NULL);

  fail_unless(p->getXOffset()==170.0);
  
  fail_unless(p->getYOffset()==100.0);
  
  fail_unless(p->getZOffset()==0.0);

  p=ls->getEnd();
  
  fail_unless(p->getXOffset()==170.0);
  
  fail_unless(p->getYOffset()==50.0);
  
  fail_unless(p->getZOffset()==0.0);

  srg=rg->getSpeciesReferenceGlyph(1);

  fail_unless(srg!=NULL);

  fail_unless(srg->isSetId());

  fail_unless(srg->getId()=="SpeciesReferenceGlyph_ATP");

  fail_unless(srg->isSetSpeciesReferenceId());

  fail_unless(srg->getSpeciesReferenceId()=="SpeciesReference_ATP");

  fail_unless(srg->isSetSpeciesGlyphId());

  fail_unless(srg->getSpeciesGlyphId()=="SpeciesGlyph_ATP");

  fail_unless(srg->getRole()==SPECIES_ROLE_SIDESUBSTRATE);

  fail_unless(srg->isSetCurve());

  curve=srg->getCurve();

  fail_unless(curve!=NULL);

  fail_unless(curve->getNumCurveSegments()==1);

  ls=curve->getCurveSegment(0);

  fail_unless(ls!=NULL);

  fail_unless(ls->getTypeCode()==SBML_LAYOUT_CUBICBEZIER);

  p=ls->getStart();

  fail_unless(p!=NULL);

  fail_unless(p->getXOffset()==170.0);
  
  fail_unless(p->getYOffset()==100.0);
  
  fail_unless(p->getZOffset()==0.0);

  p=ls->getEnd();
  
  fail_unless(p->getXOffset()==260.0);
  
  fail_unless(p->getYOffset()==80.0);
  
  fail_unless(p->getZOffset()==0.0);

  const CubicBezier* cb=dynamic_cast<const CubicBezier*>(ls);

  fail_unless(cb != NULL);

  p=cb->getBasePoint1();
  
  fail_unless(p->getXOffset()==170.0);
  
  fail_unless(p->getYOffset()==80.0);
  
  fail_unless(p->getZOffset()==0.0);

  p=cb->getBasePoint2();

  fail_unless(p->getXOffset()==170.0);
  
  fail_unless(p->getYOffset()==80.0);
  
  fail_unless(p->getZOffset()==0.0);

  srg=rg->getSpeciesReferenceGlyph(2);

  fail_unless(srg!=NULL);

  fail_unless(srg->isSetId());

  fail_unless(srg->getId()=="SpeciesReferenceGlyph_G6P_1");

  fail_unless(srg->isSetSpeciesReferenceId());

  fail_unless(srg->getSpeciesReferenceId()=="SpeciesReference_G6P");

  fail_unless(srg->isSetSpeciesGlyphId());

  fail_unless(srg->getSpeciesGlyphId()=="SpeciesGlyph_G6P");

  fail_unless(srg->getRole()==SPECIES_ROLE_PRODUCT);

  fail_unless(srg->isSetCurve());

  curve=srg->getCurve();

  fail_unless(curve!=NULL);

  fail_unless(curve->getNumCurveSegments()==1);

  ls=curve->getCurveSegment(0);

  fail_unless(ls!=NULL);

  fail_unless(ls->getTypeCode()==SBML_LAYOUT_LINESEGMENT);

  p=ls->getStart();

  fail_unless(p!=NULL);

  fail_unless(p->getXOffset()==170.0);
  
  fail_unless(p->getYOffset()==130.0);
  
  fail_unless(p->getZOffset()==0.0);

  p=ls->getEnd();
  
  fail_unless(p->getXOffset()==170.0);
  
  fail_unless(p->getYOffset()==180.0);
  
  fail_unless(p->getZOffset()==0.0);

  srg=rg->getSpeciesReferenceGlyph(3);

  fail_unless(srg!=NULL);

  fail_unless(srg->isSetId());

  fail_unless(srg->getId()=="SpeciesReferenceGlyph_ADP");

  fail_unless(srg->isSetSpeciesReferenceId());

  fail_unless(srg->getSpeciesReferenceId()=="SpeciesReference_ADP");

  fail_unless(srg->isSetSpeciesGlyphId());

  fail_unless(srg->getSpeciesGlyphId()=="glyph_ADP");

  fail_unless(srg->getRole()==SPECIES_ROLE_SIDEPRODUCT);

  fail_unless(srg->isSetCurve());

  curve=srg->getCurve();

  fail_unless(curve!=NULL);

  fail_unless(curve->getNumCurveSegments()==1);

  ls=curve->getCurveSegment(0);

  fail_unless(ls!=NULL);

  fail_unless(ls->getTypeCode()==SBML_LAYOUT_CUBICBEZIER);

  p=ls->getStart();

  fail_unless(p!=NULL);

  fail_unless(p->getXOffset()==170.0);
  
  fail_unless(p->getYOffset()==130.0);
  
  fail_unless(p->getZOffset()==0.0);

  p=ls->getEnd();
  
  fail_unless(p->getXOffset()==260.0);
  
  fail_unless(p->getYOffset()==150.0);
  
  fail_unless(p->getZOffset()==0.0);

  p=dynamic_cast<CubicBezier*>(ls)->getBasePoint1();
  
  fail_unless(p->getXOffset()==170.0);
  
  fail_unless(p->getYOffset()==150.0);
  
  fail_unless(p->getZOffset()==0.0);

  p=dynamic_cast<CubicBezier*>(ls)->getBasePoint2();

  fail_unless(p->getXOffset()==170.0);
  
  fail_unless(p->getYOffset()==150.0);
  
  fail_unless(p->getZOffset()==0.0);

  srg=rg->getSpeciesReferenceGlyph(4);

  fail_unless(srg!=NULL);

  fail_unless(srg->isSetId());

  fail_unless(srg->getId()=="SpeciesReferenceGlyph_G6P_2");

  fail_unless(srg->isSetSpeciesReferenceId());

  fail_unless(srg->getSpeciesReferenceId()=="ModifierSpeciesReference_G6P");

  fail_unless(srg->isSetSpeciesGlyphId());

  fail_unless(srg->getSpeciesGlyphId()=="SpeciesGlyph_G6P");

  fail_unless(srg->getRole()==SPECIES_ROLE_INHIBITOR);

  fail_unless(srg->isSetCurve());

  curve=srg->getCurve();

  fail_unless(curve!=NULL);

  fail_unless(curve->getNumCurveSegments()==1);

  ls=curve->getCurveSegment(0);

  fail_unless(ls!=NULL);

  fail_unless(ls->getTypeCode()==SBML_LAYOUT_CUBICBEZIER);

  p=ls->getStart();

  fail_unless(p!=NULL);

  fail_unless(p->getXOffset()==45.0);
  
  fail_unless(p->getYOffset()==200.0);
  
  fail_unless(p->getZOffset()==0.0);

  p=ls->getEnd();
  
  fail_unless(p->getXOffset()==165.0);
  
  fail_unless(p->getYOffset()==120.0);
  
  fail_unless(p->getZOffset()==0.0);

  p=dynamic_cast<CubicBezier*>(ls)->getBasePoint1();
  
  fail_unless(p->getXOffset()==0.0);
  
  fail_unless(p->getYOffset()==200.0);
  
  fail_unless(p->getZOffset()==0.0);

  p=dynamic_cast<CubicBezier*>(ls)->getBasePoint2();

  fail_unless(p->getXOffset()==0.0);
  
  fail_unless(p->getYOffset()==120.0);
  
  fail_unless(p->getZOffset()==0.0);

  srg=rg->getSpeciesReferenceGlyph(5);

  fail_unless(srg!=NULL);

  fail_unless(srg->isSetId());

  fail_unless(srg->getId()=="SpeciesReferenceGlyph_PI");

  fail_unless(srg->isSetSpeciesReferenceId());

  fail_unless(srg->getSpeciesReferenceId()=="ModifierSpeciesReference_Pi");

  fail_unless(srg->isSetSpeciesGlyphId());

  fail_unless(srg->getSpeciesGlyphId()=="SpeciesGlyph_Pi");

  fail_unless(srg->getRole()==SPECIES_ROLE_ACTIVATOR);

  fail_unless(srg->isSetCurve());

  curve=srg->getCurve();

  fail_unless(curve!=NULL);

  fail_unless(curve->getNumCurveSegments()==1);

  ls=curve->getCurveSegment(0);

  fail_unless(ls!=NULL);

  fail_unless(ls->getTypeCode()==SBML_LAYOUT_CUBICBEZIER);

  p=ls->getStart();

  fail_unless(p!=NULL);

  fail_unless(p->getXOffset()==115.0);
  
  fail_unless(p->getYOffset()==110.0);
  
  fail_unless(p->getZOffset()==0.0);

  p=ls->getEnd();
  
  fail_unless(p->getXOffset()==165.0);
  
  fail_unless(p->getYOffset()==110.0);
  
  fail_unless(p->getZOffset()==0.0);

  p=dynamic_cast<CubicBezier*>(ls)->getBasePoint1();
  
  fail_unless(p->getXOffset()==140.0);
  
  fail_unless(p->getYOffset()==110.0);
  
  fail_unless(p->getZOffset()==0.0);

  p=dynamic_cast<CubicBezier*>(ls)->getBasePoint2();

  fail_unless(p->getXOffset()==140.0);
  
  fail_unless(p->getYOffset()==110.0);
  
  fail_unless(p->getZOffset()==0.0);

  // TextGlyphs  
  fail_unless(l->getNumTextGlyphs()==5);

  TextGlyph* tg=l->getTextGlyph(0);

  fail_unless(tg!=NULL);

  fail_unless(tg->isSetId());

  fail_unless(tg->getId()=="TextGlyph_Glucose");

  fail_unless(tg->isSetGraphicalObjectId());

  fail_unless(tg->getGraphicalObjectId()=="SpeciesGlyph_Glucose");

  fail_unless(tg->isSetOriginOfTextId());

  fail_unless(tg->getOriginOfTextId()=="Glucose");
  
  bb=tg->getBoundingBox();

  fail_unless(bb!=NULL);

  fail_unless(bb->isSetId());

  fail_unless(bb->getId()=="bbA");

  position=bb->getPosition();

  fail_unless(position->getXOffset()==115.0);
  
  fail_unless(position->getYOffset()==20.0);
  
  fail_unless(position->getZOffset()==0.0);
  
  fail_unless(bb!=NULL);

  dimensions=bb->getDimensions();

  fail_unless(dimensions!=NULL);

  fail_unless(dimensions->getWidth()==110.0);
  
  fail_unless(dimensions->getHeight()==20.0);
  
  fail_unless(dimensions->getDepth()==0.0);

  tg=l->getTextGlyph(1);

  fail_unless(tg!=NULL);

  fail_unless(tg->isSetId());

  fail_unless(tg->getId()=="TextGlyph_G6P");

  fail_unless(tg->isSetGraphicalObjectId());

  fail_unless(tg->getGraphicalObjectId()=="SpeciesGlyph_G6P");

  fail_unless(tg->isSetOriginOfTextId());

  fail_unless(tg->getOriginOfTextId()=="Glucose_6_phosphate");
  
  bb=tg->getBoundingBox();

  fail_unless(bb!=NULL);

  fail_unless(bb->isSetId());

  fail_unless(bb->getId()=="bbD");

  position=bb->getPosition();

  fail_unless(position->getXOffset()==60.0);
  
  fail_unless(position->getYOffset()==190.0);
  
  fail_unless(position->getZOffset()==0.0);
  
  fail_unless(bb!=NULL);

  dimensions=bb->getDimensions();

  fail_unless(dimensions!=NULL);

  fail_unless(dimensions->getWidth()==250.0);
  
  fail_unless(dimensions->getHeight()==20.0);
  
  fail_unless(dimensions->getDepth()==0.0);

  tg=l->getTextGlyph(2);

  fail_unless(tg!=NULL);

  fail_unless(tg->isSetId());

  fail_unless(tg->getId()=="TextGlyph_ATP");

  fail_unless(tg->isSetGraphicalObjectId());

  fail_unless(tg->getGraphicalObjectId()=="SpeciesGlyph_ATP");

  fail_unless(tg->isSetOriginOfTextId());

  fail_unless(tg->getOriginOfTextId()=="ATP");
  
  bb=tg->getBoundingBox();

  fail_unless(bb!=NULL);

  fail_unless(bb->isSetId());

  fail_unless(bb->getId()=="bbB");

  position=bb->getPosition();

  fail_unless(position->getXOffset()==280.0);
  
  fail_unless(position->getYOffset()==70.0);
  
  fail_unless(position->getZOffset()==0.0);
  
  fail_unless(bb!=NULL);

  dimensions=bb->getDimensions();

  fail_unless(dimensions!=NULL);

  fail_unless(dimensions->getWidth()==60.0);
  
  fail_unless(dimensions->getHeight()==20.0);
  
  fail_unless(dimensions->getDepth()==0.0);

  tg=l->getTextGlyph(3);

  fail_unless(tg!=NULL);

  fail_unless(tg->isSetId());

  fail_unless(tg->getId()=="TextGlyph_ADP");

  fail_unless(tg->isSetGraphicalObjectId());

  fail_unless(tg->getGraphicalObjectId()=="glyph_ADP");

  fail_unless(tg->isSetOriginOfTextId());

  fail_unless(tg->getOriginOfTextId()=="ADP");
  
  bb=tg->getBoundingBox();

  fail_unless(bb!=NULL);

  fail_unless(bb->isSetId());

  fail_unless(bb->getId()=="bbC");

  position=bb->getPosition();

  fail_unless(position->getXOffset()==280.0);
  
  fail_unless(position->getYOffset()==140.0);
  
  fail_unless(position->getZOffset()==0.0);
  
  fail_unless(bb!=NULL);

  dimensions=bb->getDimensions();

  fail_unless(dimensions!=NULL);

  fail_unless(dimensions->getWidth()==60.0);
  
  fail_unless(dimensions->getHeight()==20.0);
  
  fail_unless(dimensions->getDepth()==0.0);

  tg=l->getTextGlyph(4);

  fail_unless(tg!=NULL);

  fail_unless(tg->isSetId());

  fail_unless(tg->getId()=="TextGlyph_PI");

  fail_unless(tg->isSetGraphicalObjectId());

  fail_unless(tg->getGraphicalObjectId()=="SpeciesGlyph_Pi");

  fail_unless(tg->isSetOriginOfTextId());

  fail_unless(tg->getOriginOfTextId()=="Pi");
  
  bb=tg->getBoundingBox();

  fail_unless(bb!=NULL);

  fail_unless(bb->isSetId());

  fail_unless(bb->getId()=="bbE");

  position=bb->getPosition();

  fail_unless(position->getXOffset()==60.0);
  
  fail_unless(position->getYOffset()==100.0);
  
  fail_unless(position->getZOffset()==0.0);
  
  fail_unless(bb!=NULL);

  dimensions=bb->getDimensions();

  fail_unless(dimensions!=NULL);

  fail_unless(dimensions->getWidth()==40.0);
  
  fail_unless(dimensions->getHeight()==20.0);
  
  fail_unless(dimensions->getDepth()==0.0);

  // GraphicalObjects
  fail_unless(l->getNumAdditionalGraphicalObjects()==0);
 
  plugin = (LayoutModelPlugin*)D->getModel()->getPlugin("layout");
  ListOfLayouts* pListOfLayouts = plugin->getListOfLayouts();
  /*XMLNode *lolAnnotation = */ pListOfLayouts->getAnnotation();
  RenderListOfLayoutsPlugin* lolPlugin = (RenderListOfLayoutsPlugin*)pListOfLayouts->getPlugin("render");
  fail_unless(lolPlugin == NULL);

  // the only way to enable L2 annotation parsing is using enablePackageInternal, as enablePackage results 
  // in a version conflict.
  pListOfLayouts->enablePackageInternal(RenderExtension::getXmlnsL2(), "render", true);
  
  lolPlugin = (RenderListOfLayoutsPlugin*)pListOfLayouts->getPlugin("render");
  fail_unless(lolPlugin != NULL);
  if (lolPlugin == NULL) return;

  // to actually parse the annotation you'd call
  // lolPlugin->parseAnnotation();
  
  // we have to make sure there is no global or local render information
  fail_unless (lolPlugin->getListOfGlobalRenderInformation()->size() == 0);
  int i,iMax=pListOfLayouts->size();
  for(i=0;i<iMax;++i)
  {
      RenderLayoutPlugin* currentPlugin = (RenderLayoutPlugin*)plugin->getLayout(i)->getPlugin("render");
      fail_unless(currentPlugin == NULL || currentPlugin->getListOfLocalRenderInformation()->size() == 0);
  }

  delete D;

}
END_TEST



START_TEST (test_RenderReading_read_objectrole)
{
  // in much legacy code the following is all that is present ... 
  const char* test = "<?xml version='1.0' encoding='UTF-8'?><graphicalObject xmlns='http://projects.eml.org/bcb/sbml/level2' id='test' objectRole='myRole'/>";
  XMLInputStream stream(test, false);
  XMLNode node(stream);
  GraphicalObject obj(node);
  RenderGraphicalObjectPlugin *rPlugin = static_cast<RenderGraphicalObjectPlugin*>(obj.getPlugin("render"));

  fail_unless(rPlugin != NULL);
  fail_unless(rPlugin->isSetObjectRole());
  fail_unless(rPlugin->getObjectRole() == "myRole");

  // of course more correct would be to use this one
  const char* test2 = "<?xml version='1.0' encoding='UTF-8'?><graphicalObject xmlns='http://projects.eml.org/bcb/sbml/level2' xmlns:render='http://projects.eml.org/bcb/sbml/render/level2' id='test' render:objectRole='myRole1'/>";
  XMLInputStream stream2 (test2, false);
  node = XMLNode(stream2);
  GraphicalObject obj2 (node);
  rPlugin = static_cast<RenderGraphicalObjectPlugin*>(obj2.getPlugin("render"));
  fail_unless(rPlugin != NULL);
  fail_unless(rPlugin->isSetObjectRole());
  fail_unless(rPlugin->getObjectRole() == "myRole1");

  // test assignment operator
  obj = obj2;
  rPlugin = static_cast<RenderGraphicalObjectPlugin*>(obj.getPlugin("render"));

  fail_unless(rPlugin != NULL);
  fail_unless(rPlugin->isSetObjectRole());
  fail_unless(rPlugin->getObjectRole() == "myRole1");

}
END_TEST

START_TEST(test_RenderReading_read_model_with_object_role)
{
  SBMLDocument *doc = readSBMLFromString(TEST_L2_MODEL);
  fail_unless(doc->getNumErrors(LIBSBML_SEV_ERROR) == 0);
  LayoutModelPlugin* plugin = static_cast<LayoutModelPlugin*>(doc->getModel()->getPlugin("layout"));
  fail_unless(plugin != NULL);
  fail_unless(plugin->getNumLayouts() > 0);
  Layout* layout = plugin->getLayout(0);
  fail_unless(layout != NULL);
  fail_unless(layout->getNumSpeciesGlyphs() > 0);

  SpeciesGlyph *glyph = plugin->getLayout(0)->getSpeciesGlyph(0);
  fail_unless(glyph != NULL);
  
  RenderGraphicalObjectPlugin* rplugin = static_cast<RenderGraphicalObjectPlugin*>(glyph->getPlugin("render"));
  fail_unless(rplugin != NULL);
  fail_unless(rplugin->isSetObjectRole());

  // next convert the model to L3 and ensure that all is well there. 
  SBMLNamespaces nsl3v1(3, 1);
  ConversionProperties props (&nsl3v1);
  props.addOption("convert layout", true);
  int result = doc->convert(props);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);

  //std::string resultModel = writeSBMLToString(doc);

  fail_unless(doc->isSetPackageRequired("layout") && doc->getPackageRequired("layout") == false);
  fail_unless(doc->isSetPackageRequired("render") && doc->getPackageRequired("render") == false);

  plugin = static_cast<LayoutModelPlugin*>(doc->getModel()->getPlugin("layout"));
  fail_unless(plugin != NULL);
  fail_unless(plugin->getNumLayouts() > 0);

  layout = plugin->getLayout(0);
  fail_unless(layout != NULL);
  fail_unless(layout->getNumSpeciesGlyphs() > 0);

  fail_unless(plugin->getLayout(0)->getNumSpeciesGlyphs() > 0);

  glyph = plugin->getLayout(0)->getSpeciesGlyph(0);
  fail_unless(glyph != NULL);
  
  rplugin = static_cast<RenderGraphicalObjectPlugin*>(glyph->getPlugin("render"));
  fail_unless(rplugin != NULL);
  fail_unless(rplugin->isSetObjectRole());

  // ensure we have no render annotation on the layout (where the render extension would have been)
  /*XMLNode *node = */ plugin->getLayout(0)->getAnnotation();
  fail_unless(plugin->getLayout(0)->isSetAnnotation() == true);

  // convert back to L2 just to be sure

  SBMLNamespaces nsl2v4(2, 4);
  props.setTargetNamespaces(&nsl2v4);
  props.addOption("convert layout", true);
  result = doc->convert(props);
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);

  //resultModel = writeSBMLToString(doc);

  plugin = static_cast<LayoutModelPlugin*>(doc->getModel()->getPlugin("layout"));
  fail_unless(plugin != NULL);
  fail_unless(plugin->getNumLayouts() > 0);

  layout = plugin->getLayout(0);

  fail_unless(layout->getNumSpeciesGlyphs() > 0);
  
  fail_unless(layout->getNumSpeciesGlyphs() > 0);

  fail_unless(plugin->getLayout(0)->getNumSpeciesGlyphs() > 0);

  glyph = plugin->getLayout(0)->getSpeciesGlyph(0);
  fail_unless(glyph != NULL);
  
  rplugin = static_cast<RenderGraphicalObjectPlugin*>(glyph->getPlugin("render"));
  fail_unless(rplugin != NULL);
  fail_unless(rplugin->isSetObjectRole());

  delete doc;

}
END_TEST

START_TEST(test_RenderReading_read_L2_model_annotation_with_polygon_curve)
{
  string filename(TestDataDirectory);
  filename += "simplenode.xml";
  // read document
  SBMLDocument* doc = readSBMLFromFile(filename.c_str());
  Model* model = doc->getModel();
  fail_unless(model != NULL);

  LayoutModelPlugin* mPlug = dynamic_cast<LayoutModelPlugin*>(model->getPlugin("layout"));
  fail_unless(mPlug != NULL);

  fail_unless(mPlug->getNumLayouts() > 0);

  Layout* layout = mPlug->getLayout(0);

  RenderLayoutPlugin* rPlug = dynamic_cast<RenderLayoutPlugin*>(layout->getPlugin("render"));
  fail_unless(rPlug != NULL);
  fail_unless(rPlug->getNumLocalRenderInformationObjects() > 0);

  LocalRenderInformation* info = rPlug->getRenderInformation(0);
  fail_unless(info != NULL);
  fail_unless(info->getNumStyles() > 0);
  LocalStyle* style = info->getStyle(0);
  fail_unless(style != NULL);
  fail_unless(style->getGroup() != NULL);
  fail_unless(style->getGroup()->getNumElements() > 0);

  Polygon* poly = dynamic_cast<Polygon*> (style->getGroup()->getElement(0));
  fail_unless(poly != NULL);
  fail_unless(poly->getNumElements() > 0);

 // std::string representation = poly->toSBML();
  RenderPoint* last = poly->getElement(poly->getNumElements() - 1);
  fail_unless(last != NULL);

  fail_unless(dynamic_cast<RenderCubicBezier*>(last) != NULL);

  delete doc;

}
END_TEST


Suite *
create_suite_RenderReading (void)
{
  Suite *suite = suite_create("RenderReading");
  TCase *tcase = tcase_create("RenderReading");


  tcase_add_checked_fixture( tcase,
                             RenderReading_setup,
                             RenderReading_teardown );

  tcase_add_test( tcase, test_RenderReading_read_objectrole             );
  tcase_add_test( tcase, test_RenderReading_read_model_with_object_role );
  tcase_add_test( tcase, test_RenderReading_read_model_1                );
  tcase_add_test( tcase, test_RenderReading_read_L3_model_1             );
  tcase_add_test( tcase, test_RenderReading_read_L3_model_annotation    );
  tcase_add_test( tcase, test_RenderReading_read_L2_model_annotation_with_polygon_curve);

  suite_add_tcase(suite, tcase);

  return suite;
}



END_C_DECLS
