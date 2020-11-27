/**
 * Filename    : render-package.i
 * Description : render swig file for bindings.
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
 *
 * Copyright (C) 2019 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
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
 * ---------------------------------------------------------------------- -->
 */

#ifdef USE_RENDER

%newobject removeLocalRenderInformation;
%newobject removeGlobalRenderInformation;
%newobject removeGlobalStyle;
%newobject removeStyle;
%newobject removeGradientStop;
%newobject removeLocalStyle;
%newobject removeElement;
%newobject removeColorDefinition;
%newobject removeGradientDefinition;
%newobject removeLineEnding;

%template(RenderPkgNamespaces) SBMLExtensionNamespaces<RenderExtension>;

%ignore Transformation2D::readAttributes;
%ignore GraphicalPrimitive1D::getDashArray;
%ignore GraphicalPrimitive1D::setDashArray;

%ignore operator<<;
%ignore operator+;
%ignore operator/;

%include <sbml/packages/render/extension/RenderExtension.h>
%include <sbml/packages/render/extension/RenderListOfLayoutsPlugin.h>
%include <sbml/packages/render/extension/RenderLayoutPlugin.h>
%include <sbml/packages/render/extension/RenderGraphicalObjectPlugin.h>

%include <sbml/packages/render/sbml/Transformation.h>
%include <sbml/packages/render/sbml/Transformation2D.h>
%include <sbml/packages/render/sbml/GraphicalPrimitive1D.h>
%include <sbml/packages/render/sbml/GraphicalPrimitive2D.h>
%include <sbml/packages/render/sbml/RenderInformationBase.h>
%include <sbml/packages/render/sbml/GradientBase.h>
%include <sbml/packages/render/sbml/Style.h>

%include <sbml/packages/render/sbml/ColorDefinition.h>
%include <sbml/packages/render/sbml/RelAbsVector.h>
%include <sbml/packages/render/sbml/Ellipse.h>
%include <sbml/packages/render/sbml/GlobalRenderInformation.h>
%include <sbml/packages/render/sbml/GlobalStyle.h>
%include <sbml/packages/render/sbml/GradientStop.h>
%include <sbml/packages/render/sbml/Image.h>
%include <sbml/packages/render/sbml/Text.h>
%include <sbml/packages/render/sbml/Rectangle.h>
%include <sbml/packages/render/sbml/RenderPoint.h>
%include <sbml/packages/render/sbml/RenderCubicBezier.h>
%include <sbml/packages/render/sbml/RenderCurve.h>
%include <sbml/packages/render/sbml/Polygon.h>
%include <sbml/packages/render/sbml/RenderGroup.h>
%include <sbml/packages/render/sbml/LinearGradient.h>
%include <sbml/packages/render/sbml/LineEnding.h>
%include <sbml/packages/render/sbml/LocalRenderInformation.h>
%include <sbml/packages/render/sbml/LocalStyle.h>
%include <sbml/packages/render/sbml/RadialGradient.h>
%include <sbml/packages/render/sbml/DefaultValues.h>

%include <sbml/packages/render/sbml/ListOfColorDefinitions.h>
%include <sbml/packages/render/sbml/ListOfCurveElements.h>
%include <sbml/packages/render/sbml/ListOfDrawables.h>
%include <sbml/packages/render/sbml/ListOfGlobalRenderInformation.h>
%include <sbml/packages/render/sbml/ListOfGlobalStyles.h>
%include <sbml/packages/render/sbml/ListOfGradientDefinitions.h>
%include <sbml/packages/render/sbml/ListOfGradientStops.h>
%include <sbml/packages/render/sbml/ListOfLineEndings.h>
%include <sbml/packages/render/sbml/ListOfLocalRenderInformation.h>
%include <sbml/packages/render/sbml/ListOfLocalStyles.h>

%include std_deque.i
%include std_set.i
%include std_string.i

%template(StringSet) std::set<std::string>;


%include carrays.i
%array_class(double,double_array)
%array_class(int,int_array)

#endif /* USE_RENDER */