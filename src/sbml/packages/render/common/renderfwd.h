/**
 * @file renderfwd.h
 * @brief Definition of renderfwd.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
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
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 * 3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 * Pasadena, CA, USA
 *
 * Copyright (C) 2002-2005 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. Japan Science and Technology Agency, Japan
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation. A copy of the license agreement is provided in the
 * file named "LICENSE.txt" included with this software distribution and also
 * available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 */


#ifndef renderfwd_H__
#define renderfwd_H__


/**
 * Forward declaration of all opaque C types.
 *
 * Declaring all types up-front avoids "redefinition of type Foo" compile
 * errors and allows our combined C/C++ headers to depend minimally upon each
 * other. Put another way, the type definitions below serve the same purpose as
 * "class Foo;" forward declarations in C++ code.
 */

#ifdef __cplusplus
# define CLASS_OR_STRUCT class
#else
# define CLASS_OR_STRUCT struct
#endif /* __cplusplus */


LIBSBML_CPP_NAMESPACE_BEGIN


typedef CLASS_OR_STRUCT ColorDefinition             ColorDefinition_t;
typedef CLASS_OR_STRUCT Ellipse                     Ellipse_t;
typedef CLASS_OR_STRUCT GlobalRenderInformation     GlobalRenderInformation_t;
typedef CLASS_OR_STRUCT GlobalStyle                 GlobalStyle_t;
typedef CLASS_OR_STRUCT GradientBase                GradientBase_t;
typedef CLASS_OR_STRUCT GradientStop                GradientStop_t;
typedef CLASS_OR_STRUCT RenderGroup                 RenderGroup_t;
typedef CLASS_OR_STRUCT Image                       Image_t;
typedef CLASS_OR_STRUCT LineEnding                  LineEnding_t;
typedef CLASS_OR_STRUCT LinearGradient              LinearGradient_t;
typedef CLASS_OR_STRUCT LocalRenderInformation      LocalRenderInformation_t;
typedef CLASS_OR_STRUCT LocalStyle                  LocalStyle_t;
typedef CLASS_OR_STRUCT Polygon                     Polygon_t;
typedef CLASS_OR_STRUCT RadialGradient              RadialGradient_t;
typedef CLASS_OR_STRUCT Rectangle                   Rectangle_t;
typedef CLASS_OR_STRUCT RelAbsVector                RelAbsVector_t;
typedef CLASS_OR_STRUCT RenderCubicBezier           RenderCubicBezier_t;
typedef CLASS_OR_STRUCT RenderCurve                 RenderCurve_t;
typedef CLASS_OR_STRUCT RenderPoint                 RenderPoint_t;
typedef CLASS_OR_STRUCT Text                        Text_t;
typedef CLASS_OR_STRUCT Transformation2D            Transformation2D_t;
typedef CLASS_OR_STRUCT Transformation              Transformation_t;
typedef CLASS_OR_STRUCT GraphicalPrimitive1D        GraphicalPrimitive1D_t;
typedef CLASS_OR_STRUCT GraphicalPrimitive2D        GraphicalPrimitive2D_t;
typedef CLASS_OR_STRUCT Style                       Style_t;
typedef CLASS_OR_STRUCT RenderInformationBase       RenderInformationBase_t;
typedef CLASS_OR_STRUCT DefaultValues               DefaultValues_t;
typedef CLASS_OR_STRUCT RenderGraphicalObjectPlugin RenderGraphicalObjectPlugin_t;
typedef CLASS_OR_STRUCT RenderLayoutPlugin          RenderLayoutPlugin_t;
typedef CLASS_OR_STRUCT RenderListOfLayoutsPlugin   RenderListOfLayoutsPlugin_t;


LIBSBML_CPP_NAMESPACE_END


#undef CLASS_OR_STRUCT


#endif /* !renderfwd_H__ */


