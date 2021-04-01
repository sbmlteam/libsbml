/**
 * Filename    : fbc-package.i
 * Description : fbc swig file for bindings.
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

#ifdef USE_FBC

%newobject removeAssociation;
%newobject removeFbcAssociation;
%newobject removeFluxObjective;
%newobject removeObjective;
%newobject removeGeneProduct;
%newobject removeFluxBound;
%newobject removeGeneAssociation;
%newobject removeUserDefinedConstraintComponent;
%newobject removeUserDefinedConstraint;
%newobject removeKeyValuePair;


%template (FbcPkgNamespaces) SBMLExtensionNamespaces<FbcExtension>;

%include <sbml/packages/fbc/extension/FbcExtension.h>
%include <sbml/packages/fbc/extension/FbcSBasePlugin.h>
%include <sbml/packages/fbc/extension/FbcModelPlugin.h>
%include <sbml/packages/fbc/extension/FbcSpeciesPlugin.h>
%include <sbml/packages/fbc/extension/FbcSBMLDocumentPlugin.h>


%include <sbml/packages/fbc/util/CobraToFbcConverter.h>
%include <sbml/packages/fbc/util/FbcToCobraConverter.h>
%include <sbml/packages/fbc/util/FbcV1ToV2Converter.h>
%include <sbml/packages/fbc/util/FbcV2ToV1Converter.h>

%include <sbml/packages/fbc/sbml/Association.h>
%include <sbml/packages/fbc/sbml/FluxBound.h>
%include <sbml/packages/fbc/sbml/FluxObjective.h>
%include <sbml/packages/fbc/sbml/GeneAssociation.h>
%include <sbml/packages/fbc/sbml/Objective.h>

%include <sbml/packages/fbc/validator/FbcSBMLError.h>

%include <sbml/packages/fbc/extension/FbcReactionPlugin.h>
%include <sbml/packages/fbc/sbml/FbcAssociation.h>
%include <sbml/packages/fbc/sbml/FluxObjective.h>
%include <sbml/packages/fbc/sbml/GeneProductAssociation.h>
%include <sbml/packages/fbc/sbml/GeneProduct.h>
%include <sbml/packages/fbc/sbml/GeneProductRef.h>
%include <sbml/packages/fbc/sbml/FbcAnd.h>
%include <sbml/packages/fbc/sbml/FbcOr.h>
%include <sbml/packages/fbc/sbml/UserDefinedConstraintComponent.h>
%include <sbml/packages/fbc/sbml/UserDefinedConstraint.h>
%include <sbml/packages/fbc/sbml/KeyValuePair.h>
%include <sbml/packages/fbc/sbml/ListOfUserDefinedConstraintComponents.h>
%include <sbml/packages/fbc/sbml/ListOfUserDefinedConstraints.h>
%include <sbml/packages/fbc/sbml/ListOfKeyValuePairs.h>

#endif /* USE_FBC */

