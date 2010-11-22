% toolbox\Validate_MATLAB_SBML_Structures
% 
% [y, message] = isSBML_XXX(SBMLStructure, Level, Version (optional))
% Functions that return 
%              1)   y = 1 if SBMLStructure is correct
%                   y = 0 if SBMLStructure is NOT a structure
%                    OR     does not contain the appropriate fields to be of
%                           type XXX and Level and Version of SBML
%                    OR     does not have the correct Typecode string
%              2)  a message indicating where the invalidity appears
%
% Functions include
%
% isSBML_Model(SBMLStructure) 
%   identifies the level from the model structure
%
% isSBML_AlgebraicRule(SBMLStructure,Level, (optional) Version)
% isSBML_AssignmentRule(SBMLStructure,Level, (optional) Version)
% isSBML_Compartment(SBMLStructure,Level, (optional) Version) 
% isSBML_CompartmentVolumeRule(SBMLStructure,Level, (optional) Version)
% isSBML_Event(SBMLStructure,Level, (optional) Version) 
% isSBML_EventAssignment(SBMLStructure,Level, (optional) Version) 
% isSBML_FunctionDefinition(SBMLStructure,Level, (optional) Version) 
% isSBML_KineticLaw(SBMLStructure,Level, (optional) Version) 
% isSBML_ModifierSpeciesReference(SBMLStructure,Level, (optional) Version) 
% isSBML_Parameter(SBMLStructure,Level, (optional) Version) 
% isSBML_ParameterRule(SBMLStructure,Level, (optional) Version)
% isSBML_Reaction(SBMLStructure,Level, (optional) Version) 
% isSBML_Rule(SBMLStructure,Level, (optional) Version) 
% isSBML_Species(SBMLStructure,Level, (optional) Version) 
% isSBML_SpeciesConcentrationRule(SBMLStructure,Level, (optional) Version) 
% isSBML_SpeciesReference(SBMLStructure,Level, (optional) Version) 
% isSBML_Unit(SBMLStructure,Level, (optional) Version) 
% isSBML_UnitDefinition(SBMLStructure,Level, (optional) Version) 

%  Filename    :   Contents.m
%  Description :
%  Author(s)   :   SBML Development Group <sbml-team@caltech.edu>
%  $Id: Contents.m 10829 2010-01-26 08:59:42Z sarahkeating $
%  $Source v $
%
%<!---------------------------------------------------------------------------
% This file is part of SBMLToolbox.  Please visit http://sbml.org for more
% information about SBML, and the latest version of SBMLToolbox.
%
% Copyright 2005-2007 California Institute of Technology.
% Copyright 2002-2005 California Institute of Technology and
%                     Japan Science and Technology Corporation.
% 
% This library is free software; you can redistribute it and/or modify it
% under the terms of the GNU Lesser General Public License as published by
% the Free Software Foundation.  A copy of the license agreement is provided
% in the file named "LICENSE.txt" included with this software distribution.
% and also available online as http://sbml.org/software/sbmltoolbox/license.html
%----------------------------------------------------------------------- -->

