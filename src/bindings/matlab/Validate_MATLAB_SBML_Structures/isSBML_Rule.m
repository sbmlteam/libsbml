function [y, message] = isSBML_Rule(varargin)
% isSBML_Rule(SBMLStructure, Level, Version(optional)) 
% checks that SBMLStructure represents a rule 
% within an sbml model of the specified level
% 
% if SBMLStructure represents a rule within an SBML model
% it has the appropriate fields 
% eg    Typecode
%       Metaid (L2V1)
%       Notes
%       Annotations
%       SBOTerm (L2V2)
%       Type (L1V1 - L1V2)
%       Formula
%       Variable
%       Species
%       Compartment
%       ParameterName
%       ParameterUnits
%
% NOTE: content of brackets indicates the level and version of sbml from which the given field
% is appropriate.
%
% Returns 1 if SBMLStructure is a structure containing each of the above
% fields (appropriate with the given level and version) 
% and the typecode is one of
%   "SBML_ALGEBRAIC_RULE", "SBML_SPECIES_CONCENTRATION_RULE",
%   "SBML_COMPARTMENT_VOLUME_RULE", "SBML_PARAMETER_RULE", 
%   "SBML_ASSIGNMENT_RULE", "SBML_RATE_RULE"
% 
% Returns 0 if SBMLStructure is not a structure 
% or does not contain one of the appropriate fields
% or the typecode is not one of 
%   "SBML_ALGEBRAIC_RULE", "SBML_SPECIES_CONCENTRATION_RULE",
%   "SBML_COMPARTMENT_VOLUME_RULE", "SBML_PARAMETER_RULE", 
%   "SBML_ASSIGNMENT_RULE", "SBML_RATE_RULE"
%
% Returns message indicating the structure that is invalid.

%  Filename    :   isSBML_Rule.m
%  Description :
%  Author(s)   :   SBML Development Group <sbml-team@caltech.edu>
%  $Id: isSBML_Rule.m 11823 2010-09-03 11:24:42Z sarahkeating $
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

if (nargin < 2 || nargin > 3)
    error('wrong number of input arguments');
end;

message = '';

SBMLStructure = varargin{1};
Level = varargin{2};

if (nargin == 3)
    Version = varargin{3};
else
    Version = 1;
end;

if (Level == 1) 
    SBMLfieldnames = {'typecode', 'notes', 'annotation', 'type', 'formula', ...
      'variable', 'species', 'compartment', 'name', 'units'};
    nNumberFields = 10;
elseif (Level == 2)
    if (Version == 1)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation','formula', ...
          'variable', 'species', 'compartment', 'name', 'units'};
        nNumberFields = 10;
    elseif (Version == 2)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', ...
          'formula', 'variable', 'species', 'compartment', 'name', 'units'};
        nNumberFields = 11;
    elseif (Version == 3)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', ...
          'formula', 'variable', 'species', 'compartment', 'name', 'units'};
        nNumberFields = 11;
    elseif (Version == 4)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', ...
          'formula', 'variable', 'species', 'compartment', 'name', 'units'};
        nNumberFields = 11;
    end;
elseif (Level == 3)
    if (Version == 1)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', ...
          'formula', 'variable', 'species', 'compartment', 'name', 'units'};
        nNumberFields = 11;
    end;
end;
typecodel1 = {'SBML_ALGEBRAIC_RULE', 'SBML_SPECIES_CONCENTRATION_RULE', ...
  'SBML_COMPARTMENT_VOLUME_RULE', 'SBML_PARAMETER_RULE'};
nNumberTypecodesl1 = 4;

typecodel2 = {'SBML_ALGEBRAIC_RULE', 'SBML_ASSIGNMENT_RULE', 'SBML_RATE_RULE'};
nNumberTypecodesl2 = 3;

bSBML = 0;

% check that Rule is a structure
bSBML = isstruct(SBMLStructure);

% check it contains each of the fields listed
index = 1;
while (bSBML == 1 && index <= nNumberFields)
    bSBML = isfield(SBMLStructure, char(SBMLfieldnames(index)));
    index = index + 1;
end;

% check that it contains only the fields listed
if (bSBML == 1)
    names = fieldnames(SBMLStructure);
    [m,n] = size(names);
    if (m ~= nNumberFields)
        bSBML = 0;
    end;
end;

% check that the typecode is correct
index = 1;
nMatch = 0;
if (bSBML == 1)
type = SBMLStructure.typecode;
  if (Level == 1)
    while (index <= nNumberTypecodesl1)
        k = strcmp(type, typecodel1(index));
        if (k == 1)
            nMatch = nMatch + 1;
        end;
        index = index + 1;
    end;
    if (nMatch == 0)
        bSBML = 0;
    end;
  else
    while (index <= nNumberTypecodesl2)
        k = strcmp(type, typecodel2(index));
        if (k == 1)
            nMatch = nMatch + 1;
        end;
        index = index + 1;
    end;
    if (nMatch == 0)
        bSBML = 0;
    end;
  end;
end;
    
if (bSBML == 0)
  message = 'Invalid Rule structure';
end;

y = bSBML;
