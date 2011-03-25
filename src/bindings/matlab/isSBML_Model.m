function [y, message] = isSBML_Model(SBMLStructure)
% isSBML_Model(SBMLStructure) checks that SBMLStructure matches the expected 
% form of a MATLAB_SBML structure
% 
% Returns y = 1 if SBMLStructure is a structure containing each of the
% fields (appropriate with the given level and version) and the typecode is "SBML_MODEL"
% 
% Returns y = 0 if SBMLStructure is not a structure 
% or does not contain one of the appropriate fields
% or the typecode is not "SBML_MODEL"
%
% Returns 'message' a string detailing why the structure is invalid.

%  Filename    :   isSBML_Model.m
%  Description :
%  Author(s)   :   SBML Development Group <sbml-team@caltech.edu>
%  $Id$
%  $HeadURL$
%
% This file is part of libSBML.  Please visit http://sbml.org for more
% information about SBML, and the latest version of libSBML.
%
% Copyright (C) 2009-2011 jointly by the following organizations: 
%     1. California Institute of Technology, Pasadena, CA, USA
%     2. EMBL European Bioinformatics Institute (EBML-EBI), Hinxton, UK
%  
% Copyright (C) 2006-2008 by the California Institute of Technology,
%     Pasadena, CA, USA 
%  
% Copyright (C) 2002-2005 jointly by the following organizations: 
%     1. California Institute of Technology, Pasadena, CA, USA
%     2. Japan Science and Technology Agency, Japan
% 
% This library is free software; you can redistribute it and/or modify it
% under the terms of the GNU Lesser General Public License as published by
% the Free Software Foundation.  A copy of the license agreement is provided
% in the file named "LICENSE.txt" included with this software distribution
% and also available online as http://sbml.org/software/libsbml/license.html

y = isfield(SBMLStructure, 'SBML_level');
if (y == 0)
    return;
end;
message = '';
% get level
Level = SBMLStructure.SBML_level;
if (Level < 1 || Level > 3)
    y = 0;
    message = 'Invalid SBML level';
    return;
end;

% get version
Version = SBMLStructure.SBML_version;
if (Version < 1 || Version > 4)
    y = 0;
    message = 'Invalid SBML version';
    return;
end;


if (Level == 1)
    SBMLfieldnames = {'typecode', 'notes', 'annotation', 'SBML_level', 'SBML_version', 'name', 'unitDefinition',...
        'compartment', 'species', 'parameter', 'rule', 'reaction'};
    nNumberFields = 12;
elseif (Level == 2)
    if (Version == 1)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'SBML_level', 'SBML_version', 'name', 'id', ...
            'functionDefinition', 'unitDefinition', 'compartment', 'species', 'parameter', 'rule', 'reaction',...
            'event'};
        nNumberFields = 16;
    elseif (Version == 2)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'SBML_level', 'SBML_version', 'name', 'id', ...
            'sboTerm', 'functionDefinition', 'unitDefinition', 'compartmentType', 'speciesType', 'compartment', ...
            'species', 'parameter', 'initialAssignment', 'rule', 'constraint', 'reaction', 'event'};
        nNumberFields = 21;
    elseif (Version == 3)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'SBML_level', 'SBML_version', 'name', 'id', ...
            'sboTerm', 'functionDefinition', 'unitDefinition', 'compartmentType', 'speciesType', 'compartment', ...
            'species', 'parameter', 'initialAssignment', 'rule', 'constraint', 'reaction', 'event'};
        nNumberFields = 21;
    elseif (Version == 4)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'SBML_level', 'SBML_version', 'name', 'id', ...
            'sboTerm', 'functionDefinition', 'unitDefinition', 'compartmentType', 'speciesType', 'compartment', ...
            'species', 'parameter', 'initialAssignment', 'rule', 'constraint', 'reaction', 'event'};
        nNumberFields = 21;
    end;
elseif (Level == 3)
    if (Version == 1)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'SBML_level', 'SBML_version', 'name', 'id', ...
            'sboTerm', 'functionDefinition', 'unitDefinition', 'compartment', ...
            'species', 'parameter', 'initialAssignment', 'rule', 'constraint', 'reaction', 'event', ...
            'substanceUnits', 'timeUnits', 'lengthUnits', 'areaUnits', 'volumeUnits', 'extentUnits', 'conversionFactor'};
        nNumberFields = 26;
    end;
end;
typecode = 'SBML_MODEL';

bSBML = 0;

% check that Model is a structure
bSBML = isstruct(SBMLStructure);

% check it contains each of the fields listed
index = 1;
while (bSBML == 1 && index <= nNumberFields)
    bSBML = isfield(SBMLStructure, char(SBMLfieldnames(index)));
    index = index + 1;
end;

% check that it contains only the fields listed
% take out this requirement as a user may wish to 
% add their own fields
% if (bSBML == 1)
%     names = fieldnames(SBMLStructure);
%     [m,n] = size(names);
%     if (m ~= nNumberFields)
%         bSBML = 0;
%     end;
% end;
% 
% check that the typecode is correct
if (bSBML == 1 && length(SBMLStructure) == 1)
    type = SBMLStructure.typecode;
    k = strcmp(type, typecode);
    if (k ~= 1)
        bSBML = 0;
    end;
end;
  
% check that any nested structures are appropriate
if (bSBML == 1)
    if (Level > 1)
        index = 1;
        [bSBML, message] = isSBML_FunctionDefinition(SBMLStructure.functionDefinition, Level, Version);
        [x, nNumber] = size(SBMLStructure.functionDefinition);
        while (bSBML == 1 && index <= nNumber)
            [bSBML, message] = isSBML_FunctionDefinition(SBMLStructure.functionDefinition(index), Level, Version);
            index = index + 1;
        end;
    end;

    index = 1;
    [bSBML, message] = isSBML_UnitDefinition(SBMLStructure.unitDefinition, Level, Version);
    [x, nNumber] = size(SBMLStructure.unitDefinition);
    while (bSBML == 1 && index <= nNumber)
        [bSBML, message] = isSBML_UnitDefinition(SBMLStructure.unitDefinition(index), Level, Version);
        index = index + 1;
    end;

    index = 1;
    [bSBML, message] = isSBML_Compartment(SBMLStructure.compartment, Level, Version);
    [x, nNumber] = size(SBMLStructure.compartment);
    while (bSBML == 1 && index <= nNumber)
        [bSBML, message] = isSBML_Compartment(SBMLStructure.compartment(index), Level, Version);
        index = index + 1;
    end;

    index = 1;
    [bSBML, message] = isSBML_Species(SBMLStructure.species, Level, Version);
    [x, nNumber] = size(SBMLStructure.species);
    while (bSBML == 1 && index <= nNumber)
        [bSBML, message] = isSBML_Species(SBMLStructure.species(index), Level, Version);
        index = index + 1;
    end;

    index = 1;
    [bSBML, message] = isSBML_Parameter(SBMLStructure.parameter, Level, Version);
    [x, nNumber] = size(SBMLStructure.parameter);
    while (bSBML == 1 && index <= nNumber)
        [bSBML, message] = isSBML_Parameter(SBMLStructure.parameter(index), Level, Version);
        index = index + 1;
    end;

    index = 1;
    [bSBML, message] = isSBML_Rule(SBMLStructure.rule, Level, Version);
    [x, nNumber] = size(SBMLStructure.rule);
    while (bSBML == 1 && index <= nNumber)
        [bSBML, message] = isSBML_Rule(SBMLStructure.rule(index), Level, Version);
        index = index + 1;
    end;

    index = 1;
    [bSBML, message] = isSBML_Reaction(SBMLStructure.reaction, Level, Version);
    [x, nNumber] = size(SBMLStructure.reaction);
    while (bSBML == 1 && index <= nNumber)
        [bSBML, message] = isSBML_Reaction(SBMLStructure.reaction(index), Level, Version);
        index = index + 1;
    end;

    if (Level > 1)
        index = 1;
        [bSBML, message] = isSBML_Event(SBMLStructure.event, Level, Version);
        [x, nNumber] = size(SBMLStructure.event);
        while (bSBML == 1 && index <= nNumber)
            [bSBML, message] = isSBML_Event(SBMLStructure.event(index), Level, Version);
            index = index + 1;
        end;
    end;
    
    if (Level == 2 && Version > 1)
        index = 1;
        [bSBML, message] = isSBML_CompartmentType(SBMLStructure.compartmentType, Level, Version);
        [x, nNumber] = size(SBMLStructure.compartmentType);
        while (bSBML == 1 && index <= nNumber)
            [bSBML, message] = isSBML_CompartmentType(SBMLStructure.compartmentType(index), Level, Version);
            index = index + 1;
        end;
    
        index = 1;
        [bSBML, message] = isSBML_SpeciesType(SBMLStructure.speciesType, Level, Version);
        [x, nNumber] = size(SBMLStructure.speciesType);
        while (bSBML == 1 && index <= nNumber)
            [bSBML, message] = isSBML_SpeciesType(SBMLStructure.speciesType(index), Level, Version);
            index = index + 1;
        end;

        index = 1;
        [bSBML, message] = isSBML_InitialAssignment(SBMLStructure.initialAssignment, Level, Version);
        [x, nNumber] = size(SBMLStructure.initialAssignment);
        while (bSBML == 1 && index <= nNumber)
            [bSBML, message] = isSBML_InitialAssignment(SBMLStructure.initialAssignment(index), Level, Version);
            index = index + 1;
        end;
 
        index = 1;
        [bSBML, message] = isSBML_Constraint(SBMLStructure.constraint, Level, Version);
        [x, nNumber] = size(SBMLStructure.constraint);
        while (bSBML == 1 && index <= nNumber)
            [bSBML, message] = isSBML_Constraint(SBMLStructure.constraint(index), Level, Version);
            index = index + 1;
        end;
    
    elseif (Level > 2)

        index = 1;
        [bSBML, message] = isSBML_InitialAssignment(SBMLStructure.initialAssignment, Level, Version);
        [x, nNumber] = size(SBMLStructure.initialAssignment);
        while (bSBML == 1 && index <= nNumber)
            [bSBML, message] = isSBML_InitialAssignment(SBMLStructure.initialAssignment(index), Level, Version);
            index = index + 1;
        end;
 
        index = 1;
        [bSBML, message] = isSBML_Constraint(SBMLStructure.constraint, Level, Version);
        [x, nNumber] = size(SBMLStructure.constraint);
        while (bSBML == 1 && index <= nNumber)
            [bSBML, message] = isSBML_Constraint(SBMLStructure.constraint(index), Level, Version);
            index = index + 1;
        end;
    end;
    
end;

if (bSBML == 0)
  if (isempty(message))
    message = 'Invalid Model structure';
  else
    message = sprintf('%s\n%s', message, 'Invalid Model structure');
  end;
end;

y = bSBML;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [y, message] = isSBML_AlgebraicRule(varargin)

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

typecode = 'SBML_ALGEBRAIC_RULE';

bSBML = isSBML_Rule(SBMLStructure, Level, Version);


% check that the typecode is correct
if (bSBML == 1 && length(SBMLStructure) == 1)
    type = SBMLStructure.typecode;
    k = strcmp(type, typecode);
    if (k ~= 1)
        bSBML = 0;
    end;
end;
    
if (bSBML == 0)
  message = 'Invalid AlgebraicRule structure';
end;

y = bSBML;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [y, message] = isSBML_AssignmentRule(varargin)

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

typecode = {'SBML_SPECIES_CONCENTRATION_RULE', 'SBML_COMPARTMENT_VOLUME_RULE', 'SBML_PARAMETER_RULE', 'SBML_ASSIGNMENT_RULE'};
nNumberTypecodes = 4;

bSBML = isSBML_Rule(SBMLStructure, Level, Version);


% check that the typecode is correct
index = 1;
nMatch = 0;
if (bSBML == 1 && length(SBMLStructure) == 1)
    type = SBMLStructure.typecode;
    while (index <= nNumberTypecodes)
        k = strcmp(type, typecode(index));
        if (k == 1)
            nMatch = nMatch + 1;
        end;
        index = index + 1;
    end;
    if (nMatch == 0)
        bSBML = 0;
    end;
end;
    
if (bSBML == 0)
  message = 'Invalid AssignmentRule structure';
end;

y = bSBML;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [y, message] = isSBML_Compartment(varargin)


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
    SBMLfieldnames = {'typecode', 'notes', 'annotation','name', 'volume', 'units', 'outside', 'isSetVolume'};
    nNumberFields = 8;
elseif (Level == 2)
    if (Version == 1)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation','name', 'id', 'spatialDimensions', ...
            'size', 'units', 'outside', 'constant', 'isSetSize','isSetVolume'};
        nNumberFields = 13;
    elseif (Version == 2)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation','name', 'id', 'compartmentType', ...
            'spatialDimensions', 'size', 'units', 'outside', 'constant', 'isSetSize','isSetVolume'};
        nNumberFields = 14;
    elseif (Version == 3)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', 'name', 'id', 'compartmentType', ...
            'spatialDimensions', 'size', 'units', 'outside', 'constant', 'isSetSize','isSetVolume'};
        nNumberFields = 15;
    elseif (Version == 4)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', 'name', 'id', 'compartmentType', ...
            'spatialDimensions', 'size', 'units', 'outside', 'constant', 'isSetSize','isSetVolume'};
        nNumberFields = 15;
    end;
elseif (Level == 3)
    if (Version == 1)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', 'name', 'id',  ...
            'spatialDimensions', 'size', 'units', 'constant', 'isSetSize','isSetSpatialDimensions'};
        nNumberFields = 13;
    end;
end;
typecode = 'SBML_COMPARTMENT';

bSBML = 0;

% check that Model is a structure
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
if (bSBML == 1 && length(SBMLStructure) == 1)
    type = SBMLStructure.typecode;
    k = strcmp(type, typecode);
    if (k ~= 1)
        bSBML = 0;
    end;
end;
    
if (bSBML == 0)
  message = 'Invalid Compartment structure';
end;

y = bSBML;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [y, message] = isSBML_CompartmentType(varargin)


%input arguments
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
    y = 0;
    return;
elseif (Level == 2)
  if (Version == 1)
        y = 0;
        return;
    elseif (Version == 2)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'name', 'id'};
        nNumberFields = 6;
    elseif (Version == 3)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', 'name', 'id'};
        nNumberFields = 7;
    elseif (Version == 4)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', 'name', 'id'};
        nNumberFields = 7;
    end;
elseif (Level == 3)
    if (Version == 1)
    y = 0;
    return;
    end;
end;
    
 typecode = 'SBML_COMPARTMENT_TYPE';

bSBML = 0;

% check that argument is a structure
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
if (bSBML == 1 && length(SBMLStructure) == 1)
    type = SBMLStructure.typecode;
    k = strcmp(type, typecode);
    if (k ~= 1)
        bSBML = 0;
    end;
end;

if (bSBML == 0)
  message = 'Invalid CompartmentType structure';
end;
 
y = bSBML;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [y, message] = isSBML_CompartmentVolumeRule(varargin)

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

typecode = 'SBML_COMPARTMENT_VOLUME_RULE';

bSBML = isSBML_Rule(SBMLStructure, Level, Version);


% check that the typecode is correct
if (bSBML == 1 && length(SBMLStructure) == 1)
    type = SBMLStructure.typecode;
    k = strcmp(type, typecode);
    if (k ~= 1)
        bSBML = 0;
    end;
end;
    
if (bSBML == 0)
  message = 'Invalid CompartmentVolumeRule structure';
end;

y = bSBML;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [y, message] = isSBML_Constraint(varargin)


%input arguments
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
    y = 0;
    return;
elseif (Level == 2)
    if (Version == 1)
        y = 0;
        return;
    elseif (Version == 2)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', 'math', 'message'};
        nNumberFields = 7;
    elseif (Version == 3)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', 'math', 'message'};
        nNumberFields = 7;
    elseif (Version == 4)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', 'math', 'message'};
        nNumberFields = 7;
    end;
elseif (Level == 3)
    if (Version == 1)
         SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', 'math', 'message'};
        nNumberFields = 7;
   end;
end;
    
 typecode = 'SBML_CONSTRAINT';

bSBML = 0;

% check that argument is a structure
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
if (bSBML == 1 && length(SBMLStructure) == 1)
    type = SBMLStructure.typecode;
    k = strcmp(type, typecode);
    if (k ~= 1)
        bSBML = 0;
    end;
end;

if (bSBML == 0)
  message = 'Invalid Constraint structure';
end;

y = bSBML;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [y, message] = isSBML_Delay(varargin)


%input arguments
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
    y = 0;
    return;
elseif (Level == 2)
    if (Version == 1)
        y = 0;
        return;
    elseif (Version == 2)
        y = 0;
        return;
    elseif (Version == 3)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', 'math'};
        nNumberFields = 6;
    elseif (Version == 4)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', 'math'};
        nNumberFields = 6;
    end;
elseif (Level == 3)
    if (Version == 1)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', 'math'};
        nNumberFields = 6;
    end;
end;
    
 typecode = 'SBML_DELAY';

bSBML = 0;

% check that argument is a structure
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
if (bSBML == 1 && length(SBMLStructure) == 1)
    type = SBMLStructure.typecode;
    k = strcmp(type, typecode);
    if (k ~= 1)
        bSBML = 0;
    end;
end;

if (bSBML == 0)
  message = 'Invalid Delay structure';
end;

y = bSBML;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [y, message] = isSBML_Event(varargin)


%input arguments
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
    y = 0;
    message = 'Event is invalid for SBML L1';
    return;
elseif (Level == 2)
    if (Version == 1)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'name', 'id', 'trigger', 'delay', ...
            'timeUnits', 'eventAssignment'};
        nNumberFields = 10;
    elseif (Version == 2)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'name', 'id', 'trigger', 'delay', ...
            'timeUnits', 'sboTerm', 'eventAssignment'};
        nNumberFields = 11;
    elseif (Version == 3)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', 'name', 'id', 'trigger', 'delay', ...
            'eventAssignment'};
        nNumberFields = 10;
    elseif (Version == 4)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', 'name', 'id', ...
          'useValuesFromTriggerTime', 'trigger', 'delay', 'eventAssignment'};
        nNumberFields = 11;
    end;
elseif (Level == 3)
    if (Version == 1)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', 'name', 'id', ...
          'useValuesFromTriggerTime', 'trigger', 'delay', 'priority', 'eventAssignment'};
        nNumberFields = 12;
    end;
end;
    
 typecode = 'SBML_EVENT';

bSBML = 0;

% check that argument is a structure
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
if (bSBML == 1 && length(SBMLStructure) == 1)
    type = SBMLStructure.typecode;
    k = strcmp(type, typecode);
    if (k ~= 1)
        bSBML = 0;
    end;
end;
 
% check that any nested structures are appropriate
if(bSBML == 1 && length(SBMLStructure) == 1)
    index = 1;
    [bSBML, message] = isSBML_EventAssignment(SBMLStructure.eventAssignment, Level, Version);
    [x, nNumber] = size(SBMLStructure.eventAssignment); 
    while (bSBML == 1 && index <= nNumber)
        [bSBML, message] = isSBML_EventAssignment(SBMLStructure.eventAssignment(index), Level, Version);
        index = index + 1;
    end;
end;

if (bSBML == 1 && length(SBMLStructure) == 1)
% nested structures that are empty should have the structure but with no
% elements e.g. [1x0 struct]
if ((Level == 2 && Version > 2))
  if (length(SBMLStructure.trigger) > 1 || length(SBMLStructure.delay) > 1)
    bSBML = 0;
  end;

  if(bSBML == 1) % && ~isempty(SBMLStructure.trigger))
    [bSBML, mess1] = isSBML_Trigger(SBMLStructure.trigger, Level, Version);
    if (bSBML == 0)
      if (isempty(message))
        message = mess1;
      else
        message = sprintf('%s\n%s', message, mess1);
      end;
    end;
  end;
  if(bSBML == 1) % && ~isempty(SBMLStructure.delay))
    [bSBML, mess1] = isSBML_Delay(SBMLStructure.delay, Level, Version);
    if (bSBML == 0)
      if (isempty(message))
        message = mess1;
      else
        message = sprintf('%s\n%s', message, mess1);
      end;
    end;
  end;
end;
if (Level > 2)
  if (length(SBMLStructure.trigger) > 1  ...
      || length(SBMLStructure.delay) > 1 ...
      || length(SBMLStructure.priority) > 1)
    bSBML = 0;
  end;

  if(bSBML == 1) % && ~isempty(SBMLStructure.trigger))
    [bSBML, mess1] = isSBML_Trigger(SBMLStructure.trigger, Level, Version);
    if (bSBML == 0)
      if (isempty(message))
        message = mess1;
      else
        message = sprintf('%s\n%s', message, mess1);
      end;
    end;
  end;
  if(bSBML == 1) % && ~isempty(SBMLStructure.delay))
    [bSBML, mess1] = isSBML_Delay(SBMLStructure.delay, Level, Version);
    if (bSBML == 0)
      if (isempty(message))
        message = mess1;
      else
        message = sprintf('%s\n%s', message, mess1);
      end;
    end;
  end;
  if(bSBML == 1) % && ~isempty(SBMLStructure.priority))
    [bSBML, mess1] = isSBML_Priority(SBMLStructure.priority, Level, Version);
    if (bSBML == 0)
      if (isempty(message))
        message = mess1;
      else
        message = sprintf('%s\n%s', message, mess1);
      end;
    end;
  end;
end;
end;
if (bSBML == 0)
  if (isempty(message))
    message = 'Invalid Event structure';
  else
    message = sprintf('%s\n%s', message, 'Invalid Event structure');
  end;
end;
y = bSBML;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [y, message] = isSBML_EventAssignment(varargin)

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
    y = 0;
    return;
elseif (Level == 2)
    if (Version == 1)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation','variable', 'math'};
        nNumberFields = 6;
    elseif (Version == 2)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation','variable', 'sboTerm', 'math'};
        nNumberFields = 7;
    elseif (Version == 3)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', 'variable', 'math'};
        nNumberFields = 7;
    elseif (Version == 4)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', 'variable', 'math'};
        nNumberFields = 7;
    end;
elseif (Level == 3)
    if (Version == 1)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', 'variable', 'math'};
        nNumberFields = 7;
    end;
end;
    
 typecode = 'SBML_EVENT_ASSIGNMENT';

bSBML = 0;

% check that argument is a structure
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
if (bSBML == 1 && length(SBMLStructure) == 1)
    type = SBMLStructure.typecode;
    k = strcmp(type, typecode);
    if (k ~= 1)
        bSBML = 0;
    end;
end;

if (bSBML == 0)
  message = 'Invalid EventAssignment structure';
end;

y = bSBML;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [y, message] = isSBML_FunctionDefinition(varargin)

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
    y = 0;
    return;
elseif (Level == 2)
    if (Version == 1)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation','name', 'id', 'math'};
        nNumberFields = 7;
    elseif (Version == 2)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', 'name', 'id', 'math'};
        nNumberFields = 8;
    elseif (Version == 3)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', 'name', 'id', 'math'};
        nNumberFields = 8;
    elseif (Version == 4)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', 'name', 'id', 'math'};
        nNumberFields = 8;
    end;
elseif (Level == 3)
    if (Version == 1)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', 'name', 'id', 'math'};
        nNumberFields = 8;
    end;
end;
    
 typecode = 'SBML_FUNCTION_DEFINITION';

bSBML = 0;

% check that argument is a structure
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
if (bSBML == 1 && length(SBMLStructure) == 1)
    type = SBMLStructure.typecode;
    k = strcmp(type, typecode);
    if (k ~= 1)
        bSBML = 0;
    end;
end;

if (bSBML == 0)
  message = 'Invalid FunctionDefinition structure';
end;

y = bSBML;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [y, message] = isSBML_InitialAssignment(varargin)


%input arguments
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
    y = 0;
    return;
elseif (Level == 2)
    if (Version == 1)
        y = 0;
        return;
    elseif (Version == 2)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', 'symbol', 'math'};
        nNumberFields = 7;
    elseif (Version == 3)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', 'symbol', 'math'};
        nNumberFields = 7;
    elseif (Version == 4)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', 'symbol', 'math'};
        nNumberFields = 7;
    end;
elseif (Level == 3)
    if (Version == 1)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', 'symbol', 'math'};
        nNumberFields = 7;
    end;
end;
    
 typecode = 'SBML_INITIAL_ASSIGNMENT';

bSBML = 0;

% check that argument is a structure
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
if (bSBML == 1 && length(SBMLStructure) == 1)
    type = SBMLStructure.typecode;
    k = strcmp(type, typecode);
    if (k ~= 1)
        bSBML = 0;
    end;
end;
 
if (bSBML == 0)
  message = 'Invalid InitialAssignment structure';
end;

y = bSBML;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [y, message] = isSBML_KineticLaw(varargin)

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
    SBMLfieldnames = {'typecode', 'notes', 'annotation','formula', 'parameter', 'timeUnits', 'substanceUnits'};
    nNumberFields = 7;
elseif (Level == 2)
    if (Version == 1)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation','formula', 'math','parameter', ...
            'timeUnits','substanceUnits'};
        nNumberFields = 9;
    elseif (Version == 2)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation','formula', 'math','parameter', 'sboTerm'};
        nNumberFields = 8;
    elseif (Version == 3)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', 'formula', 'math','parameter'};
        nNumberFields = 8;
    elseif (Version == 4)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', 'formula', 'math','parameter'};
        nNumberFields = 8;
    end;
elseif (Level == 3)
    if (Version == 1)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', 'math','localParameter'};
        nNumberFields = 7;
    end;
end;
typecode = 'SBML_KINETIC_LAW';

bSBML = 0;

% check that argument is a structure
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
if (bSBML == 1 && length(SBMLStructure) == 1)
    type = SBMLStructure.typecode;
    k = strcmp(type, typecode);
    if (k ~= 1)
        bSBML = 0;
    end;
end;
  
% check that any nested structures are appropriate
if(bSBML == 1 && length(SBMLStructure) == 1)
    index = 1;
    if (Level < 3)
      [x, nNumberParameters] = size(SBMLStructure.parameter); 
      [bSBML, message] = isSBML_Parameter(SBMLStructure.parameter, Level, Version);
      while (bSBML == 1 && index <= nNumberParameters)
          [bSBML, message] = isSBML_Parameter(SBMLStructure.parameter(index), Level, Version);
          index = index + 1;
      end;
    else
      [x, nNumberParameters] = size(SBMLStructure.localParameter); 
      [bSBML, message] = isSBML_LocalParameter(SBMLStructure.localParameter, Level, Version);
      while (bSBML == 1 && index <= nNumberParameters)
          [bSBML, message] = isSBML_LocalParameter(SBMLStructure.localParameter(index), Level, Version);
          index = index + 1;
      end;
    end;
end;

if (bSBML == 0)
  if (isempty(message))
    message = 'Invalid KineticLaw structure';
  else
    message = sprintf('%s\n%s', message, 'Invalid KineticLaw structure');
  end;
end;

y = bSBML;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [y, message] = isSBML_LocalParameter(varargin)

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
    y = 0;
    return;
elseif (Level == 2)
    y = 0;
    return;
elseif (Level == 3)
    if (Version == 1)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', 'name', 'id', 'value', 'units', ...
            'isSetValue'};
        nNumberFields = 10;
    end;
end;
    
typecode = 'SBML_LOCAL_PARAMETER';

bSBML = 0;

% check that argument is a structure
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
if (bSBML == 1 && length(SBMLStructure) == 1)
    type = SBMLStructure.typecode;
    k = strcmp(type, typecode);
    if (k ~= 1)
        bSBML = 0;
    end;
end;
    
if (bSBML == 0)
  message = 'Invalid Parameter structure';
end;

y = bSBML;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [y, message] = isSBML_ModifierSpeciesReference(varargin)


%input arguments
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
    y = 0;
    return;
elseif (Level == 2)
    if (Version == 1)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation','species'};
        nNumberFields = 5;
    elseif (Version == 2)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation','species', 'id', 'name', 'sboTerm'};
        nNumberFields = 8;
    elseif (Version == 3)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', 'species', 'id', 'name'};
        nNumberFields = 8;
    elseif (Version == 4)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', 'species', 'id', 'name'};
        nNumberFields = 8;
    end;
elseif (Level == 3)
    if (Version == 1)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', 'species', 'id', 'name'};
        nNumberFields = 8;
    end;
end;
typecode = 'SBML_MODIFIER_SPECIES_REFERENCE';

bSBML = 0;

% check that argument is a structure
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
if (bSBML == 1 && length(SBMLStructure) == 1)
    type = SBMLStructure.typecode;
    k = strcmp(type, typecode);
    if (k ~= 1)
        bSBML = 0;
    end;
end;
 
if (bSBML == 0)
  message = 'Invalid ModifierSpeciesReference structure';
end;


y = bSBML;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [y, message] = isSBML_Parameter(varargin)

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
    SBMLfieldnames = {'typecode', 'notes', 'annotation','name', 'value', 'units', 'isSetValue'};
    nNumberFields = 7;
elseif (Level == 2)
    if (Version == 1)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation','name', 'id', 'value', 'units', 'constant', 'isSetValue'};
        nNumberFields = 10;
    elseif (Version == 2)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation','name', 'id', 'value', 'units', ...
            'constant', 'sboTerm', 'isSetValue'};
        nNumberFields = 11;
    elseif (Version == 3)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', 'name', 'id', 'value', 'units', ...
            'constant', 'isSetValue'};
        nNumberFields = 11;
    elseif (Version == 4)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', 'name', 'id', 'value', 'units', ...
            'constant', 'isSetValue'};
        nNumberFields = 11;
    end;
elseif (Level == 3)
    if (Version == 1)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', 'name', 'id', 'value', 'units', ...
            'constant', 'isSetValue'};
        nNumberFields = 11;
    end;
end;
    
typecode = 'SBML_PARAMETER';

bSBML = 0;

% check that argument is a structure
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
if (bSBML == 1 && length(SBMLStructure) == 1)
    type = SBMLStructure.typecode;
    k = strcmp(type, typecode);
    if (k ~= 1)
        bSBML = 0;
    end;
end;
    
if (bSBML == 0)
  message = 'Invalid Parameter structure';
end;

y = bSBML;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [y, message] = isSBML_ParameterRule(varargin)

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

typecode = 'SBML_PARAMETER_RULE';

bSBML = isSBML_Rule(SBMLStructure, Level, Version);


% check that the typecode is correct
if (bSBML == 1 && length(SBMLStructure) == 1)
    type = SBMLStructure.typecode;
    k = strcmp(type, typecode);
    if (k ~= 1)
        bSBML = 0;
    end;
end;
    
if (bSBML == 0)
  message = 'Invalid ParameterRule structure';
end;

y = bSBML;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [y, message] = isSBML_Priority(varargin)


%input arguments
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
    y = 0;
    return;
elseif (Level == 2)
    y = 0;
    return;
elseif (Level == 3)
    if (Version == 1)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', 'math'};
        nNumberFields = 6;
    end;
end;
    
 typecode = 'SBML_PRIORITY';

bSBML = 0;

% check that argument is a structure
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
if (bSBML == 1 && length(SBMLStructure) == 1)
    type = SBMLStructure.typecode;
    k = strcmp(type, typecode);
    if (k ~= 1)
        bSBML = 0;
    end;
end;

if (bSBML == 0)
  message = 'Invalid Priority structure';
end;

y = bSBML;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [y, message] = isSBML_RateRule(varargin)

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

typecodel1 = {'SBML_SPECIES_CONCENTRATION_RULE', ...
  'SBML_COMPARTMENT_VOLUME_RULE', 'SBML_PARAMETER_RULE'};
index = 1;

typecode = 'SBML_RATE_RULE';

bSBML = isSBML_Rule(SBMLStructure, Level, Version);


% check that the typecode is correct
if (bSBML == 1&& length(SBMLStructure) == 1)
  code = SBMLStructure.typecode;
  k = strcmp(code, typecode);
  if (k ~= 1)
    nMatch = 0;
    while (index <= 3)
      k = strcmp(code, typecodel1(index));
      if (k == 1 && strcmp(SBMLStructure.type, 'rate'))
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
  message = 'Invalid RateRule structure';
end;

y = bSBML;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [y, message] = isSBML_Reaction(varargin)

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
    SBMLfieldnames = {'typecode', 'notes', 'annotation','name', 'reactant', 'product', 'kineticLaw', ...
        'reversible', 'fast'};
    nNumberFields = 9;
elseif (Level == 2)
    if (Version == 1)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation','name', 'id', 'reactant', 'product', ...
            'modifier', 'kineticLaw', 'reversible', 'fast', 'isSetFast'};
        nNumberFields = 13;
    elseif (Version == 2)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation','name', 'id', 'reactant', 'product', ...
            'modifier', 'kineticLaw', 'reversible', 'fast', 'sboTerm', 'isSetFast'};
        nNumberFields = 14;
    elseif (Version == 3)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation','sboTerm', 'name', 'id', 'reactant', 'product', ...
            'modifier', 'kineticLaw', 'reversible', 'fast', 'isSetFast'};
        nNumberFields = 14;
    elseif (Version == 4)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation','sboTerm', 'name', 'id', 'reactant', 'product', ...
            'modifier', 'kineticLaw', 'reversible', 'fast', 'isSetFast'};
        nNumberFields = 14;
    end;
elseif (Level == 3)
    if (Version == 1)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation','sboTerm', 'name', 'id', 'reactant', 'product', ...
            'modifier', 'kineticLaw', 'reversible', 'fast', 'isSetFast', 'compartment'};
        nNumberFields = 15;
    end;
end;
    
typecode = 'SBML_REACTION';

bSBML = 0;

% check that argument is a structure
bSBML = isstruct(SBMLStructure);

% check it contains each of the fields listed
index = 1;
while (bSBML == 1 && index <= nNumberFields)
    bSBML = isfield(SBMLStructure, char(SBMLfieldnames(index)));
    index = index + 1;
end;

% catch cases where isSetFast field was capitalised (Version 1.0.2)
if (Level == 2 && bSBML == 0)
    % may have failed on case
    if (index == 13)
        bSBML = isfield(SBMLStructure, 'IsSetFast');
    end;
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
if (bSBML == 1 && length(SBMLStructure) == 1)
    type = SBMLStructure.typecode;
    k = strcmp(type, typecode);
    if (k ~= 1)
        bSBML = 0;
    end;
end;
    
% check that any nested structures are appropriate
if(bSBML == 1 && length(SBMLStructure) == 1)
    index = 1;
    [bSBML, message] = isSBML_SpeciesReference(SBMLStructure.reactant, Level, Version);
    [x, nNumberReactants] = size(SBMLStructure.reactant);
    while (bSBML == 1 && index <= nNumberReactants)
        [bSBML, message] = isSBML_SpeciesReference(SBMLStructure.reactant(index), Level, Version);
        index = index + 1;
    end;

    index = 1;
    [bSBML, message] = isSBML_SpeciesReference(SBMLStructure.product, Level, Version);
    [x, nNumberProducts] = size(SBMLStructure.product);
    while (bSBML == 1 && index <= nNumberProducts)
        [bSBML, message] = isSBML_SpeciesReference(SBMLStructure.product(index), Level, Version);
        index = index + 1;
    end;

    if (Level > 1)
        index = 1;
        [bSBML, message] = isSBML_ModifierSpeciesReference(SBMLStructure.modifier, Level, Version);
        [x, nNumberModifiers] = size(SBMLStructure.modifier);
        while (bSBML == 1 && index <= nNumberModifiers)
            [bSBML, message] = isSBML_ModifierSpeciesReference(SBMLStructure.modifier(index), Level, Version);
            index = index + 1;
        end;
    end;

    % if a kinetic law is present check that it is valid
    if (bSBML == 1)% && ~isempty(SBMLStructure.kineticLaw))
        [bSBML, message] = isSBML_KineticLaw(SBMLStructure.kineticLaw, Level, Version);
    end;
end;

if (bSBML == 0)
  if (isempty(message))
    message = 'Invalid Reaction structure';
  else
    message = sprintf('%s\n%s', message, 'Invalid Reaction structure');
  end;
end;

y = bSBML;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [y, message] = isSBML_Rule(varargin)

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
if (bSBML == 1 && length(SBMLStructure) == 1)
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [y, message] = isSBML_Species(varargin)

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
    SBMLfieldnames = {'typecode', 'notes', 'annotation','name', 'compartment', 'initialAmount', ...
        'units', 'boundaryCondition', 'charge', 'isSetInitialAmount', 'isSetCharge'};
    nNumberFields = 11;
elseif (Level == 2)
    if (Version == 1)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation','name', 'id', 'compartment', 'initialAmount', ...
            'initialConcentration', 'substanceUnits', 'spatialSizeUnits',  'hasOnlySubstanceUnits', ...
            'boundaryCondition', 'charge', 'constant', 'isSetInitialAmount', 'isSetInitialConcentration', ...
            'isSetCharge'};
        nNumberFields = 18;
    elseif (Version == 2)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation','name', 'id', 'speciesType', 'compartment', ...
            'initialAmount', 'initialConcentration', 'substanceUnits', 'spatialSizeUnits',  ...
            'hasOnlySubstanceUnits', 'boundaryCondition', 'charge', 'constant', 'isSetInitialAmount', ...
            'isSetInitialConcentration','isSetCharge'};
        nNumberFields = 19;
    elseif (Version == 3)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation','sboTerm', 'name', 'id', 'speciesType', 'compartment', ...
            'initialAmount', 'initialConcentration', 'substanceUnits',  ...
            'hasOnlySubstanceUnits', 'boundaryCondition', 'charge', 'constant', 'isSetInitialAmount', ...
            'isSetInitialConcentration','isSetCharge'};
        nNumberFields = 19;
    elseif (Version == 4)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation','sboTerm', 'name', 'id', 'speciesType', 'compartment', ...
            'initialAmount', 'initialConcentration', 'substanceUnits',  ...
            'hasOnlySubstanceUnits', 'boundaryCondition', 'charge', 'constant', 'isSetInitialAmount', ...
            'isSetInitialConcentration','isSetCharge'};
        nNumberFields = 19;
    end;
elseif (Level == 3)
    if (Version == 1)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation','sboTerm', 'name', 'id', 'compartment', ...
            'initialAmount', 'initialConcentration', 'substanceUnits',  ...
            'hasOnlySubstanceUnits', 'boundaryCondition', 'constant', 'isSetInitialAmount', ...
            'isSetInitialConcentration','conversionFactor'};
        nNumberFields = 17;
    end;
end;
typecode = 'SBML_SPECIES';

bSBML = 0;

% check that argument is a structure
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
if (bSBML == 1 && length(SBMLStructure) == 1)
    type = SBMLStructure.typecode;
    k = strcmp(type, typecode);
    if (k ~= 1)
        bSBML = 0;
    end;
end;
    
if (bSBML == 0)
  message = 'Invalid Species structure';
end;

y = bSBML;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [y, message] = isSBML_SpeciesConcentrationRule(varargin)

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

typecode = 'SBML_SPECIES_CONCENTRATION_RULE';

bSBML = isSBML_Rule(SBMLStructure, Level, Version);


% check that the typecode is correct
if (bSBML == 1 && length(SBMLStructure) == 1)
    type = SBMLStructure.typecode;
    k = strcmp(type, typecode);
    if (k ~= 1)
        bSBML = 0;
    end;
end;
    
if (bSBML == 0)
  message = 'Invalid SpeciesConcentrationRule structure';
end;

y = bSBML;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [y, message] = isSBML_SpeciesReference(varargin)


%input arguments
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
    SBMLfieldnames = {'typecode', 'notes', 'annotation','species', 'stoichiometry', 'denominator'};
    nNumberFields = 6;
elseif (Level == 2)
    if (Version == 1)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation','species', 'stoichiometry', ...
            'denominator', 'stoichiometryMath'};
        nNumberFields = 8;
    elseif (Version == 2)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation','species', 'id', 'name', ...
            'sboTerm', 'stoichiometry', 'stoichiometryMath'};
        nNumberFields = 10;
    elseif (Version == 3)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', 'species', 'id', 'name', ...
            'stoichiometry', 'stoichiometryMath'};
        nNumberFields = 10;
    elseif (Version == 4)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', 'species', 'id', 'name', ...
            'stoichiometry', 'stoichiometryMath'};
        nNumberFields = 10;
    end;
elseif (Level == 3)
    if (Version == 1)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', 'species', 'id', 'name', ...
            'stoichiometry', 'constant', 'isSetStoichiometry'};
        nNumberFields = 11;
    end;
end;
typecode = 'SBML_SPECIES_REFERENCE';

bSBML = 0;

% check that argument is a structure
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

% check that any nested structures are appropriate
if(bSBML == 1 && length(SBMLStructure) == 1)
  if (Level == 2 && Version > 2)
    if (length(SBMLStructure.stoichiometryMath) > 1)
      bSBML = 0;
    end;

    if(bSBML == 1 && ~isempty(SBMLStructure.stoichiometryMath))
      [bSBML, message] = isSBML_StoichiometryMath(SBMLStructure.stoichiometryMath, Level, Version);
    end;
  end;
end;
% check that the typecode is correct
if (bSBML == 1 && length(SBMLStructure) == 1)
    type = SBMLStructure.typecode;
    k = strcmp(type, typecode);
    if (k ~= 1)
        bSBML = 0;
    end;
end;

if (bSBML == 0)
  if (isempty(message))
    message = 'Invalid SpeciesReference structure';
  else
    message = sprintf('%s\n%s', message, 'Invalid SpeciesReference structure');
  end;
end;

y = bSBML;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [y, message] = isSBML_SpeciesType(varargin)


%input arguments
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
    y = 0;
    return;
elseif (Level == 2)
    if (Version == 1)
        y = 0;
        return;
    elseif (Version == 2)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'name', 'id'};
        nNumberFields = 6;
    elseif (Version == 3)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', 'name', 'id'};
        nNumberFields = 7;
    elseif (Version == 4)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', 'name', 'id'};
        nNumberFields = 7;
    end;
elseif (Level == 3)
    if (Version == 1)
    y = 0;
    return;
    end;
end;
    
 typecode = 'SBML_SPECIES_TYPE';

bSBML = 0;

% check that argument is a structure
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
if (bSBML == 1 && length(SBMLStructure) == 1)
    type = SBMLStructure.typecode;
    k = strcmp(type, typecode);
    if (k ~= 1)
        bSBML = 0;
    end;
end;
 
if (bSBML == 0)
  message = 'Invalid SpeciesType structure';
end;

y = bSBML;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [y, message] = isSBML_StoichiometryMath(varargin)


%input arguments
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
    y = 0;
    return;
elseif (Level == 2)
    if (Version == 1)
        y = 0;
        return;
    elseif (Version == 2)
        y = 0;
        return;
    elseif (Version == 3)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', 'math'};
        nNumberFields = 6;
    elseif (Version == 4)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', 'math'};
        nNumberFields = 6;
    end;
elseif (Level == 3)
    if (Version == 1)
    y = 0;
    return;
    end;
end;
    
 typecode = 'SBML_STOICHIOMETRY_MATH';

bSBML = 0;

% check that argument is a structure
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
if (bSBML == 1 && length(SBMLStructure) == 1)
    type = SBMLStructure.typecode;
    k = strcmp(type, typecode);
    if (k ~= 1)
        bSBML = 0;
    end;
end;
 
if (bSBML == 0)
  message = 'Invalid StoichiometryMath structure';
end;

y = bSBML;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [y, message] = isSBML_Trigger(varargin)


%input arguments
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
    y = 0;
    return;
elseif (Level == 2)
    if (Version == 1)
        y = 0;
        return;
    elseif (Version == 2)
        y = 0;
        return;
    elseif (Version == 3)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', 'math'};
        nNumberFields = 6;
    elseif (Version == 4)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', 'math'};
        nNumberFields = 6;
    end;
elseif (Level == 3)
    if (Version == 1)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', ...
          'persistent', 'initialValue' 'math'};
        nNumberFields = 8;
    end;
end;
    
 typecode = 'SBML_TRIGGER';

bSBML = 0;

% check that argument is a structure
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
if (bSBML == 1 && length(SBMLStructure) == 1)
    type = SBMLStructure.typecode;
    k = strcmp(type, typecode);
    if (k ~= 1)
        bSBML = 0;
    end;
end;
 
if (bSBML == 0)
  message = 'Invalid Trigger structure';
end;

y = bSBML;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [y, message] = isSBML_Unit(varargin)


%input arguments
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
  SBMLfieldnames = {'typecode', 'notes', 'annotation','kind', ...
    'exponent', 'scale'};
  nNumberFields = 6;
elseif (Level == 2)
  if (Version == 1)
    SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'kind', ...
      'exponent', 'scale', 'multiplier', 'offset'};
    nNumberFields = 9;
  elseif (Version == 2)
    SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'kind', ...
      'exponent', 'scale', 'multiplier'};
    nNumberFields = 8;
  elseif (Version == 3)
    SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', ...
      'kind', 'exponent', 'scale', 'multiplier'};
    nNumberFields = 9;
  elseif (Version == 4)
    SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', ...
      'kind', 'exponent', 'scale', 'multiplier'};
    nNumberFields = 9;
  end;
elseif (Level == 3)
    if (Version == 1)
    SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', ...
      'kind', 'exponent', 'scale', 'multiplier'};
    nNumberFields = 9;
    end;
end;

typecode = 'SBML_UNIT';

bSBML = 0;

% check that argument is a structure
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
if (bSBML == 1 && length(SBMLStructure) == 1)
  type = SBMLStructure.typecode;
  k = strcmp(type, typecode);
  if (k ~= 1)
    bSBML = 0;
  end;
end;
 
if (bSBML == 0)
  message = 'Invalid Unit structure';
end;


y = bSBML;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [y, message] = isSBML_UnitDefinition(varargin)

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
    SBMLfieldnames = {'typecode', 'notes', 'annotation','name', 'unit'};
    nNumberFields = 5;
elseif (Level == 2)
    if (Version == 1)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation','name', 'id', 'unit'};
        nNumberFields = 7;
    elseif (Version == 2)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation','name', 'id', 'unit'};
        nNumberFields = 7;
    elseif (Version == 3)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', 'name', 'id', 'unit'};
        nNumberFields = 8;
    elseif (Version == 4)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', 'name', 'id', 'unit'};
        nNumberFields = 8;
    end;
elseif (Level == 3)
    if (Version == 1)
        SBMLfieldnames = {'typecode', 'metaid', 'notes', 'annotation', 'sboTerm', 'name', 'id', 'unit'};
        nNumberFields = 8;
    end;
end;
    
 typecode = 'SBML_UNIT_DEFINITION';

bSBML = 0;

% check that argument is a structure
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
if (bSBML == 1 && length(SBMLStructure) == 1)
    type = SBMLStructure.typecode;
    k = strcmp(type, typecode);
    if (k ~= 1)
        bSBML = 0;
    end;
end;
    
% check that any nested structures are appropriate
if(bSBML == 1 && length(SBMLStructure) == 1)
    index = 1;
    [x, nNumber] = size(SBMLStructure.unit); 
    while (bSBML == 1 && index <= nNumber)
        [bSBML, message] = isSBML_Unit(SBMLStructure.unit(index), Level, Version);
        index = index + 1;
    end;
end;

if (bSBML == 0)
  if (isempty(message))
    message = 'Invalid UnitDefinition structure';
  else
    message = sprintf('%s\n%s', message, 'Invalid UnitDefinition structure');
  end;
end;

y = bSBML;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

