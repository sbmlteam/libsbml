function [y, message] = isSBML_Event(varargin)
% isSBML_Event(SBMLStructure, Level, Version(optional)) 
% checks that SBMLStructure represents a Event
% within an sbml model of specified level
% 
% if SBMLStructure represents a Event within an SBML model
% it has the appropriate fields (ONLY IN LEVEL 2)
% eg    Typecode (L2V1)
%       Metaid (L2V1)
%       Notes (L2V1)
%       Annotations (L2V1)
%       Name (L2V1)
%       Id (L2V1)
%       Trigger (L2V1)
%       Delay (L2V1)
%       TimeUnits (L2V1 - L2V2)
%       SBOTerm (L2V2)
%       ListEventAssignment (L2V1)
%
% NOTE: content of brackets indicates the level and version of sbml from which the given field
% is appropriate.
%
% Returns 1 if SBMLStructure is a structure containing each of the above
% fields (appropriate with the given level and version) 
% and the typecode is "SBML_EVENT"
% 
% Returns 0 if SBMLStructure is not a structure 
% or does not contain one of the appropriate fields
% or the typecode is not "SBML_EVENT"
%
% Returns message indicating the structure that is invalid.

%  Filename    :   isSBML_Event.m
%  Description :
%  Author(s)   :   SBML Development Group <sbml-team@caltech.edu>
%  $Id: isSBML_Event.m 11823 2010-09-03 11:24:42Z sarahkeating $
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
          'useValuesFromTriggerTime', 'trigger', 'delay', 'eventAssignment'};
        nNumberFields = 11;
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
if (bSBML == 1)
    type = SBMLStructure.typecode;
    k = strcmp(type, typecode);
    if (k ~= 1)
        bSBML = 0;
    end;
end;
 
% check that any nested structures are appropriate
if(bSBML == 1)
    index = 1;
    [x, nNumber] = size(SBMLStructure.eventAssignment); 
    while (bSBML == 1 && index <= nNumber)
        [bSBML, message] = isSBML_EventAssignment(SBMLStructure.eventAssignment(index), Level, Version);
        index = index + 1;
    end;
end;
if (Level == 2 && Version > 2)
  if (length(SBMLStructure.trigger) > 1 || length(SBMLStructure.delay) > 1)
    bSBML = 0;
  end;

  if(bSBML == 1 && ~isempty(SBMLStructure.trigger))
    [bSBML, mess1] = isSBML_Trigger(SBMLStructure.trigger, Level, Version);
    if (bSBML == 0)
      if (isempty(message))
        message = mess1;
      else
        message = sprintf('%s\n%s', message, mess1);
      end;
    end;
  end;
  if(bSBML == 1 && ~isempty(SBMLStructure.delay))
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

if (bSBML == 0)
  if (isempty(message))
    message = 'Invalid Event structure';
  else
    message = sprintf('%s\n%s', message, 'Invalid Event structure');
  end;
end;
y = bSBML;
