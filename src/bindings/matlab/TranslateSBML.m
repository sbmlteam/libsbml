% TranslateSBML((optional)'filename', (optional) validateFlag) 
% imports an SBML model into a matlab structure
%
% filename is the name of the file containing the sbml definition of a
% model - if not supplied a browse window is opened
%
% validateFlag is an optional argument indicating whether the model should
% be validated. The default value is 0; indicating no validation.
%
% NOTE: In the case of no arguments to the function; 
%       which opens a browse window to locate the file to import; 
%       the user will be prompted to indicate whether validation should be done. 
%
% In the case of validation errors these will be displayed to the user, who 
% will be prompted as to whether to import the model regardless.
%
% TranslateSBML returns a structure with the following fields
% i.e. a sbml model structure
% eg    Typecode
%       Notes
%       Annotations
%       Level
%       Version
%       Name
%       Id (l2v1)
%       ListFunctionDefinition (l2v1)
%       ListUnitDefinition
%       ListCompartmentType (l2v2)
%       ListSpeciesType (l2v2)
%       ListCompartment
%       ListSpecies
%       ListParameter
%       ListInitialAssignment (l2v2)
%       ListRule
%       ListConstraint (l2v2)
%       ListReaction
%       ListEvent (l2v1)
%
% NOTE number in brackets indicates field is appropriate beginning with that 
% level and version of SBML.
%
% where Typecode,  Notes,  Annotations & Name are all of type char * 
%                           (ie mxArray of char)
%       ListXX refers to an array of structures of type XXX
%

% Filename    : TranslateSBML.m
% Description : MATLAB help file for TranslateSBML
% Author(s)   : SBML Team <sbml-team@caltech.edu>
% Organization: University of Hertfordshire STRC
% Created     : 2003-09-15
% Revision    : $Id$
% $HeadURL$
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
%
% The original code contained here was initially developed by:
%
%      Sarah Keating
%      Science and Technology Research Centre
%      University of Hertfordshire
%      Hatfield, AL10 9AB
%      United Kingdom
%
%      http://www.sbml.org
%      mailto:sbml-team@caltech.edu
%
% Contributor(s):
%
