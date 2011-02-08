% buildMac builds the TranslateSBML/OutputSBML executable in a Mac OSX
% environment

% Filename    : buildMac.m
% Description : MATLAB help file for BuildTranslate_Win32
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

if (strcmp(isoctave(), '0'))
  mex  TranslateSBML.c  -I'../../../include' ../../../usr/local/lib/libsbml.dylib
  mex  OutputSBML.c  -I'../../../include' ../../../usr/local/lib/libsbml.dylib
else
  error('This file has not been tested using octave on a Mac OSX system');
end;

