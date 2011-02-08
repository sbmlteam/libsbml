%  Filename: install_Win32.m
%  
%  installs the MATLAB binding on a Windows platform

% Description : File to install SBMLToolbox
% Author(s)   : SBML Team <sbml-team@caltech.edu>
% Organization: University of Hertfordshire STRC
% Created     : 2003-10-01
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

if (strcmp(isoctave(), '0'))
  matlab = 1;
else
  matlab = 0;
end;
% add the current directory to the Matlab search
% path and save
addpath(pwd);

% path2rc is deprecated by version 7.0.4 
% replaced by savepath
% but savepath doesnt exist in version 6.5.1 or lower

if (matlab)
  v = version;
  v_num = str2num(v(1));

  if (v_num < 7)
    saved = path2rc;
  else
    saved = savepath;
  end;
else
  saved = savepath;
end;

if (saved ~= 0)
    error('Directory NOT added to the path');
end;

% try the executable
% if it doesnt work the library files are not on the system path and need
% to be placed there
try
    M = TranslateSBML('test.xml');
catch
    % determine the matlabroot for windows executable
    % this directory is saved to the environmental variable PATH
    Path_to_libs = matlabroot;

    if (matlab)
      Path_to_libs = strcat(Path_to_libs, '\bin\win32');
      % determine the location of the library files
      lib{1} = '..\..\win32\lib\libsbml.lib';
      lib{2} = '..\..\win32\bin\libsbml.dll';
      lib{3} = '..\..\win32\lib\libxml2.lib';
      lib{4} = '..\..\win32\bin\libxml2.dll';
      lib{5} = '..\..\win32\lib\iconv.lib';
      lib{6} = '..\..\win32\bin\iconv.dll';

      for i = 1:6
        copyfile(lib{i}, Path_to_libs);
      end;
    else
      Path_to_libs = strcat(Path_to_libs, '\bin');

      % determine the location of the library files
      lib{1} = '..\..\win32\lib\libsbml.lib';
      lib{2} = '..\..\win32\bin\libsbml.dll';

      for i = 1:2
        copyfile(lib{i}, Path_to_libs);
      end;
    end;
end;

try
  M = TranslateSBML('test.xml');
  disp('Installation successful');
catch
  disp('Installation failed.');
end;
