function uninstall_Win64()
% uninstalls the libsbml MATLAB binding on a windows 64 bit operating system

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

% check we are in the correct directory
files = dir('*.mexw32');

if (isempty(files))
  error('Need to be in the directory where you installed the libsbml binding');
else
  match = 0;
  for i=1:length(files)
    if (strcmp(files(i).name, 'TranslateSBML.mex64'))
      match = 1;
    elseif (strcmp(files(i).name, 'OutputSBML.mex64'))
      match = 1;
    end;
  end;
  if (match == 0)
    error('Need to be in the directory where you installed the libsbml binding');
  end;
    
end;

delete TranslateSBML.mexw32
delete OutputSBML.mexw32

Path_to_libs = matlabroot;
Path_to_libs = strcat(Path_to_libs, '\bin\win64');

dll = strcat(Path_to_libs, '\libsbml.dll');
lib = strcat(Path_to_libs, '\libsbml.lib');

delete (dll);
delete (lib);
