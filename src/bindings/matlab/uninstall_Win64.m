function uninstall_Win64()
% uninstalls the libsbml MATLAB binding on a windows 64 bit operating system

% Copyright 2002 California Institute of Technology and Japan Science and
% Technology Corporation.
%
% This library is free software; you can redistribute it and/or modify it
% under the terms of the GNU Lesser General Public License as published by
% the Free Software Foundation.  A copy of the license agreement is
% provided in the file named "LICENSE.txt" included with this software
% distribution.  It is also available online at
% http://sbml.org/software/libsbml/license.html
%
% You should have received a copy of the GNU Lesser General Public License
% along with this library; if not, write to the Free Software Foundation,
% Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.

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
