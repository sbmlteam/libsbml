function y = CompareFiles(file1, file2)

%  Filename    :   CompareFiles.m
%  Description :
%  Author(s)   :   SBML Development Group <sbml-team@caltech.edu>
%  $Id: CompareFiles.m 7155 2008-06-26 20:24:00Z mhucka $
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

fid1 = fopen(file1);
fid2 = fopen(file2);
y = 0;
while (~feof(fid1) || ~feof(fid2))
  line1 = fgetl(fid1);
  line2 = fgetl(fid2);

  if (~strcmp(line1, line2))
    disp(sprintf('%s is Not equal to %s', line1, line2));
     y = 1;
     return;
  end;
end;
