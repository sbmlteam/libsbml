function fail = testMissingOutput(outdir, in_installer, fbcEnabled)

%  Filename    :   TestOutput.m
%  Description :
%  $Source v $
%
%<!---------------------------------------------------------------------------
% This file is part of SBMLToolbox.  Please visit http://sbml.org for more
% information about SBML, and the latest version of SBMLToolbox.
%
% Copyright (C) 2013-2018 jointly by the following organizations:
%     1. California Institute of Technology, Pasadena, CA, USA
%     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
%     3. University of Heidelberg, Heidelberg, Germany
%
% Copyright (C) 2009-2013 jointly by the following organizations:
%     1. California Institute of Technology, Pasadena, CA, USA
%     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
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
% in the file named "LICENSE.txt" included with this software distribution.
% and also available online as http://sbml.org/software/sbmltoolbox/license.html
%----------------------------------------------------------------------- -->

if fbcEnabled == 0
    fail = 0;
    return;
end;

if (~isdir(outdir))
    mkdir (outdir);
end;

disp('Testing output with missing fields');

fail = 0;
test = 0;

load (['test-data', filesep, 'missing_out.mat']);

filename = 'missing_out.xml';
if (in_installer == 1)
    OutputSBML(m, [outdir, filesep, filename], in_installer);
else
    OutputSBML(m, [outdir, filesep, filename]);
end;
test = test + 1;
if (compareFiles(['test-data', filesep, filename], [outdir, filesep, filename]))
    disp(sprintf('Output of %s failed', filename));
    fail = fail + 1;
end;

