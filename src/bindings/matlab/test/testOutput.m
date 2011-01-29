function fail = testOutput(outdir)

%  Filename    :   TestOutput.m
%  Description :
%  Author(s)   :   SBML Development Group <sbml-team@caltech.edu>
%  $Id: TestOutput.m 10323 2009-11-26 11:24:31Z sarahkeating $
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

if (~isdir(outdir))
mkdir (outdir);
end;

if exist('OCTAVE_VERSION')
  ff = dir('test-data');
  j = 1;
  for i=1:length(ff)
    if (ff(i).isdir == 0 && ~isempty(strfind(ff(i).name, '.xml')))
      files(j) = ff(i);
      j = j+1;
    end;
  end;
else
  files = dir(['test-data', filesep, '*.xml']);
end;

fail = 0;

for i=1:length(files)
  model = [];
  %skip models that will cause read errors
  if (strcmp(files(i).name, 'readerror.xml'))
    % donothing
  elseif (strcmp(files(i).name, 'fatal.xml'))
    %do nothing
  elseif (strcmp(files(i).name, 'convertedFormulas.xml') && exist('OCTAVE_VERSION'))
    %do nothing
  elseif (strcmp(files(i).name, 'convertedFormulasL2.xml') && exist('OCTAVE_VERSION'))
    %do nothing
  else
    model = TranslateSBML(['test-data', filesep, files(i).name]);
    if (~isempty(model))
      OutputSBML(model, [outdir, filesep, files(i).name]);
      if (compareFiles(['test-data', filesep, files(i).name], [outdir, filesep, files(i).name]))
        disp(sprintf('Output of %s failed', files(i).name));
        fail = fail + 1;
      end;
    end;
  end;
end;

 