function fail = testOutput(outdir, in_installer, fbcEnabled)

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

if (~isfolder(outdir))
  mkdir (outdir);
end
is_octave = 0;
if exist('OCTAVE_VERSION')
  is_octave = 1;
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

disp('Testing output model');

fail = 0;
test = 0;
for i=1:length(files)
  disp(sprintf('File: %s', files(i).name));
  if isInGroup(files(i).name, 'expected', is_octave) == 0 || ...
          isInGroup(files(i).name, 'readerrors', is_octave)
        disp(sprintf('Skipping %s', files(i).name));
  elseif ~fbcEnabled && isInGroup(files(i).name, 'fbc', is_octave)
        disp(sprintf('Skipping %s', files(i).name));
    %donothing
  else
    model = [];
    disp(sprintf('Reading  %s', files(i).name));
    model = TranslateSBML(['test-data', filesep, files(i).name], 0, 0);
    if (~isempty(model))
        disp(sprintf('Printing  %s', files(i).name));
        
      if (in_installer == 1)
        OutputSBML(model, [outdir, filesep, files(i).name], in_installer);
      else
        OutputSBML(model, [outdir, filesep, files(i).name]);
      end
      test = test + 1;
      if (compareFiles(['test-data', filesep, files(i).name], [outdir, filesep, files(i).name]))
        disp(sprintf('Output of %s failed', files(i).name));
        fail = fail + 1;
      end
    end
  end
end

if (fbcEnabled)
    % test new arguments to Translate/Output
    filename = ['test-data', filesep, 'fbcV2Labels.xml'];
    outfile = [outdir, filesep, 'fbcV2Labels.xml'];
    outfile1 = [outdir, filesep, 'fbcV2Labels-ids.xml'];
    outfile2 = [outdir, filesep, 'fbcV2Labels-explicit.xml'];
    model = TranslateSBML(filename);
    if (~isempty(model))
        disp(sprintf('Printing  %s', filename));
        OutputSBML(model, outfile);
    end;
    test = test + 1;
    if (compareFiles(filename, outfile))
        disp(sprintf('Output of %s failed', outfile));
        fail = fail + 1;
    end;
    model = TranslateSBML(filename, 0, 0, [1,0]);
    if (~isempty(model))
        disp(sprintf('Printing  %s', outfile1));
        OutputSBML(model, outfile1, 0, 0, [1,0]);
    end;
    test = test + 1;
    if (compareFiles(filename, outfile1))
        disp(sprintf('Output of %s failed', outfile1));
        fail = fail + 1;
    end;
    model = TranslateSBML(filename, 0, 0, [0,1]);
    if (~isempty(model))
        disp(sprintf('Printing  %s', outfile2));
        OutputSBML(model, outfile2, 0, 0, [0,1]);
    end;
    test = test + 1;
    if (compareFiles(filename, outfile2))
        disp(sprintf('Output of %s failed', outfile2));
        fail = fail + 1;
    end
end % fbc enabled

if (isEnabled('qual'))
    disp('Reading qual');
    filename = ['test-data', filesep, 'qual.xml'];    
    outfile = [outdir, filesep, 'qual.xml'];

    model = TranslateSBML(filename);
    if (~isempty(model))
        disp(sprintf('Printing  %s', filename));
        OutputSBML(model, outfile);
    end;
    test = test + 1;
    if (compareFiles(filename, outfile))
        disp(sprintf('Output of %s failed', outfile));
        fail = fail + 1;
    end;

end; 

if (isEnabled('groups'))
    disp('Reading groups-example1');
    filename = ['test-data', filesep, 'groups-example1.xml'];    
    outfile = [outdir, filesep, 'groups-example1.xml'];

    model = TranslateSBML(filename);
    if (~isempty(model))
        disp(sprintf('Printing  %s', filename));
        OutputSBML(model, outfile);
    end;
    test = test + 1;
    if (compareFiles(filename, outfile))
        disp(sprintf('Output of %s failed', outfile));
        fail = fail + 1;
    end;
end; 

if (isEnabled('groups') && isEnabled('fbc'))
    disp('Reading fbc_groups');
    filename = ['test-data', filesep, 'fbc_groups.xml'];    
    outfile = [outdir, filesep, 'fbc_groups.xml'];

    model = TranslateSBML(filename);
    if (~isempty(model))
        disp(sprintf('Printing  %s', filename));
        OutputSBML(model, outfile);
    end;
    test = test + 1;
    if (compareFiles(filename, outfile))
        disp(sprintf('Output of %s failed', outfile));
        fail = fail + 1;
    end;
end; 

if (isEnabled('qual') && isEnabled('fbc'))
    disp('Reading fbc_qual');
    filename = ['test-data', filesep, 'fbc_qual.xml'];    
    outfile = [outdir, filesep, 'fbc_qual.xml'];

    model = TranslateSBML(filename);
    if (~isempty(model))
        disp(sprintf('Printing  %s', filename));
        OutputSBML(model, outfile);
    end;
    test = test + 1;
    if (compareFiles(filename, outfile))
        disp(sprintf('Output of %s failed', outfile));
        fail = fail + 1;
    end;
end; 

if (isEnabled('groups') && isEnabled('fbc') && isEnabled('qual'))
    disp('Reading fbc_qual_groups');
    filename = ['test-data', filesep, 'fbc_qual_groups.xml'];    
    outfile = [outdir, filesep, 'fbc_qual_groups.xml'];

    model = TranslateSBML(filename);
    if (~isempty(model))
        disp(sprintf('Printing  %s', filename));
        OutputSBML(model, outfile);
    end;
    test = test + 1;
    if (compareFiles(filename, outfile))
        disp(sprintf('Output of %s failed', outfile));
        fail = fail + 1;
    end;
end; 

if (isEnabled('groups') && isEnabled('qual'))
    disp('Reading groups_qual');
    filename = ['test-data', filesep, 'groups_qual.xml'];    
    outfile = [outdir, filesep, 'groups_qual.xml'];

    model = TranslateSBML(filename);
    if (~isempty(model))
        disp(sprintf('Printing  %s', filename));
        OutputSBML(model, outfile);
    end;
    test = test + 1;
    if (compareFiles(filename, outfile))
        disp(sprintf('Output of %s failed', outfile));
        fail = fail + 1;
    end;
end; 


disp ('************************************');
disp('Overall tests in testOutput:');
disp(sprintf('Number tests: %d', test));
disp(sprintf('Number fails: %d', fail));
disp(sprintf('Pass rate: %d%%\n', ((test-fail)/test)*100));


disp('Testing invalid model structures');
test = 0;
invalidFail = 0;

test = test + 2;
m = [];
[v, mess] = isSBML_Model(m);

expected = sprintf('Invalid Model structure');
if v ~= 0 || ~strcmp(mess, expected)
  invalidFail = invalidFail + 1;
  disp('empty [] failed');
end;

try
  OutputSBML(m, [outdir, filesep, 'temp.xml']);
  invalidFail = invalidFail + 1;
  disp('empty [] write failed');
catch
end;

test = test + 2;
m = struct();
[v, mess] = isSBML_Model(m);

expected = sprintf('model missing typecode field');
if v ~= 0 || ~strcmp(mess, expected)
  invalidFail = invalidFail + 1;
  disp('empty structure failed');
end;

try
  OutputSBML(m, [outdir, filesep, 'temp.xml']);
  invalidFail = invalidFail + 1;
  disp('empty structure write failed');
catch
end;

disp ('************************************');
disp('Overall invalid tests in testOutput:');
disp(sprintf('Number tests: %d', test));
disp(sprintf('Number fails: %d', invalidFail));
disp(sprintf('Pass rate: %d%%\n', ((test-invalidFail)/test)*100));

fail = fail + invalidFail;

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% unexpected files cause problems

function isInGroup = isInGroup(filename, group_name, is_octave)

expected_files = { ...
'algebraicRules.xml', ...
'both.xml', ...
'convertedFormulas.xml', ...
'convertedFormulas2.xml', ...
'convertedFormulasL2.xml', ...
'csymbolAvogadro.xml', ...
'csymbolDelay.xml', ...
'csymbolTime-reaction-l2.xml', ...
'csymbolTime.xml', ...
'errors.xml', ...
'fatal.xml', ...
'fbc.xml', ...
'fbcV2.xml', ...
'fbcV3.xml', ...
'fbcV3_1.xml', ...
'fbc_kvp.xml', ...
'fbcL3V2V1.xml', ...
'fbcL3V2V2.xml', ...
'funcDefsWithInitialAssignments.xml', ...
'functionDefinition.xml', ...
'initialAssignments.xml', ...
'l1v1-branch.xml', ...
'l1v1-minimal.xml', ...
'l1v1-rules.xml', ...
'l1v1-units.xml', ...
'l1v1.xml', ...
'l1v2-all.xml', ...
'l2v1-all.xml', ...
'l2v1-allelements.xml', ...
'l2v1-assignment.xml', ...
'l2v2-all.xml', ...
'l2v2-newComponents.xml', ...
'l2v2-newelements.xml', ...
'l2v3-all.xml', ...
'l2v3-newelements.xml', ...
'l2v3-newMath.xml', ...
'l2v4-all.xml', ...
'l2v5-all.xml', ...
'l3v1core.xml', ...
'math_no_arguments.xml', ...
'nestedPiecewise.xml', ...
'none.xml', ...
'nonLogicals.xml', ...
'notes_annotations.xml', ...
'piecewise.xml', ...
'rateRules.xml', ...
'readerror.xml', ...
'test-greek.xml', ...
 'l3v2core.xml', ...
 'l3v2-no-model.xml', ...
 'l3v2-empty-math.xml', ...
 'l3v2-empty-event.xml', ...
 'l3v2-newmath.xml', ...
 };

readerrors_files = {'readerror.xml', 'fatal.xml'};
fbc_files = {'fbc.xml', 'fbcV2.xml', 'fbcL3V2V1.xml', 'fbcL3V2V2.xml', ...
    'fbcV3.xml', 'fbc_kvp.xml'};

if is_octave == 0
    group = eval(group_name + "_files");
else
    mygrp = strcat(group_name, "_files");
    group = eval(mygrp);
end
if sum(ismember(group, filename)) == 1
  isInGroup = 1;
else
  isInGroup = 0;
end


