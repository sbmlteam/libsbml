function buildLibSBMLOctave
% builds the libsbml - octave binding
% This script assumes that libsbml has been built and installed on your system.
%
% For Linux or Mac users this means that the libsbml library must be on
% the library path for your system.
%
% For windows users this means that libsbml must be built and the binaries (.dll
% and .lib files) for libsbml and all its dependencies must be located together
% in one directory. The build assumes libxml2 is the xml parser to be used.

% Filename    : buildLibSBMLOctave.m
% Description : build octave binding
% Author(s)   : SBML Team <sbml-team@caltech.edu>
% Organization: EMBL-EBI
% Created     : 2011-02-08
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

  [detected_os, matlab, root, bit64, writeAccess] = determine_system();

  matlab_dir = [root, filesep, 'src', filesep, 'bindings', filesep, 'matlab'];
  copyMatlabDir(matlab_dir, pwd);
  buildSBML;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% check what we are using
function [detected_os, matlab, root, bit64, writeAccess] = determine_system

  disp('Checking system ...');
  disp('Looking at software ...');
  % matlab = [0, 1]
  %     0 - octave
  %     1 - matlab
  if (strcmp(isoctave(), '0'))
    matlab = 1;
    error('The buildLibSBMLOctave function needs to run from Octave');
  else
    matlab = 0;
    disp('Octave detected');
  end;

  disp('Looking at OS ...');
  bit64 = 0;
  % detected_os = [0, 1, 2]
  %      0 - windows
  %      1 - mac
  %      2 - unix
  if (ispc())
    detected_os = 0;
    if (strcmp(computer(), 'PCWIN64') == 1)
      bit64 = 1;
      disp('Windows 64 bit OS detected');
    else
      disp('Windows 32 bit OS detected');
    end;
  elseif(ismac())
    detected_os = 1;
    disp('Mac OS detected');
  elseif(isunix())
    detected_os = 2;
    disp('Linux OS detected');
  else
    error('Unable to establish operating system');
  end;

  disp('Checking directory structure ...');
  % THIS WILL BE octave/matlab dependant
  % need to check which directory we are in
  % if we are in the src directory we should 
  % be in directory ../src/bindings/matlab
  % if not we should not really be building !!
  [remain, first] = fileparts(pwd);
  [remain1, second] = fileparts(remain);
  [remain2, third] = fileparts(remain1);

  if (matlab == 1)
    if (~strcmp(first, 'matlab'))
	  report_incorrect_dir(pwd, '/src/bindings/matlab');
    elseif (~strcmp(second, 'bindings'))
	  report_incorrect_dir(pwd, '/src/bindings/matlab');
    elseif (~strcmp(third, 'src'))
	  report_incorrect_dir(pwd, '/src/bindings/matlab');
    else
	  root = remain2;
	  disp('Expected directory structure found');
    end;
  else
    if (~strcmp(first, 'octave'))
	  report_incorrect_dir(pwd, '/src/bindings/octave');
    elseif (~strcmp(second, 'bindings'))
	  report_incorrect_dir(pwd, '/src/bindings/octave');
    elseif (~strcmp(third, 'src'))
	  report_incorrect_dir(pwd, '/src/bindings/octave');
    else
	  root = remain2;
	  disp('Expected directory structure found');
	end;
  end;

  % check whether we have write access to this directory
  writeAccess = 1;
  if (fopen('temp', 'w') == -1)
    writeAccess = 0;
  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% tell user we are in the wrong location
function report_incorrect_dir(this_dir, expected)

  message = sprintf('You are in directory %s \nbut should be in %s', this_dir, expected);
  error(message);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% creates a copy of the matlab binding directory with tests
function copied = copyMatlabDir(orig_dir, target_dir)
    
    copied = 0;

    cd(orig_dir);
    copyfile('TranslateSBML.c', target_dir);
    copyfile('OutputSBML.c', target_dir);
    copyfile('*.m', target_dir);
    copyfile('*.xml', target_dir);
    cd(target_dir);
    
    % create test dir
    testdir = fullfile(pwd, 'test');
    if (exist(testdir, 'dir') == 0)
      mkdir('test');
    end;
    cd('test');
    new_dir = pwd;
    
    %copy test files
    cd(orig_dir);
    cd('test');
    % copy all except testReadFromFile9
    copyfile('testReadFromFile1.m', new_dir);
    copyfile('testReadFromFile2.m', new_dir);
    copyfile('testReadFromFile3.m', new_dir);
    copyfile('testReadFromFile4.m', new_dir);
    copyfile('testReadFromFile5.m', new_dir);
    copyfile('testReadFromFile6.m', new_dir);
    copyfile('testReadFromFile7.m', new_dir);
    copyfile('testReadFromFile8.m', new_dir);
    copyfile('testReadFromFile10.m', new_dir);
    copyfile('testReadFromFile11.m', new_dir);
    copyfile('testReadFromFile12.m', new_dir);
    copyfile('testReadFromFile13.m', new_dir);
    copyfile('testReadFromFile14.m', new_dir);
    copyfile('testReadFromFile15.m', new_dir);
    copyfile('testBinding.m', new_dir);
    copyfile('testOutput.m', new_dir);
    copyfile('compareFiles.m', new_dir);
    
    % create test-data dir
    cd(new_dir);
    testdir = fullfile(pwd, 'test-data');
    if (exist(testdir, 'dir') == 0)
      mkdir('test-data');
    end;
    cd('test-data');
    new_dir = pwd;
    
    %copy test-data files
    cd(orig_dir);
    cd ('test');
    cd ('test-data');
    copyfile('*.xml', new_dir);
    
    %navigate to octave directory
    cd(target_dir);
    
    % put in some tests here
    copied = 1;
   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function t=isoctave()

  if exist('OCTAVE_VERSION')
    % Only Octave has this variable.
    t='1';
  else
    t='0';
  end;


