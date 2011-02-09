function installLibSBML
% installs the libsbml - matlab binding
% This script assumes that the libsbml matlab binding executables files already
% exist; either because the user has built them using buildLibSBML (only
% in the src release) or the binding is being installed from an installer.
%
% Currently prebuilt executables are only available in the windows installers
% of libSBML.
%
% For Linux or Mac users this means that you need to build libsbml and then
% run the buildLibSBML script.


% Filename    : installLibSBML.m
% Description : install matlab binding
% Author(s)   : SBML Team <sbml-team@caltech.edu>
% Organization: EMBL-EBI
% Created     : 2011-02-08
% Revision    : $Id:  $
% $HeadURL:  $
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


  % check to see whether the executable exist
  checkForExecutables();

  [detected_os, matlab, root, bit64, writeAccess, in_win_installer] = determine_system();

  switch detected_os
    case 0
      install_win(matlab, root, bit64, writeAccess, in_win_installer);
    case 1
      install_mac(matlab, root);
    case 2
      install_linux(matlab, root);
    otherwise
      error('OS not recognised');
  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% check what we are using
function [detected_os, matlab, root, bit64, writeAccess, in_win_installer] = determine_system

  disp('Checking system ...');
  disp('Looking at software ...');
  % matlab = [0, 1]
  %     0 - octave
  %     1 - matlab
  if (strcmp(isoctave(), '0'))
    matlab = 1;
    disp('MATLAB detected');
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

  root = '';
  in_win_installer = 0;
  if (detected_os == 0)
    disp('Checking directory structure ...');
    
    % are we in the windows installer
    % should be in directory ../bindings/matlab
    [remain, first] = fileparts(pwd);
    [remain1, second] = fileparts(remain);
    [remain2, third] = fileparts(remain1);
   
    if (strcmp(first, 'matlab') && strcmp(second, 'bindings'))
      if (~strcmp(third, 'src'))
        root = remain1;
        in_win_installer = 1;
      else
        % we might be in the windows installer but in a location the user chose
        % for the matlab bindings
        % buildLibSBML will not exist in this directory
        if (exist([pwd, filesep, 'buildLibSBML.m']) == 0)
          in_win_installer = 1;
          count = 1;
          while(exist(root, 'dir') == 0 && count < 3)
            root = input(sprintf('%s: ', ...
              'Please enter the location of the top-level libsbml directory'), 's');
            count = count + 1;
          end;
         if (exist(root, 'dir') == 0)
           error('Failed to find libsbml directory');
         end;
        end;
      end;
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% install on windows
function install_win(ismatlab, root, bit64, writeAccess, in_win_installer)

  % list the library files
  if (bit64 == 0)
    lib{1} = ['libsbml.lib'];
    lib{2} = ['libsbml.dll'];
    lib{3} = ['libxml2.lib'];
    lib{4} = ['libxml2.dll'];
    lib{5} = ['iconv.lib'];
    lib{6} = ['iconv.dll'];
    lib{7} = ['bzip2.lib'];
    lib{8} = ['bzip2.dll'];
  else
    lib{1} = ['libsbml.lib'];
    lib{2} = ['libsbml.dll'];
    lib{3} = ['libxml2.lib'];
    lib{4} = ['libxml2.dll'];
    lib{5} = ['libiconv.lib'];
    lib{6} = ['libiconv.dll'];
    lib{7} = ['bzip2.lib'];
    lib{8} = ['libbz2.dll'];
  end;

  % if we are in windows installer
  if (in_win_installer == 1)
    % if writeAccess
    % copy library files to this dir
    if (writeAccess == 1)% must be 1 0 for testing
      this_dir = pwd;
      user_dir = pwd;
      disp(sprintf('Copying library files to %s ...', user_dir)); 
      if (copyLibraries(this_dir, user_dir, lib, root, bit64) == 1)
      disp('Copy library files successful');
      disp('All dependencies found');
      else
      error (sprintf('%s\nNot all dependencies could be found\n%s%s', ...
       'Error copying files', ...
      'expected the dependencies to be in ', pwd));
      end;
    else
      % copy executables/scripts and library files 
      % cd to that dir
      this_dir = pwd;
      user_dir = userpath;
      user_dir = user_dir(1:length(user_dir)-1);
      disp(sprintf('Copying library files to %s ...', user_dir)); 
      if (copyLibraries(this_dir, user_dir, lib, root, bit64) == 1)
        disp('Copy library files successful');
        disp('All dependencies found');
      else
        error (sprintf('%s\nNot all dependencies could be found\n%s%s', ...
        'Error copying files', ...
        'expected the dependencies to be in ', pwd));
      end;
      disp(sprintf('Copying matlab binding files to %s ...', user_dir)); 
      if (copyMatlabDir(this_dir, user_dir) == 1)
        disp('Copy matlab binding files successful');
      else
        error('Cannot copy matlab binding files on this system');
      end;
    end;
  end;

  addDir(pwd);

  success = testInstallation(ismatlab, in_win_installer);

  if (success == 1)
  disp ('Installation completed');
  end;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% install on mac
function install_mac(ismatlab, root)

  % check that libsbml is in usr/local/lib directory
  bin_dir = [];

  for i=1:length(strfind(pwd, filesep))
      bin_dir = [bin_dir, '..', filesep];
  end;
  bin_dir = [bin_dir, 'usr', filesep, 'local', filesep, 'lib'];
  disp('Checking for libraries ...');
  % check that the library files are all there
  lib{1} = [bin_dir, filesep, 'libsbml.dylib'];
  if (exist(lib{1}) == 0)
      disp('libsbml.dylib could not be found');
      new_bin_dir = input(sprintf('\n\n%s\n%s\n%s\n%s', ...
          'The install process assumes that libsbml', ...
          'has been installed at usr/local', ...
          'if libsbml.dylib is in another directory please enter the ', ...
          'full path to reach the directory from this directory: '), 's');
      lib{2} = [new_bin_dir, filesep, 'libsbml.dylib'];
      
      if (exist(lib{2}) == 0)
          error('libsbml.dylib could not be found');
      else
        lib{1} = lib{2};
        bin_dir = new_bin_dir;
      end;
  end;
  
  addDir(pwd);

  success = testInstallation(ismatlab, 0);
  
  if (success == 1)
    disp ('Installation completed');
  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% install on linux
function install_linux(ismatlab, root)

  % check that libsbml is in usr/local/lib directory
  bin_dir = [];

  for i=1:length(strfind(pwd, filesep))
      bin_dir = [bin_dir, '..', filesep];
  end;
  bin_dir = [bin_dir, 'usr', filesep, 'local', filesep, 'lib'];
  disp('Checking for libraries ...');
  % check that the library files are all there
  lib{1} = [bin_dir, filesep, 'libsbml.so'];
  if (exist(lib{1}) == 0)
      disp('libsbml.so could not be found');
      new_bin_dir = input(sprintf('\n\n%s\n%s\n%s\n%s', ...
          'The install process assumes that libsbml', ...
          'has been installed at usr/local', ...
          'if libsbml.so is in another directory please enter the ', ...
          'full path to reach the directory from this directory: '), 's');
      lib{2} = [new_bin_dir, filesep, 'libsbml.so'];
      
      if (exist(lib{2}) == 0)
          error('libsbml.so could not be found');
      else
        lib{1} = lib{2};
        bin_dir = new_bin_dir;
      end;
  end;
  
  addDir(pwd);
 
  success = testInstallation(ismatlab, 0);
  
  if (success == 1)
    disp ('Installation completed');
  end;
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% add directory to the matlab path
function addDir(name)

  disp(sprintf('adding %s to the path', name));
  addpath(name);
  if (savepath ~= 0)
    error(sprintf('adding %s failed', name));
  end;

  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% check for executables and that they are right ones
function checkForExecutables()  
  disp('Checking for executables ...');
  transFile = strcat('TranslateSBML.', mexext());
  outFile = strcat('OutputSBML.', mexext());

  if (~(exist(transFile) == 3 && ...
      exist(outFile) == 3))     
    error(sprintf('%s\n%s', 'Executables not found', ...
      'Run the buildLibSBML script to build the relevant files'));
  elseif (~((strcmp(which(transFile), [pwd, filesep, transFile])) && ...
            (strcmp(which(outFile), [pwd, filesep, outFile]))))     
    % they exist are the the right ones         
    error(sprintf('%s\n%s', 'Other executables from other installations found', ...
            'Run the buildLibSBML script to build the relevant files for this installation'));
  else
    disp('Executables found');
  end;
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% test the installation
function success = testInstallation(ismatlab, in_win_installer)
  if (ismatlab)  
    success = 1;
    try
      disp('checking for TranslateSBML');
      M = TranslateSBML('test.xml');
    catch
      disp(sprintf('Installation failed - MATLAB cannot find all the libraries\n%s', ...
          'Add the path to the libraries to your matlab path'));
      success = 0;
    end;

    outFile = [tempdir, filesep, 'test-out.xml'];
    if (success == 1)
      try
        disp('checking OutputSBML');
        OutputSBML(M, outFile);
      catch
      disp(sprintf('Installation failed - MATLAB cannot find all the libraries\n%s', ...
          'Add the path to the libraries to your matlab path'));
        success = 0;
      end;
    end;
    if (in_win_installer == 0)
      if (success == 1)
        disp ('running tests for TranslateSBML');
        cd test;
        pass = testBinding(1);
        cd ..;
        if (pass == 0)
          disp('TranslateSBML successful');
        else
          disp('Binding present but problem detected. Seek help.');
          success = 0;
        end;
      end;

      if (success == 1)
        disp('running tests for OutputSBML');
        cd test;
        pass = testOutput(tempdir);
        cd ..;
        if (pass == 0)
          disp('OutputSBML successful');
        else
          disp('Output function present but problem detected. Seek help.');
          success = 0;
        end;
      end;
    end;

  else
    % TO DO
    disp('install on octave');
  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% copies the library files to given directory
function copied = copyLibraries(orig_dir, target_dir, lib, root, bit64)
  
  copied = 0;
  cd (target_dir);
  
  % if we moving to another location create a libsbml directory
  % if we are staying copy here
  if (~strcmp(orig_dir, target_dir))
    if (exist('libsbml', 'dir') == 0)
      mkdir('libsbml');
    end;
    cd libsbml;
  end;
  % copy the necessary files
  for i=1:8
    if (bit64 == 0)
      if (rem(i,2) == 1)
        copyfile([root, filesep, 'win32', filesep, 'lib', filesep, lib{i}]);
      else
        copyfile([root, filesep, 'win32', filesep, 'bin', filesep, lib{i}]);
      end;
    else
      if (rem(i,2) == 1)
        copyfile([root, filesep, 'win64', filesep, 'lib', filesep, lib{i}]);
      else
        copyfile([root, filesep, 'win64', filesep, 'bin', filesep, lib{i}]);
      end;
    end;
  end;
 % all files should be in this dir
  disp('Checking for library files ...');
  copied = 1;
  for i = 1:8
    if (strcmp(which(lib{i}), [pwd, filesep, lib{i}]))
      disp(sprintf('%s found', lib{i}));
    else
      disp(sprintf('%s not found', lib{i}));
      copied = 0;
    end;
  end; 
  cd(orig_dir);
  
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  % creates a copy of the matlab binding directory with tests
  function copied = copyMatlabDir(orig_dir, target_dir)
    
    copied = 0;
    cd (target_dir);
    % create libsbml dir
    if (exist('libsbml', 'dir') == 0)
      mkdir('libsbml');
    end;
    cd libsbml;
    new_dir = pwd;

    %copy files to libsbml
    cd(orig_dir);
    copyfile('TranslateSBML.*', new_dir);
    copyfile('OutputSBML.*', new_dir);
    copyfile('*.m', new_dir);
    copyfile('*.xml', new_dir);
    cd(new_dir);

    % put in some tests here
    copied = 1;
    

