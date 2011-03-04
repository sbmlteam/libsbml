function installSBML
% Installs the MATLAB language interface for libSBML.
%
% This script assumes that the libsbml matlab binding executables files already
% exist; either because the user has built them using buildLibSBML (only
% in the src release) or the binding is being installed from an installer.
%
% Currently prebuilt executables are only available in the windows installers
% of libSBML.
%
% For Linux or Mac users this means that you need to build libsbml and then
% run the buildLibSBML script.


% Filename    : installSBML.m
% Description : install matlab binding
% Author(s)   : SBML Team <sbml-team@caltech.edu>
% Organization: EMBL-EBI
% Created     : 2011-02-08
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

% =========================================================================
% Main loop.
% =========================================================================

  disp(sprintf('\nInstalling the libSBML MATLAB interface.\n'));

  [matlab_octave, bit64]  = check_system();

  % check to see whether the executable exist
  % if they do not this function will halt the process asking the user
  % to build the executables
  [functioning] = checkForExecutables(matlab_octave);
  
  if (functioning == 1)
    % everything is working - add this directory to path and we are done
    addDir(pwd);
    [root, writeAccess, in_installer] = check_location(matlab_octave, functioning);
  else
    [root, writeAccess, in_installer] = check_location(matlab_octave, functioning);
    if ispc()
      % if we are not in an installer then something has gone wrong
      if (in_installer == 0)
        error('Error detected - please contact libsbml-team@caltech.edu for help');
      else
        install_win(matlab_octave, root, bit64, writeAccess);
      end;
    elseif ismac()
      install_mac();
    elseif islinux()
      install_linux();
    else
      error('OS not recognised');
    end;
  end;
  
  testInstallation(matlab_octave, in_installer);
  

% =========================================================================
% Support functions.
% =========================================================================

% 
% Assess our computing environment.
% -------------------------------------------------------------------------
function [matlab_octave, bit64] = check_system()
  disp('* Doing preliminary checks of runtime environment ...');

  if (strcmp(isoctave(), '0'))
    matlab_octave = 'MATLAB';
    disp('  - This appears to be MATLAB and not Octave.');
  else
    matlab_octave = 'Octave';
    disp('  - This appears to be Octave and not MATLAB.');
  end;

  bit64 = 32;
  if ispc()
    if strcmp(computer(), 'PCWIN64') == 1
      bit64 = 64;
      disp(sprintf('  - %s reports the OS is Windows 64-bit.', matlab_octave));
    else
      disp(sprintf('  - %s reports the OS is Windows 32-bit.', matlab_octave));
    end;
  elseif ismac()
    if strcmp(computer(), 'MACI64') == 1
      bit64 = 64;
      disp(sprintf('  - %s reports the OS is 64-bit MacOS.', matlab_octave));
    else
      % Reading http://www.mathworks.com/help/techdoc/ref/computer.html
      % it is still not clear to me what a non-64-bit MacOS will report.
      % Let's not assume the only other alternative is 32-bit, since we
      % actually don't care here.  Let's just say "macos".
      %
      disp(sprintf('  - %s reports the OS is MacOS.', matlab_octave));
    end;
  elseif isunix()
    if strcmp(computer(), 'GLNXA64') == 1
      bit64 = 64;
      disp(sprintf('  - %s reports the OS is 64-bit Linux.', matlab_octave));
    else
      disp(sprintf('  - %s reports the OS is 32-bit Linux.', matlab_octave));
    end;
  end;


%
% Assess our location in the file system.
% -------------------------------------------------------------------------
% Possible values returned:
%   LOCATION:
%     'installed'  -> installation directory
%     'source'    -> libsbml source tree
%   
%  WRITEACCESS:
%     1 -> we can write in this directory
%     0 -> we can't write in this directory
%
function [location, writeAccess, in_installer] = check_location(matlab_octave, ...
    functioning)
  
  myDisp('* Trying to establish our location ...', functioning);
  
  % This is where things get iffy.  There are a lot of possibilities, and 
  % we have to resort to heuristics.
  % 
  % Linux and Mac: we look for 2 possibilities
  % - installation dir ends in "libsbml/bindings/matlab"
  %   Detect it by looking for ../../VERSION.txt.
  %   Assume we're in .../share/libsbml/bindings/matlab and that our
  %   library is in   .../lib/
  %
  % - source dir ends in "libsbml/src/bindings/matlab"
  %   Detect it by looking for ../../../VERSION.txt.
  %   Assume our library is in ../../
  % 

  in_installer = 0;
  
  [remain, first] = fileparts(pwd);
  if strcmpi(matlab_octave, 'matlab')
    if ~strcmp(first, 'matlab')
      if ~ispc()
        error_incorrect_dir('matlab');
      else
        in_installer = 1;
      end;
    else
      myDisp('  - We are in the libSBML subdirectory for Matlab.', functioning);
    end;
  else
    if ~strcmp(first, 'octave')
      if ~ispc()
        error_incorrect_dir('octave');
      else
        in_installer = 1;
      end;
    else
      myDisp('  - We are in the libSBML subdirectory for Octave.', functioning);
    end;
  end;


  location = '';
  % if in_installer == 1 then we are in the windows installer but in
  % path provided by the user
  % checking further is pointless
  if (in_installer == 0)
    [above_bindings, bindings] = fileparts(remain);
    if exist(fullfile(above_bindings, 'VERSION.txt'))
      myDisp('  - We appear to be in the installation target directory.', functioning);  
      in_installer = 1;
      if ispc()
        location = above_bindings;
      else
        location = 'installed';
      end;
    else
      [libsbml_root, src] = fileparts(above_bindings);
      if exist(fullfile(libsbml_root, 'Makefile.in'))
        myDisp('  - We appear to be in the libSBML source tree.', functioning); 
        if ispc()
          location = libsbml_root;
        else
          location = 'source';
        end;
      else
        if ispc()
          % we might be in the windows installer but in a location the user chose
          % for the bindings
          % Makefile.in will not exist in this directory
          if (exist([pwd, filesep, 'Makefile.in']) == 0)
            in_installer = 1;
          else
             % We don't know where we are.
            if strcmpi(matlab_octave, 'MATLAB')
              error_incorrect_dir('matlab');
            else
              error_incorrect_dir('octave');
            end;         
          end;        
        else
          % We don't know where we are.
          if strcmpi(matlab_octave, 'MATLAB')
            error_incorrect_dir('matlab');
          else
            error_incorrect_dir('octave');
          end;
        end;
      end;
    end;
  end;
  
  % if we are in the windows installer but in a location the user chose
  % for the bindings
  % we need the user to tell use the root directory for the rest of libsbml
  % unless we are already functioning
  if (ispc() && functioning == 0 && in_installer == 1 && isempty(location))
    count = 1;
    while(exist(location, 'dir') == 0 && count < 3)
    location = input(sprintf('%s: ', ...
      'Please enter the location of the top-level libsbml directory'), 's');
    count = count + 1;
    end;
    if (exist(location, 'dir') == 0)
      error('Failed to find libsbml directory');
    end; 
  end;

  % Test that it looks like we have the expected pieces in this directory.
  % We don't want to assume particular paths, because we might be
  % getting run from the libSBML source tree or the installed copy of
  % the matlab bindings sources.  So, we test for just a couple of
  % things: the tail of the name of the directory in which this file is
  % located (should be either "matlab" or "octave") and the presence of
  % another file, "OutputSBML.c", which became part of libsbml at the
  % same time this new build scheme was introduced.

  our_name   = sprintf('%s.m', mfilename);
  other_name = 'OutputSBML.c';
  if ~exist(fullfile(pwd, our_name), 'file') ...
        || ~exist(fullfile(pwd, other_name), 'file')
    error_incorrect_dir('matlab');
  end;  

  % Check whether we have write access to this directory.

  fid = fopen('temp.txt', 'w');
  writeAccess = 1;
  if fid == -1
    myDisp('  - We do not have write access here -- will write elsewhere.', functioning);
    writeAccess = 0;
  else
    myDisp('  - We have write access here!  That makes us happy.', functioning);
    fclose(fid);
    delete('temp.txt');
  end;


%  
% install on windows
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function install_win(matlab_octave, root, bit64, writeAccess)

  % list the library files
  if (bit64 == 32)
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
  
  % if we can write here do - otherwise find somewhere else
  if (writeAccess == 1)% must be 1 0 for testing
    this_dir = pwd;
    user_dir = pwd;
  else
    this_dir = pwd;
    if strcmpi(matlab_octave, 'matlab')
      user_dir = userpath;
      user_dir = user_dir(1:length(user_dir)-1);
	  else
	    user_dir = matlabroot;
	  end;
  end;
 

  disp(sprintf('Copying library files to %s ...', user_dir)); 
  if (copyLibraries(this_dir, user_dir, lib, root, bit64, matlab_octave) == 1)
    disp('Copy library files successful');
    disp('All dependencies found');
  else
    error (sprintf('%s\nNot all dependencies could be found\n%s%s', ...
      'Error copying files', ...
      'expected the dependencies to be in ', pwd));
  end;
  
  if (writeAccess == 0)
    disp(sprintf('Copying matlab binding files to %s ...', user_dir)); 
    if (copyMatlabDir(this_dir, user_dir) == 1)
      disp('Copy matlab binding files successful');
    else
      error('Cannot copy matlab binding files on this system');
    end;
  end;
    
  addDir(pwd);


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% install on mac
function install_mac()

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% install on linux
function install_linux()

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
function success = checkForExecutables(matlab_octave)  
  disp('Checking for executables ...');
  transFile = strcat('TranslateSBML.', mexext());
  outFile = strcat('OutputSBML.', mexext());

  if strcmpi(matlab_octave, 'matlab')
    if (~(exist(transFile) ~= 0 && exist(outFile) ~= 0))     
      error(sprintf('%s\n%s', 'Executables not found', ...
      'Run the buildSBML script to build the relevant files'));
    elseif (~((strcmp(which(transFile), [pwd, filesep, transFile])) && ...
          (strcmp(which(outFile), [pwd, filesep, outFile]))))     
    % they exist but are they the right ones         
      error(sprintf('%s\n%s', 'Other executables from other installations found', ...
      'Run the buildSBML script to build the relevant files for this installation'));
    else
      disp('Executables found');
    end;
  else
    % octave is much more picky about whether the files exist
	% it wants to find the libraries at the same time
	% exist throws an exception if it cannot find them
    if (~(myExist(transFile) ~= 0 && myExist(outFile) ~= 0))     
	    error(sprintf('%s\n%s', 'Executables not found', ...
		  'Run the buildSBML script to build the relevant files'));
	  % which does not work in octave in the same way
	  % elseif (~((strcmp(which(transFile), pwd)) && ...
	  %			(strcmp(which(outFile), pwd))))     
	  % they exist but are they the right ones    
	  %  error(sprintf('%s\n%s', 'Other executables from other installations found', ...
	  %	'Run the buildLibSBML script to build the relevant files for this installation'));
    else
	    disp('Executables found');
	  end;
  end;
  
  success = doesItRun(matlab_octave);
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% test the installation
function success = testInstallation(matlab_octave, in_installer, inwin)
  disp(sprintf('\nTesting the installation.\n'));

  success = 1;

  try
    disp('checking for TranslateSBML');
    M = TranslateSBML('test.xml');
  catch
    disp(sprintf('Installation failed - %s cannot find all the libraries\n%s\n%s', ...
        matlab_octave, 'Add the path to the libraries to your path', ...
        'If this fails please contact the libsbml-team@caltech.edu'));
    success = 0;
  end;

  if strcmpi(matlab_octave, 'matlab')
    outFile = [tempdir, filesep, 'test-out.xml'];
  else
    if ispc()
      outFile = [tempdir, 'temp', filesep, 'test-out.xml'];
    else
      outFile = [tempdir, 'test-out.xml'];
    end;
  end;
  
  if (success == 1)
    try
      disp('checking OutputSBML');
      OutputSBML(M, outFile);
    catch
      disp(sprintf('Installation failed - %s cannot find all the libraries\n%s\n%s', ...
        matlab_octave, 'Add the path to the libraries to your path', ...
        'If this fails please contact the libsbml-team@caltech.edu'));
      success = 0;
    end;
  end;
  
  %if we are in the src tree run through all the tests
  if (in_installer == 0)
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
      pass = testOutput([tempdir, 'temp'], 1);
      cd ..;
      if (pass == 0)
        disp('OutputSBML successful');
      else
        disp('Output function present but problem detected. Seek help.');
        success = 0;
      end;
    end;
  end;
  
  if (success == 1)
    disp ('Installation successfully completed');
  end;
    
  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% test the installation
function success = doesItRun(matlab_octave)
    
  success = 1;
    
  try
    M = TranslateSBML('test.xml');
  catch
    success = 0;
  end;

  if strcmpi(matlab_octave, 'matlab')
    outFile = [tempdir, filesep, 'test-out.xml'];
  else
    if ispc()
      outFile = [tempdir, 'temp', filesep, 'test-out.xml'];
    else
      outFile = [tempdir, 'test-out.xml'];
    end;
  end;
      
  if (success == 1)
    try
      OutputSBML(M, outFile, 1);
    catch
      success = 0;
    end;
  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% copies the library files to given directory
function copied = copyLibraries(orig_dir, target_dir, lib, root, bit64, matlab_octave)
  
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
    if (bit64 == 32)
      if (rem(i,2) == 1)
        copyfile([root, filesep, 'win32', filesep, 'lib', filesep, lib{i}], pwd);
      else
        copyfile([root, filesep, 'win32', filesep, 'bin', filesep, lib{i}], pwd);
      end;
    else
      if (rem(i,2) == 1)
        copyfile([root, filesep, 'win64', filesep, 'lib', filesep, lib{i}], pwd);
      else
        copyfile([root, filesep, 'win64', filesep, 'bin', filesep, lib{i}], pwd);
      end;
    end;
  end;
 % all files should be in this dir
  disp('Checking for library files ...');
  copied = 1;
  if strcmpi(matlab_octave, 'matlab')	  
    for i = 1:8
      if (strcmp(which(lib{i}), [pwd, filesep, lib{i}]))
        disp(sprintf('%s found', lib{i}));
      else
        disp(sprintf('%s not found', lib{i}));
        copied = 0;
      end;
	  end; 
  else
    for i = 1:8
	    if (myExist(lib{i}) == 0)
		    disp(sprintf('%s not found', lib{i}));
        copied = 0;
      else
        disp(sprintf('%s found', lib{i}));
      end;
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
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function found = myExist(filename)

  found = 0;
  dirnames = dir(pwd);
  i = 1;
  while (found == 0 && i <= length(dirnames))
    if (dirnames(i).isdir == 0)
	  found = strcmp(dirnames(i).name, filename);
	end;
	i = i+1;
  end;

% 
% Print error about being in the wrong location.
% -------------------------------------------------------------------------
function error_incorrect_dir(expected)
  message = sprintf('\n%s\n%s%s%s\n%s\n%s%s%s\n%s\n', ...
      'This script needs to be invoked from the libSBML subdirectory ', ...
      'ending in "', expected, '". However, it is being invoked', ...
      'from the directory', '   "', pwd, '"', ...
      'instead.  Please change your working directory and re-run this script.');
  error(message);

  
  
%
% only display the message if we are running in non-silent mode
%
  function myDisp(message, silent);
    if (silent == 0)
      disp(message);
    end;