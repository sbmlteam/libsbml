function buildSBML
% Builds the MATLAB language interface for libSBML.
%
% This script is meant to be invoked from libSBML's MATLAB bindings
% source directory.  LibSBML must already have been compiled and
% installed on your system.  This script makes the following
% assumptions:
%
% * Linux and Mac systems: the compiled libSBML library must be on the
%   appropriate library search paths, and/or the appropriate environment
%   variables must have been set so that programs such as MATLAB can
%   load the library dynamically.
%
% * Windows systems: the libSBML binaries (.dll and .lib files) and its
%   dependencies (such as the XML parser library being used) must be
%   located together in the same directory.  This script also assumes
%   that libSBML was configured to use the libxml2 XML parser library.
%   (This assumption is under Windows only.)
%
% After this script is executed successfully, a second step is necessary
% to install the results.  This second step is performed by the
% "installSBML" script found in the same location as this script.
%
% (File $Revision$ of $Date::                           $

% Filename    : buildSBML.m
% Description : Build MATLAB binding.
% Author(s)   : SBML Team <sbml-team@caltech.edu>
% Organization: EMBL-EBI, Caltech
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
% This library is free software; you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License as
% published by the Free Software Foundation.  A copy of the license
% agreement is provided in the file named "LICENSE.txt" included with
% this software distribution and also available online as
% http://sbml.org/software/libsbml/license.html


% =========================================================================
% Main loop.
% =========================================================================

  disp(sprintf('\nConstructing the libSBML MATLAB interface.\n'));

  [matlab_octave, bit64]  = check_system();
  [location, writeAccess] = check_location(matlab_octave);

  if ispc()
    build_win(matlab_octave, location, writeAccess, bit64);
  elseif ismac() || isunix()
    build_unix(matlab_octave, location, writeAccess, bit64);
  else
    message = sprintf('\n%s\n%s\n', ...
      'Unable to determine the type of operating system in use.', ...
      'Please contact libsbml-team@caltech.edu to help resolve this problem.');
      error(message);
  end;

  disp(sprintf('\n%s%s\n', 'Successfully finished. ', ...
               'If appropriate, please run "installSBML" next.'));


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
function [location, writeAccess] = check_location(matlab_octave)
  disp('* Trying to establish our location ...');
  
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

  [remain, first] = fileparts(pwd);
  if strcmpi(matlab_octave, 'matlab')
    if ~strcmp(first, 'matlab')
      error_incorrect_dir('matlab');
    else
      disp('  - We are in the libSBML subdirectory for Matlab.');
    end;
  else
    if ~strcmp(first, 'octave')
      error_incorrect_dir('octave');
    else
      disp('  - We are in the libSBML subdirectory for Octave.');
    end;
  end;

  [remain, above_bindings] = fileparts(remain);
  if exist(fullfile(above_bindings, 'VERSION.txt'))
    disp('  - We appear to be in the installation target directory.');    
    location = 'installed';
  else
    [libsbml_root, src] = fileparts(remain);
    if exist(fullfile(libsbml_root, 'VERSION.txt'))
      disp('  - We appear to be in the libSBML source tree.'); 
      if ispc()
        location = libsbml_root;
      else
        location = 'source';
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

  writeAccess = 1;
  if fopen('temp', 'w') == -1
    disp('  - We do not have write access here -- will write elsewhere.');
    writeAccess = 0;
  else
    disp('  - We have write access here!  That makes us happy.');
  end;


% 
% Find include and library dirs (Linux & Mac case).
% -------------------------------------------------------------------------
% Return values:
%   INCLUDE -> the full path to the include directory
%   LIB     -> the full path to the libsbml library directory
%
function [include, lib] = find_unix_dirs(location, bit64)
  disp('* Locating libSBML library and include files ...');

  % The 'location' argument guides us:
  % 'installed'  -> installation directory
  %    look for libsbml.so or libsbml.dylib in ../../../lib{64}/
  % 'source'    -> libsbml source tree
  %    look for libsbml.so or libsbml.dylib in ../../.libs/
  
  if ismac()
    libname = 'libsbml.dylib';
  else
    libname = 'libsbml.so';  
  end;

  if strcmpi(location, 'source')
    [parent, here] = fileparts(pwd);     % ..
    [parent, here] = fileparts(parent);  % ..
    lib = fullfile(parent, '.libs');
    libfile = fullfile(lib, libname);
    if exist(libfile)
      disp(sprintf('  - Found %s', libfile));
    else
      lib = 'unfound';
    end;

    % In our source tree on unix systems, the libsbml build process
    % puts a copy of the include directory in [libsbml]/include.
    %
    [parent, here] = fileparts(parent);  % ..
    include = fullfile(parent, 'include');
    if exist(include)
      disp(sprintf('  - Include dir will be %s', include));
    else
      error_incorrect_dir('matlab');
    end;
  else
    % location is 'installed'
    [parent, here] = fileparts(pwd);     % ..
    [parent, here] = fileparts(parent);  % ..
    [parent, here] = fileparts(parent);  % ..
    lib = fullfile(parent, 'lib');
    libfile = fullfile(lib, libname);
    if exist(libfile)
      disp(sprintf('  - Found %s', libfile));
    else
      if bit64 == 64
        % Try one more common alternative.
        lib = fullfile(parent, 'lib64');
        libfile = fullfile(lib, libname);
        if exist(libfile)
          disp(sprintf('  - Found %s', libfile));
        else
          lib = 'unfound';
        end;
      end;
    end;

    % In the installed target directory, include will be something 
    % like /usr/local/include
    %
    include = fullfile(parent, 'include');
    if exist(include)
      disp(sprintf('  - Found %s', include));
    else
      error_incorrect_dir('matlab');
    end;
  end;


% 
% Drive the build process (Windows version).
% -------------------------------------------------------------------------
function build_win(matlab_octave, root, writeAccess, bit64)

  disp('Phase 2: tests for libraries and other dependencies ...');
  % check that the win/bin directory exists
  bin_dir = [root, filesep, 'win', filesep, 'bin'];
  disp(sprintf('  - Checking for the existence of the %s directory ...\n', bin_dir));
  if (exist(bin_dir, 'dir') ~= 7)
      disp(sprintf('%s directory could not be found\n\n%s\n%s %s', bin_dir, ... 
          'The build process assumes that the libsbml binaries', ...
          'exist at', bin_dir));
      message = sprintf('\n%s\n%s', ...
          'if they are in another directory please enter the ', ...
          'full path to reach the directory from this directory: ');
      new_bin_dir = input(message, 's');
        
      if (exist(new_bin_dir, 'dir') == 0)
          error('libraries could not be found');
      else
        bin_dir = new_bin_dir;
      end;
  end;

  disp('  - Checking for the presence of needed libraries ...');
  if (bit64 == 32)
    % check that the library files are all there
    lib{1} = [bin_dir, filesep, 'libsbml.lib'];
    lib{2} = [bin_dir, filesep, 'libsbml.dll'];
    lib{3} = [bin_dir, filesep, 'libxml2.lib'];
    lib{4} = [bin_dir, filesep, 'libxml2.dll'];
    lib{5} = [bin_dir, filesep, 'iconv.lib'];
    lib{6} = [bin_dir, filesep, 'iconv.dll'];
    lib{7} = [bin_dir, filesep, 'bzip2.lib'];
    lib{8} = [bin_dir, filesep, 'bzip2.dll'];
  else
    % check that the library files are all there
    lib{1} = [bin_dir, filesep, 'libsbml.lib'];
    lib{2} = [bin_dir, filesep, 'libsbml.dll'];
    lib{3} = [bin_dir, filesep, 'libxml2.lib'];
    lib{4} = [bin_dir, filesep, 'libxml2.dll'];
    lib{5} = [bin_dir, filesep, 'libiconv.lib'];
    lib{6} = [bin_dir, filesep, 'libiconv.dll'];
    lib{7} = [bin_dir, filesep, 'bzip2.lib'];
    lib{8} = [bin_dir, filesep, 'libbz2.dll'];
  end;

  found = 1;
  for i = 1:8
    if (exist(lib{i}) ~= 0)
      disp(sprintf('%s found', lib{i}));
    else
      disp(sprintf('%s not found', lib{i}));
      found = 0;
    end;
  end; 

  if (found == 0)
    error (sprintf('Not all dependencies could be found\n%s%s', ...
    'expected the dependencies to be in ', bin_dir));
  else
    disp('  - All dependencies found.  Good.');
  end;
  
  % check that the include directory exists
  include_dir = [root, filesep, 'include'];
  disp(sprintf('  - Checking for the existence of the %s directory ...\n', include_dir));
  if (exist(include_dir, 'dir') ~= 7)
      disp(sprintf('%s directory could not be found\n\n%s\n%s %s', include_dir, ... 
          'The build process assumes that the libsbml include files', ...
          'exist at', include_dir));
      message = sprintf('\n%s\n%s', ...
          'if they are in another directory please enter the ', ...
          'full path to reach the directory from this directory: ');
      new_inc_dir = input(message, 's');
        
      if (exist(new_inc_dir, 'dir') == 0)
          error('include files could not be found');
      else
        include_dir = new_inc_dir;
      end;
  end;
  
  % if we do not have write access need to find somewhere else to build
  if (writeAccess == 0)% must be 0; 1 is for testing
    % create a new dir in the users path
    this_dir = pwd;
    if (matlab_octave == 'MATLAB')
      user_dir = userpath;
      user_dir = user_dir(1:length(user_dir)-1);
    else
      % This is Octave.  Octave doesn't have 'userpath'.
      user_dir = tempdir;
    end;
    disp(sprintf('  - Copying library files to %s ...', user_dir)); 
    if (copyLibraries(this_dir, user_dir, lib) == 1)
      disp('  - Copying of library files successful.');
    else
      error('Cannot copy library files on this system');
    end;
    disp(sprintf('  - Copying MATLAB binding files to %s ...', user_dir)); 
    if (copyMatlabDir(this_dir, user_dir) == 1)
      disp('- Copying of MATLAB binding files successful');
    else
      error('Cannot copy matlab binding files on this system');
    end;
  else
    this_dir = pwd;
    user_dir = pwd;
    % copy the library files to here
    disp(sprintf('  - Copying library files to %s ...', user_dir)); 
    if (copyLibraries(this_dir, user_dir, lib) == 1)
      disp('  - Copying of library files successful');
    else
      error('Cannot copy library files on this system');
    end;
    
  end; 
    
  % build the files
  compile_mex(include_dir, lib{1}, matlab_octave);


% 
% Drive the build process (Mac and Linux version).
% -------------------------------------------------------------------------
function build_unix(matlab_octave, location, writeAccess, bit64)

  [include, lib] = find_unix_dirs(location, bit64);

  if writeAccess == 1
    % We can write to the current directory.  Our job is easy-peasy.
    % 
    compile_mex(include, lib, matlab_octave);
  else
    % We don't have write access to this directory.  Copy the files 
    % somewhere else, relocate to there, and then try building.
    %
    working_dir = find_working_dir(matlab_octave);
    current_dir = pwd;
    copy_matlab_dir(current_dir, working_dir);
    cd(working_dir);
    compile_mex(include, lib, matlab_octave);
    cd(current_dir);
  end;


% 
% Run mex/mkoctfile.
% -------------------------------------------------------------------------
function compile_mex(include_dir, library_dir, matlab_octave)
  disp(sprintf('* Creating mex files in %s', pwd));  

  inc_arg    = ['-I', include_dir];
  lib_arg    = ['-L', library_dir];
  added_args = [' '];

  if ismac() || isunix()
    added_args = ['-lsbml'];
  end;

  % The messy file handle stuff is because this seems to be the best way to 
  % be able to pass arguments to the feval function.

  if strcmpi(matlab_octave, 'matlab')
    % on windows the command needs to be different
    if ispc()
      fhandle = @mex;
      disp('  - Building TranslateSBML ...');
      feval(fhandle, 'TranslateSBML.c', inc_arg, library_dir);
      disp('  - Building OutputSBML ...');
      feval(fhandle, 'OutputSBML.c', inc_arg, library_dir);
    else
      fhandle = @mex;
      disp('  - Building TranslateSBML ...');
      feval(fhandle, 'TranslateSBML.c', inc_arg, lib_arg, added_args);
      disp('  - Building OutputSBML ...');
      feval(fhandle, 'OutputSBML.c', inc_arg, lib_arg, added_args);
    end;
  else
    if ispc()
      fhandle = @mkoctfile;
      disp('  - Building TranslateSBML ...');
      feval(fhandle, '--mex', 'TranslateSBML.c', '-DUSE_OCTAVE', inc_arg, ...
            '-lbz2', '-lz', library_dir); 
      disp('  - Building OutputSBML ...');
      feval(fhandle, '--mex', 'OutputSBML.c', '-DUSE_OCTAVE', inc_arg, ...
            '-lbz2', '-lz', library_dir);
    else
      fhandle = @mkoctfile;
      disp('  - Building TranslateSBML ...');
      feval(fhandle, '--mex', 'TranslateSBML.c', '-DUSE_OCTAVE', inc_arg, ...
            '-lbz2', '-lz', lib_arg, added_args); 
      disp('  - Building OutputSBML ...');
      feval(fhandle, '--mex', 'OutputSBML.c', '-DUSE_OCTAVE', inc_arg, ...
            '-lbz2', '-lz', lib_arg, added_args);
    end;
%   mkoctfile --mex TranslateSBML.c -DUSE_OCTAVE inc_arg -lbz2 -lz lib_arg;
  end;

  transFile = strcat('TranslateSBML.', mexext());
  outFile = strcat('OutputSBML.', mexext());
  if ~exist(transFile) || ~exist(outFile)
    error('Build failed.');
  end;


%
% Find a directory where we can copy our files (Linux & Mac version).
%
% -------------------------------------------------------------------------
function working_dir = find_working_dir(matlab_octave)
  if strcmpi(matlab_octave, 'matlab')
    user_dir = userpath;
    user_dir = user_dir(1:length(user_dir)-1);
  else
    % This is Octave.  Octave doesn't have 'userpath'.  
    user_dir = tempdir;
  end;
  
  working_dir = fullfile(user_dir, 'libsbml');

  if ~exist(working_dir, 'dir')
    [success, msg, msgid] = mkdir(working_dir);
    if ~success
      error(sprintf('\n%s\n%s\n', msg, 'Build failed.'));
    end;
  end;


% 
% Copy the matlab binding directory, with tests.
% 
% This also creates the necessary directories and subdirectories.
% -------------------------------------------------------------------------
function copy_matlab_dir(orig_dir, working_dir)
  disp(sprintf('  - Copying files to %s', working_dir));
  
  % Copy files from src/bindings/matlab.

  [success, msg, msgid] = copyfile('TranslateSBML.c', working_dir);
  if ~success
    error(sprintf('\n%s\n%s\n', msg, 'Build failed.'));
  end;
    
  [success, msg, msgid] = copyfile('OutputSBML.c', working_dir);
  if ~success
    error(sprintf('\n%s\n%s\n', msg, 'Build failed.'));
  end;

  [success, msg, msgid] = copyfile('*.m', working_dir);
  if ~success
    error(sprintf('\n%s\n%s\n', msg, 'Build failed.'));
  end;

  [success, msg, msgid] = copyfile('*.xml', working_dir);
  if ~success
    error(sprintf('\n%s\n%s\n', msg, 'Build failed.'));
  end;

  % Copy files from src/bindings/matlab/test.

  test_subdir = fullfile(working_dir, 'test');

  if ~exist(test_subdir, 'dir')
    [success, msg, msgid] = mkdir(test_subdir);
    if ~success
      error(sprintf('\n%s\n%s\n', msg, 'Build failed.'));
    end;
  end;

  cd 'test';

  [success, msg, msgid] = copyfile('*.m', test_subdir);
  if ~success
    error(sprintf('\n%s\n%s\n', msg, 'Build failed.'));
  end;
  
  % Copy files from src/bindings/matlab/test/test-data/.

  test_data_subdir = fullfile(test_subdir, 'test-data');

  if ~exist(test_data_subdir, 'dir')
    [success, msg, msgid] = mkdir(test_data_subdir);
    if ~success
      error(sprintf('\n%s\n%s\n', msg, 'Build failed.'));
    end;
  end;

  cd 'test-data';

  [success, msg, msgid] = copyfile('*.xml', test_data_subdir);
  if ~success
    error(sprintf('\n%s\n%s\n', msg, 'Build failed.'));
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
% Copy library files to the given directory on windows
% -------------------------------------------------------------------------
function copied = copyLibraries(orig_dir, target_dir, lib)
  
  copied = 0;
  cd (target_dir);
  
  % if we moving to another location create a libsbml directory
  % if we are staying in src/matlab/bindings copy here
  if (~strcmp(orig_dir, target_dir))
    if (exist('libsbml', 'dir') == 0)
      mkdir('libsbml');
    end;
    cd libsbml;
  end;
  new_dir = pwd;
  % copy the necessary files
  for i = 1:8
    copyfile(lib{i}, new_dir);
  end;
  cd(orig_dir);
  
  copied = 1;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
    copyfile('TranslateSBML.c', new_dir);
    copyfile('OutputSBML.c', new_dir);
    copyfile('*.m', new_dir);
    copyfile('*.xml', new_dir);
    cd(new_dir);
%     delete ('buildLibSBML.m');
    
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
    copyfile('*.m', new_dir);
    
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
    
    %navigate to new libsbml directory
    cd(new_dir);
    cd ..;
    cd ..;
    
    % put in some tests here
    copied = 1;


% =========================================================================
% The end.
% 
% Please leave the following for [X]Emacs users:
% Local Variables:
% matlab-indent-level: 2
% fill-column: 72
% End:
% =========================================================================
