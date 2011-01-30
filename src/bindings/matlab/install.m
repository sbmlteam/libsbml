function install


  [detected_os, matlab, root] = determine_system();

  % check to see whether the executable exist
  checkForExecutables();

  switch detected_os
    case 0
      install_win(matlab, root);
    case 1
      install_mac(matlab, root);
    case 2
      install_linux(matlab, root);
    otherwise
      error('OS not recognised');
  end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% check what we are using
function [detected_os, matlab, root] = determine_system

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
  % detected_os = [0, 1, 2]
  %      0 - windows
  %      1 - mac
  %      2 - unix
  if (ispc())
    detected_os = 0;
    disp('Windows OS detected');
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
  % need to check which directory we are in
  % if we are in the src directory we should 
  % be in directory ../src/bindings/matlab
  [remain, first] = fileparts(pwd);
  [remain1, second] = fileparts(remain);
  [remain2, third] = fileparts(remain1);

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% tell user we are in the wrong location
function report_incorrect_dir(this_dir, expected)

  message = sprintf('You are in directory %s \nbut should be in %s', this_dir, expected);
  error(message);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% install on windows
function install_win(ismatlab, root)

  bin_dir = [root, filesep, 'win', filesep, 'bin'];
  if (exist(bin_dir, 'dir') ~= 7)
    %dir doesnot exist prompt
    error('should not be here');
  else
    addDir(pwd);
    addDir(bin_dir);
  end;
  
  if (ismatlab)
    this_dir = pwd;
    % install the files
    try
      M = TranslateSBML('test.xml');
    catch
      cd(bin_dir);
      try
        M = TranslateSBML('test.xml');
        cd(this_dir);
      catch
        cd(this_dir);
      end;
    end;
    
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

    if (success == 1)
      disp ('Installation completed');
    end;
   
  else
    % TO DO
    disp('install on octave');
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

  
  if (ismatlab)
    this_dir = pwd;
    % install the files
    try
      M = TranslateSBML('test.xml');
    catch
      cd(bin_dir);
      try
        M = TranslateSBML('test.xml');
        cd(this_dir);
      catch
        cd(this_dir);
      end;
    end;
    
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

    if (success == 1)
      disp ('Installation completed');
    end;
   

  else
    disp('install on octave');
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
 
  if (ismatlab)
    this_dir = pwd;
    % install the files
    try
      M = TranslateSBML('test.xml');
    catch
      cd(bin_dir);
      try
        M = TranslateSBML('test.xml');
        cd(this_dir);
      catch
        cd(this_dir);
      end;
    end;
    
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

    if (success == 1)
      disp ('Installation completed');
    end;
   

  else
    disp('install on octave');
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
  transFile = strcat('TranslateSBML.', mexext());
  outFile = strcat('OutputSBML.', mexext());

  if (~(exist(transFile) == 3 && ...
      exist(outFile) == 3))     
    error(sprintf('%s\n%s', 'Executables not found', ...
      'Run the buid script to build the relevant files'));
  else
      % they exist are the the right ones
      if (~((strcmp(which(transFile), [pwd, filesep, transFile])) && ...
            (strcmp(which(outFile), [pwd, filesep, outFile]))))     
          error(sprintf('%s\n%s', 'Other executables from other installations found', ...
            'Run the buid script to build the relevant files for this installation'));
      end;
  end;
