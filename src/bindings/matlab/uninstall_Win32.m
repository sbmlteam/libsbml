function uninstall_Win32()

%delete TranslateSBML.mexw32

Path_to_libs = matlabroot;
Path_to_libs = strcat(Path_to_libs, '\bin\win32');

dll = strcat(Path_to_libs, '\libsbml.dll');
lib = strcat(Path_to_libs, '\libsbml.lib');

delete (dll);
delete (lib);
