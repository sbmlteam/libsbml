;  Filename    : install_libsbml_script.iss
;  Description : file to create windows installation of libsbml
;  Author(s)   : SBML Development Group <sbml-team@caltech.edu>
;  Organization: University of Hertfordshire STRC
;  Created     : 2004-03-15
;  Revision    : $Id$
;  Source      : $Source$
;
;  Copyright 2003 California Institute of Technology, the Japan Science
;  and Technology Corporation, and the University of Hertfordshire
;
;  This library is free software; you can redistribute it and/or modify it
;  under the terms of the GNU Lesser General Public License as published
;  by the Free Software Foundation; either version 2.1 of the License, or
;  any later version.
;
;  This library is distributed in the hope that it will be useful, but
;  WITHOUT ANY WARRANTY, WITHOUT EVEN THE IMPLIED WARRANTY OF
;  MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  The software and
;  documentation provided hereunder is on an "as is" basis, and the
;  California Institute of Technology, the Japan Science and Technology
;  Corporation, and the University of Hertfordshire have no obligations to
;  provide maintenance, support, updates, enhancements or modifications.  In
;  no event shall the California Institute of Technology, the Japan Science
;  and Technology Corporation or the University of Hertfordshire be liable
;  to any party for direct, indirect, special, incidental or consequential
;  damages, including lost profits, arising out of the use of this software
;  and its documentation, even if the California Institute of Technology
;  and/or Japan Science and Technology Corporation and/or University of
;  Hertfordshire have been advised of the possibility of such damage.  See
;  the GNU Lesser General Public License for more details.
;
;  You should have received a copy of the GNU Lesser General Public License
;  along with this library; if not, write to the Free Software Foundation,
;  Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
;
;  The original code contained here was initially developed by:
;
;      Sarah Keating
;      Science and Technology Research Centre
;      University of Hertfordshire
;      Hatfield, AL10 9AB
;      United Kingdom
;
;      http://www.sbml.org
;      mailto:sbml-team@caltech.edu
;
;  Contributor(s):

[Setup]
AppName=libsbml
AppVerName=libsbml 2.0.3
AppPublisher=SBML Team
AppPublisherURL=http://www.sbml.org
AppSupportURL=http://www.sbml.org
AppUpdatesURL=http://www.sbml.org
DefaultDirName=C:\libsbml-2.0.3-xerces
DefaultGroupName=libsbml
DisableProgramGroupPage=yes
WizardSmallImageFile=libsbml-installer-mini-logo.bmp
WizardImageFile=libsbml-installer-graphic-v3.bmp



[Files]
;Source: "C:\InstalledLibsbml\libsbml-2.0.3-xerces\*.txt"; DestDir: "{app}"; Flags: ignoreversion
Source: "C:\InstalledLibsbml\libsbml-2.0.3-xerces\*.html"; DestDir: "{app}"; Flags: ignoreversion
Source: "C:\InstalledLibsbml\libsbml-2.0.3-xerces\bindings\matlab\TranslateSBML.m"; DestDir: "{app}\bindings\matlab"; Flags: ignoreversion recursesubdirs
;Source: "C:\InstalledLibsbml\libsbml-2.0.3-xerces\docs\*"; DestDir: "{app}\docs"; Flags: ignoreversion recursesubdirs
;Source: "C:\InstalledLibsbml\libsbml-2.0.3-xerces\examples\*"; DestDir: "{app}\examples"; Flags: ignoreversion
;Source: "C:\InstalledLibsbml\libsbml-2.0.3-xerces\expat\*"; DestDir: "{app}\expat"; Flags: ignoreversion
;Source: "C:\InstalledLibsbml\libsbml-2.0.3-xerces\src\*"; DestDir: "{app}\src"; Flags: ignoreversion recursesubdirs
;Source: "C:\InstalledLibsbml\libsbml-2.0.3-xerces\win32\*"; DestDir: "{app}\win32"; Flags: ignoreversion recursesubdirs
;Source: "C:\InstalledLibsbml\libsbml-2.0.3-xerces\xml-schemas\*"; DestDir: "{app}\xml-schemas"; Flags: ignoreversion recursesubdirs
Source: "C:\InstalledLibsbml\libsbml-2.0.3-xerces\win32\bin\libsbml.dll"; DestDir: "{sys}"; Check: GetValue;
;Source: "C:\InstalledLibsbml\libsbml-2.0.3-xerces\win32\bin\*.dll"; DestDir: "{sys}"; Check: GetValue;
Source: "C:\InstalledLibsbml\libsbml-2.0.3-xerces\bindings\matlab\TranslateSBML.m"; DestDir: "{code:GetMatlabRoot}\toolbox\SBMLBinding"; Check: GetML; Flags: ignoreversion recursesubdirs


; NOTE: Don't use "Flags: ignoreversion" on any shared system files
[Registry]
Root: HKCU; Subkey: "Software\SBML"; Flags: uninsdeletekeyifempty
Root: HKCU; Subkey: "Software\SBML\libsbml"; Flags: uninsdeletekey
Root: HKLM; Subkey: "Software\SBML"; Flags: uninsdeletekeyifempty
Root: HKLM; Subkey: "Software\SBML\libsbml"; Flags: uninsdeletekey
Root: HKLM; Subkey: "Software\SBML\libsbml"; ValueType: string; ValueName: "Version"; ValueData: "2.0.3"
Root: HKLM; Subkey: "Software\SBML\libsbml"; ValueType: string; ValueName: "InstallPath"; ValueData: "{app}"

[code]
{global variables}
var
  LibsbmlExists, MatlabExists: Boolean;
  LibInstallPrompts, LibInstallValues: TArrayOfString;
  LibInstallNumber: Integer;
  MLInstallPrompt, MLInstallValue, VersionNo: String;

{function to look for libsbml.dll in the system folder
  I'm assuming that this dll is key and that others will be there if this is}
  
function LibExists() : Boolean ;
var
Exists: Boolean;
Filename: String;

begin
  Filename := GetSystemDir();
  Filename := Filename + '\libsbml.dll';
  Exists := FileExists(Filename);

  Result := Exists;
end;

{function to return matlab root directory}
function GetMatlabRoot(S : String): String;
var
  Names: TArrayOfString;
  Root:String;
  Number: Longint;
  Key: String;

begin
  RegGetSubKeyNames(HKLM, 'Software\Mathworks\MATLAB', Names);

  Number := GetArrayLength(Names);
  if Number = 0 then
    Root := ''
  else begin
    Key := '';
    Key := Key + 'Software\Mathworks\MATLAB\';
    Key := Key + Names[Number-1];
    RegQueryStringValue(HKLM, Key, 'MATLABROOT', Root);
  end;
  
  Result := Root;
end;

{function to return version number stored in registry}
function GetVersion(): String;
var
  Vers:String;
  Key: String;

begin

  Key := '';
  Key := Key + 'Software\SBML\libsbml\';
  RegQueryStringValue(HKLM, Key, 'Version', Vers);

  Result := Vers;
end;

{function to check whether a preinstalled version number is later than the
  current installation
  returns 1 is the version installed is later than this version
          0 if the version installed equals this version
          -1 if the version installed is earlier than this version}
function LaterVersion(VersionNo, CurrentVers : String) : Integer;
var
  First, Second, Third: Longint;
  CurrentFirst, CurrentSecond, CurrentThird: Longint;
  Later : Integer;
begin
  if (VersionNo = '') then begin
    First := 0;
    Second := 0;
    Third := 0;
  end else begin
    First := StrToInt(StrGet(VersionNo, 1));
    Second := StrToInt(StrGet(VersionNo, 3));
    Third := StrToInt(StrGet(VersionNo, 5));
  end;
  CurrentFirst := StrToInt(StrGet(CurrentVers, 1));
  CurrentSecond := StrToInt(StrGet(CurrentVers, 3));
  CurrentThird := StrToInt(StrGet(CurrentVers, 5));

  if (First > CurrentFirst) then  begin
    Later := 1;
  end else if ((First = CurrentFirst) and (Second > CurrentSecond)) then begin
    Later := 1;
  end else if ((First = CurrentFirst) and (Second = CurrentSecond) and (Third > CurrentThird)) then begin
    Later := 1;
  end else if ((First = CurrentFirst) and (Second = CurrentSecond) and (Third = CurrentThird)) then begin
    Later := 0;
  end else begin
    Later := -1;
  end;

  Result:= Later;

end;

function InitializeSetup(): Boolean;
var
  MLRoot : String;

begin
  LibsbmlExists := LibExists();
  VersionNo := GetVersion();

  SetArrayLength(LibInstallPrompts, 2)
  LibInstallPrompts[0] := 'Overwrite libraries in system directory';
  LibInstallPrompts[1] := 'Install library files to system directory';

  SetArrayLength(LibInstallValues, 2);
  {set default values}
  LibInstallValues[0] := '1';
  LibInstallValues[1] := '1';

  MLInstallPrompt := 'Install matlab binding to matlab toolbox directory';
  MLRoot := GetMatlabRoot('');
  if MLRoot = '' then begin
    MatlabExists := False;
    MLInstallValue := '0';
  end else begin
    MatlabExists := True;
    MLInstallValue := '1';
  end;

  LibInstallValues[0] := GetPreviousData('Overwrite', LibInstallValues[0]);
  LibInstallValues[1] := GetPreviousData('Add', LibInstallValues[1]);
  MLInstallValue := GetPreviousData('ML', MLInstallValue);

  { Let Setup run }
  Result := True;
end;

procedure RegisterPreviousData(PreviousDataKey: Integer);
begin
  { Store the settings so we can restore them next time }
  SetPreviousData(PreviousDataKey, 'Overwrite', LibInstallValues[0]);
  SetPreviousData(PreviousDataKey, 'Add', LibInstallValues[1]);
  SetPreviousData(PreviousDataKey, 'ML', MLInstallValue);
end;

function ScriptDlgPages(CurPage: Integer; BackClicked: Boolean): Boolean;
var
  CurSubPage, Later: Integer;
  Next: Boolean;

begin
  if ((not BackClicked and (CurPage = wpWelcome)) or (BackClicked and (CurPage = wpSelectDir))) then begin

    { insert a page after the welcome page that only displays
      if a version of libsbml has been found in the system directory}
    if (LibsbmlExists) then begin

      ScriptDlgPageOpen();

      { Set some captions  }
      ScriptDlgPageSetCaption('Previously installed versions');
      ScriptDlgPageSetSubCaption1('');

      Later := LaterVersion(VersionNo, '2.0.3');
      
      if (VersionNo = '') then begin
        Next := OutputMsg('An unidentified version of libsbml has been found in the system directory'#13#13'Press Next to continue with the installation or Cancel to exit', True);
      end else if (Later = 0) then begin
        Next := OutputMsg('This version of libsbml has already been installed on the system'#13#13'Press Next to continue with the installation or Cancel to exit', True);
      end else if (Later = 1) then begin
        Next := OutputMsg('A later version of libsbml has already been installed on the system'#13#13'Press Next to continue with the installation or Cancel to exit', True);
      end else begin
        Next := OutputMsg('An earlier version of libsbml has already been installed on the system. Files may be overwritten.'#13#13'Press Next to continue with the installation or Cancel to exit', True);
      end;

    {libsbml doesnt already exist so do nothing}
    end else begin
    
      if not BackClicked then
        Next := True
      else
        Next := False;
    end;

    if not BackClicked then
      Result := Next
    else
      Result := not Next;

    { Close the wizard page. Do a FullRestore only if the click (see above) is not allowed }
    ScriptDlgPageClose(not Result);


  end else if ((not BackClicked and (CurPage = wpSelectDir)) or (BackClicked and (CurPage = wpReady))) then begin
    { after the select directory page
      prompt user for whether to install the library files in the system directory
      and where to additionally put any bindings }

    { First open the custom wizard page }
    if not BackClicked then
      CurSubPage := 0
    else
      CurSubPage := 1;

    ScriptDlgPageOpen();

    { Set some captions }
    ScriptDlgPageSetCaption('Adding library files to the system directory');
    ScriptDlgPageSetSubCaption1('Having library files in the system directory makes them visible to other programs');
    while (CurSubPage >= 0) and (CurSubPage <= 1) and not Terminated do begin
      case CurSubPage of
        0:
          begin
            ScriptDlgPageClearCustom();

            if LibsbmlExists then
              LibInstallNumber := 0
            else
              LibInstallNumber := 1;

            { get the value from the check box}
            Next := InputOption(LibInstallPrompts[LibInstallNumber], LibInstallValues[LibInstallNumber]);
        end;
        1:
          begin
            ScriptDlgPageSetCaption('Adding libsbml bindings');
            ScriptDlgPageSetSubCaption1('');

            ScriptDlgPageClearCustom();
            if MatlabExists then begin
              { get the value from the check box}
              Next := InputOption(MLInstallPrompt, MLInstallValue);

            end else begin
              MsgBox('No bindings to install!', mbInformation, mb_Ok);

            end;


          end;
      end;

    { See NextButtonClick and BackButtonClick: return True if the click should be allowed }
      if Next then
        CurSubPage := CurSubPage + 1
      else
        CurSubPage := CurSubPage - 1;
    end; {end of while}

    if not BackClicked then
      Result := Next
    else
      Result := not Next;

    { Close the wizard page. Do a FullRestore only if the click (see above) is not allowed }

    ScriptDlgPageClose(not Result);

  end else begin

    Result := True;

  end;

end;

function NextButtonClick(CurPage: Integer): Boolean;
begin
  Result := ScriptDlgPages(CurPage, False);
end;

function BackButtonClick(CurPage: Integer): Boolean;
begin
  Result := ScriptDlgPages(CurPage, True);
end;


function UpdateReadyMemo(Space, NewLine, MemoUserInfoInfo, MemoDirInfo, MemoTypeInfo, MemoComponentsInfo, MemoGroupInfo, MemoTasksInfo: String): String;
var
  S: String;
begin
  { Fill the 'Ready Memo' with the normal settings and the custom settings }
  S := '';
  S := S + MemoDirInfo + NewLine;
  S := S + NewLine;

  if (LibInstallNumber = 0) then begin
    if (LibInstallValues[0] = '1') then begin
      S := S + 'Overwrite library files in system directory' + NewLine;
      S := S + '      ' + GetSystemDir();
      S := S + NewLine;
    end else begin
      S := S + 'Library files already in system directory' + NewLine;
      S := S + '      ' + GetSystemDir();
      S := S + NewLine;
      S := S + 'No overwrite required.' + NewLine;
    end;
  end else begin
    if (LibInstallValues[1] = '1') then begin
      S := S + 'Writing library files to system directory' + NewLine;
      S := S + '      ' + GetSystemDir();
      S := S + NewLine;
    end else begin
      S := S + NewLine;
    end;
  end;
  S := S + NewLine;

  if MatlabExists then begin
    if (MLInstallValue = '1') then begin
      S := S + 'Writing matlab binding files to' + NewLine;
      S := S + '      ' + GetMatlabroot('') + '\toolbox\SBMLbinding';
      S := S + NewLine;
    end else begin
      S := S + 'Not installing matlab binding files' + NewLine;
      S := S + NewLine;
    end;
  end;

  Result := S;
end;

{ function to return flag as to whether to write libraries to system directory}
function GetValue() : Boolean;
begin
  if LibInstallValues[LibInstallNumber] = '1' then
    Result := True
  else
    Result := False;
end;

{ function to return flag as to whether matlab binding to be written to matlab toolbox directory}
function GetML() : Boolean;
begin
  if ((MatlabExists) and (MLInstallValue = '1')) then
    Result := True
  else
    Result := False;
end;

