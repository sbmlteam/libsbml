;************************************************************************************
; Sarah's comments for using this script are all in boxes surrounded by stars
; comments refer to the line below the comment
;************************************************************************************

;************************************************************************************
; the version number appears 5 times. It is currently reading 2.2.0
;************************************************************************************




[Setup]
AppName=libSBML

;***********************************************************************************

; this must read libsbml 'space' version number

;***********************************************************************************
AppVerName=libSBML 2.2.0


AppPublisher=SBML Team
AppPublisherURL=http://www.sbml.org
AppSupportURL=http://www.sbml.org
AppUpdatesURL=http://www.sbml.org

;***********************************************************************************

; this must read {pf}\SBML\ followed by the name of the libsbml directory
; this is the top level directory where the installation will be copied
; {pf} indicates that the Program Files directory

; Note this directory name includes the version number

;***********************************************************************************
DefaultDirName={pf}\SBML\libsbml-2.2.0-xerces
;DefaultDirName={pf}\SBML\libsbml-2.1.1-expat

DefaultGroupName=libsbml
DisableProgramGroupPage=yes
WizardSmallImageFile=libsbml-installer-mini-logo.bmp
WizardImageFile=libsbml-installer-graphic-v3.bmp


[Files]
;***************************************************************************************

; these directions tell the installer where to copy each file
; To be obvious Source should be the path on your system where the files are




; this direction copies the whole tree to the directory specified by the user
; default Program Files/SBML/libsbml-2.1.1-xerces
;
;****************************************************************************************
Source: "C:\libsbml_for_xerces_release\*.*"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs


;Source: "C:\libsbml_for_expat_release\*.*"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs





;********************************************************************************************
; these are the optional copies to the system directory or matlabroot
; the Check flag retrievs the user reponse via the installer GUI before deciding whether to make the copy


;********************************************************************************************

; FILES FOR XERCES RELEASE

Source: "C:\libsbml_for_xerces_release\win32\bin\libsbml*.dll"; DestDir: "{sys}"; Check: GetValue;
Source: "C:\libsbml_for_xerces_release\win32\bin\libsbml*.lib"; DestDir: "{sys}"; Check: GetValue;
Source: "C:\libsbml_for_xerces_release\win32\bin\xerces-c*.dll"; DestDir: "{sys}"; Check: GetValue;
Source: "C:\libsbml_for_xerces_release\win32\bin\xerces-c*.lib"; DestDir: "{sys}"; Check: GetValue;

Source: "C:\libsbml_for_xerces_release\bindings\matlab\*.*"; DestDir: "{code:GetMatlabRoot}\SBMLBinding"; Check: GetML;

Source: "C:\libsbml_for_xerces_release\win32\bin\sbmlj*.dll"; DestDir: "{sys}"; Check: GetJavaValue;
Source: "C:\libsbml_for_xerces_release\win32\bin\sbmlj*.lib"; DestDir: "{sys}"; Check: GetJavaValue;


;Source: "C:\libsbml_for_xerces_release\bindings\python\_libsbml.dll"; DestDir: "{sys}"; Check: GetPythonValue;
;Source: "C:\libsbml_for_xerces_release\bindings\python\_libsbml.lib"; DestDir: "{sys}"; Check: GetPythonValue;

; FILES FOR EXPAT RELEASE
;Source: "C:\libsbml_for_expat_release\win32\bin\libsbml*.dll"; DestDir: "{sys}"; Check: GetValue;
;Source: "C:\libsbml_for_expat_release\win32\bin\libsbml*.lib"; DestDir: "{sys}"; Check: GetValue;
;Source: "C:\libsbml_for_expat_release\win32\bin\libexpat.dll"; DestDir: "{sys}"; Check: GetValue;
;Source: "C:\libsbml_for_expat_release\win32\bin\libexpat.lib"; DestDir: "{sys}"; Check: GetValue;

;Source: "C:\libsbml_for_expat_release\bindings\matlab\*.*"; DestDir: "{code:GetMatlabRoot}\SBMLBinding"; Check: GetML;

;Source: "C:\libsbml_for_expat_release\win32\bin\sbmlj*.dll"; DestDir: "{sys}"; Check: GetJavaValue;
;Source: "C:\libsbml_for_expat_release\win32\bin\sbmlj*.lib"; DestDir: "{sys}"; Check: GetJavaValue;


;Source: "C:\libsbml_for_expat_release\bindings\python\_libsbml.dll"; DestDir: "{sys}"; Check: GetPythonValue;
;Source: "C:\libsbml_for_expat_release\bindings\python\_libsbml.lib"; DestDir: "{sys}"; Check: GetPythonValue;



[Registry]
Root: HKCU; Subkey: "Software\SBML"; Flags: uninsdeletekeyifempty
Root: HKCU; Subkey: "Software\SBML\libSBML"; Flags: uninsdeletekey
Root: HKLM; Subkey: "Software\SBML"; Flags: uninsdeletekeyifempty
Root: HKLM; Subkey: "Software\SBML\libSBML"; Flags: uninsdeletekey

;********************************************************************************************************

; version number is inserted here as a string
;**********************************************************************************************************
Root: HKLM; Subkey: "Software\SBML\libSBML"; ValueType: string; ValueName: "Version"; ValueData: "2.2.0"
Root: HKLM; Subkey: "Software\SBML\libSBML"; ValueType: string; ValueName: "InstallPath"; ValueData: "{app}"


[code]
{global variables}
var
  LibsbmlExists, MatlabExists: Boolean;
 UserPrompts, UserValues: TArrayOfString;
   InstallationOptionPrompts, InstallationOptionValues: TArrayOfString;
   PreviousInstalledVersionNumber, ThisVersionNumber, MLRoot: String;


{function to return version number stored in registry}
function GetVersion(): String;
var
  Vers:String;
  Key: String;

begin

  Key := '';
  Key := Key + 'Software\SBML\libSBML\';
  RegQueryStringValue(HKLM, Key, 'Version', Vers);

  Result := Vers;
end;

{function to determine whether libsbml has been previously installed}
function LibExists() : Boolean ;
var
Exists: Boolean;

begin
  PreviousInstalledVersionNumber := GetVersion();
  
  if PreviousInstalledVersionNumber = '' then begin
    Exists := False;
  end else begin
    Exists := True;
  end;

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




{functions to activate buttons and url on screen}
procedure AboutButtonOnClick(Sender: TObject);
begin

{*********************************************************************************************************
 The text for this message box is what the user will see if they click the About button during installation
 
 Feel free to alter it to taste but beware it must all be on one line.
 
  Note: it includes a version number
**********************************************************************************************************}
  MsgBox('This setup installs the Windows release of libSBML 2.2.0 built using the Xerces XML Parser libraries. All the necessary libraries are included. The source code is available as a seperate download.', mbInformation, mb_Ok);
 { MsgBox('This setup installs the Windows release of libSBML 2.1.1 built using the Expat XML Parser libraries. All the necessary libraries are included. The source code is available as a seperate download.', mbInformation, mb_Ok); }
end;

procedure URLLabelOnClick(Sender: TObject);
var
  Dummy: Integer;
begin
  InstShellExec('http://www.sbml.org', '', '', SW_SHOWNORMAL, Dummy);
end;


{function to initialise the wizard form
i.e. add an about button and the url to all sheets}
procedure InitializeWizard();
var
  AboutButton, CancelButton: TButton;
  URLLabel: TNewStaticText;
begin
 CancelButton := WizardForm.CancelButton;


  AboutButton := TButton.Create(WizardForm);
  AboutButton.Left := WizardForm.ClientWidth - CancelButton.Left - CancelButton.Width;
  AboutButton.Top := CancelButton.Top;
  AboutButton.Width := CancelButton.Width;
  AboutButton.Height := CancelButton.Height;
  AboutButton.Caption := '&About...';
  AboutButton.OnClick := @AboutButtonOnClick;
  AboutButton.Parent := WizardForm;

  URLLabel := TNewStaticText.Create(WizardForm);
  URLLabel.Top := AboutButton.Top + AboutButton.Height - URLLabel.Height - 2;
  URLLabel.Left := AboutButton.Left + AboutButton.Width + 20;
  URLLabel.Caption := 'www.sbml.org';
  URLLabel.Font.Style := URLLabel.Font.Style + [fsUnderLine];
  URLLabel.Font.Color := clBlue;
  URLLabel.Cursor := crHand;
  URLLabel.OnClick := @URLLabelOnClick;
  URLLabel.Parent := WizardForm;

 end;


{function to provide initial data for the wizard}
function InitializeSetup(): Boolean;
begin
  LibsbmlExists := LibExists();

{************************************************************************

this is the version number for the current installation

*************************************************************************}
  ThisVersionNumber := '2.2.0';
  
  MLRoot := GetMatlabRoot('1');
  if (MLRoot = '') then begin
    MatlabExists := False;
  end else begin
    MatlabExists := True;
  end;


  SetArrayLength(UserValues, 4);

  UserValues[0] := '0';
  UserValues[1] := '0';
  UserValues[2] := '0';
  UserValues[3] := '0';

  SetArrayLength(UserPrompts, 4);

  UserPrompts[0] := 'Copy libraries to system directory';
  UserPrompts[1] := 'Install Java binding libraries to system directory';
  UserPrompts[2] := 'Install Python binding libraries to system directory';
  UserPrompts[3] := 'Install MATLAB binding function';


  SetArrayLength(InstallationOptionValues, 2)
  InstallationOptionValues[0] := '1';
  InstallationOptionValues[1] := '0';

  SetArrayLength(InstallationOptionPrompts, 2)
  InstallationOptionPrompts[0] := 'Typical';
  InstallationOptionPrompts[1] := 'Custom';

 { Try to find the settings that were stored last time (also see below). }
  UserValues[0] := GetPreviousData('Libraries', UserValues[0]);
  UserValues[1] := GetPreviousData('Java', UserValues[1]);
  UserValues[2] := GetPreviousData('Python', UserValues[2]);
  UserValues[3] := GetPreviousData('Matlab', UserValues[3]);
  InstallationOptionValues[0] := GetPreviousData('Typical', InstallationOptionValues[0]);
  InstallationOptionValues[1] := GetPreviousData('Custom', InstallationOptionValues[1]);


  { Let Setup run }
  Result := True;
end;

procedure RegisterPreviousData(PreviousDataKey: Integer);
begin
  { Store the settings so we can restore them next time }
  SetPreviousData(PreviousDataKey, 'Libraries', UserValues[0]);
  SetPreviousData(PreviousDataKey, 'Java', UserValues[1]);
  SetPreviousData(PreviousDataKey, 'Python', UserValues[2]);
  SetPreviousData(PreviousDataKey, 'Matlab', UserValues[3]);
  SetPreviousData(PreviousDataKey, 'Typical', InstallationOptionValues[0]);
  SetPreviousData(PreviousDataKey, 'Custom', InstallationOptionValues[1]);
end;

{function to navigate between pages}
function ScriptDlgPages(CurPage: Integer; BackClicked: Boolean): Boolean;
var
  CurSubPage, Later: Integer;
  Next: Boolean;
begin
 if (not BackClicked and (CurPage = wpSelectDir)) or (BackClicked and (CurPage = wpReady)) then begin
     if (LibsbmlExists) then begin

      Later := LaterVersion(PreviousInstalledVersionNumber, ThisVersionNumber);

      if (PreviousInstalledVersionNumber = '') then begin
        MsgBox('No bindings to install!', mbInformation, mb_Ok);

      end else if (Later = 0) then begin
        MsgBox('This version of libSBML has already been installed on the system', mbInformation, mb_Ok);
      end else if (Later = 1) then begin
        MsgBox('A later version of libSBML has already been installed on the system', mbInformation, mb_Ok);
      end else begin
        MsgBox('An earlier version of libSBML has already been installed on the system. Files may be overwritten.', mbInformation, mb_Ok);
      end;
    end;

    if not BackClicked then
      CurSubPage := 0
    else begin
      if InstallationOptionValues[1] = '1' then
        CurSubPage := 1
      else
        CurSubPage := 0
    end;
    ScriptDlgPageOpen();
    ScriptDlgPageSetCaption('Select installation');
    ScriptDlgPageSetSubCaption2('');
    while (CurSubPage >= 0) and (CurSubPage <= 1) and not Terminated do begin
      case CurSubPage of
        0:
          begin
            ScriptDlgPageSetSubCaption1('');
            ScriptDlgPageClearCustom();

            Next := InputOptionArray(InstallationOptionPrompts, InstallationOptionValues, True, False);
          end;
        1:
          begin

            {custom installation}
            if InstallationOptionValues[1] = '1' then begin
               ScriptDlgPageClearCustom();

             { Set some captions }
              ScriptDlgPageSetCaption('Customise setup');
              ScriptDlgPageSetSubCaption1('Select the following options');

              Next := InputOptionArray(UserPrompts, UserValues, False, False);
              
              {if matlab not detected but user selects install matlab show message}
              if ((not MatlabExists) and (UserValues[3] = '1')) then begin
                UserValues[3] := '0';
                MsgBox('MATLAB has not been detected on your system. Setup will copy the matlab bindings files to the installation directory but will not attempt to put them on the MATLAB path', mbInformation, mb_OK);
              end;
              
            end else begin
              {typical installation}
              UserValues[0] := '1';
              UserValues[1] := '1';
              UserValues[2] := '1';
              
              if MatlabExists then begin
                UserValues[3] := '1';
              end else begin
                UserValues[3] := '0';
              end;
              
              Next := True;
            end;
    
          end;
      end; {of case}

      if Next then
        CurSubPage := CurSubPage + 1
      else
        CurSubPage := CurSubPage - 1
    end; {of while}
    
    if not BackClicked then
      Result := Next
    else
      Result := not Next;

    ScriptDlgPageClose(not Result);
  end else
    Result := True;

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


  if (InstallationOptionValues[1] = '1') then begin
  {custom installation}
    S := S + 'Custom installation' + NewLine;

    if (UserValues[0] = '1') then begin
        S := S + 'Writing libSBML library files to system directory' + NewLine;
        S := S + '      ' + GetSystemDir();
        S := S + NewLine;
    end else begin
        S := S + 'Not writing libSBML library files to system directory' + NewLine;
        S := S + NewLine;
    end;
  
    if (UserValues[1] = '1') then begin
        S := S + NewLine;
        S := S + 'Writing libSBML Java library files (sbmlj.*) to system directory' + NewLine;
        S := S + '      ' + GetSystemDir();
        S := S + NewLine;
    end else begin
        S := S + NewLine;
        S := S + 'Not writing libSBML Java library files to system directory' + NewLine;
        S := S + NewLine;
    end;

    if (UserValues[2] = '1') then begin
        S := S + NewLine;
        S := S + 'Writing libSBML Python library files (_libsbml.*) to system directory' + NewLine;
        S := S + '      ' + GetSystemDir();
        S := S + NewLine;
    end else begin
        S := S + NewLine;
        S := S + 'Not writing libSBML Python library files to system directory' + NewLine;
        S := S + NewLine;
    end;

    if (MatlabExists) then begin
    if (UserValues[3] = '1') then begin
        S := S + NewLine;
        S := S + 'Installing matlab binding files to ' + NewLine;
        S := S + '      ' + GetMatlabRoot('1');
        S := S + NewLine;
    end else begin
        S := S + NewLine;
        S := S + 'Not installing matlab binding files' + NewLine;
        S := S + NewLine;
    end;
    end;

  end else begin

  {typical installation}
    S := S + 'Typical installation' + NewLine;

    S := S + 'Writing libSBML library files to system directory' + NewLine;
    S := S + '      ' + GetSystemDir();
    S := S + NewLine + NewLine;

    S := S + 'Writing libSBML Java library files (sbmlj.*) to system directory' + NewLine;
    S := S + '      ' + GetSystemDir();
    S := S + NewLine + NewLine;

    S := S + 'Writing libSBML Python library files (_libsbml.*) to system directory' + NewLine;
    S := S + '      ' + GetSystemDir();
    S := S + NewLine + NewLine;

    if (MatlabExists) then begin
    S := S + 'Installing matlab binding files to ' + NewLine;
    S := S + '      ' + GetMatlabRoot('1');
    S := S + NewLine + NewLine;
    end;

  end;

  Result := S;
end;

{ function to return flag as to whether to write libraries to system directory}
function GetValue() : Boolean;
begin
  if UserValues[0] = '1' then
    Result := True
  else
    Result := False;
end;

{ function to return flag as to whether matlab binding to be written to matlab toolbox directory}
function GetML() : Boolean;
begin
  if ((MatlabExists) and (UserValues[3] = '1')) then
    Result := True
  else
    Result := False;
end;

{ function to return flag as to whether to write libraries to system directory}
function GetJavaValue() : Boolean;
begin
  if UserValues[1] = '1' then
    Result := True
  else
    Result := False;
end;

{ function to return flag as to whether to write libraries to system directory}
function GetPythonValue() : Boolean;
begin
  if UserValues[2] = '1' then
    Result := True
  else
    Result := False;
end;


