[Setup]

AppName=libsbml_test
AppVerName=libsbml 2.1.0
AppPublisher=SBML Team
AppPublisherURL=http://www.sbml.org
AppSupportURL=http://www.sbml.org
AppUpdatesURL=http://www.sbml.org

DefaultDirName={pf}\SBML\libsbml-2.1.0-xerces
DefaultGroupName=libsbml
DisableProgramGroupPage=yes
WizardSmallImageFile=libsbml-installer-mini-logo.bmp
WizardImageFile=libsbml-installer-graphic-v3.bmp


[Files]
;Source: "C:\Libsbml-2.1.0-xerces\*.*"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs
;Source: "C:\Libsbml-2.1.0-xerces\win32\bin\*.dll"; DestDir: "{sys}"; Check: GetValue;
;Source: "C:\Libsbml-2.1.0-xerces\win32\bin\*.lib"; DestDir: "{sys}"; Check: GetValue;
;Source: "C:\Libsbml-2.1.0-xerces\bindings\matlab\*.*"; DestDir: "{code:GetMatlabRoot}\toolbox\SBMLBinding"; Check: GetML; Flags: ignoreversion recursesubdirs
;Source: "C:\Libsbml-2.1.0-xerces\bindings\java\sbmlj.dll"; DestDir: "{sys}"; Check: GetJavaValue;
;Source: "C:\Libsbml-2.1.0-xerces\bindings\python\_libsbml.dll"; DestDir: "{sys}"; Check: GetPythonValue;
Source: "C:\Libsbml-2.1.0-xerces\version.txt"; DestDir: "{app}"; Flags: ignoreversion

[Registry]
;Root: HKCU; Subkey: "Software\SBML"; Flags: uninsdeletekeyifempty
;Root: HKCU; Subkey: "Software\SBML\libsbml"; Flags: uninsdeletekey
;Root: HKLM; Subkey: "Software\SBML"; Flags: uninsdeletekeyifempty
;Root: HKLM; Subkey: "Software\SBML\libsbml"; Flags: uninsdeletekey
;Root: HKLM; Subkey: "Software\SBML\libsbml"; ValueType: string; ValueName: "Version"; ValueData: "2.1.0"
;Root: HKLM; Subkey: "Software\SBML\libsbml"; ValueType: string; ValueName: "InstallPath"; ValueData: "{app}"


[code]
{global variables}
var
  LibsbmlExists, MatlabExists: Boolean;
 UserPrompts, UserValues: TArrayOfString;
   InstallationOptionPrompts, InstallationOptionValues: TArrayOfString;
   VersionNumber, MLRoot: String;
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


function LibExists() : Boolean ;
var
Exists: Boolean;

begin
  VersionNumber := GetVersion();
  
  if VersionNumber = '' then begin
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
  MsgBox('This is a test and only installs one test file.', mbInformation, mb_Ok);
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

  MLRoot := GetMatlabRoot('1');
  if (MLRoot = '') then begin
    MatlabExists := False;
  end else begin
    MatlabExists := True;
  end;


  if (MatlabExists) then begin
    SetArrayLength(UserValues, 4);

    UserValues[0] := '1';
    UserValues[1] := '1';
    UserValues[2] := '1';
    UserValues[3] := '1';

    SetArrayLength(UserPrompts, 4);

    UserPrompts[0] := 'Copy libraries to system directory';
    UserPrompts[1] := 'Install java binding libraries to system directory';
    UserPrompts[2] := 'Install python binding libraries to system directory';
    UserPrompts[3] := 'Install MATLAB binding function';
  end else begin
    SetArrayLength(UserValues, 3);

    UserValues[0] := '1';
    UserValues[1] := '1';
    UserValues[2] := '1';

    SetArrayLength(UserPrompts, 3);

    UserPrompts[0] := 'Copy libraries to system directory';
    UserPrompts[1] := 'Install java binding libraries to system directory';
    UserPrompts[2] := 'Install python binding libraries to system directory';

  end;
SetArrayLength(InstallationOptionValues, 2)
 InstallationOptionValues[0] := '1';
  InstallationOptionValues[1] := '0';

  SetArrayLength(InstallationOptionPrompts, 2)
 InstallationOptionPrompts[0] := 'Typical';
  InstallationOptionPrompts[1] := 'Custom';

  { Let Setup run }
  Result := True;
end;

{function to navigate between pages}
function ScriptDlgPages(CurPage: Integer; BackClicked: Boolean): Boolean;
var
  CurSubPage, Later: Integer;
  Next: Boolean;
begin
 if (not BackClicked and (CurPage = wpSelectDir)) or (BackClicked and (CurPage = wpReady)) then begin
     if (LibsbmlExists) then begin

      Later := LaterVersion(VersionNumber, '2.1.0');

      if (VersionNumber = '') then begin
        MsgBox('No bindings to install!', mbInformation, mb_Ok);

      end else if (Later = 0) then begin
        MsgBox('This version of libsbml has already been installed on the system', mbInformation, mb_Ok);
      end else if (Later = 1) then begin
        MsgBox('A later version of libsbml has already been installed on the system', mbInformation, mb_Ok);
      end else begin
        MsgBox('An earlier version of libsbml has already been installed on the system. Files may be overwritten.', mbInformation, mb_Ok);
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

            if InstallationOptionValues[1] = '1' then begin
               ScriptDlgPageClearCustom();
          { Set some captions }
              ScriptDlgPageSetCaption('Customise setup');
              ScriptDlgPageSetSubCaption1('Select the following options');

              Next := InputOptionArray(UserPrompts, UserValues, False, False);
            end else
              Next := True
    
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
        S := S + 'Writing libsbml library files to system directory' + NewLine;
        S := S + '      ' + GetSystemDir();
        S := S + NewLine;
    end else begin
        S := S + 'Not writing libsbml library files to system directory' + NewLine;
        S := S + NewLine;
    end;
  
    if (UserValues[1] = '1') then begin
        S := S + NewLine;
        S := S + 'Writing libsbml java library files (sbmlj.*) to system directory' + NewLine;
        S := S + '      ' + GetSystemDir();
        S := S + NewLine;
    end else begin
        S := S + NewLine;
        S := S + 'Not writing libsbml java library files to system directory' + NewLine;
        S := S + NewLine;
    end;

    if (UserValues[2] = '1') then begin
        S := S + NewLine;
        S := S + 'Writing libsbml python library files (_libsbml.*) to system directory' + NewLine;
        S := S + '      ' + GetSystemDir();
        S := S + NewLine;
    end else begin
        S := S + NewLine;
        S := S + 'Not writing libsbml python library files to system directory' + NewLine;
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

    S := S + 'Writing libsbml library files to system directory' + NewLine;
    S := S + '      ' + GetSystemDir();
    S := S + NewLine + NewLine;

    S := S + 'Writing libsbml java library files (sbmlj.*) to system directory' + NewLine;
    S := S + '      ' + GetSystemDir();
    S := S + NewLine + NewLine;

    S := S + 'Writing libsbml python library files (_libsbml.*) to system directory' + NewLine;
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


