; Version No is currently 3.1.1
; Check before use


[Setup]
AppName=libSBML
AppVerName=libSBML 3.1.1
AppPublisher=SBML Team
AppPublisherURL=http://sbml.org
AppSupportURL=http://sbml.org
AppUpdatesURL=http://sbml.org
DefaultDirName={pf}\SBML\libSBML-3.1.1-xerces
DefaultGroupName=libSBML
DisableProgramGroupPage=yes
OutputDir=..\..\..\libsbml_3\win32\installer\Output
OutputBaseFilename=libSBML-3.1.1-win-xerces
WizardSmallImageFile=libsbml-installer-mini-logo.bmp
WizardImageFile=libsbml-installer-graphic-v3.bmp
UsePreviousAppDir=no
Compression=lzma
SolidCompression=yes

[Languages]
Name: english; MessagesFile: compiler:Default.isl

[Files]
Source: C:\libsbml_3\win32\installer\libsbml_3_xerces\*; DestDir: {app}; Flags: ignoreversion recursesubdirs createallsubdirs
Source: C:\libsbml_3\win32\installer\libsbml_3_xerces\bindings\java\sbmlj.lib; DestDir: {sys}; Check: GetJava
Source: C:\libsbml_3\win32\installer\libsbml_3_xerces\bindings\java\sbmlj.dll; DestDir: {sys}; Check: GetJava
Source: C:\libsbml_3\win32\installer\libsbml_3_xerces\bindings\matlab\*; DestDir: {code:GetMatlabDir}; Flags: ignoreversion recursesubdirs createallsubdirs; Check: GetMatlab
Source: C:\libsbml_3\win32\installer\libsbml_3_xerces\bindings\python\python23\libsbml.py; DestDir: {code:GetPython23Dir}; Check: GetPython23
Source: C:\libsbml_3\win32\installer\libsbml_3_xerces\bindings\python\python23\_libsbml.dll; DestDir: {code:GetPython23Dir}; Check: GetPython23
Source: C:\libsbml_3\win32\installer\libsbml_3_xerces\bindings\python\python24\libsbml.py; DestDir: {code:GetPython24Dir}; Check: GetPython24
Source: C:\libsbml_3\win32\installer\libsbml_3_xerces\bindings\python\python24\_libsbml.dll; DestDir: {code:GetPython24Dir}; Check: GetPython24
Source: C:\libsbml_3\win32\installer\libsbml_3_xerces\bindings\python\python25\libsbml.py; DestDir: {code:GetPython25Dir}; Check: GetPython25
Source: C:\libsbml_3\win32\installer\libsbml_3_xerces\bindings\python\python25\_libsbml.pyd; DestDir: {code:GetPython25Dir}; Check: GetPython25
Source: C:\libsbml_3\win32\installer\libsbml_3_xerces\win32\bin\xerces-c_2D.lib; DestDir: {sys}; Check: GetLibrary
Source: C:\libsbml_3\win32\installer\libsbml_3_xerces\win32\bin\libsbml.dll; DestDir: {sys}; Check: GetLibrary
Source: C:\libsbml_3\win32\installer\libsbml_3_xerces\win32\bin\libsbml.lib; DestDir: {sys}; Check: GetLibrary
Source: C:\libsbml_3\win32\installer\libsbml_3_xerces\win32\bin\libsbmlD.dll; DestDir: {sys}; Check: GetLibrary
Source: C:\libsbml_3\win32\installer\libsbml_3_xerces\win32\bin\libsbmlD.lib; DestDir: {sys}; Check: GetLibrary
Source: C:\libsbml_3\win32\installer\libsbml_3_xerces\win32\bin\xerces-c_2.lib; DestDir: {sys}; Check: GetLibrary
Source: C:\libsbml_3\win32\installer\libsbml_3_xerces\win32\bin\xerces-c_2_7.dll; DestDir: {sys}; Check: GetLibrary
Source: C:\libsbml_3\win32\installer\libsbml_3_xerces\win32\bin\xerces-c_2_7D.dll; DestDir: {sys}; Check: GetLibrary

[Registry]
Root: HKCU; Subkey: Software\SBML; Flags: uninsdeletekeyifempty
Root: HKCU; Subkey: Software\SBML\libSBML; Flags: uninsdeletekey
Root: HKLM; Subkey: Software\SBML; Flags: uninsdeletekeyifempty
Root: HKLM; Subkey: Software\SBML\libSBML; Flags: uninsdeletekey
Root: HKLM; Subkey: Software\SBML\libSBML; ValueType: string; ValueName: Version; ValueData: 3.1.1
Root: HKLM; Subkey: Software\SBML\libSBML; ValueType: string; ValueName: InstallPath; ValueData: {app}

[Code]
var
  InstallTypePage: TInputOptionWizardPage;
  InstallOptionsPage: TInputOptionWizardPage;
  PythonPage: TInputOptionWizardPage;
  MatlabPage: TInputDirWizardPage;
  URLLabel: TNewStaticText;
  AboutButton, CancelButton: TButton;

  MatlabPresent: Boolean;
  MatlabVersion: String;
  MatlabRoot: String;

  PreviousInstalledVersion, ThisVersion: String;

  libSBMLPresent: Boolean;

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

{function to return matlab root directory}
function GetMatlabRoot(S : String): String;
var
  Names: TArrayOfString;
  Root:String;
  Number: Longint;
  Key: String;

begin
  RegGetSubKeyNames(HKLM, 'Software\Mathworks\MATLAB', Names);

  {deals with possible multiple installations of Matlab
   and choses most recent }
  Number := GetArrayLength(Names);
  if Number = 0 then begin
    Root := '';
    MatlabPresent := False;
  end else begin
    Key := '';
    Key := Key + 'Software\Mathworks\MATLAB\';
    Key := Key + Names[Number-1];
    RegQueryStringValue(HKLM, Key, 'MATLABROOT', Root);
    MatlabPresent := True;
    MatlabVersion := Names[Number-1];
  end;

  Result := Root;
end;

{function to return python 2.5 dir directory}
function GetPython25Dir(S : String): String;
var
  Root:String;
  Key: String;

begin
  Key := '';
  Key := Key + 'Software\Python\PythonCore\2.5\InstallPath\';
  RegQueryStringValue(HKLM, Key, '', Root);

  Root:= Root + 'Lib\site-packages\';
  Result := Root;
end;

{function to return python 2.4 dir directory}
function GetPython24Dir(S : String): String;
var
  Root:String;
  Key: String;

begin
  Key := '';
  Key := Key + 'Software\Python\PythonCore\2.4\InstallPath\';
  RegQueryStringValue(HKLM, Key, '', Root);

  Root:= Root + 'Lib\site-packages\';
  Result := Root;
end;

{function to return python 2.3 dir directory}
function GetPython23Dir(S : String): String;
var
  Root:String;
  Key: String;

begin
  Key := '';
  Key := Key + 'Software\Python\PythonCore\2.3\InstallPath\';
  RegQueryStringValue(HKLM, Key, '', Root);

  Root:= Root + 'Lib\site-packages\';
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
    if (Length(VersionNo) > 3) then begin
      Third := StrToInt(StrGet(VersionNo, 5));
    end else begin
      Third := 0  ;
    end;
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
  MsgBox('This setup installs the Windows version of libSBML 3.1.1 built using the Xerces XML Parser libraries. All the necessary libraries are included. The source code is available as a seperate download.', mbInformation, mb_Ok);
 { MsgBox('This setup installs the Windows release of libSBML 3.0.2 built using the Expat XML Parser libraries. All the necessary libraries are included. The source code is available as a seperate download.', mbInformation, mb_Ok);  }
end;


{send to url}
procedure URLLabelOnClick(Sender: TObject);
var
  Dummy: Integer;
begin
  ShellExec('open', 'http://www.sbml.org', '', '', SW_SHOW, ewNoWait, Dummy);
end;

procedure InitializeWizard;
begin
  {get data from system}
  PreviousInstalledVersion := GetVersion();
  ThisVersion := '3.1.1';
  MatlabRoot := GetMatlabRoot('');

  if (PreviousInstalledVersion = '') then begin
    libSBMLPresent := False;
  end else begin
    libSBMLPresent := True;
  end;

  {add an about button and a url to all pages}
  {need a cancel button to locate other}
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

  { Create the pages }

  {install type : whether typical or custom install }
  InstallTypePage := CreateInputOptionPage(wpSelectDir,
    'Select installation', '', '', True, False);
  InstallTypePage.Add('Typical');
  InstallTypePage.Add('Custom');

  { install options : custom install possible settings }
  InstallOptionsPage := CreateInputOptionPage(InstallTypePage.ID,
    'Customise installation', '',
    'Select the following options',
    False, False);
  InstallOptionsPage.Add('Copy libraries to systems directories');
  InstallOptionsPage.Add('Copy Java binding libraries to systems directory');
  InstallOptionsPage.Add('Copy Python binding libraries to site-packages directory');
  if (MatlabPresent) then begin
    InstallOptionsPage.Add('Install MATLAB binding');
  end;

  { python page : version of python builds to be installed }
  PythonPage := CreateInputOptionPage(InstallOptionsPage.ID,
    'Python binding', '',
    'Select the version of python you wish to install', True, False);
  PythonPage.Add('Python 2.3');
  PythonPage.Add('Python 2.4');
  PythonPage.Add('Python 2.5');

  { matlab page : location to install matlab binding }
  MatlabPage := CreateInputDirPage(InstallOptionsPage.ID,
    'MATLAB binding', '',
    'Select the folder in which Setup should install MATLAB binding files, then click Next.',
    False, '');
  MatlabPage.Add('');

  { Set default values, using settings that were stored last time if possible }

  {install type page}
  case GetPreviousData('InstallMode', '') of
    'typical': InstallTypePage.SelectedValueIndex := 0;
    'custom': InstallTypePage.SelectedValueIndex := 1;
  else
    InstallTypePage.SelectedValueIndex := 0;
  end;

  {install options page}
  if GetPreviousData('Libraries', '') = '0' then begin
    InstallOptionsPage.Values[0] := False;
  end else begin
    InstallOptionsPage.Values[0] := True;
  end;

  if GetPreviousData('Java', '') = '0' then begin
    InstallOptionsPage.Values[1] := False;
  end else begin
    InstallOptionsPage.Values[1] := True;
  end;

  if GetPreviousData('Python', '') = '0' then begin
    InstallOptionsPage.Values[2] := False;
  end else begin
    InstallOptionsPage.Values[2] := True;
  end;

  if (MatlabPresent) then begin
    if GetPreviousData('Matlab', '') = '0' then begin
      InstallOptionsPage.Values[3] := False;
    end else begin
      InstallOptionsPage.Values[3] := True;
    end;
  end;

  {python page}
  case GetPreviousData('PythonVers', '') of
    'Python 2.3': PythonPage.SelectedValueIndex := 0;
    'Python 2.4': PythonPage.SelectedValueIndex := 1;
    'Python 2.5': PythonPage.SelectedValueIndex := 2;
  else
    PythonPage.SelectedValueIndex := 2;
  end;

  {matlab page}
  MatlabPage.Values[0] := GetPreviousData('MatlabDir', MatlabRoot);

end;

procedure RegisterPreviousData(PreviousDataKey: Integer);
var
  InstallMode: String;
  InstallOptions: TArrayOfString;
  PythonVers: String;
begin
  { Store the settings so we can restore them next time }

  {install type page}
  case InstallTypePage.SelectedValueIndex of
    0: InstallMode := 'typical';
    1: InstallMode := 'custom';
  end;
  SetPreviousData(PreviousDataKey, 'InstallMode', InstallMode);

  {install options page }
  SetArrayLength(InstallOptions, 4);

  if InstallOptionsPage.Values[0] then begin
    InstallOptions[0] := '1';
  end else begin
    InstallOptions[0] := '0';
  end;

  if InstallOptionsPage.Values[1] then begin
    InstallOptions[1] := '1';
  end else begin
    InstallOptions[1] := '0';
  end;

  if InstallOptionsPage.Values[2] then begin
    InstallOptions[2] := '1';
  end else begin
    InstallOptions[2] := '0';
  end;

  if (MatlabPresent) then begin
    if InstallOptionsPage.Values[3] then begin
      InstallOptions[3] := '1';
    end else begin
      InstallOptions[3] := '0';
    end;
  end;

  SetPreviousData(PreviousDataKey, 'Libraries', InstallOptions[0]);
  SetPreviousData(PreviousDataKey, 'Java',      InstallOptions[1]);
  SetPreviousData(PreviousDataKey, 'Python',    InstallOptions[2]);
  if (MatlabPresent) then begin
    SetPreviousData(PreviousDataKey, 'Matlab',    InstallOptions[3]);
  end;

  {python page}
  case PythonPage.SelectedValueIndex of
    0: PythonVers := 'Python 2.3';
    1: PythonVers := 'Python 2.4';
    2: PythonVers := 'Python 2.5';
  end;
  SetPreviousData(PreviousDataKey, 'PythonVers', PythonVers);

  {matlab page}
  SetPreviousData(PreviousDataKey, 'MatlabDir', MatlabPage.Values[0]);
end;

function ShouldSkipPage(PageID: Integer): Boolean;
begin
  { Skip pages that shouldn't be shown }
  if (PageID = InstallOptionsPage.ID) and (InstallTypePage.SelectedValueIndex = 0) then
    Result := True
  else if (PageID = PythonPage.ID) and (InstallOptionsPage.Values[2] = False) then
    Result := True
  else if (PageID = MatlabPage.ID) and (MatlabPresent = False) then
    Result := True
  else if (PageID = MatlabPage.ID) and (InstallOptionsPage.Values[3] = False) then
    Result := True
  else
    Result := False;
end;

function NextButtonClick(CurPageID: Integer): Boolean;
var
  Later: Integer;
begin
  { Validate certain pages before allowing the user to proceed }
  if CurPageID = wpWelcome then begin
    if (libSBMLPresent) then begin
      Later := LaterVersion(PreviousInstalledVersion, ThisVersion);

      if (Later = 0) then begin
        MsgBox('This version of libSBML has already been installed on the system', mbInformation, mb_Ok);
      end else if (Later = 1) then begin
        MsgBox('A later version of libSBML has already been installed on the system', mbInformation, mb_Ok);
      end else begin
        MsgBox('An earlier version of libSBML has already been installed on the system. Files may be overwritten.', mbInformation, mb_Ok);
      end;
    end;
    Result := True;
  end else if CurPageID = MatlabPage.ID then begin
    Later := LaterVersion(MatlabVersion, '7.5.0');
    if (Later > -1) then begin
      if MsgBox('The Xerces version of the libSBML MATLAB binding conflicts with MATLAB Version 2007b and will not work. Install anyway?', mbConfirmation, MB_YESNO or MB_DEFBUTTON2) = IDNO then
        begin
          InstallOptionsPage.Values[3] := False;
      end;
    end;
    Result := True;
  end else begin
    Result := True;
  end;
end;

function UpdateReadyMemo(Space, NewLine, MemoUserInfoInfo, MemoDirInfo, MemoTypeInfo, MemoComponentsInfo, MemoGroupInfo, MemoTasksInfo: String): String;
var
  S: String;
begin
  { Fill the 'Ready Memo' with the normal settings and the custom settings }
  S := '';
  S := S + MemoDirInfo + NewLine;
  S := S + NewLine;


  if (InstallTypePage.SelectedValueIndex = 1) then begin
  {custom installation}
    S := S + 'Custom installation' + NewLine;

    if (InstallOptionsPage.Values[0] = True) then begin
        S := S + NewLine;
        S := S + 'Writing libSBML library files to system directory' + NewLine;
        S := S + '      ' + GetSystemDir();
        S := S + NewLine;
    end else begin
        S := S + NewLine;
        S := S + 'Not writing libSBML library files to system directory' + NewLine;
        S := S + NewLine;
    end;

    if (InstallOptionsPage.Values[1] = True) then begin
        S := S + NewLine;
        S := S + 'Writing libSBML Java library files (sbmlj.*) to system directory' + NewLine;
        S := S + '      ' + GetSystemDir();
        S := S + NewLine;
    end else begin
        S := S + NewLine;
        S := S + 'Not writing libSBML Java library files to system directory' + NewLine;
        S := S + NewLine;
    end;

    if (InstallOptionsPage.Values[2] = True) then begin
        if (PythonPage.SelectedValueIndex = 0) then begin
          S := S + NewLine;
          S := S + 'Writing libSBML Python 2.3 files to site-packages directory' + NewLine;
          S := S + '      ' + GetPython23Dir('');
          S := S + NewLine;
        end else if (PythonPage.SelectedValueIndex = 1) then begin
          S := S + NewLine;
          S := S + 'Writing libSBML Python 2.4 files to site-packages directory' + NewLine;
          S := S + '      ' + GetPython24Dir('');
          S := S + NewLine;
        end else begin
          S := S + NewLine;
          S := S + 'Writing libSBML Python 2.5 files to site-packages directory' + NewLine;
          S := S + '      ' + GetPython25Dir('');
          S := S + NewLine;
        end;
    end else begin
        S := S + NewLine;
        S := S + 'Not writing libSBML Python library files to system directory' + NewLine;
        S := S + NewLine;
    end;

    if (MatlabPresent) then begin
      if(InstallOptionsPage.Values[3] = True) then begin
        S := S + NewLine;
        S := S + 'Installing matlab binding files to ' + NewLine;
        S := S + '      ' + MatlabPage.Values[0];
        S := S + NewLine;
      end else begin
        S := S + NewLine;
        S := S + 'Not installing matlab binding files' + NewLine;
        S := S + NewLine;
      end;
    end;

  end else begin

  {typical installation}
    S := S + 'Typical installation' + NewLine + NewLine;

    S := S + 'Writing libSBML library files to system directory' + NewLine;
    S := S + '      ' + GetSystemDir();
    S := S + NewLine + NewLine;

    S := S + 'Writing libSBML Java library files (sbmlj.*) to system directory' + NewLine;
    S := S + '      ' + GetSystemDir();
    S := S + NewLine + NewLine;

    if (PythonPage.SelectedValueIndex = 0) then begin
      S := S + 'Writing libSBML Python 2.3 library files (_libsbml.*) to system directory' + NewLine;
      S := S + '      ' + GetSystemDir();
      S := S + NewLine + NewLine;
    end else if (PythonPage.SelectedValueIndex = 1) then begin
      S := S + 'Writing libSBML Python 2.4 library files (_libsbml.*) to system directory' + NewLine;
      S := S + '      ' + GetSystemDir();
      S := S + NewLine + NewLine;
    end else begin
      S := S + 'Writing libSBML Python 2.5 library files (_libsbml.*) to system directory' + NewLine;
      S := S + '      ' + GetSystemDir();
      S := S + NewLine + NewLine;
    end;

    if (MatlabPresent) then begin
      S := S + 'Installing matlab binding files to ' + NewLine;
      S := S + '      ' + MatlabPage.Values[0];
      S := S + NewLine + NewLine;
    end;

  end;

  Result := S;
end;

{ function to return flag as to whether to write libraries to system directory}
function GetPython24() : Boolean;
begin
  if (InstallOptionsPage.Values[2] = True) and (PythonPage.SelectedValueIndex = 1) then
    Result := True
  else
    Result := False;
end;

function GetPython23() : Boolean;
begin
  if (InstallOptionsPage.Values[2] = True) and (PythonPage.SelectedValueIndex = 0) then
    Result := True
  else
    Result := False;
end;

function GetPython25() : Boolean;
begin
  if (InstallOptionsPage.Values[2] = True) and (PythonPage.SelectedValueIndex = 2) then
    Result := True
  else
    Result := False;
end;

function GetJava() : Boolean;
begin
  if (InstallOptionsPage.Values[1] = True) then
    Result := True
  else
    Result := False;
end;

function GetLibrary() : Boolean;
begin
  if (InstallOptionsPage.Values[0] = True) then
    Result := True
  else
    Result := False;
end;

function GetMatlab() : Boolean;
begin
  if (MatlabPresent) then begin
    if (InstallOptionsPage.Values[3] = True) then
      Result := True
    else
      Result := False;
  end else begin
    Result := False;
  end;
end;

function GetMatlabDir(Param: String): String;
begin
  { Return the selected DataDir }
  Result := MatlabPage.Values[0];
end;
