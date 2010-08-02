; Version No is currently 4.1.0
; Check before use


[Setup]
AppName=libSBML
AppVerName=libSBML 4.1.0
AppPublisher=SBML Team
AppPublisherURL=http://sbml.org
AppSupportURL=http://sbml.org
AppUpdatesURL=http://sbml.org
DefaultDirName={pf}\SBML\libSBML-4.1.0-libxml2-x86
DefaultGroupName=libSBML
DisableProgramGroupPage=yes
OutputDir=.\Output
OutputBaseFilename=libSBML-4.1.0-win-libxml2-x86
WizardSmallImageFile=.\graphics\libsbml-installer-mini-logo.bmp
WizardImageFile=.\graphics\libsbml-installer-graphic.bmp
UsePreviousAppDir=no
Compression=lzma
SolidCompression=yes

[Languages]
Name: english; MessagesFile: compiler:Default.isl

[Files]
Source: C:\libsbml_trunk\dev\utilities\win_installer\libsbml\*; DestDir: {app}; Flags: ignoreversion recursesubdirs createallsubdirs
Source: C:\libsbml_trunk\dev\utilities\win_installer\libsbml\bindings\java\*; DestDir: {code:GetJavaDir}; Flags: ignoreversion recursesubdirs createallsubdirs; Check: GetJava
Source: C:\libsbml_trunk\dev\utilities\win_installer\libsbml\bindings\matlab\*; DestDir: {code:GetMatlabDir}; Flags: ignoreversion recursesubdirs createallsubdirs; Check: GetMatlab
Source: C:\libsbml_trunk\dev\utilities\win_installer\libsbml\bindings\octave\*; DestDir: {code:GetOctaveDir}; Flags: ignoreversion recursesubdirs createallsubdirs; Check: GetOctave
Source: C:\libsbml_trunk\dev\utilities\win_installer\libsbml\bindings\csharp\*; DestDir: {code:GetCSharpDir}; Flags: ignoreversion recursesubdirs createallsubdirs; Check: GetCSharp
Source: C:\libsbml_trunk\dev\utilities\win_installer\libsbml\bindings\python\python25\libsbml.py; DestDir: {code:GetPython25Dir}; Check: GetPython25
Source: C:\libsbml_trunk\dev\utilities\win_installer\libsbml\bindings\python\python25\_libsbml.pyd; DestDir: {code:GetPython25Dir}; Check: GetPython25
Source: C:\libsbml_trunk\dev\utilities\win_installer\libsbml\bindings\python\python26\libsbml.py; DestDir: {code:GetPython26Dir}; Check: GetPython26
Source: C:\libsbml_trunk\dev\utilities\win_installer\libsbml\bindings\perl\*; DestDir: {code:GetPerlDir}; Flags: ignoreversion recursesubdirs createallsubdirs; Check: GetPerl
;Source: C:\libsbml_trunk\dev\utilities\win_installer\libsbml\bindings\python\python26\_libsbml.pyd; DestDir: {code:GetPython26Dir}; Check: GetPython26

[Registry]
Root: HKCU; Subkey: Software\SBML; Flags: uninsdeletekeyifempty
Root: HKCU; Subkey: Software\SBML\libSBML; Flags: uninsdeletekey
Root: HKLM; Subkey: Software\SBML; Flags: uninsdeletekeyifempty
Root: HKLM; Subkey: Software\SBML\libSBML; Flags: uninsdeletekey
Root: HKLM; Subkey: Software\SBML\libSBML; ValueType: string; ValueName: Version; ValueData: 4.1.0
Root: HKLM; Subkey: Software\SBML\libSBML; ValueType: string; ValueName: InstallPath; ValueData: {app}

[Code]
var
  InstallOptionsPage: TInputOptionWizardPage;
  PythonPage: TInputOptionWizardPage;
  MatlabPage: TInputDirWizardPage;
  CSharpPage: TInputDirWizardPage;
  JavaPage: TInputDirWizardPage;
  OctavePage: TInputDirWizardPage;
  PerlPage: TInputDirWizardPage;
  URLLabel: TNewStaticText;
  AboutButton, CancelButton: TButton;

  MatlabPresent: Boolean;
  MatlabVersion: String;
  MatlabRoot: String;
  CSharpRoot: String;
  JavaRoot: String;
  OctaveRoot: String;
  PerlRoot: String;

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

{function to return python 2.6 dir directory}
function GetPython26Dir(S : String): String;
var
  Root:String;
  Key: String;

begin
  Key := '';
  Key := Key + 'Software\Python\PythonCore\2.6\InstallPath\';
  RegQueryStringValue(HKLM, Key, '', Root);

  Root:= Root + 'Lib\site-packages\';
  Result := Root;
end;

function GetJavaDir(Param: String): String;
begin
  { Return the selected DataDir }
    Result := JavaPage.Values[0];
end;

function GetPerlDir(Param: String): String;
begin
  { Return the selected DataDir }
    Result := PerlPage.Values[0];
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
  MsgBox('This setup installs the Windows version of libSBML 4.1.0 built using the libxml2 2.7.3 XML Parser libraries. All the necessary libraries are included. The source code is available as a seperate download.', mbInformation, mb_Ok);
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
  ThisVersion := '4.1.0';
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

  { install options : custom install possible settings }
  InstallOptionsPage := CreateInputOptionPage(wpSelectDir,
    'Customise installation', '',
    'Select the bindings to install',
    False, False);
  InstallOptionsPage.Add('Install C# binding');
  InstallOptionsPage.Add('Install Java binding');
  InstallOptionsPage.Add('Install MATLAB binding');
  InstallOptionsPage.Add('Install Octave binding');
  InstallOptionsPage.Add('Install Perl binding');
  InstallOptionsPage.Add('Install Python binding');

  { python page : version of python builds to be installed }
  PythonPage := CreateInputOptionPage(InstallOptionsPage.ID,
    'Python binding', '',
    'Select the version of python you wish to install', True, False);
  PythonPage.Add('Python 2.5');
  PythonPage.Add('Python 2.6');


  { perl page : location to install perl binding }
  PerlPage := CreateInputDirPage(InstallOptionsPage.ID,
    'Perl binding', '',
    'Select the folder in which Setup should install Perl binding files, then click Next.',
    False, '');
  PerlPage.Add('');

 { Octave page : location to install Octave binding }
  OctavePage := CreateInputDirPage(InstallOptionsPage.ID,
    'Octave binding', '',
    'Select the folder in which Setup should install Octave binding files, then click Next.',
    False, '');
  OctavePage.Add('');


  { matlab page : location to install matlab binding }
  MatlabPage := CreateInputDirPage(InstallOptionsPage.ID,
    'MATLAB binding', '',
    'Select the folder in which Setup should install MATLAB binding files, then click Next.',
    False, '');
  MatlabPage.Add('');

  { java page : location to install java binding }
  JavaPage := CreateInputDirPage(InstallOptionsPage.ID,
    'Java binding', '',
    'Select the folder in which Setup should install Java binding files, then click Next.',
    False, '');
  JavaPage.Add('');

  { csharp page : location to install csharp binding }
  CSharpPage := CreateInputDirPage(InstallOptionsPage.ID,
    'C Sharp binding', '',
    'Select the folder in which Setup should install C# binding files, then click Next.',
    False, '');
  CSharpPage.Add('');


  { Set default values, using settings that were stored last time if possible }

  {install options page}
  if GetPreviousData('CSharp', '') = '0' then begin
    InstallOptionsPage.Values[0] := False;
  end else begin
    InstallOptionsPage.Values[0] := True;
  end;

  if GetPreviousData('Java', '') = '0' then begin
    InstallOptionsPage.Values[1] := False;
  end else begin
    InstallOptionsPage.Values[1] := True;
  end;

  if GetPreviousData('Matlab', '') = '0' then begin
    InstallOptionsPage.Values[2] := False;
  end else begin
    InstallOptionsPage.Values[2] := True;
  end;

  if GetPreviousData('Octave', '') = '0' then begin
    InstallOptionsPage.Values[3] := False;
  end else begin
    InstallOptionsPage.Values[3] := True;
  end;
  
  if GetPreviousData('Perl', '') = '0' then begin
    InstallOptionsPage.Values[4] := False;
  end else begin
    InstallOptionsPage.Values[4] := True;
  end;

  if GetPreviousData('Python', '') = '0' then begin
    InstallOptionsPage.Values[5] := False;
  end else begin
    InstallOptionsPage.Values[5] := True;
  end;

  {python page}
  case GetPreviousData('PythonVers', '') of
    'Python 2.5': PythonPage.SelectedValueIndex := 0;
    'Python 2.6': PythonPage.SelectedValueIndex := 1;
   else
    PythonPage.SelectedValueIndex := 1;
  end;

  {matlab page}
  MatlabPage.Values[0] := GetPreviousData('MatlabDir', MatlabRoot);

  {csharp page}
  CSharpPage.Values[0] := GetPreviousData('CSharpDir', CSharpRoot);
  
  {java page}
  JavaPage.Values[0] := GetPreviousData('JavaDir', JavaRoot);

  {octave page}
  OctavePage.Values[0] := GetPreviousData('OctaveDir', OctaveRoot);

  {perl page}
  PerlPage.Values[0] := GetPreviousData('PerlDir', PerlRoot);

end;

procedure RegisterPreviousData(PreviousDataKey: Integer);
var
  InstallOptions: TArrayOfString;
  PythonVers: String;
begin
  { Store the settings so we can restore them next time }

  {install options page }
  SetArrayLength(InstallOptions, 7);

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

  if InstallOptionsPage.Values[3] then begin
    InstallOptions[3] := '1';
  end else begin
    InstallOptions[3] := '0';
  end;

  if InstallOptionsPage.Values[4] then begin
    InstallOptions[4] := '1';
  end else begin
    InstallOptions[4] := '0';
  end;

  if InstallOptionsPage.Values[5] then begin
    InstallOptions[5] := '1';
  end else begin
    InstallOptions[5] := '0';
  end;



  SetPreviousData(PreviousDataKey, 'CSharp',  InstallOptions[0]);
  SetPreviousData(PreviousDataKey, 'Java',    InstallOptions[1]);
  SetPreviousData(PreviousDataKey, 'Matlab',  InstallOptions[2]);
  SetPreviousData(PreviousDataKey, 'Octave',  InstallOptions[3]);
  SetPreviousData(PreviousDataKey, 'Perl',    InstallOptions[4]);
  SetPreviousData(PreviousDataKey, 'Python',  InstallOptions[5]);

  {python page}
  case PythonPage.SelectedValueIndex of
    0: PythonVers := 'Python 2.5';
    1: PythonVers := 'Python 2.6';
   end;
  SetPreviousData(PreviousDataKey, 'PythonVers', PythonVers);

  {matlab page}
  SetPreviousData(PreviousDataKey, 'MatlabDir', MatlabPage.Values[0]);

  {csharp page}
  SetPreviousData(PreviousDataKey, 'CSharpDir', CSharpPage.Values[0]);

  {java page}
  SetPreviousData(PreviousDataKey, 'JavaDir', JavaPage.Values[0]);

  {java page}
  SetPreviousData(PreviousDataKey, 'OctaveDir', OctavePage.Values[0]);

  {java page}
  SetPreviousData(PreviousDataKey, 'PerlDir', PerlPage.Values[0]);

end;

function ShouldSkipPage(PageID: Integer): Boolean;
begin
  { Skip pages that shouldn't be shown }
  if (PageID = JavaPage.ID) and (InstallOptionsPage.Values[1] = False) then
    Result := True
  else if (PageID = PythonPage.ID) and (InstallOptionsPage.Values[5] = False) then
    Result := True
  else if (PageID = CSharpPage.ID) and (InstallOptionsPage.Values[0] = False) then
    Result := True
  else if (PageID = OctavePage.ID) and (InstallOptionsPage.Values[3] = False) then
    Result := True
  else if (PageID = MatlabPage.ID) and (InstallOptionsPage.Values[2] = False) then
    Result := True
  else if (PageID = PerlPage.ID) and (InstallOptionsPage.Values[4] = False) then
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
  end else if CurPageId = MatlabPage.ID  then begin
    if (MatlabPresent) then begin
      Result := True;
    end else begin
      if MsgBox('MATLAB has not been detected on your system. Install anyway?', mbConfirmation, MB_YESNO or MB_DEFBUTTON2) = IDNO then
        begin
          InstallOptionsPage.Values[2] := False;
      end;
      Result := True;
    end;
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


  S := S + 'The following binding installation options have been selected' + NewLine;

  if (InstallOptionsPage.Values[0] = True) then begin
      S := S + NewLine;
      S := S + 'Writing libSBML C# library files (libsbmlcs.*) to ' + NewLine;
      S := S + '      ' + CSharpPage.Values[0];
      S := S + NewLine;
  end else begin
      S := S + NewLine;
      S := S + 'Not installing libSBML C# library files' + NewLine;
      S := S + NewLine;
  end;

  if (InstallOptionsPage.Values[1] = True) then begin
      S := S + NewLine;
      S := S + 'Writing libSBML Java library files (sbmlj.*) to ' + NewLine;
      S := S + '      ' + JavaPage.Values[0];
      S := S + NewLine;
  end else begin
      S := S + NewLine;
      S := S + 'Not installing libSBML Java library files' + NewLine;
      S := S + NewLine;
  end;

  if(InstallOptionsPage.Values[2] = True) then begin
      S := S + NewLine;
      S := S + 'Writing libSBML MATLAB files to ' + NewLine;
      S := S + '      ' + MatlabPage.Values[0];
      S := S + NewLine;
  end else begin
      S := S + NewLine;
      S := S + 'Not installing libSBML MATLAB files' + NewLine;
      S := S + NewLine;
  end;
  
  if (InstallOptionsPage.Values[3] = True) then begin
      S := S + NewLine;
      S := S + 'Writing libSBML Octave files  to ' + NewLine;
      S := S + '      ' + OctavePage.Values[0];
      S := S + NewLine;
  end else begin
      S := S + NewLine;
      S := S + 'Not installing libSBML Octave files' + NewLine;
      S := S + NewLine;
  end;

  if (InstallOptionsPage.Values[4] = True) then begin
      S := S + NewLine;
      S := S + 'Writing libSBML perl library files (to ' + NewLine;
      S := S + '      ' + PerlPage.Values[0];
      S := S + NewLine;
  end else begin
      S := S + NewLine;
      S := S + 'Not installing libSBML perl library files' + NewLine;
      S := S + NewLine;
  end;

  if (InstallOptionsPage.Values[5] = True) then begin
      if (PythonPage.SelectedValueIndex = 0) then begin
        S := S + NewLine;
        S := S + 'Writing libSBML Python 2.5 files to site-packages directory' + NewLine;
        S := S + '      ' + GetPython25Dir('');
        S := S + NewLine;
      end else if (PythonPage.SelectedValueIndex = 1) then begin
        S := S + NewLine;
        S := S + 'Writing libSBML Python 2.6 files to site-packages directory' + NewLine;
        S := S + '      ' + GetPython26Dir('');
        S := S + NewLine;
       end;
  end else begin
      S := S + NewLine;
      S := S + 'Not installing libSBML Python library files' + NewLine;
      S := S + NewLine;
  end;



  S := S + NewLine;
  S := S + 'NOTE: all files will be available in the main destination location' + NewLine;

  Result:= S;
end;

{ function to return flag as to whether to write libraries to system directory}
function GetPython26() : Boolean;
begin
  if (InstallOptionsPage.Values[5] = True) and (PythonPage.SelectedValueIndex = 1) then
    Result := True
  else
    Result := False;
end;

function GetPython25() : Boolean;
begin
  if (InstallOptionsPage.Values[5] = True) and (PythonPage.SelectedValueIndex = 0) then
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

function GetPerl() : Boolean;
begin
  if (InstallOptionsPage.Values[4] = True) then
    Result := True
  else
    Result := False;
end;

function GetMatlab() : Boolean;
begin
    if (InstallOptionsPage.Values[2] = True) then
      Result := True
    else
      Result := False;
end;

function GetMatlabDir(Param: String): String;
begin
  { Return the selected DataDir }
  Result := MatlabPage.Values[0];
end;

function GetCSharp() : Boolean;
begin
  if (InstallOptionsPage.Values[0] = True) then
    Result := True
  else
    Result := False;
end;

function GetCSharpDir(Param: String): String;
begin
  { Return the selected DataDir }
  Result := CSharpPage.Values[0];
end;

function GetOctave() : Boolean;
begin
  if (InstallOptionsPage.Values[3] = True) then
    Result := True
  else
    Result := False;
end;

function GetOctaveDir(Param: String): String;
begin
  { Return the selected DataDir }
  Result := OctavePage.Values[0];
end;
