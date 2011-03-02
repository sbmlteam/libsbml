; Version No is currently 4.3.0
; Check before use


[Setup]
AppName=libSBML-matlab
AppVerName=libSBML-matlab 4.3.0
AppPublisher=SBML Team
AppPublisherURL=http://sbml.org
AppSupportURL=http://sbml.org
AppUpdatesURL=http://sbml.org
DefaultDirName={pf}\SBML\matlab-libSBML-4.3.0-libxml2-x86
DefaultGroupName=matlab-libSBML
DisableProgramGroupPage=yes
OutputDir=.\Output
OutputBaseFilename=matlab-libSBML-4.3.0-win-libxml2-x86
WizardSmallImageFile=..\graphics\libsbml-installer-mini-logo.bmp
WizardImageFile=..\graphics\libsbml-installer-graphic.bmp
UsePreviousAppDir=no
Compression=lzma
SolidCompression=yes

[Languages]
Name: english; MessagesFile: compiler:Default.isl

[Files]
Source: C:\libsbml_trunk\dev\utilities\win_installer\matlab\libsbml-matlab\*; DestDir: {app}; Flags: ignoreversion recursesubdirs createallsubdirs; Check: GetProceed

[Registry]
;Root: HKCU; Subkey: Software\SBML; Flags: uninsdeletekeyifempty
;Root: HKCU; Subkey: Software\SBML\libSBML; Flags: uninsdeletekey
;Root: HKLM; Subkey: Software\SBML; Flags: uninsdeletekeyifempty
;Root: HKLM; Subkey: Software\SBML\libSBML; Flags: uninsdeletekey
;Root: HKLM; Subkey: Software\SBML\libSBML; ValueType: string; ValueName: Version; ValueData: 4.3.0
;Root: HKLM; Subkey: Software\SBML\libSBML; ValueType: string; ValueName: InstallPath; ValueData: {app}

[Code]
var
  URLLabel: TNewStaticText;
  AboutButton, CancelButton: TButton;

  MatlabPresent: Boolean;
  MatlabVersion: String;
  MatlabRoot: String;
  
  Proceed: Boolean;

function GetRunMatlab(Param: String): String;
begin
  Result:= MatlabRoot;
end;


{function to return matlab root directory}
function GetMatlabRoot(S : String): String;
var
  Names: TArrayOfString;
  Root:String;
  Number: Longint;
  Key: String;
  len: Integer;
  c: Char;

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

  len := Length(root);
  if len = 0 then begin
    Result := Root;
  end else begin
    c := Root[len];
    if not (c = '\') then begin
      Root := Root + '\';
    end;
    Result := Root;
  end;
end;

function GetProceed : Boolean;
begin
  Result := Proceed;
end;

function GetMatlabPresent : Boolean;
begin
  Result := MatlabPresent;
end;
{functions to activate buttons and url on screen}
procedure AboutButtonOnClick(Sender: TObject);
begin

{*********************************************************************************************************
 The text for this message box is what the user will see if they click the About button during installation

 Feel free to alter it to taste but beware it must all be on one line.

  Note: it includes a version number
**********************************************************************************************************}
  MsgBox('This setup installs the Windows version of the MATLAB binding of libSBML 4.3.0 built using the libxml2 2.7.3 XML Parser library. All the necessary libraries are included. The source code is available as a separate download.', mbInformation, mb_Ok);
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
  MatlabRoot := GetMatlabRoot('');
  MatlabRoot := MatlabRoot + 'bin\matlab.exe';
  Proceed := True;

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


end;

procedure CurPageChanged(CurPageID: Integer);
begin
  if (not Proceed and (CurPageID = wpReady)) then
    WizardForm.NextButton.Enabled := False;
end;

function ShouldSkipPage(PageID: Integer): Boolean;
begin
  { Skip pages that shouldn't be shown }
  if (not Proceed and (PageID = wpSelectDir)) then
    Result := True
  else
   Result := False;
end;

function NextButtonClick(CurPageID: Integer): Boolean;

begin
  { Validate certain pages before allowing the user to proceed }
  if CurPageID = wpWelcome then begin
    if (MatlabPresent) then begin
      Proceed := True;
    end else begin
      if MsgBox('MATLAB has not been detected on your system. Files can be copied but not installed. Proceed?', mbError, MB_YESNO or MB_DEFBUTTON2) = IDNO then
        begin
          Proceed := False;
        end else begin
          Proceed := True;
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
  if (Proceed) then begin
   S := '';
   S := S + MemoDirInfo + NewLine;
   S := S + NewLine;
  end else begin
   S := 'Installation cancelled';
   S := S + NewLine;
  end;



  Result:= S;
end;

[Run]


Filename: "{app}\install\install.bat"; Parameters: """{code:GetRunMatlab}""" ; Check: GetMatlabPresent

