{
Ultibo QEMU Launcher Tool.

Copyright (C) 2016 - SoftOz Pty Ltd.

Arch
====

 <All>

Boards
======

 <All>

Licence
=======

 LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)
 
Credits
=======

 Information for this unit was obtained from:

 
References
==========


QEMU Launcher
=============

 The QEMU launcher provides a convenient way to launch the QEMU machine emulator
 with a compiled Ultibo project.

 The tool determines all of the default configuration information automatically but
 this can be overridden by creating a QEMULauncher.ini file in the same directory and
 setting a number of parameters.

 The format of the INI file is:

 [QEMULauncher]
 Path=
 SystemArm=
 SystemAarch64=

 ExtraParams=
 CommandLine=

 A brief explanation of each parameter along with the standard default value:

 Path - The path to the QEMU installation (Default: C:\Ultibo\Core\qemu) (Detected from the application path)
 SystemArm - The name of the QEMU ARM system emulator (Default: qemu-system-arm.exe)
 SystemAarch64 - The name of the QEMU AARCH64 system emulator (Default: qemu-system-aarch64.exe)

 ExtraParams - Any extra parameters to pass to QEMU on launch (Default: <Blank>)
 CommandLine - The command line parameters to pass to the Ultibo application (Default: <Blank>)

}

unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, IniFiles;

type
  TQEMULaunch = class(TObject)
   constructor Create;
  private
   {}
   function GetExe:String;
   function GetCPU:String;
   function GetKernel:String;
   function GetMemory:String;
   function GetMachine:String;
  public
   {QEMU Variables}
   Path:String;
   SystemArm:String;
   SystemAarch64:String;

   ExtraParams:String;
   CommandLine:String;

   {Ultibo Variables}
   Project:String;
   CPU:String;
   Processor:String;
   Controller:String;

   {Public Methods}
   function Launch:Boolean;

   function LoadConfig:Boolean;
   function LoadParams:Boolean;
  end;

  TfrmMain = class(TForm)
    lblProject: TLabel;
    txtProject: TEdit;
    cmdProject: TButton;
    lblCPU: TLabel;
    cmbCPU: TComboBox;
    lblProcessor: TLabel;
    cmbProcessor: TComboBox;
    lblController: TLabel;
    cmbController: TComboBox;
    cmdLaunch: TButton;
    cmdClose: TButton;
    openMain: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure txtProjectChange(Sender: TObject);
    procedure cmdProjectClick(Sender: TObject);
    procedure cmbCPUChange(Sender: TObject);
    procedure cmbProcessorChange(Sender: TObject);
    procedure cmbControllerChange(Sender: TObject);
    procedure cmdLaunchClick(Sender: TObject);
    procedure cmdCloseClick(Sender: TObject);
  private
    { Private declarations }
    FLaunch:TQEMULaunch;

  public
    { Public declarations }
  end;

const
 {From FPCProcessorNames in definetemplates.pas}
 CPUNames:array[0..1] of String = (
  'aarch64',
  'arm');

 {From GetTargetProcessors in definetemplates.pas}
 ProcessorNames:array[0..2] of String = (
  'ARMV6',
  'ARMV7A',
  'ARMV8');

 {From FPCControllerNames in definetemplates.pas}
 ControllerNames:array[0..5] of String = (
  'RPIA',
  'RPIB',
  'RPIZERO',
  'RPI2B',
  'RPI3B',
  'QEMUVPB');

type
 TWow64DisableWow64FsRedirection = function(var OldValue:Pointer):BOOL; stdcall;
 TWow64RevertWow64FsRedirection = function(OldValue:Pointer):BOOL; stdcall;

var
 Wow64FsRedirectionHandle:THandle = 0;

 _Wow64DisableWow64FsRedirection:TWow64DisableWow64FsRedirection = nil;
 _Wow64RevertWow64FsRedirection:TWow64RevertWow64FsRedirection = nil;

function Wow64DisableWow64FsRedirection(var OldValue:Pointer):BOOL; stdcall;
function Wow64RevertWow64FsRedirection(OldValue:Pointer):BOOL; stdcall;

var
  frmMain: TfrmMain;

function AddQuotes(const AValue:String):String;

function AddTrailingSlash(const FilePath:String):String;
function StripTrailingSlash(const FilePath:String):String;

function StripLeadingChar(const AFilePath,ASlashChar:String):String;

function ParameterValue(const AParameter:String):String;
function ParameterValueEx(const AParameter:String;APlus,AMinus:Boolean):String;

function ParameterExists(const AParameter:String):Boolean;
function ParameterExistsEx(const AParameter:String;APlus,AMinus:Boolean):Boolean;

function StartProgramEx(const ACommand:String;AWait,ANoShow:Boolean):Boolean;

implementation

{$R *.DFM}

{==============================================================================}
{==============================================================================}

function Wow64DisableWow64FsRedirection(var OldValue:Pointer):BOOL; stdcall;
begin
 {}
 Result:=False;
 if Assigned(_Wow64DisableWow64FsRedirection) then Result:=_Wow64DisableWow64FsRedirection(OldValue);
end;

{==============================================================================}

function Wow64RevertWow64FsRedirection(OldValue:Pointer):BOOL; stdcall;
begin
 {}
 Result:=False;
 if Assigned(_Wow64RevertWow64FsRedirection) then Result:=_Wow64RevertWow64FsRedirection(OldValue);
end;

{==============================================================================}
{==============================================================================}

function AddQuotes(const AValue:String):String;
var
 WorkBuffer:String;
begin
 {}
 WorkBuffer:=AValue; {Trim(AValue);} {Dont Trim Space is Allowed}
 if Length(WorkBuffer) > 0 then
  begin
   if WorkBuffer[1] <> '"' then
    begin
     WorkBuffer:='"' + WorkBuffer;
    end;
  end
 else
  begin
   WorkBuffer:='"';
  end;
 if Length(WorkBuffer) > 1 then
  begin
   if WorkBuffer[Length(WorkBuffer)] <> '"' then
    begin
     WorkBuffer:=WorkBuffer + '"';
    end;
  end
 else
  begin
   WorkBuffer:=WorkBuffer + '"';
  end;
 Result:=WorkBuffer;
end;

{==============================================================================}

function AddTrailingSlash(const FilePath:String):String;
var
 WorkBuffer:String;
begin
 {}
 WorkBuffer:=Trim(FilePath);
 Result:=WorkBuffer;
 if Length(WorkBuffer) > 0 then
  begin
   if WorkBuffer[Length(WorkBuffer)] <> '\' then
    begin
     Result:=WorkBuffer + '\';
    end;
  end;
end;

{==============================================================================}

function StripTrailingSlash(const FilePath:String):String;
var
 WorkBuffer:String;
begin
 {}
 WorkBuffer:=Trim(FilePath);
 Result:=WorkBuffer;
 if Length(WorkBuffer) > 0 then
  begin
   if WorkBuffer[Length(WorkBuffer)] = '\' then
    begin
     Delete(WorkBuffer,Length(WorkBuffer),1);
     Result:=WorkBuffer;
    end;
  end;
end;

{==============================================================================}

function StripLeadingChar(const AFilePath,ASlashChar:String):String;
var
 WorkBuffer:String;
begin
 {}
 WorkBuffer:=Trim(AFilePath);

 if Length(WorkBuffer) > 0 then
  begin
   if WorkBuffer[1] = ASlashChar then
    begin
     Delete(WorkBuffer,1,1);
    end;
  end;

 Result:=WorkBuffer;
end;

{==============================================================================}

function ParameterValue(const AParameter:String):String;
begin
 {}
 Result:=ParameterValueEx(AParameter,False,True);
end;

{==============================================================================}

function ParameterValueEx(const AParameter:String;APlus,AMinus:Boolean):String;
{Return the value of the command line parameter specified by Parameter}
{Note: Allows for parameters prefixed with Slash (/), Plus (+) or Minus (-)}
var
 Count:Integer;
 PosIdx:Integer;
 Value:String;
 Current:String;
 Parameter:String;
begin
 {}
 Result:='';
 
 {Check Exists}
 if ParameterExistsEx(AParameter,APlus,AMinus) then
  begin
   {Format Parameter}
   Parameter:=Uppercase(Trim(AParameter));
   for Count:=1 to ParamCount do
    begin
     {Get Parameter}
     Current:=Trim(ParamStr(Count));

     {Remove Slash}
     Current:=StripLeadingChar(Current,'/');
     
     {Remove Plus}
     if APlus then Current:=StripLeadingChar(Current,'+');

     {Remove Minus}
     if AMinus then Current:=StripLeadingChar(Current,'-');
     
     {Get First Equals}
     PosIdx:=Pos('=',Current);
     if PosIdx <> 0 then
      begin
       {Get Value}
       Value:=Copy(Current,PosIdx + 1,Length(Current));
       
       {Remove Value}
       Delete(Current,PosIdx,Length(Current));
       
       {Check Parameter} 
       if Parameter = Uppercase(Current) then
        begin
         Result:=Value;
         Exit;
        end;
      end;
    end;
  end;
end;

{==============================================================================}

function ParameterExists(const AParameter:String):Boolean;
begin
 {}
 Result:=ParameterExistsEx(AParameter,False,True);
end;

{==============================================================================}

function ParameterExistsEx(const AParameter:String;APlus,AMinus:Boolean):Boolean;
{Check for the existence of the command line parameter specified by Parameter}
{Note: Allows for parameters prefixed with Slash (/), Plus (+) or Minus (-)}
var
 Count:Integer;
 PosIdx:Integer;
 Current:String;
 Parameter:String;
begin
 {}
 Result:=False;

 {Check Parameter}
 if Trim(AParameter) = '' then Exit;

 {Format Parameter}
 Parameter:=Uppercase(Trim(AParameter));
 for Count:=1 to ParamCount do
  begin
   {Get Parameter}
   Current:=Trim(ParamStr(Count));
   
   {Remove Slash}
   Current:=StripLeadingChar(Current,'/');

   {Remove Plus}
   if APlus then Current:=StripLeadingChar(Current,'+');
   
   {Remove Minus}
   if AMinus then Current:=StripLeadingChar(Current,'-');
   
   {Get First Equals}
   PosIdx:=Pos('=',Current);
   if PosIdx <> 0 then
    begin
     {Remove Value}
     Delete(Current,PosIdx,Length(Current));
    end;
    
   {Check Parameter}
   if Parameter = Uppercase(Current) then
    begin
     Result:=True;
     Exit;
    end;
  end;
end;

{==============================================================================}

function StartProgramEx(const ACommand:String;AWait,ANoShow:Boolean):Boolean;
var
 OldValue:Pointer;
 WorkBuffer:String;
 ExitCode:LongWord;
 StartupInfo:TStartupInfo;
 ProcessInformation:TProcessInformation;
begin
 {}
 Result:=False;
 try
  Wow64DisableWow64FsRedirection(OldValue);
  try
   WorkBuffer:=ACommand;

   FillChar(StartupInfo,SizeOf(StartupInfo),#0);
   StartupInfo.cb:=SizeOf(TStartupInfo);
   if ANoShow then
    begin
     StartupInfo.dwFlags:=STARTF_USESHOWWINDOW;
     StartupInfo.wShowWindow:=SW_HIDE;
    end; 

   FillChar(ProcessInformation,SizeOf(ProcessInformation),#0);
   if CreateProcess(nil,PChar(WorkBuffer),nil,nil,False,0,nil,nil,StartupInfo,ProcessInformation) then
    begin
     CloseHandle(ProcessInformation.hThread);

     if AWait then
      begin
       ExitCode:=STILL_ACTIVE;
       while GetExitCodeProcess(ProcessInformation.hProcess,ExitCode) do
        begin
         Application.ProcessMessages;
         if ExitCode <> STILL_ACTIVE then Break;
        end;

       CloseHandle(ProcessInformation.hProcess);
      end
     else
      begin
       CloseHandle(ProcessInformation.hProcess);
      end;
     Result:=True;
    end;
  finally
   Wow64RevertWow64FsRedirection(OldValue);
  end;
 except
  {}
 end;
end;

{==============================================================================}
{==============================================================================}

constructor TQEMULaunch.Create;
var
 InstallPath:String;
begin
 {}
 inherited Create;

 {Assume that this is running from <InstallPath>\tools and that the InstallPath will be the folder above}
 InstallPath:=ExtractFileDir(ExtractFileDir(Application.ExeName));

 {QEMU Variables}
 Path:=AddTrailingSlash(InstallPath) + 'qemu';
 SystemArm:='qemu-system-arm.exe';
 SystemAarch64:='qemu-system-aarch64.exe';

 ExtraParams:='';
 CommandLine:='';

 {Ultibo Variables}
 Project:='';
 CPU:='';
 Processor:='';
 Controller:='';
end;

{==============================================================================}

function TQEMULaunch.GetExe:String;
begin
 {}
 Result:='';

 if Uppercase(CPU) = 'ARM' then
  begin
   Result:=SystemArm;
  end
 else if Uppercase(CPU) = 'AARCH64' then
  begin
   Result:=SystemAarch64;
  end;
end;

{==============================================================================}

function TQEMULaunch.GetCPU:String;
begin
 {}
 Result:='';

 if Uppercase(CPU) = 'ARM' then
  begin
   if Uppercase(Processor) = 'ARMV6' then
    begin
     //Result:='arm1176'; //Not yet supported
    end
   else if Uppercase(Processor) = 'ARMV7A' then
    begin
     Result:='cortex-a8';
    end;
  end
 else if Uppercase(CPU) = 'AARCH64' then
  begin
   if Uppercase(Processor) = 'ARMV8' then
    begin
     Result:='cortex-a53';
    end;
  end;
end;

{==============================================================================}

function TQEMULaunch.GetKernel:String;
begin
 {}
 Result:='';

 if Uppercase(CPU) = 'ARM' then
  begin
   if Uppercase(Controller) = 'RPIA' then
    begin
     //Result:='kernel.img'; //Not yet supported
    end
   else if Uppercase(Controller) = 'RPIB' then
    begin
     //Result:='kernel.img'; //Not yet supported
    end
   else if Uppercase(Controller) = 'RPIZERO' then
    begin
     //Result:='kernel.img'; //Not yet supported
    end
   else if Uppercase(Controller) = 'RPI2B' then
    begin
     //Result:='kernel7.img'; //Not yet supported
    end
   else if Uppercase(Controller) = 'RPI3B' then
    begin
     //Result:='kernel7.img'; //Not yet supported
    end
   else if Uppercase(Controller) = 'QEMUVPB' then
    begin
     Result:='kernel.bin';
    end;
  end
 else if Uppercase(CPU) = 'AARCH64' then
  begin
   if Uppercase(Controller) = 'RPI3B' then
    begin
     //Result:='kernel8.img'; //Not yet supported
    end
   else if Uppercase(Controller) = 'QEMUVPB' then
    begin
     Result:='kernel64.bin';
    end;
  end;
end;

{==============================================================================}

function TQEMULaunch.GetMemory:String;
begin
 {}
 Result:='';

 if Uppercase(CPU) = 'ARM' then
  begin
   if Uppercase(Controller) = 'RPIA' then
    begin
     //Result:='256M'; //Not yet supported
    end
   else if Uppercase(Controller) = 'RPIB' then
    begin
     //Result:='512M'; //Not yet supported
    end
   else if Uppercase(Controller) = 'RPIZERO' then
    begin
     //Result:='1024M'; //Not yet supported
    end
   else if Uppercase(Controller) = 'RPI2B' then
    begin
     //Result:='1024M'; //Not yet supported
    end
   else if Uppercase(Controller) = 'RPI3B' then
    begin
     //Result:='1024M'; //Not yet supported
    end
   else if Uppercase(Controller) = 'QEMUVPB' then
    begin
     Result:='256M';
    end;
  end
 else if Uppercase(CPU) = 'AARCH64' then
  begin
   if Uppercase(Controller) = 'RPI3B' then
    begin
     //Result:='1024M'; //Not yet supported
    end
   else if Uppercase(Controller) = 'QEMUVPB' then
    begin
     Result:='256M';
    end;
  end;
end;

{==============================================================================}

function TQEMULaunch.GetMachine:String;
begin
 {}
 Result:='';

 if Uppercase(CPU) = 'ARM' then
  begin
   if Uppercase(Controller) = 'RPIA' then
    begin
     //Result:='raspi'; //Not yet supported
    end
   else if Uppercase(Controller) = 'RPIB' then
    begin
     //Result:='raspi'; //Not yet supported
    end
   else if Uppercase(Controller) = 'RPIZERO' then
    begin
     //Result:='raspi'; //Not yet supported
    end
   else if Uppercase(Controller) = 'RPI2B' then
    begin
     //Result:='raspi2'; //Not yet supported
    end
   else if Uppercase(Controller) = 'RPI3B' then
    begin
     //Result:='raspi2'; //Not yet supported
    end
   else if Uppercase(Controller) = 'QEMUVPB' then
    begin
     Result:='versatilepb';
    end;
  end
 else if Uppercase(CPU) = 'AARCH64' then
  begin
   if Uppercase(Controller) = 'RPI3B' then
    begin
     //Result:='raspi2'; //Not yet supported
    end
   else if Uppercase(Controller) = 'QEMUVPB' then
    begin
     Result:='versatilepb';
    end;
  end;
end;

{==============================================================================}

function TQEMULaunch.Launch:Boolean;
var
 Param:String;
 Command:String;
 ProjectPath:String;
begin
 {}
 Result:=False;
 try
  {Create Command}
  Command:='';

  {Get Exe}
  Param:=GetExe;
  if Length(Param) = 0 then
   begin
    MessageDlg('Error: Unable to determine QEMU system emulator name, make sure you have selected a valid CPU type.',mtInformation,[mbOk],0);
    Exit;
   end;

  {Get Path}
  Param:=AddTrailingSlash(Path) + Param;
  if FileExists(Param) then
   begin
    {Add Exe}
    Command:=AddQuotes(Param);

    {Get Machine}
    Param:=GetMachine;
    if Length(Param) = 0 then
     begin
      MessageDlg('Error: Unable to determine QEMU machine, make sure you have selected a valid Board type.',mtInformation,[mbOk],0);
      Exit;
     end;

    {Add Machine}
    Command:=Command + ' -M ' + Param;

    {Get CPU}
    Param:=GetCPU;
    if Length(Param) = 0 then
     begin
      MessageDlg('Error: Unable to determine QEMU CPU, make sure you have selected a valid CPU model.',mtInformation,[mbOk],0);
      Exit;
     end;

    {Add CPU}
    Command:=Command + ' -cpu ' + Param;

    {Get Kernel}
    Param:=GetKernel;
    if Length(Param) = 0 then
     begin
      MessageDlg('Error: Unable to determine kernel name, make sure you have selected valid options for Board and CPU type.',mtInformation,[mbOk],0);
      Exit;
     end;

    {Get Path}
    ProjectPath:=ExtractFileDir(Project);
    if FileExists(AddTrailingSlash(ProjectPath) + Param) then
     begin
      {Add Kernel}
      Command:=Command + ' -kernel ' + AddQuotes(AddTrailingSlash(ProjectPath) + Param);

      {Get Memory}
      Param:=GetMemory;
      if Length(Param) = 0 then
       begin
        MessageDlg('Error: Unable to determine memory size, make sure you have selected valid options for Board and CPU type.',mtInformation,[mbOk],0);
        Exit;
       end;

      {Add Memory}
      Command:=Command + ' -m ' + Param;

      {Add USB}
      Command:=Command + ' -usb';

      {Add Append}
      Command:=Command + ' -append ' + AddQuotes(CommandLine);

      {Add Extras}
      Command:=Command + ' ' + ExtraParams;

      {Start Program}
      Result:=StartProgramEx(Command,True,False);
     end
    else
     begin
      MessageDlg('Error: Unable to locate kernel file ' + AddTrailingSlash(ProjectPath) + Param + '.',mtInformation,[mbOk],0);
     end;
   end
  else
   begin
    MessageDlg('Error: Unable to locate QEMU system emulator ' + Param + '.',mtInformation,[mbOk],0);
   end;
 except
  {}
 end;
end;

{==============================================================================}

function TQEMULaunch.LoadConfig:Boolean;
var
 Section:String;
 Filename:String;
 IniFile:TIniFile;
begin
 {}
 Result:=False;
 try
  {Get Filename}
  Filename:=ChangeFileExt(Application.ExeName,'.ini');

  {Check File}
  if FileExists(Filename) then
   begin
    IniFile:=TIniFile.Create(Filename);
    try
     Section:='QEMULauncher';

     {Get Path}
     Path:=IniFile.ReadString(Section,'Path',Path);
     {Get SystemArm}
     SystemArm:=IniFile.ReadString(Section,'SystemArm',SystemArm);
     {Get SystemAarch64}
     SystemAarch64:=IniFile.ReadString(Section,'SystemAarch64',SystemAarch64);
     {Get ExtraParams}
     ExtraParams:=IniFile.ReadString(Section,'ExtraParams',ExtraParams);
     {Get CommandLine}
     CommandLine:=IniFile.ReadString(Section,'CommandLine',CommandLine);
    finally
     IniFile.Free;
    end;
   end;

  Result:=True;
 except
  {}
 end;
end;

{==============================================================================}

function TQEMULaunch.LoadParams:Boolean;
begin
 {}
 Result:=False;
 try
  {Get Project}
  Project:=ParameterValue('PROJECT');
  {Get CPU}
  CPU:=ParameterValue('CPU');
  {Get Processor}
  Processor:=ParameterValue('PROCESSOR');
  {Get Controller}
  Controller:=ParameterValue('CONTROLLER');

  Result:=True;
 except
  {}
 end;
end;

{==============================================================================}
{==============================================================================}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
 {}
 FLaunch:=TQEMULaunch.Create;
 FLaunch.LoadConfig;
 FLaunch.LoadParams;
end;

{==============================================================================}

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
 {}
 FLaunch.Free;
end;

{==============================================================================}

procedure TfrmMain.FormShow(Sender: TObject);
var
 Scale:Double;
 Count:Integer;
begin
 {}
 cmbCPU.Clear;
 for Count:=0 to 1 do
  begin
   cmbCPU.Items.Add(CPUNames[Count]);
  end;

 cmbProcessor.Clear;
 for Count:=0 to 2 do
  begin
   cmbProcessor.Items.Add(ProcessorNames[Count]);
  end;

 cmbController.Clear;
 for Count:=0 to 5 do
  begin
   cmbController.Items.Add(ControllerNames[Count]);
  end;

 txtProject.Text:=FLaunch.Project;
 cmbCPU.ItemIndex:=cmbCPU.Items.IndexOf(FLaunch.CPU);
 cmbProcessor.ItemIndex:=cmbProcessor.Items.IndexOf(FLaunch.Processor);
 cmbController.ItemIndex:=cmbController.Items.IndexOf(FLaunch.Controller);

 {Check PixelsPerInch}
 if PixelsPerInch > 96 then
  begin
   {Calculate Scale}
   Scale:=(PixelsPerInch / 96);

   {Disable Anchors}
   txtProject.Anchors:=[akLeft,akTop];
   cmdProject.Anchors:=[akLeft,akTop];
   cmdLaunch.Anchors:=[akLeft,akTop];
   cmdClose.Anchors:=[akLeft,akTop];

   {Resize Form}
   Width:=Trunc(Width * Scale);
   Height:=Trunc(Height * Scale);

   {Adjust Text}
   {txtProject.Width:=Trunc(469 * Scale);}

   {Adjust Button}
   cmdProject.Top:=txtProject.Top;
   cmdProject.Height:=txtProject.Height;
   cmdProject.Width:=cmdProject.Height;

   {Move Buttons}
   {cmdProject.Left:=Width - Trunc(46 * Scale);}  {602 - 556 = 46}
   {cmdLaunch.Left:=Width - Trunc(186 * Scale);}  {602 - 416 = 186}
   {cmdClose.Left:=Width - Trunc(102 * Scale);}   {602 - 500 = 102}

   {Enable Anchors}
   txtProject.Anchors:=[akLeft,akTop,akRight];
   cmdProject.Anchors:=[akTop,akRight];
   cmdLaunch.Anchors:=[akRight,akBottom];
   cmdClose.Anchors:=[akRight,akBottom];
  end;
end;

{==============================================================================}

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 {}

end;

{==============================================================================}

procedure TfrmMain.txtProjectChange(Sender: TObject);
begin
 {}
 FLaunch.Project:=txtProject.Text;
end;

{==============================================================================}

procedure TfrmMain.cmdProjectClick(Sender: TObject);
begin
 {}
 openMain.Title:='Select project';
 openMain.Filter:='Lararus projects|*.lpr|All files|*.*';
 if Length(openMain.InitialDir) = 0 then
  begin
   openMain.InitialDir:=ExtractFilePath(Application.ExeName);
  end;
 if openMain.Execute then
  begin
   txtProject.Text:=openMain.FileName;
  end;
end;

{==============================================================================}

procedure TfrmMain.cmbCPUChange(Sender: TObject);
begin
 {}
 if cmbCPU.ItemIndex = -1 then Exit;
 FLaunch.CPU:=cmbCPU.Items[cmbCPU.ItemIndex];
end;

{==============================================================================}

procedure TfrmMain.cmbProcessorChange(Sender: TObject);
begin
 {}
 if cmbProcessor.ItemIndex = -1 then Exit;
 FLaunch.Processor:=cmbProcessor.Items[cmbProcessor.ItemIndex];
end;

{==============================================================================}

procedure TfrmMain.cmbControllerChange(Sender: TObject);
begin
 {}
 if cmbController.ItemIndex = -1 then Exit;
 FLaunch.Controller:=cmbController.Items[cmbController.ItemIndex];
end;

{==============================================================================}

procedure TfrmMain.cmdLaunchClick(Sender: TObject);
begin
 {}
 lblProject.Enabled:=False;
 txtProject.Enabled:=False;
 cmdProject.Enabled:=False;
 lblCPU.Enabled:=False;
 cmbCPU.Enabled:=False;
 lblProcessor.Enabled:=False;
 cmbProcessor.Enabled:=False;
 lblController.Enabled:=False;
 cmbController.Enabled:=False;
 cmdLaunch.Enabled:=False;
 cmdClose.Enabled:=False;
 try
  FLaunch.Launch;
 finally
  lblProject.Enabled:=True;
  txtProject.Enabled:=True;
  cmdProject.Enabled:=True;
  lblCPU.Enabled:=True;
  cmbCPU.Enabled:=True;
  lblProcessor.Enabled:=True;
  cmbProcessor.Enabled:=True;
  lblController.Enabled:=True;
  cmbController.Enabled:=True;
  cmdLaunch.Enabled:=True;
  cmdClose.Enabled:=True;
 end;
end;

{==============================================================================}

procedure TfrmMain.cmdCloseClick(Sender: TObject);
begin
 {}
 Application.Terminate;
end;

{==============================================================================}
{==============================================================================}

initialization
 Wow64FsRedirectionHandle:=LoadLibrary(PChar(kernel32));
 if Wow64FsRedirectionHandle > HINSTANCE_ERROR then
  begin
   _Wow64DisableWow64FsRedirection:=GetProcAddress(Wow64FsRedirectionHandle,'Wow64DisableWow64FsRedirection');
   _Wow64RevertWow64FsRedirection:=GetProcAddress(Wow64FsRedirectionHandle,'Wow64RevertWow64FsRedirection');
  end;

{==============================================================================}

finalization
 if Wow64FsRedirectionHandle > HINSTANCE_ERROR then FreeLibrary(Wow64FsRedirectionHandle);

{==============================================================================}
{==============================================================================}

end.
