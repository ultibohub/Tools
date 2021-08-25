{
Ultibo QEMU Launcher Tool.

Copyright (C) 2021 - SoftOz Pty Ltd.

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

 
 A QEMULauncher.ini file can also be created in the same directory as a the project file (the .lpi file)
 and used to provide project specific parameters such as network settings or disk images to attach.

 Any settings contained in a QEMULauncher.ini file in the project directory will override the same settings
 in the default QEMULauncher.ini file.
}

unit Main;

{$MODE Delphi}

interface

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF LINUX}
  BaseUnix,
  {$ENDIF}
  LCLIntf,
  LCLType,
  LMessages,
  Messages,
  SysUtils,
  Classes,
  {$IFDEF FPC}
  Process,
  {$ENDIF}
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  ComCtrls,
  IniFiles;

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
   function GetDevices:String;
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
   function LoadProjectConfig:Boolean;
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
 ControllerNames:array[0..13] of String = (
  'RPIA',
  'RPIB',
  'RPIZERO',
  'RPI2B',
  'RPI3A',
  'RPI3B',
  'RPI4B',
  'RPI400',
  'QEMUVPB',
  'QEMURPIA',
  'QEMURPIZERO',
  'QEMURPI2B',
  'QEMURPI3A',
  'QEMURPI3B'
  );

{$IFDEF WINDOWS}
type
 TWow64DisableWow64FsRedirection = function(var OldValue:Pointer):BOOL; stdcall;
 TWow64RevertWow64FsRedirection = function(OldValue:Pointer):BOOL; stdcall;

var
 Wow64FsRedirectionHandle:THandle = 0;

 _Wow64DisableWow64FsRedirection:TWow64DisableWow64FsRedirection = nil;
 _Wow64RevertWow64FsRedirection:TWow64RevertWow64FsRedirection = nil;

function Wow64DisableWow64FsRedirection(var OldValue:Pointer):BOOL; stdcall;
function Wow64RevertWow64FsRedirection(OldValue:Pointer):BOOL; stdcall;
{$ENDIF}

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

{$IFDEF WINDOWS}
function StartProgramEx(const ACommand,ADirectory:String;AWait,ANoShow:Boolean):Boolean;
{$ENDIF}
{$IFDEF LINUX}
function StartProgramEx(const AExecutable:String;AParams:TStrings;AWait,ATerminal:Boolean):Boolean;
function StartQEMUSystem(const ACommand,APidFile:String;AWait:Boolean):Boolean;
{$ENDIF}

implementation

{$R *.lfm}

{==============================================================================}
{==============================================================================}
{$IFDEF WINDOWS}
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
{$ENDIF}
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
 SlashChar:Char;
 WorkBuffer:String;
begin
 {}
 {$IFDEF WINDOWS}
 SlashChar:='\';
 {$ENDIF}
 {$IFDEF LINUX}
 SlashChar:='/';
 {$ENDIF}
 WorkBuffer:=Trim(FilePath);
 Result:=WorkBuffer;
 if Length(WorkBuffer) > 0 then
  begin
   if WorkBuffer[Length(WorkBuffer)] <> SlashChar then
    begin
     Result:=WorkBuffer + SlashChar;
    end;
  end;
end;

{==============================================================================}

function StripTrailingSlash(const FilePath:String):String;
var
 SlashChar:Char;
 WorkBuffer:String;
begin
 {}
 {$IFDEF WINDOWS}
 SlashChar:='\';
 {$ENDIF}
 {$IFDEF LINUX}
 SlashChar:='/';
 {$ENDIF}
 WorkBuffer:=Trim(FilePath);
 Result:=WorkBuffer;
 if Length(WorkBuffer) > 0 then
  begin
   if WorkBuffer[Length(WorkBuffer)] = SlashChar then
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
{$IFDEF WINDOWS}
function StartProgramEx(const ACommand,ADirectory:String;AWait,ANoShow:Boolean):Boolean;
var
 Command:String;
 Directory:String;
 OldValue:Pointer;
 ExitCode:LongWord;
 StartupInfo:TStartupInfo;
 ProcessInformation:TProcessInformation;
begin
 {}
 Result:=False;
 try
  Wow64DisableWow64FsRedirection(OldValue);
  try
   Command:=ACommand;
   Directory:=ADirectory;

   FillChar(StartupInfo,SizeOf(StartupInfo),#0);
   StartupInfo.cb:=SizeOf(TStartupInfo);
   if ANoShow then
    begin
     StartupInfo.dwFlags:=STARTF_USESHOWWINDOW;
     StartupInfo.wShowWindow:=SW_HIDE;
    end;

   FillChar(ProcessInformation,SizeOf(ProcessInformation),#0);
   if CreateProcess(nil,PChar(Command),nil,nil,False,0,nil,PChar(Directory),StartupInfo,ProcessInformation) then
    begin
     CloseHandle(ProcessInformation.hThread);

     if AWait then
      begin
       ExitCode:=STILL_ACTIVE;
       while GetExitCodeProcess(ProcessInformation.hProcess,ExitCode) do
        begin
         Sleep(1);
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
{$ENDIF}
{==============================================================================}
{$IFDEF LINUX}
function StartProgramEx(const AExecutable:String;AParams:TStrings;AWait,ATerminal:Boolean):Boolean;
{Launch an external program with specified parameters and options}
var
 Count:Integer;
 ProcessInfo:TProcess;
begin
 Result:=False;
 try
  if Length(AExecutable) = 0 then Exit;

  ProcessInfo:=TProcess.Create(nil);
  try
   {Create a detached process}
   ProcessInfo.InheritHandles:=False;
   ProcessInfo.Options:=[];
   ProcessInfo.ShowWindow:=swoShow;

   {Copy default environment variables including DISPLAY variable for GUI application to work}
   for Count:=1 to GetEnvironmentVariableCount do
    begin
     ProcessInfo.Environment.Add(GetEnvironmentString(Count));
    end;

   {Check Terminal}
   if ATerminal then
    begin
     ProcessInfo.Options:=[poNewConsole];
    end;

   {Add Executable}
   ProcessInfo.Executable:=AExecutable;

   {Add Parameters}
   if AParams <> nil then
    begin
     for Count:=0 to AParams.Count - 1 do
      begin
       ProcessInfo.Parameters.Add(AParams.Strings[Count]);
      end;
    end;

   {Execute Process}
   ProcessInfo.Execute;

   {Check Wait}
   if AWait then
    begin
     {Wait while Running}
     while ProcessInfo.Running do
      begin
       Sleep(1);
       Application.ProcessMessages;
      end;
    end;

   Result:=True;
  finally
   ProcessInfo.Free;
  end;
 except
  {EProcess exception raised on error}
 end;
end;

{==============================================================================}

function StartQEMUSystem(const ACommand,APidFile:String;AWait:Boolean):Boolean;
{Launch a QEMU session}
{The qeum-system-??? executables require a new terminal session to be created which
 then launches them via a bash session. This means that the process id recorded is
 the id of the terminal and not the qeum-system-??? executable so waiting does not
 work.

 To resolve this a pid file name is passed to the qeum-system-??? executable and the
 id recorded in that file is used to determine when the session has ended instead}

 function GetProcessIdFromFile(const AFilename:String):THandle;
 var
  Lines:TStringList;
 begin
  Result:=THandle(-1);

  if Length(AFilename) = 0 then Exit;

  if not FileExists(AFilename) then Exit;

  Lines:=TStringList.Create;
  try
   Lines.LoadFromFile(AFilename);
   if Lines.Count = 1 then
    begin
     Result:=StrToIntDef(Lines.Strings[0],0);
     if Result = 0 then Result:=THandle(-1);
    end;
  finally
   Lines.Free;
  end;
 end;

 function CheckProcessIdRunning(AHandle:THandle):Boolean;
 var
  Res:cint;
 begin
  Result:=False;

  Res:=FpKill(AHandle, 0);
  if Res = 0 then Result:=True;
 end;

var
 Count:Integer;
 ProcessInfo:TProcess;
 ProcessHandle:THandle;
begin
 Result:=False;
 try
  if Length(ACommand) = 0 then Exit;
  if AWait and (Length(APidFile) = 0) then Exit;

  ProcessInfo:=TProcess.Create(nil);
  try
   {Create a detached process in a new terminal}
   ProcessInfo.InheritHandles:=False;
   ProcessInfo.Options:=[poNewConsole];
   ProcessInfo.ShowWindow:=swoShow;

   {Copy default environment variables including DISPLAY variable for GUI application to work}
   for Count:=1 to GetEnvironmentVariableCount do
    begin
     ProcessInfo.Environment.Add(GetEnvironmentString(Count));
    end;

   {Create a bash session}
   ProcessInfo.Executable:='/bin/bash';
   ProcessInfo.Parameters.Add('-c');
   ProcessInfo.Parameters.Add(ACommand + ' -pidfile "' + APidFile + '"');

   {Execute Process}
   ProcessInfo.Execute;

   {Check Wait}
   if AWait then
    begin
     Sleep(2000);

     ProcessHandle:=GetProcessIdFromFile(APidFile);
     if ProcessHandle <> THandle(-1) then
      begin
       while CheckProcessIdRunning(ProcessHandle) do
        begin
         Sleep(100);
         Application.ProcessMessages;
        end;
      end;
    end;

   {Delete the PidFile}
   if FileExists(APidFile) then
    begin
     DeleteFile(APidFile);
    end;

   Result:=True;
  finally
   ProcessInfo.Free;
  end;
 except
  {EProcess exception raised on error}
 end;
end;
{$ENDIF}
{==============================================================================}
{==============================================================================}

constructor TQEMULaunch.Create;
{$IFDEF WINDOWS}
var
 InstallPath:String;
{$ENDIF}
begin
 {}
 inherited Create;

 {$IFDEF WINDOWS}
 {Assume that this is running from <InstallPath>\tools and that the InstallPath will be the folder above}
 InstallPath:=ExtractFileDir(ExtractFileDir(Application.ExeName));
 {$ENDIF}

 {QEMU Variables}
 {$IFDEF WINDOWS}
 Path:=AddTrailingSlash(InstallPath) + 'qemu';
 SystemArm:='qemu-system-arm.exe';
 SystemAarch64:='qemu-system-aarch64.exe';
 {$ENDIF}
 {$IFDEF LINUX}
 Path:='/usr/bin';
 SystemArm:='qemu-system-arm';
 SystemAarch64:='qemu-system-aarch64';
 {$ENDIF}

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
     if Uppercase(Controller) = 'RPIA' then
      begin
       //Result:='arm1176'; //Not yet supported
      end
     else if Uppercase(Controller) = 'RPIB' then
      begin
       //Result:='arm1176'; //Not yet supported
      end
     else if Uppercase(Controller) = 'RPIZERO' then
      begin
       //Result:='arm1176'; //Not yet supported
      end
     else if Uppercase(Controller) = 'QEMURPIA' then
      begin
       Result:='arm1176';
      end
     else if Uppercase(Controller) = 'QEMURPIZERO' then
      begin
       Result:='arm1176';
      end;
    end
   else if Uppercase(Processor) = 'ARMV7A' then
    begin
     if Uppercase(Controller) = 'RPI2B' then
      begin
       //Result:='cortex-a7'; //Not yet supported
      end
     else if Uppercase(Controller) = 'RPI3A' then
      begin
       //Result:='cortex-a7'; //Not yet supported
      end
     else if Uppercase(Controller) = 'RPI3B' then
      begin
       //Result:='cortex-a7'; //Not yet supported
      end
     else if Uppercase(Controller) = 'QEMUVPB' then
      begin
       Result:='cortex-a8';
      end
     else if Uppercase(Controller) = 'QEMURPI2B' then
      begin
       Result:='cortex-a7';
      end
     else if Uppercase(Controller) = 'QEMURPI3A' then
      begin
       Result:='cortex-a7';
      end
     else if Uppercase(Controller) = 'QEMURPI3B' then
      begin
       Result:='cortex-a7';
      end;
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
   else if Uppercase(Controller) = 'RPI3A' then
    begin
     //Result:='kernel7.img'; //Not yet supported
    end
   else if Uppercase(Controller) = 'RPI3B' then
    begin
     //Result:='kernel7.img'; //Not yet supported
    end
   else if Uppercase(Controller) = 'RPI4B' then
    begin
     //Result:='kernel7l.img'; //Not yet supported
    end
   else if Uppercase(Controller) = 'RPI400' then
    begin
     //Result:='kernel7l.img'; //Not yet supported
    end
   else if Uppercase(Controller) = 'QEMUVPB' then
    begin
     Result:='kernel.bin';
    end
   else if Uppercase(Controller) = 'QEMURPIA' then
    begin
     Result:='kernel.qimg';
    end
   else if Uppercase(Controller) = 'QEMURPIZERO' then
    begin
     Result:='kernel.qimg';
    end
   else if Uppercase(Controller) = 'QEMURPI2B' then
    begin
     Result:='kernel7.qimg';
    end
   else if Uppercase(Controller) = 'QEMURPI3A' then
    begin
     Result:='kernel7.qimg';
    end
   else if Uppercase(Controller) = 'QEMURPI3B' then
    begin
     Result:='kernel7.qimg';
    end;
  end
 else if Uppercase(CPU) = 'AARCH64' then
  begin
   if Uppercase(Controller) = 'RPI3A' then
    begin
     Result:='kernel8.img';
    end
   else if Uppercase(Controller) = 'RPI3B' then
    begin
     Result:='kernel8.img';
    end
   else if Uppercase(Controller) = 'RPI4B' then
    begin
     //Result:='kernel8.img'; //Not yet supported
    end
   else if Uppercase(Controller) = 'RPI400' then
    begin
     //Result:='kernel8.img'; //Not yet supported
    end
   else if Uppercase(Controller) = 'QEMUVPB' then
    begin
     Result:='kernel64.bin';
    end
   else if Uppercase(Controller) = 'QEMURPI3A' then
    begin
     Result:='kernel8.img';
    end
   else if Uppercase(Controller) = 'QEMURPI3B' then
    begin
     Result:='kernel8.img';
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
     //Result:='512M'; //Not yet supported
    end
   else if Uppercase(Controller) = 'RPI2B' then
    begin
     //Result:='1G'; //Not yet supported
    end
   else if Uppercase(Controller) = 'RPI3A' then
    begin
     //Result:='512M'; //Not yet supported
    end
   else if Uppercase(Controller) = 'RPI3B' then
    begin
     //Result:='1G'; //Not yet supported
    end
   else if Uppercase(Controller) = 'RPI4B' then
    begin
     //Result:='4G'; //Not yet supported
    end
   else if Uppercase(Controller) = 'RPI400' then
    begin
     //Result:='4G'; //Not yet supported
    end
   else if Uppercase(Controller) = 'QEMUVPB' then
    begin
     Result:='256M';
    end
   else if Uppercase(Controller) = 'QEMURPIA' then
    begin
     Result:='512M';
    end
   else if Uppercase(Controller) = 'QEMURPIZERO' then
    begin
     Result:='512M';
    end
   else if Uppercase(Controller) = 'QEMURPI2B' then
    begin
     Result:='1G';
    end
   else if Uppercase(Controller) = 'QEMURPI3A' then
    begin
     Result:='1G'; //'512M'; //Using raspi2b in 32-bit mode
    end
   else if Uppercase(Controller) = 'QEMURPI3B' then
    begin
     Result:='1G';
    end;
  end
 else if Uppercase(CPU) = 'AARCH64' then
  begin
   if Uppercase(Controller) = 'RPI3A' then
    begin
     Result:='512M';
    end
   else if Uppercase(Controller) = 'RPI3B' then
    begin
     Result:='1G';
    end
   else if Uppercase(Controller) = 'RPI4B' then
    begin
     //Result:='4G'; //Not yet supported
    end
   else if Uppercase(Controller) = 'RPI400' then
    begin
     //Result:='4G'; //Not yet supported
    end
   else if Uppercase(Controller) = 'QEMUVPB' then
    begin
     Result:='256M';
    end
   else if Uppercase(Controller) = 'QEMURPI3A' then
    begin
     Result:='512M';
    end
   else if Uppercase(Controller) = 'QEMURPI3B' then
    begin
     Result:='1G';
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
   else if Uppercase(Controller) = 'RPI3A' then
    begin
     //Result:='raspi2'; //Not yet supported
    end
   else if Uppercase(Controller) = 'RPI3B' then
    begin
     //Result:='raspi2'; //Not yet supported
    end
   else if Uppercase(Controller) = 'RPI4B' then
    begin
     //Result:='raspi2'; //Not yet supported
    end
   else if Uppercase(Controller) = 'RPI400' then
    begin
     //Result:='raspi2'; //Not yet supported
    end
   else if Uppercase(Controller) = 'QEMUVPB' then
    begin
     Result:='versatilepb';
    end
   else if Uppercase(Controller) = 'QEMURPIA' then
    begin
     Result:='raspi1ap';
    end
   else if Uppercase(Controller) = 'QEMURPIZERO' then
    begin
     Result:='raspi0';
    end
   else if Uppercase(Controller) = 'QEMURPI2B' then
    begin
     Result:='raspi2b';
    end
   else if Uppercase(Controller) = 'QEMURPI3A' then
    begin
     Result:='raspi2b'; //raspi3ap only supported by QEMU Aarch64
    end
   else if Uppercase(Controller) = 'QEMURPI3B' then
    begin
     Result:='raspi2b'; //raspi3b only supported by QEMU Aarch64
    end;
  end
 else if Uppercase(CPU) = 'AARCH64' then
  begin
   if Uppercase(Controller) = 'RPI3A' then
    begin
     Result:='raspi3ap';
    end
   else if Uppercase(Controller) = 'RPI3B' then
    begin
     Result:='raspi3b';
    end
   else if Uppercase(Controller) = 'RPI4B' then
    begin
     //Result:='raspi4'; //Not yet supported
    end
   else if Uppercase(Controller) = 'RPI400' then
    begin
     //Result:='raspi4'; //Not yet supported
    end
   else if Uppercase(Controller) = 'QEMUVPB' then
    begin
     Result:='versatilepb';
    end
   else if Uppercase(Controller) = 'QEMURPI3A' then
    begin
     Result:='raspi3ap';
    end
   else if Uppercase(Controller) = 'QEMURPI3B' then
    begin
     Result:='raspi3b';
    end;
  end;
end;

{==============================================================================}

function TQEMULaunch.GetDevices:String;
begin
 {}
 Result:='';

 if Uppercase(CPU) = 'ARM' then
  begin
   if Uppercase(Controller) = 'RPIA' then
    begin
     //Result:='-device usb-kbd -device usb-mouse'; //Not yet supported
    end
   else if Uppercase(Controller) = 'RPIB' then
    begin
     //Result:='-device usb-kbd -device usb-mouse'; //Not yet supported
    end
   else if Uppercase(Controller) = 'RPIZERO' then
    begin
     //Result:='-device usb-kbd -device usb-mouse'; //Not yet supported
    end
   else if Uppercase(Controller) = 'RPI2B' then
    begin
     //Result:='-device usb-kbd -device usb-mouse'; //Not yet supported
    end
   else if Uppercase(Controller) = 'RPI3A' then
    begin
     //Result:='-device usb-kbd -device usb-mouse'; //Not yet supported
    end
   else if Uppercase(Controller) = 'RPI3B' then
    begin
     //Result:='-device usb-kbd -device usb-mouse'; //Not yet supported
    end
   else if Uppercase(Controller) = 'RPI4B' then
    begin
     //Result:='-device usb-kbd -device usb-mouse'; //Not yet supported
    end
   else if Uppercase(Controller) = 'RPI400' then
    begin
     //Result:='-device usb-kbd -device usb-mouse'; //Not yet supported
    end
   else if Uppercase(Controller) = 'QEMUVPB' then
    begin
     Result:=''; //Nothing
    end
   else if Uppercase(Controller) = 'QEMURPIA' then
    begin
     Result:='-device usb-kbd -device usb-mouse';
    end
   else if Uppercase(Controller) = 'QEMURPIZERO' then
    begin
     Result:='-device usb-kbd -device usb-mouse';
    end
   else if Uppercase(Controller) = 'QEMURPI2B' then
    begin
     Result:='-device usb-kbd -device usb-mouse';
    end
   else if Uppercase(Controller) = 'QEMURPI3A' then
    begin
     Result:='-device usb-kbd -device usb-mouse';
    end
   else if Uppercase(Controller) = 'QEMURPI3B' then
    begin
     Result:='-device usb-kbd -device usb-mouse';
    end;
  end
 else if Uppercase(CPU) = 'AARCH64' then
  begin
   if Uppercase(Controller) = 'RPI3A' then
    begin
     Result:='-device usb-kbd -device usb-mouse';
    end
   else if Uppercase(Controller) = 'RPI3B' then
    begin
     Result:='-device usb-kbd -device usb-mouse';
    end
   else if Uppercase(Controller) = 'RPI4B' then
    begin
     //Result:='-device usb-kbd -device usb-mouse'; //Not yet supported
    end
   else if Uppercase(Controller) = 'RPI400' then
    begin
     //Result:='-device usb-kbd -device usb-mouse'; //Not yet supported
    end
   else if Uppercase(Controller) = 'QEMUVPB' then
    begin
     Result:=''; //Nothing
    end
   else if Uppercase(Controller) = 'QEMURPI3A' then
    begin
     Result:='-device usb-kbd -device usb-mouse';
    end
   else if Uppercase(Controller) = 'QEMURPI3B' then
    begin
     Result:='-device usb-kbd -device usb-mouse';
    end;
  end;
end;

{==============================================================================}

function TQEMULaunch.Launch:Boolean;
var
 Param:String;
 Command:String;
 {$IFDEF LINUX}
 PidFile:String;
 PidCount:Integer;
 {$ENDIF}
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

      {$IFDEF LINUX}
      {Get PidFile}
      PidFile:=AddTrailingSlash(ProjectPath) + Param;
      PidCount:=1;
      while FileExists(PidFile + '.pid' + IntToStr(PidCount)) do
       begin
        Inc(PidCount);
       end;
      PidFile:=PidFile + '.pid' + IntToStr(PidCount);
      {$ENDIF}

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

      {Get Devices}
      Param:=GetDevices;
      if Length(Param) > 0 then
       begin
        {Add Devices}
        Command:=Command + ' ' + Param;
       end;

      if Length(CommandLine) > 0 then
       begin
        {Add Append}
        Command:=Command + ' -append ' + AddQuotes(CommandLine);
       end;

      if Length(ExtraParams) > 0 then
       begin
        {Add Extras}
        Command:=Command + ' ' + ExtraParams;
       end;

      {Start Program}
      {$IFDEF WINDOWS}
      Result:=StartProgramEx(Command,StripTrailingSlash(ProjectPath),True,False);
      {$ENDIF}
      {$IFDEF LINUX}
      Result:=StartQEMUSystem(Command,PidFile,True);
      {$ENDIF}
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

function TQEMULaunch.LoadProjectConfig:Boolean;
var
 Section:String;
 Filename:String;
 IniFile:TIniFile;
 WorkBuffer:String;
 ProjectPath:String;
begin
 {}
 Result:=False;
 try
  {Get Path}
  ProjectPath:=ExtractFileDir(Project);

  {Get Filename}
  Filename:=AddTrailingSlash(ProjectPath) + ExtractFileName(ChangeFileExt(Application.ExeName,'.ini'));

  {Check File}
  if FileExists(Filename) then
   begin
    IniFile:=TIniFile.Create(Filename);
    try
     Section:='QEMULauncher';

     {Note: Any parameters from QEMULauncher.ini in the project directory override those in the default QEMULauncher.ini}
     {Get Path}
     WorkBuffer:=IniFile.ReadString(Section,'Path','');
     if Length(WorkBuffer) <> 0 then Path:=WorkBuffer;

     {Get SystemArm}
     WorkBuffer:=IniFile.ReadString(Section,'SystemArm','');
     if Length(WorkBuffer) <> 0 then SystemArm:=WorkBuffer;

     {Get SystemAarch64}
     WorkBuffer:=IniFile.ReadString(Section,'SystemAarch64','');
     if Length(WorkBuffer) <> 0 then SystemAarch64:=WorkBuffer;

     {Get ExtraParams}
     WorkBuffer:=IniFile.ReadString(Section,'ExtraParams','');
     if Length(WorkBuffer) <> 0 then ExtraParams:=WorkBuffer;

     {Get CommandLine}
     WorkBuffer:=IniFile.ReadString(Section,'CommandLine','');
     if Length(WorkBuffer) <> 0 then CommandLine:=WorkBuffer;
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
{==============================================================================}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
 {}
 FLaunch:=TQEMULaunch.Create;
 FLaunch.LoadConfig;
 FLaunch.LoadParams;
 FLaunch.LoadProjectConfig;
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
 for Count:=Low(CPUNames) to High(CPUNames) do
  begin
   cmbCPU.Items.Add(CPUNames[Count]);
  end;

 cmbProcessor.Clear;
 for Count:=Low(ProcessorNames) to High(ProcessorNames) do
  begin
   cmbProcessor.Items.Add(ProcessorNames[Count]);
  end;

 cmbController.Clear;
 for Count:=Low(ControllerNames) to High(ControllerNames) do
  begin
   cmbController.Items.Add(ControllerNames[Count]);
  end;

 txtProject.Text:=FLaunch.Project;
 cmbCPU.ItemIndex:=cmbCPU.Items.IndexOf(FLaunch.CPU);
 cmbProcessor.ItemIndex:=cmbProcessor.Items.IndexOf(FLaunch.Processor);
 cmbController.ItemIndex:=cmbController.Items.IndexOf(FLaunch.Controller);

 {Adjust Labels}
 lblProject.Top:=txtProject.Top + ((txtProject.Height - lblProject.Height) div 2);
 lblCPU.Top:=cmbCPU.Top + ((cmbCPU.Height - lblCPU.Height) div 2);
 lblProcessor.Top:=cmbProcessor.Top + ((cmbProcessor.Height - lblProcessor.Height) div 2);
 lblController.Top:=cmbController.Top + ((cmbController.Height - lblController.Height) div 2);

 {Adjust Buttons}
 if txtProject.Height > cmdProject.Height then
 begin
  cmdProject.Height:=txtProject.Height;
  cmdProject.Width:=txtProject.Height;
  cmdProject.Top:=txtProject.Top + ((txtProject.Height - cmdProject.Height) div 2);
 end
 else
 begin
  cmdProject.Height:=txtProject.Height + 2;
  cmdProject.Width:=txtProject.Height + 2;
  cmdProject.Top:=txtProject.Top - 1;
 end;

 if cmdProject.Height > cmdLaunch.Height then
 begin
  cmdLaunch.Height:=cmdProject.Height;
  cmdClose.Height:=cmdProject.Height;
 end;

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
 {$IFDEF WINDOWS}
 openMain.Filter:='Lararus projects|*.lpr|All files|*.*';
 {$ENDIF}
 {$IFDEF LINUX}
 openMain.Filter:='Lararus projects|*.lpr|All files|*';
 {$ENDIF}
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
{$IFDEF WINDOWS}
initialization
 Wow64FsRedirectionHandle:=LoadLibrary(PChar(kernel32));
 if Wow64FsRedirectionHandle > HINSTANCE_ERROR then
  begin
   _Wow64DisableWow64FsRedirection:=GetProcAddress(Wow64FsRedirectionHandle,'Wow64DisableWow64FsRedirection');
   _Wow64RevertWow64FsRedirection:=GetProcAddress(Wow64FsRedirectionHandle,'Wow64RevertWow64FsRedirection');
  end;
{$ENDIF}
{==============================================================================}
{$IFDEF WINDOWS}
finalization
 if Wow64FsRedirectionHandle > HINSTANCE_ERROR then FreeLibrary(Wow64FsRedirectionHandle);
{$ENDIF}
{==============================================================================}
{==============================================================================}

end.
