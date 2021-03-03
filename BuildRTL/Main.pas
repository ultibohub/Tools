{
Ultibo RTL Builder Tool.

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


RTL Builder
===========

 The RTL builder creates a batch file or shell script which compiles the RTL and
 Packages for any of the supported architectures and displays the output in a
 window during the compile.

 While the tool is designed to determine the correct paths and locations without
 configuration it does support creating a BuildRTL.ini file in the same directory
 and setting a number of parameters to adjust the behavior.

 The format of the INI file is:

 [BuildRTL]
 PathPrefix=
 InstallPath=
 CompilerName=
 CompilerPath=
 CompilerVersion=
 SourcePath=

 ARMCompiler=
 AARCH64Compiler=

 BuildRTL=
 BuildPackages=

 PlatformARMv6=
 PlatformARMv7=
 PlatformARMv8=

 A brief explanation of each parameter along with the standard default value:

 PathPrefix - A text value to prepend to the path variable in the batch file (Default: <Blank>)


 InstallPath - The path where Ultibo core is installed (Default Windows: C:\Ultibo\Core) (Detected from the application path)
                                                       (        Linux: $HOME/ultibo/core)

 CompilerName - The name of the Free Pascal compiler (Default Windows: fpc.exe)
                                                     (        Linux: fpc)

 CompilerPath - The path where the Ultibo version of FPC is installed (Default Windows: <InstallPath>\fpc\<CompilerVersion>)
                                                                      (        Linux: <InstallPath>/fpc)

 CompilerVersion - The version of the FPC compiler (Default: 3.1.1)


 SourcePath - The path to RTL and Packages source code (Default Windows: <CompilerPath>\source)
                                                       (        Linux: <CompilerPath>/source)

 ARMCompiler - The name of the Free Pascal ARM Compiler or Cross Compiler (Default: <Blank>)


 AARCH64Compiler - The name of the Free Pascal AARCH64 Compiler or Cross Compiler (Default: <Blank>)


 BuildRTL - Enable or disable building the RTL (0=Disable / 1=Enable) (Default: 1)


 BuildPackages - Enable or disable building the Packages (0=Disable / 1=Enable) (Default: 1)


 PlatformARMv6 - Build the RTL and Packages for ARMv6 architecture (0=Disable / 1=Enable) (Default: 1)


 PlatformARMv7 - Build the RTL and Packages for ARMv7 architecture (0=Disable / 1=Enable) (Default: 1)


 PlatformARMv8 - Build the RTL and Packages for ARMv8 architecture (0=Disable / 1=Enable) (Default: 1)

}

unit Main;

{$MODE Delphi}

interface

uses
  LCLIntf,
  LCLType,
  LMessages,
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF LINUX}
  BaseUnix,
  opensslsockets,
  {$ENDIF}
  Messages,
  SysUtils,
  Classes,
  {$IFDEF FPC}
  Process,
  fphttpclient,
  Zipper,
  FileUtil,
  {$ENDIF}
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  ComCtrls,
  IniFiles;

const
 {$IFDEF WINDOWS}
 LineEnd = Chr(13) + Chr(10); {CR LF}
 {$ENDIF}
 {$IFDEF LINUX}
 LineEnd = Chr(10); {LF}
 {$ENDIF}
 MarkerEnd = '!!!EndOfBuild!!!';
 MarkerProgress = '!!!Progress'; {Suffix will be progress value as a percentage}
 {$IFDEF WINDOWS}
 SlashChar = '\';
 BuildScript = '__buildrtl.bat';
 {$ENDIF}
 {$IFDEF LINUX}
 SlashChar = '/';
 BuildScript = '__buildrtl.sh';
 {$ENDIF}
 VersionId = '__version.id';
 VersionLast = '__version.last';
 DownloadZip = 'master.zip';
 DefaultVersionURL = 'https://raw.githubusercontent.com/ultibohub/Core/master/source/';
 DefaultDownloadURL = 'https://github.com/ultibohub/Core/archive/';


type

  { TfrmMain }

  TfrmMain = class(TForm)
    cmdDownload: TButton;
    cmdOffline: TButton;
    cmdCheck: TButton;
    mmoMain: TMemo;
    openMain: TOpenDialog;
    progressMain: TProgressBar;
    sbMain: TStatusBar;
    pnlMain: TPanel;
    cmdExit: TButton;
    lblCompiler: TLabel;
    lblPlatforms: TLabel;
    cmdBuild: TButton;
    chkARMv6: TCheckBox;
    chkARMv7: TCheckBox;
    chkARMv8: TCheckBox;
    lblMain: TLabel;
    procedure cmdCheckClick(Sender: TObject);
    procedure cmdDownloadClick(Sender: TObject);
    procedure cmdOfflineClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure cmdExitClick(Sender: TObject);
    procedure cmdBuildClick(Sender: TObject);
    procedure chkARMv6Click(Sender: TObject);
    procedure chkARMv7Click(Sender: TObject);
    procedure chkARMv8Click(Sender: TObject);
  private
    { Private declarations }
    PathPrefix:String;
    InstallPath:String;
    SourcePath:String;
    CompilerName:String;
    CompilerPath:String;
    CompilerVersion:String;

    ARMCompiler:String;
    AARCH64Compiler:String;

    BuildRTL:Boolean;
    BuildPackages:Boolean;

    PlatformARMv6:Boolean;
    PlatformARMv7:Boolean;
    PlatformARMv8:Boolean;

    VersionURL:String;
    DownloadURL:String;

    procedure DoProgress(Sender:TObject;const Percent:Double);
    procedure DoDataReceived(Sender:TObject;const ContentLength,CurrentPos:Int64);
  public
    { Public declarations }
    function LoadConfig:Boolean;

    function CheckForUpdates:Boolean;
    function DownloadLatest:Boolean;
    function ExtractFile(const AFilename:String):Boolean;

    function CreateBuildFile:Boolean;
    function ExecuteBuildFile:Boolean;
  end;

var
  frmMain: TfrmMain;

{$IFDEF WINDOWS}
function GetDosOutput(CommandLine: string; Work: string = 'C:\'): string;
procedure CaptureConsoleOutput(const ACommand, AParameters: String; AMemo: TMemo);

function ExecuteConsoleProcess(const ACommand,AParameters,AWorking:String;AMemo:TMemo):Boolean;
function ExecuteConsoleProcessEx(const ACommand,AParameters,AWorking,AEndMark,AProgressMark:String;AMemo:TMemo;AProgress:TProgressBar):Boolean;
{$ENDIF}
{$IFDEF LINUX}
function ExecuteShellProcess(const ACommand:String;AParameters:TStrings;const AWorking,AEndMark,AProgressMark:String;AMemo:TMemo;AProgress:TProgressBar):Boolean;
{$ENDIF}

{For details on how to capture command prompt output see:

 http://www.delphidabbler.com/tips/61

 http://stackoverflow.com/questions/9119999/getting-output-from-a-shell-dos-app-into-a-delphi-app

 https://wiki.lazarus.freepascal.org/Executing_External_Programs#Reading_large_output

 See also JclSysUtils unit in the Jedi JCL

}

function AddTrailingSlash(const FilePath:String):String;
function StripTrailingSlash(const FilePath:String):String;

{$IFDEF WINDOWS}
function IsWinNT6orAbove:Boolean;
{$ENDIF}

implementation

{$R *.lfm}

{==============================================================================}
{$IFDEF WINDOWS}
function GetDosOutput(CommandLine: string; Work: string = 'C:\'): string;
{From: http://www.delphidabbler.com/tips/61}
var
  SA: TSecurityAttributes;
  SI: TStartupInfo;
  PI: TProcessInformation;
  StdOutPipeRead, StdOutPipeWrite: THandle;
  WasOK: Boolean;
  Buffer: array[0..255] of AnsiChar;
  BytesRead: Cardinal;
  WorkDir: string;
  Handle: Boolean;
begin
  Result := '';
  with SA do begin
    nLength := SizeOf(SA);
    bInheritHandle := True;
    lpSecurityDescriptor := nil;
  end;
  CreatePipe(StdOutPipeRead, StdOutPipeWrite, @SA, 0);
  try
    with SI do
    begin
      FillChar(SI, SizeOf(SI), 0);
      cb := SizeOf(SI);
      dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
      wShowWindow := SW_HIDE;
      hStdInput := GetStdHandle(STD_INPUT_HANDLE); // don't redirect stdin
      hStdOutput := StdOutPipeWrite;
      hStdError := StdOutPipeWrite;
    end;
    WorkDir := Work;
    Handle := CreateProcess(nil, PChar('cmd.exe /C ' + CommandLine),
                            nil, nil, True, 0, nil,
                            PChar(WorkDir), SI, PI);
    CloseHandle(StdOutPipeWrite);
    if Handle then
      try
        repeat
          WasOK := ReadFile(StdOutPipeRead, Buffer, 255, BytesRead, nil);
          if BytesRead > 0 then
          begin
            Buffer[BytesRead] := #0;
            Result := Result + Buffer;
          end;
        until not WasOK or (BytesRead = 0);
        WaitForSingleObject(PI.hProcess, INFINITE);
      finally
        CloseHandle(PI.hThread);
        CloseHandle(PI.hProcess);
      end;
  finally
    CloseHandle(StdOutPipeRead);
  end;
end;

{==============================================================================}

procedure CaptureConsoleOutput(const ACommand, AParameters: String; AMemo: TMemo);
{From: http://stackoverflow.com/questions/9119999/getting-output-from-a-shell-dos-app-into-a-delphi-app}
 const
   CReadBuffer = 2400;
 var
   saSecurity: TSecurityAttributes;
   hRead: THandle;
   hWrite: THandle;
   suiStartup: TStartupInfo;
   piProcess: TProcessInformation;
   pBuffer: array[0..CReadBuffer] of AnsiChar;      //<----- update
   dRead: DWord;
   dRunning: DWord;
 begin
   saSecurity.nLength := SizeOf(TSecurityAttributes);
   saSecurity.bInheritHandle := True;  
   saSecurity.lpSecurityDescriptor := nil;

   if CreatePipe(hRead, hWrite, @saSecurity, 0) then
   begin
     FillChar(suiStartup, SizeOf(TStartupInfo), #0);
     suiStartup.cb := SizeOf(TStartupInfo);
     suiStartup.hStdInput := hRead;
     suiStartup.hStdOutput := hWrite;
     suiStartup.hStdError := hWrite;
     suiStartup.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;    
     suiStartup.wShowWindow := SW_HIDE; 

     if CreateProcess(nil, PChar(ACommand + ' ' + AParameters), @saSecurity,
       @saSecurity, True, NORMAL_PRIORITY_CLASS, nil, nil, suiStartup, piProcess)
       then
     begin
       repeat
         dRunning  := WaitForSingleObject(piProcess.hProcess, 100);
         Application.ProcessMessages();
         repeat
           dRead := 0;
           ReadFile(hRead, pBuffer[0], CReadBuffer, dRead, nil);
           pBuffer[dRead] := #0; 

           OemToAnsi(pBuffer, pBuffer);
           AMemo.Lines.Add(String(pBuffer));
         until (dRead < CReadBuffer);      
       until (dRunning <> WAIT_TIMEOUT);
       CloseHandle(piProcess.hProcess);
       CloseHandle(piProcess.hThread);
     end;

     CloseHandle(hRead);
     CloseHandle(hWrite);
   end;
end;

{==============================================================================}

function ExecuteConsoleProcess(const ACommand,AParameters,AWorking:String;AMemo:TMemo):Boolean;
{Modified from: CaptureConsoleOutput}
const
 READ_BUFFER_SIZE = 2400;
var
 Status:DWORD;
 WorkDir:PChar;
 Command:String;
 BytesRead:DWORD;
 ReadData:String;
 ReadRemain:String;
 ReadResult:Boolean;
 ReadHandle:THandle;
 WriteHandle:THandle;
 StartupInfo:TStartupInfo;
 ProcessInformation:TProcessInformation;
 SecurityAttributes:TSecurityAttributes;
 ReadBuffer:array[0..READ_BUFFER_SIZE] of AnsiChar;
begin
 {}
 Result:=False;

 {Check Parameters}
 if (Length(ACommand) = 0) and (Length(AParameters) = 0) then Exit;
 if AMemo = nil then Exit;

 {Setup Security Attributes}
 FillChar(SecurityAttributes,SizeOf(TSecurityAttributes),0);
 SecurityAttributes.nLength:=SizeOf(TSecurityAttributes);
 SecurityAttributes.bInheritHandle:=True;
 SecurityAttributes.lpSecurityDescriptor:=nil;

 {Create Pipe}
 if CreatePipe(ReadHandle,WriteHandle,@SecurityAttributes,0) then
  begin
   try
    {Setup Startup Info}
    FillChar(StartupInfo,SizeOf(TStartupInfo),0);
    StartupInfo.cb:=SizeOf(TStartupInfo);
    StartupInfo.hStdInput:=ReadHandle;
    StartupInfo.hStdOutput:=WriteHandle;
    StartupInfo.hStdError:=WriteHandle;
    StartupInfo.dwFlags:=STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
    StartupInfo.wShowWindow:=SW_HIDE;

    {Setup Process Information}
    FillChar(ProcessInformation,SizeOf(TProcessInformation),0);

    {Create Command}
    Command:=Trim(ACommand + ' ' + AParameters);

    {Check Working}
    WorkDir:=nil;
    if Length(AWorking) <> 0 then
     begin
      WorkDir:=PChar(AWorking);
     end;

    {Create Process}
    if CreateProcess(nil,PChar(Command),@SecurityAttributes,@SecurityAttributes,True,NORMAL_PRIORITY_CLASS,nil,WorkDir,StartupInfo,ProcessInformation) then
     begin
      try
       {Wait for Process}
       Status:=WaitForSingleObject(ProcessInformation.hProcess,100);
       if Status <> WAIT_TIMEOUT then Exit;

       ReadData:='';
       ReadRemain:='';
       repeat
        {Read from Pipe}
        BytesRead:=0;
        ReadResult:=ReadFile(ReadHandle,ReadBuffer[0],READ_BUFFER_SIZE,BytesRead,nil);

        {Check Bytes Read}
        if BytesRead > 0 then
         begin
          {Update Buffer}
          ReadBuffer[BytesRead]:=#0;

          {Convert Output}
          OemToAnsi(ReadBuffer,ReadBuffer);

          //To Do //ReadData/ReadRemain //See HTTPBuffer and ExecuteConsoleProcessEx etc

          {Add Output}
          AMemo.Lines.Add(String(ReadBuffer));
          Application.ProcessMessages;
         end;

        {Check Bytes Read}
        if (not(ReadResult) or (BytesRead < READ_BUFFER_SIZE)) then
         begin
          {Wait for Process}
          Status:=WaitForSingleObject(ProcessInformation.hProcess,100);
         end;

       until (Status <> WAIT_TIMEOUT);

       Result:=True;
      finally
       CloseHandle(ProcessInformation.hProcess);
       CloseHandle(ProcessInformation.hThread);
      end;
     end;
   finally
    CloseHandle(ReadHandle);
    CloseHandle(WriteHandle);
   end;
  end;
end;

{==============================================================================}

function ExecuteConsoleProcessEx(const ACommand,AParameters,AWorking,AEndMark,AProgressMark:String;AMemo:TMemo;AProgress:TProgressBar):Boolean;
{Modified from: CaptureConsoleOutput}
var
 WorkDir:PChar;
 Command:String;
 BytesRead:DWORD;
 ReadData:String;
 ReadChar:AnsiChar;
 ReadResult:Boolean;
 ReadHandle:THandle;
 WriteHandle:THandle;
 StartupInfo:TStartupInfo;
 ProcessInformation:TProcessInformation;
 SecurityAttributes:TSecurityAttributes;
begin
 {}
 Result:=False;

 {Check Parameters}
 if (Length(ACommand) = 0) and (Length(AParameters) = 0) then Exit;
 if AMemo = nil then Exit;
 if AProgress = nil then Exit;

 {Setup Security Attributes}
 FillChar(SecurityAttributes,SizeOf(TSecurityAttributes),0);
 SecurityAttributes.nLength:=SizeOf(TSecurityAttributes);
 SecurityAttributes.bInheritHandle:=True;
 SecurityAttributes.lpSecurityDescriptor:=nil;

 {Create Pipe}
 if CreatePipe(ReadHandle,WriteHandle,@SecurityAttributes,0) then
  begin
   try
    {Setup Startup Info}
    FillChar(StartupInfo,SizeOf(TStartupInfo),0);
    StartupInfo.cb:=SizeOf(TStartupInfo);
    StartupInfo.hStdInput:=ReadHandle;
    if not(IsWinNT6orAbove) then StartupInfo.hStdInput:=INVAlID_HANDLE_VALUE; {Don't pass StdInput handle if less the Vista}
    StartupInfo.hStdOutput:=WriteHandle;
    StartupInfo.hStdError:=WriteHandle;
    StartupInfo.dwFlags:=STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
    StartupInfo.wShowWindow:=SW_HIDE;

    {Setup Process Information}
    FillChar(ProcessInformation,SizeOf(TProcessInformation),0);

    {Create Command}
    Command:=Trim(ACommand + ' ' + AParameters);

    {Check Working}
    WorkDir:=nil;
    if Length(AWorking) <> 0 then
     begin
      WorkDir:=PChar(AWorking);
     end;

    {Create Process}
    if CreateProcess(nil,PChar(Command),@SecurityAttributes,@SecurityAttributes,True,NORMAL_PRIORITY_CLASS,nil,WorkDir,StartupInfo,ProcessInformation) then
     begin
      try
       ReadData:='';

       repeat
        {Read from Pipe}
        BytesRead:=0;
        ReadResult:=ReadFile(ReadHandle,ReadChar,1,BytesRead,nil);

        {Check Bytes Read}
        if BytesRead > 0 then
         begin
          {Convert Output}
          OemToAnsiBuff(@ReadChar,@ReadChar,1);

          {Check for CR LF}
          if not(ReadChar in [#10,#13]) then
           begin
            ReadData:=ReadData + ReadChar;
           end
          else
           begin
            {Check for LF}
            if ReadChar = #10 then
             begin
              {Check End Mark}
              if Length(AEndMark) > 0 then
               begin
                if ReadData = AEndMark then Break;
               end;

              {Check Progress Mark}
              if (Length(AProgressMark) > 0) and (Copy(ReadData,1,Length(AProgressMark)) = AProgressMark) then
               begin
                {Update Progress}
                AProgress.Position:=StrToIntDef(Copy(ReadData,Length(AProgressMark) + 1,Length(ReadData)),AProgress.Position);
               end
              else
               begin
                {Add Output}
                AMemo.Lines.Add(ReadData);
                AMemo.SelStart:=Length(AMemo.Text);
               end;

              Application.ProcessMessages;

              {Reset Data}
              ReadData:='';
             end;
           end;
         end;

       until (not(ReadResult) or (BytesRead = 0));

       Result:=True;
      finally
       CloseHandle(ProcessInformation.hProcess);
       CloseHandle(ProcessInformation.hThread);
      end;
     end;
   finally
    CloseHandle(ReadHandle);
    CloseHandle(WriteHandle);
   end;
  end;
end;
{$ENDIF}
{==============================================================================}
{$IFDEF LINUX}
function ExecuteShellProcess(const ACommand:String;AParameters:TStrings;const AWorking,AEndMark,AProgressMark:String;AMemo:TMemo;AProgress:TProgressBar):Boolean;
var
 Count:Integer;
 BytesRead:LongInt;
 ReadData:String;
 ReadChar:AnsiChar;
 ProcessInfo:TProcess;
begin
 {}
 Result:=False;
 try
  {Check Parameters}
  if Length(ACommand) = 0 then Exit;
  if AParameters = nil then Exit;
  if Length(AWorking) = 0 then Exit;
  if AMemo = nil then Exit;
  if AProgress = nil then Exit;

  ProcessInfo:=TProcess.Create(nil);
  try
   {Setup Process Options}
   ProcessInfo.Options:=[poUsePipes,poStderrToOutPut];

   {Add Executable}
   ProcessInfo.Executable:=ACommand;

   {Add Working}
   ProcessInfo.CurrentDirectory:=AWorking;

   {Add Parameters}
   for Count:=0 to AParameters.Count - 1 do
    begin
     ProcessInfo.Parameters.Add(AParameters.Strings[Count]);
    end;

   {Execute Process}
   ProcessInfo.Execute;

   ReadData:='';
   ReadChar:=#0;

   repeat
    {Read from Pipe}
    BytesRead := ProcessInfo.Output.Read(ReadChar, SizeOf(AnsiChar));

    {Check Bytes Read}
    if BytesRead > 0 then
     begin
      {Check for CR LF}
      if not(ReadChar in [#10,#13]) then
       begin
        ReadData:=ReadData + ReadChar;
       end
      else
       begin
        {Check for LF}
        if ReadChar = #10 then
         begin
          {Check End Mark}
          if Length(AEndMark) > 0 then
           begin
            if ReadData = AEndMark then Break;
           end;

          {Check Progress Mark}
          if (Length(AProgressMark) > 0) and (Copy(ReadData,1,Length(AProgressMark)) = AProgressMark) then
           begin
            {Update Progress}
            AProgress.Position:=StrToIntDef(Copy(ReadData,Length(AProgressMark) + 1,Length(ReadData)),AProgress.Position);
           end
          else
           begin
            {Add Output}
            AMemo.Lines.Add(ReadData);
            AMemo.SelStart:=Length(AMemo.Text);
           end;

          Application.ProcessMessages;

          {Reset Data}
          ReadData:='';
         end;
       end;
     end;

   until BytesRead = 0;

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

function AddTrailingSlash(const FilePath:String):String;
var
 WorkBuffer:String;
begin
 {}
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
 WorkBuffer:String;
begin
 {}
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
{$IFDEF WINDOWS}
function IsWinNT6orAbove:Boolean;
{Returns True for WindowsVista(6.0) or higher}
var
 OsVersionInfo:TOSVersionInfo;
begin
 {}
 Result:=False;
 FillChar(OsVersionInfo,SizeOf(OsVersionInfo),0);
 OsVersionInfo.dwOSVersionInfoSize:=SizeOf(OsVersionInfo);
 if not GetVersionEx(OsVersionInfo) then Exit;
 if OsVersionInfo.dwPlatformId <> VER_PLATFORM_WIN32_NT then Exit;
 Result:=(OsVersionInfo.dwMajorVersion >= 6);
end;
{$ENDIF}
{==============================================================================}
{==============================================================================}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
 {}
 {A value to prefix on the path}
 PathPrefix:='';

 {Assume that this is running from <InstallPath>\tools and that the InstallPath will be the folder above}
 InstallPath:=ExtractFileDir(ExtractFileDir(Application.ExeName));

 {The current compiler version}
 CompilerVersion:='3.1.1';

 {$IFDEF WINDOWS}
 {Assume the compiler name is fpc.exe}
 CompilerName:='fpc.exe';
 {$ENDIF}
 {$IFDEF LINUX}
 {Assume the compiler name is fpc}
 CompilerName:='fpc';
 {$ENDIF}

 {$IFDEF WINDOWS}
 {Assume that the compiler path will be \fpc\<CompilerVersion> under the InstallPath}
 CompilerPath:=InstallPath + '\fpc\' + CompilerVersion;
 {$ENDIF}
 {$IFDEF LINUX}
 {Assume that the compiler path will be /fpc/bin under the InstallPath}
 CompilerPath:=InstallPath + '/fpc/bin';
 {$ENDIF}

 {$IFDEF WINDOWS}
 {Assume that the source path will be \source under the CompilerPath}
 SourcePath:=CompilerPath + '\source';
 {$ENDIF}
 {$IFDEF LINUX}
 {Assume that the source path will be /source under the CompilerPath}
 SourcePath:=CompilerPath + '/source';
 {$ENDIF}

 {The names of the ARM compiler or cross compiler}
 ARMCompiler:='';

 {The names of the AARCH64 compiler or cross compiler}
 AARCH64Compiler:='';

 BuildRTL:=True;
 BuildPackages:=True;

 PlatformARMv6:=True;
 PlatformARMv7:=True;
 PlatformARMv8:=True;

 VersionURL:=DefaultVersionURL;
 DownloadURL:=DefaultDownloadURL;

 LoadConfig;

 {$IFDEF LINUX}
 {Temporarily disable ARMv8 RTL build for Linux}
 PlatformARMv8:=False;
 chkARMv8.Visible:=False;
 {$ENDIF}
end;

{==============================================================================}

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
 {}

end;

{==============================================================================}

procedure TfrmMain.FormShow(Sender: TObject);
var
 Scale:Double;
begin
 {}
 chkARMv6.Checked:=PlatformARMv6;
 chkARMv7.Checked:=PlatformARMv7;
 chkARMv8.Checked:=PlatformARMv8;

 {Check PixelsPerInch}
 if PixelsPerInch > 96 then
  begin
   {Calculate Scale}
   Scale:=(PixelsPerInch / 96);

   {Disable Anchors}
   lblMain.Anchors:=[akLeft,akTop];
   chkARMv6.Anchors:=[akLeft,akTop];
   chkARMv7.Anchors:=[akLeft,akTop];
   chkARMv8.Anchors:=[akLeft,akTop];
   cmdCheck.Anchors:=[akLeft,akTop];
   cmdDownload.Anchors:=[akLeft,akTop];
   cmdOffline.Anchors:=[akLeft,akTop];
   cmdBuild.Anchors:=[akLeft,akTop];
   cmdExit.Anchors:=[akLeft,akTop];

   {Resize Form}
   Width:=Trunc(Width * Scale);
   Height:=Trunc(Height * Scale);

   {Move Buttons}
   cmdCheck.Left:=pnlMain.Width - Trunc(150 * Scale); {907 - 757 = 150}
   cmdDownload.Left:=pnlMain.Width - Trunc(150 * Scale); {907 - 757 = 150}
   cmdOffline.Left:=pnlMain.Width - Trunc(150 * Scale); {907 - 757 = 150}
   cmdBuild.Left:=pnlMain.Width - Trunc(150 * Scale); {907 - 757 = 150}
   cmdExit.Left:=pnlMain.Width - Trunc(150 * Scale);  {907 - 757 = 150}

   {Enable Anchors}
   lblMain.Anchors:=[akLeft,akTop,akRight];
   chkARMv6.Anchors:=[akLeft,akTop,akRight];
   chkARMv7.Anchors:=[akLeft,akTop,akRight];
   chkARMv8.Anchors:=[akLeft,akTop,akRight];
   cmdCheck.Anchors:=[akTop,akRight];
   cmdDownload.Anchors:=[akTop,akRight];
   cmdOffline.Anchors:=[akTop,akRight];
   cmdBuild.Anchors:=[akTop,akRight];
   cmdExit.Anchors:=[akTop,akRight];
  end;

 {Check Check Button}
 if (cmdCheck.Left + cmdCheck.Width) > Width then
  begin
   {Adjust Check Button}
   cmdCheck.Left:=ClientWidth - 150; {907 - 757 = 150}
  end;

 {Check Download Button}
 if (cmdDownload.Left + cmdDownload.Width) > Width then
  begin
   {Adjust Download Button}
   cmdDownload.Left:=ClientWidth - 150; {907 - 757 = 150}
  end;

 {Check Offline Button}
 if (cmdOffline.Left + cmdOffline.Width) > Width then
  begin
   {Adjust Offline Button}
   cmdOffline.Left:=ClientWidth - 150; {907 - 757 = 150}
  end;

 {Check Build Button}
 if (cmdBuild.Left + cmdBuild.Width) > Width then
  begin
   {Adjust Build Button}
   cmdBuild.Left:=ClientWidth - 150; {907 - 757 = 150}
  end;

 {Check Exit Button}
 if (cmdExit.Left + cmdExit.Width) > Width then
  begin
   {Adjust Exit Button}
   cmdExit.Left:=ClientWidth - 150; {907 - 757 = 150}
  end;

 {Check Main Label}
 if (lblMain.Left + lblMain.Width) >= cmdBuild.Left then
  begin
    {Adjust Main Label}
    lblMain.Width:=(cmdBuild.Left - lblMain.Left) - 150;
  end;
end;

{==============================================================================}

procedure TfrmMain.FormHide(Sender: TObject);
begin
 {}

end;

{==============================================================================}

procedure TfrmMain.chkARMv6Click(Sender: TObject);
begin
 {}
 PlatformARMv6:=chkARMv6.Checked;
end;

{==============================================================================}

procedure TfrmMain.chkARMv7Click(Sender: TObject);
begin
 {}
 PlatformARMv7:=chkARMv7.Checked;
end;

{==============================================================================}

procedure TfrmMain.chkARMv8Click(Sender: TObject);
begin
 {}
 PlatformARMv8:=chkARMv8.Checked;
end;

{==============================================================================}

procedure TfrmMain.cmdExitClick(Sender: TObject);
begin
 {}
 Application.Terminate;
end;

{==============================================================================}

procedure TfrmMain.cmdCheckClick(Sender: TObject);
begin
 {}
 lblMain.Enabled:=False;
 lblPlatforms.Enabled:=False;
 chkARMv6.Enabled:=False;
 chkARMv7.Enabled:=False;
 chkARMv8.Enabled:=False;
 cmdCheck.Enabled:=False;
 cmdDownload.Enabled:=False;
 cmdOffline.Enabled:=False;
 cmdBuild.Enabled:=False;
 cmdExit.Enabled:=False;
 progressMain.Position:=0;
 try
  {Clear Memo}
  mmoMain.Lines.Clear;

  {Add Banner}
  mmoMain.Lines.Add('Checking for Ultibo RTL Updates');
  mmoMain.Lines.Add('');

  {Add Version URL}
  mmoMain.Lines.Add(' Version URL is ' + VersionURL);
  mmoMain.Lines.Add('');

  {Check for Updates}
  if CheckForUpdates then
   begin
    {Prompt for Download}
    if MessageDlg('An update is available for the Ultibo RTL, do you want to download and install it now?',mtConfirmation,[mbYes,mbNo],0) = mrYes then
     begin
      {Add Banner}
      mmoMain.Lines.Add('Downloading Latest Ultibo RTL');
      mmoMain.Lines.Add('');

      {Download Latest}
      if not DownloadLatest then
       begin
        mmoMain.Lines.Add(' Error: Failed to download latest RTL');
        Exit;
       end;

      {Add Banner}
      mmoMain.Lines.Add('Extracting Ultibo RTL');
      mmoMain.Lines.Add('');

      {Extract File}
      if not ExtractFile(DownloadZip) then
       begin
        mmoMain.Lines.Add(' Error: Failed to extract RTL');
        Exit;
       end;

      {Add Banner}
      mmoMain.Lines.Add('Building Ultibo RTL');
      mmoMain.Lines.Add('');

      {Add Path Prefix}
      mmoMain.Lines.Add(' Path Prefix is ' + PathPrefix);
      mmoMain.Lines.Add('');
      {Add Install Path}
      mmoMain.Lines.Add(' Install Path is ' + InstallPath);
      mmoMain.Lines.Add('');
      {Add Compiler Name}
      mmoMain.Lines.Add(' Compiler Name is ' + CompilerName);
      mmoMain.Lines.Add('');
      {Add Compiler Path}
      mmoMain.Lines.Add(' Compiler Path is ' + CompilerPath);
      mmoMain.Lines.Add('');
      {Add Source Path}
      mmoMain.Lines.Add(' Source Path is ' + SourcePath);
      mmoMain.Lines.Add('');

      {Add ARM Compiler}
      if Length(ARMCompiler) <> 0 then
       begin
        mmoMain.Lines.Add(' ARM Compiler is ' + ARMCompiler);
        mmoMain.Lines.Add('');
       end;
      {Add AARCH64 Compiler}
      if Length(AARCH64Compiler) <> 0 then
       begin
        mmoMain.Lines.Add(' AARCH64 Compiler is ' + AARCH64Compiler);
        mmoMain.Lines.Add('');
       end;

      {Create Build File}
      mmoMain.Lines.Add(' Creating Build Script');
      mmoMain.Lines.Add('');
      if not CreateBuildFile then
       begin
        mmoMain.Lines.Add(' Error: Failed to create build script');
        Exit;
       end;

      {Execute Build File}
      mmoMain.Lines.Add(' Executing Build Script');
      mmoMain.Lines.Add('');
      if not ExecuteBuildFile then
       begin
        mmoMain.Lines.Add(' Error: Failed to execute build script');
        Exit;
       end;
     end;
   end;

  {Add Footer}
  mmoMain.Lines.Add('');
  mmoMain.Lines.Add('Check Completed');
  mmoMain.Lines.Add('');
 finally
  lblMain.Enabled:=True;
  lblPlatforms.Enabled:=True;
  chkARMv6.Enabled:=True;
  chkARMv7.Enabled:=True;
  chkARMv8.Enabled:=True;
  cmdCheck.Enabled:=True;
  cmdDownload.Enabled:=True;
  cmdOffline.Enabled:=True;
  cmdBuild.Enabled:=True;
  cmdExit.Enabled:=True;
 end;
end;

{==============================================================================}

procedure TfrmMain.cmdDownloadClick(Sender: TObject);
begin
 {}
 lblMain.Enabled:=False;
 lblPlatforms.Enabled:=False;
 chkARMv6.Enabled:=False;
 chkARMv7.Enabled:=False;
 chkARMv8.Enabled:=False;
 cmdCheck.Enabled:=False;
 cmdDownload.Enabled:=False;
 cmdOffline.Enabled:=False;
 cmdBuild.Enabled:=False;
 cmdExit.Enabled:=False;
 progressMain.Position:=0;
 try
  {Clear Memo}
  mmoMain.Lines.Clear;

  {Add Banner}
  mmoMain.Lines.Add('Downloading Latest Ultibo RTL');
  mmoMain.Lines.Add('');

  {Download Latest}
  if not DownloadLatest then
   begin
    mmoMain.Lines.Add(' Error: Failed to download latest RTL');
    Exit;
   end;

  {Prompt for Install}
  if MessageDlg('The latest RTL has been downloaded, do you want to install it now?',mtConfirmation,[mbYes,mbNo],0) = mrYes then
   begin
    {Add Banner}
    mmoMain.Lines.Add('Extracting Ultibo RTL');
    mmoMain.Lines.Add('');

    {Extract File}
    if not ExtractFile(DownloadZip) then
     begin
      mmoMain.Lines.Add(' Error: Failed to extract RTL');
      Exit;
     end;

    {Add Banner}
    mmoMain.Lines.Add('Building Ultibo RTL');
    mmoMain.Lines.Add('');

    {Add Path Prefix}
    mmoMain.Lines.Add(' Path Prefix is ' + PathPrefix);
    mmoMain.Lines.Add('');
    {Add Install Path}
    mmoMain.Lines.Add(' Install Path is ' + InstallPath);
    mmoMain.Lines.Add('');
    {Add Compiler Name}
    mmoMain.Lines.Add(' Compiler Name is ' + CompilerName);
    mmoMain.Lines.Add('');
    {Add Compiler Path}
    mmoMain.Lines.Add(' Compiler Path is ' + CompilerPath);
    mmoMain.Lines.Add('');
    {Add Source Path}
    mmoMain.Lines.Add(' Source Path is ' + SourcePath);
    mmoMain.Lines.Add('');

    {Add ARM Compiler}
    if Length(ARMCompiler) <> 0 then
     begin
      mmoMain.Lines.Add(' ARM Compiler is ' + ARMCompiler);
      mmoMain.Lines.Add('');
     end;
    {Add AARCH64 Compiler}
    if Length(AARCH64Compiler) <> 0 then
     begin
      mmoMain.Lines.Add(' AARCH64 Compiler is ' + AARCH64Compiler);
      mmoMain.Lines.Add('');
     end;

    {Create Build File}
    mmoMain.Lines.Add(' Creating Build Script');
    mmoMain.Lines.Add('');
    if not CreateBuildFile then
     begin
      mmoMain.Lines.Add(' Error: Failed to create build script');
      Exit;
     end;

    {Execute Build File}
    mmoMain.Lines.Add(' Executing Build Script');
    mmoMain.Lines.Add('');
    if not ExecuteBuildFile then
     begin
      mmoMain.Lines.Add(' Error: Failed to execute build script');
      Exit;
     end;
   end;

  {Add Footer}
  mmoMain.Lines.Add('');
  mmoMain.Lines.Add('Download Completed');
  mmoMain.Lines.Add('');
 finally
  lblMain.Enabled:=True;
  lblPlatforms.Enabled:=True;
  chkARMv6.Enabled:=True;
  chkARMv7.Enabled:=True;
  chkARMv8.Enabled:=True;
  cmdCheck.Enabled:=True;
  cmdDownload.Enabled:=True;
  cmdOffline.Enabled:=True;
  cmdBuild.Enabled:=True;
  cmdExit.Enabled:=True;
 end;
end;

{==============================================================================}

procedure TfrmMain.cmdOfflineClick(Sender: TObject);
begin
 {}
 lblMain.Enabled:=False;
 lblPlatforms.Enabled:=False;
 chkARMv6.Enabled:=False;
 chkARMv7.Enabled:=False;
 chkARMv8.Enabled:=False;
 cmdCheck.Enabled:=False;
 cmdDownload.Enabled:=False;
 cmdOffline.Enabled:=False;
 cmdBuild.Enabled:=False;
 cmdExit.Enabled:=False;
 progressMain.Position:=0;
 try
  {Clear Memo}
  mmoMain.Lines.Clear;

  {Add Banner}
  mmoMain.Lines.Add('Extracting Offline Ultibo RTL');
  mmoMain.Lines.Add('');

  {Browse for file}
  openMain.Title:='Extract Offline RTL';
  openMain.InitialDir:=ExtractFileDir(Application.ExeName);
  {$IFDEF WINDOWS}
  openMain.Filter:='Zip files (*.zip)|*.zip|All files (*.*)|*.*';
  {$ENDIF}
  {$IFDEF LINUX}
  openMain.Filter:='Zip files (*.zip)|*.zip|All files (*.*)|*';
  {$ENDIF}
  if not openMain.Execute then Exit;

  {Prompt for Extract}
  if MessageDlg('Do you want to extract and install the RTL from the file "' + openMain.Filename + '", this will overwrite your existing RTL?',mtConfirmation,[mbYes,mbNo],0) <> mrYes then
    begin
     Exit;
    end;

  {Extract File}
  if not ExtractFile(openMain.Filename) then
   begin
    mmoMain.Lines.Add(' Error: Failed to extract offline RTL');
    Exit;
   end;

  {Prompt for Build}
  if MessageDlg('The RTL has been extracted and installed, do you want to build it now?',mtConfirmation,[mbYes,mbNo],0) = mrYes then
   begin
    {Add Banner}
    mmoMain.Lines.Add('Building Ultibo RTL');
    mmoMain.Lines.Add('');

    {Add Path Prefix}
    mmoMain.Lines.Add(' Path Prefix is ' + PathPrefix);
    mmoMain.Lines.Add('');
    {Add Install Path}
    mmoMain.Lines.Add(' Install Path is ' + InstallPath);
    mmoMain.Lines.Add('');
    {Add Compiler Name}
    mmoMain.Lines.Add(' Compiler Name is ' + CompilerName);
    mmoMain.Lines.Add('');
    {Add Compiler Path}
    mmoMain.Lines.Add(' Compiler Path is ' + CompilerPath);
    mmoMain.Lines.Add('');
    {Add Source Path}
    mmoMain.Lines.Add(' Source Path is ' + SourcePath);
    mmoMain.Lines.Add('');

    {Add ARM Compiler}
    if Length(ARMCompiler) <> 0 then
     begin
      mmoMain.Lines.Add(' ARM Compiler is ' + ARMCompiler);
      mmoMain.Lines.Add('');
     end;
    {Add AARCH64 Compiler}
    if Length(AARCH64Compiler) <> 0 then
     begin
      mmoMain.Lines.Add(' AARCH64 Compiler is ' + AARCH64Compiler);
      mmoMain.Lines.Add('');
     end;

    {Create Build File}
    mmoMain.Lines.Add(' Creating Build Script');
    mmoMain.Lines.Add('');
    if not CreateBuildFile then
     begin
      mmoMain.Lines.Add(' Error: Failed to create build script');
      Exit;
     end;

    {Execute Build File}
    mmoMain.Lines.Add(' Executing Build Script');
    mmoMain.Lines.Add('');
    if not ExecuteBuildFile then
     begin
      mmoMain.Lines.Add(' Error: Failed to execute build script');
      Exit;
     end;
   end;

  {Add Footer}
  mmoMain.Lines.Add('');
  mmoMain.Lines.Add('Extract Completed');
  mmoMain.Lines.Add('');
 finally
  lblMain.Enabled:=True;
  lblPlatforms.Enabled:=True;
  chkARMv6.Enabled:=True;
  chkARMv7.Enabled:=True;
  chkARMv8.Enabled:=True;
  cmdCheck.Enabled:=True;
  cmdDownload.Enabled:=True;
  cmdOffline.Enabled:=True;
  cmdBuild.Enabled:=True;
  cmdExit.Enabled:=True;
 end;
end;

{==============================================================================}

procedure TfrmMain.cmdBuildClick(Sender: TObject);
begin
 {}
 lblMain.Enabled:=False;
 lblPlatforms.Enabled:=False;
 chkARMv6.Enabled:=False;
 chkARMv7.Enabled:=False;
 chkARMv8.Enabled:=False;
 cmdCheck.Enabled:=False;
 cmdDownload.Enabled:=False;
 cmdOffline.Enabled:=False;
 cmdBuild.Enabled:=False;
 cmdExit.Enabled:=False;
 progressMain.Position:=0;
 try
  {Clear Memo}
  mmoMain.Lines.Clear;

  {Add Banner}
  mmoMain.Lines.Add('Building Current Ultibo RTL');
  mmoMain.Lines.Add('');

  {Add Path Prefix}
  mmoMain.Lines.Add(' Path Prefix is ' + PathPrefix);
  mmoMain.Lines.Add('');
  {Add Install Path}
  mmoMain.Lines.Add(' Install Path is ' + InstallPath);
  mmoMain.Lines.Add('');
  {Add Compiler Name}
  mmoMain.Lines.Add(' Compiler Name is ' + CompilerName);
  mmoMain.Lines.Add('');
  {Add Compiler Path}
  mmoMain.Lines.Add(' Compiler Path is ' + CompilerPath);
  mmoMain.Lines.Add('');
  {Add Source Path}
  mmoMain.Lines.Add(' Source Path is ' + SourcePath);
  mmoMain.Lines.Add('');

  {Add ARM Compiler}
  if Length(ARMCompiler) <> 0 then
   begin
    mmoMain.Lines.Add(' ARM Compiler is ' + ARMCompiler);
    mmoMain.Lines.Add('');
   end;
  {Add AARCH64 Compiler}
  if Length(AARCH64Compiler) <> 0 then
   begin
    mmoMain.Lines.Add(' AARCH64 Compiler is ' + AARCH64Compiler);
    mmoMain.Lines.Add('');
   end;

  {Create Build File}
  mmoMain.Lines.Add(' Creating Build Script');
  mmoMain.Lines.Add('');
  if not CreateBuildFile then
   begin
    mmoMain.Lines.Add(' Error: Failed to create build script');
    Exit;
   end;

  {Execute Build File}
  mmoMain.Lines.Add(' Executing Build Script');
  mmoMain.Lines.Add('');
  if not ExecuteBuildFile then
   begin
    mmoMain.Lines.Add(' Error: Failed to execute build script');
    Exit;
   end;

  {Add Footer}
  mmoMain.Lines.Add('');
  mmoMain.Lines.Add('Completed Build');
  mmoMain.Lines.Add('');
 finally
  lblMain.Enabled:=True;
  lblPlatforms.Enabled:=True;
  chkARMv6.Enabled:=True;
  chkARMv7.Enabled:=True;
  chkARMv8.Enabled:=True;
  cmdCheck.Enabled:=True;
  cmdDownload.Enabled:=True;
  cmdOffline.Enabled:=True;
  cmdBuild.Enabled:=True;
  cmdExit.Enabled:=True;
 end;
end;

{==============================================================================}

procedure TfrmMain.DoProgress(Sender:TObject;const Percent:Double);
begin
 {}
 progressMain.Position:=Trunc(Percent);
 Application.ProcessMessages;
end;

{==============================================================================}

procedure TfrmMain.DoDataReceived(Sender:TObject;const ContentLength,CurrentPos:Int64);
begin
 {}
 if ContentLength > 0 then
  begin
   if CurrentPos < ContentLength then
    begin
     progressMain.Position:=Trunc((CurrentPos / ContentLength) * 100);
    end
   else
    begin
     progressMain.Position:=100;
    end;
   Application.ProcessMessages;
  end;
end;

{==============================================================================}

function TfrmMain.LoadConfig:Boolean;
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
     Section:='BuildRTL';

     {Get PathPrefix}
     PathPrefix:=IniFile.ReadString(Section,'PathPrefix',PathPrefix);
     {Get InstallPath}
     InstallPath:=IniFile.ReadString(Section,'InstallPath',InstallPath);
     {Get CompilerName}
     CompilerName:=IniFile.ReadString(Section,'CompilerName',CompilerName);
     {Get CompilerPath}
     CompilerPath:=IniFile.ReadString(Section,'CompilerPath',CompilerPath);
     {Get CompilerVersion}
     CompilerVersion:=IniFile.ReadString(Section,'CompilerVersion',CompilerVersion);
     {Get SourcePath}
     SourcePath:=IniFile.ReadString(Section,'SourcePath',SourcePath);

     {Get ARMCompiler}
     ARMCompiler:=IniFile.ReadString(Section,'ARMCompiler',ARMCompiler);
     {Get AARCH64Compiler}
     AARCH64Compiler:=IniFile.ReadString(Section,'AARCH64Compiler',AARCH64Compiler);

     {Get BuildRTL}
     BuildRTL:=IniFile.ReadBool(Section,'BuildRTL',BuildRTL);
     {Get BuildPackages}
     BuildPackages:=IniFile.ReadBool(Section,'BuildPackages',BuildPackages);

     {Get PlatformARMv6}
     PlatformARMv6:=IniFile.ReadBool(Section,'PlatformARMv6',PlatformARMv6);
     {Get PlatformARMv7}
     PlatformARMv7:=IniFile.ReadBool(Section,'PlatformARMv7',PlatformARMv7);
     {Get PlatformARMv8}
     PlatformARMv8:=IniFile.ReadBool(Section,'PlatformARMv8',PlatformARMv8);

     {Get VersionURL}
     VersionURL:=IniFile.ReadString(Section,'VersionURL',VersionURL);
     {Get DownloadURL}
     DownloadURL:=IniFile.ReadString(Section,'DownloadURL',DownloadURL);
    finally
     IniFile.Free;
    end;
   end;

  Result:=True;
 except
  {}
 end;
end;

function TfrmMain.CheckForUpdates:Boolean;
var
 FileURL:String;
 Filename:String;
 Lastname:String;
 Lines:TStringList;
 FileVersion:String;
 LastVersion:String;
 Client:TFPHTTPClient;
begin
 {}
 Result:=False;
 try
  if Length(SourcePath) = 0 then Exit;
  if Length(VersionURL) = 0 then Exit;

  {Check Source Path}
  if not DirectoryExists(StripTrailingSlash(SourcePath)) then
   begin
    mmoMain.Lines.Add(' Error: SourcePath "' + SourcePath + '" does not exist');
    mmoMain.Lines.Add('');
    Exit;
   end;

  {Get FileURL}
  FileURL:=VersionURL + VersionId;
  mmoMain.Lines.Add('  Version File URL is ' + FileURL);
  mmoMain.Lines.Add('');

  {Get Filename}
  Filename:=AddTrailingSlash(SourcePath) + VersionId;
  mmoMain.Lines.Add('  Latest Version Filename is ' + Filename);
  mmoMain.Lines.Add('');

  {Get Lastname}
  Lastname:=AddTrailingSlash(SourcePath) + VersionLast;
  mmoMain.Lines.Add('  Current Version Filename is ' + Lastname);
  mmoMain.Lines.Add('');

  {Set Defaults}
  FileVersion:='<Unknown>';
  LastVersion:='<Unknown>';

  {Check File}
  if FileExists(Filename) then
   begin
    {Delete File}
    DeleteFile(Filename);

    if FileExists(Filename) then Exit;
   end;

  {Create HTTP Client}
  Client:=TFPHTTPClient.Create(nil);
  try
   Client.AllowRedirect:=True;

   {Get Version File}
   mmoMain.Lines.Add('  Downloading file: ' + VersionId);
   mmoMain.Lines.Add('');
   Client.Get(FileURL,Filename);

   Lines:=TStringList.Create;
   try
    {Open Version File}
    Lines.LoadFromFile(Filename);
    if Lines.Count = 0 then
     begin
      mmoMain.Lines.Add(' Error: Latest version file contains no data');
      mmoMain.Lines.Add('');
      Exit;
     end;

    {Get File Version}
    FileVersion:=Lines.Strings[0];
    mmoMain.Lines.Add('  Latest Version is ' + FileVersion);
    mmoMain.Lines.Add('');

    {Open Last Version File}
    if FileExists(Lastname) then
     begin
      Lines.Clear;
      Lines.LoadFromFile(Lastname);
      if Lines.Count = 0 then
       begin
        mmoMain.Lines.Add(' Error: Current version file contains no data');
        mmoMain.Lines.Add('');
        Exit;
       end;

      {Get Last Version}
      LastVersion:=Lines.Strings[0];
     end;
    mmoMain.Lines.Add('  Current Version is ' + LastVersion);
    mmoMain.Lines.Add('');

    Result:=(LastVersion <> FileVersion);
   finally
    Lines.Free;
   end;
  finally
   Client.Free;
  end;
 except
  on E: Exception do
   begin
    mmoMain.Lines.Add(' Error: Exception checking for RTL updates - Message: ' + E.Message);
   end;
 end;
end;

{==============================================================================}

function TfrmMain.DownloadLatest:Boolean;
var
 FileURL:String;
 Filename:String;
 Client:TFPHTTPClient;
begin
 {}
 Result:=False;
 try
  if Length(SourcePath) = 0 then Exit;
  if Length(DownloadURL) = 0 then Exit;

  {Check Source Path}
  if not DirectoryExists(StripTrailingSlash(SourcePath)) then
   begin
    mmoMain.Lines.Add(' Error: SourcePath "' + SourcePath + '" does not exist');
    mmoMain.Lines.Add('');
    Exit;
   end;

  {Get FileURL}
  FileURL:=DownloadURL + DownloadZip;
  mmoMain.Lines.Add('  Download File URL is ' + FileURL);
  mmoMain.Lines.Add('');

  {Get Filename}
  Filename:=AddTrailingSlash(SourcePath) + DownloadZip;
  mmoMain.Lines.Add('  Download Filename is ' + Filename);
  mmoMain.Lines.Add('');

  {Check File}
  if FileExists(Filename) then
   begin
    {Delete File}
    DeleteFile(Filename);

    if FileExists(Filename) then Exit;
   end;

  {Create HTTP Client}
  Client:=TFPHTTPClient.Create(nil);
  try
   Client.AllowRedirect:=True;
   Client.OnDataReceived:=DoDataReceived;

   {Get Zip File}
   mmoMain.Lines.Add('  Downloading file: ' + DownloadZip);
   mmoMain.Lines.Add('');
   Client.Get(FileURL,Filename);

   Result := True;
  finally
   Client.Free;
  end;
 except
  on E: Exception do
   begin
    mmoMain.Lines.Add(' Error: Exception downloading latest RTL - Message: ' + E.Message);
   end;
 end;
end;

{==============================================================================}

function TfrmMain.ExtractFile(const AFilename:String):Boolean;
var
 Pathname:String;
 Tempname:String;
 Filename:String;
 Destname:String;
 Sourcename:String;
 UnZipper:TUnZipper;
begin
 {}
 Result:=False;
 try
  if Length(AFilename) = 0 then Exit;

  {Check Source Path}
  if not DirectoryExists(StripTrailingSlash(SourcePath)) then
   begin
    mmoMain.Lines.Add(' Error: SourcePath "' + SourcePath + '" does not exist');
    mmoMain.Lines.Add('');
    Exit;
   end;

  {Get Filename}
  Filename:=AFilename;
  if ExtractFilePath(Filename) = '' then
   begin
    Filename:=AddTrailingSlash(SourcePath) + Filename;
   end;
  mmoMain.Lines.Add('  Filename is ' + Filename);
  mmoMain.Lines.Add('');

  {Get Pathname}
  Pathname:=StripTrailingSlash(SourcePath);
  mmoMain.Lines.Add('  Pathname is ' + Pathname);
  mmoMain.Lines.Add('');

  {Get Tempname}
  Tempname:=AddTrailingSlash(SourcePath) + '__temp';

  {Check Tempname}
  if DirectoryExists(Tempname) then
   begin
    DeleteDirectory(Tempname,True);
   end;
  if not DirectoryExists(Tempname) then
   begin
    CreateDir(Tempname);

    if not DirectoryExists(Tempname) then Exit;
   end;

  {Create Unzipper}
  UnZipper:=TUnZipper.Create;
  try
   UnZipper.OnProgress:=DoProgress;
   UnZipper.FileName:=Filename;
   UnZipper.OutputPath:=Tempname;

   {Extract Zip File}
   mmoMain.Lines.Add('  Extracting file: ' + AFilename);
   mmoMain.Lines.Add('');
   UnZipper.Examine;
   UnZipper.UnZipAllFiles;

   {Get Source and Destination}
   Destname:=ExtractFileDir(Pathname);
   Sourcename:=AddTrailingSlash(Tempname) + 'Core-master';

   {Copy Files}
   if CopyDirTree(Sourcename,Destname,[cffOverwriteFile,cffCreateDestDirectory,cffPreserveTime]) then
    begin
     {Cleanup Tempname}
     DeleteDirectory(Tempname,False);

     {Get Source and Destination}
     Destname:=AddTrailingSlash(Pathname) + VersionLast;
     Sourcename:=AddTrailingSlash(Pathname) + VersionId;

     {Check Destname}
     if FileExists(Destname) then
      begin
       DeleteFile(Destname);

       if FileExists(Destname) then Exit;
      end;

     {Rename Version File}
     Result:=RenameFile(Sourcename,Destname);
    end;
  finally
   UnZipper.Free;
  end;
 except
  on E: Exception do
   begin
    mmoMain.Lines.Add(' Error: Exception extracting RTL - Message: ' + E.Message);
   end;
 end;
end;

{==============================================================================}

function TfrmMain.CreateBuildFile:Boolean;
var
 Total:Integer;
 Progress:Integer;
 Percent:Integer;
 Filename:String;
 WorkBuffer:String;
 FileStream:TFileStream;
begin
 {}
 Result:=False;
 try
  if Length(SourcePath) = 0 then Exit;
  if Length(InstallPath) = 0 then Exit;
  if Length(CompilerName) = 0 then Exit;
  if Length(CompilerPath) = 0 then Exit;
  if Length(CompilerVersion) = 0 then Exit;
  if not(BuildRTL) and not(BuildPackages) then Exit;
  if not(PlatformARMv6) and not(PlatformARMv7) and not(PlatformARMv8) then Exit;

  {Get ARM Compiler}
  if Length(ARMCompiler) = 0 then ARMCompiler:=CompilerName;

  {Get AARCH64 Compiler}
  if Length(AARCH64Compiler) = 0 then AARCH64Compiler:=CompilerName;

  {Get Filename}
  Filename:=AddTrailingSlash(SourcePath) + BuildScript;
  mmoMain.Lines.Add('  Build Script is ' + Filename);
  mmoMain.Lines.Add('');

  {Check File}
  if FileExists(Filename) then
   begin
    {Delete File}
    DeleteFile(Filename);

    if FileExists(Filename) then Exit;
   end;

  {Create File}
  FileStream:=TFileStream.Create(Filename,fmCreate);
  try
   {Calculate Progress}
   Total:=0;
   Progress:=0;
   Percent:=0;

   {Check RTL}
   if BuildRTL then
    begin
     if PlatformARMv6 then Inc(Total, 3);
     if PlatformARMv7 then Inc(Total, 3);
     if PlatformARMv8 then Inc(Total, 3);
    end;

   {Check Packages}
   if BuildPackages then
    begin
     if PlatformARMv6 then Inc(Total, 4);
     if PlatformARMv7 then Inc(Total, 4);
     if PlatformARMv8 then Inc(Total, 4);
    end;

   {Calculate Percent}
   if Total > 0 then
    begin
     Percent:=100 div Total;
    end;

   {$IFDEF WINDOWS}
   {Add Header}
   WorkBuffer:='';
   WorkBuffer:=WorkBuffer + '@echo off' + LineEnd;
   WorkBuffer:=WorkBuffer + 'set path=' + PathPrefix + StripTrailingSlash(CompilerPath) + '\bin\i386-win32' + LineEnd;
   WorkBuffer:=WorkBuffer + '' + LineEnd;

   {Add Start}
   WorkBuffer:=WorkBuffer + 'echo .' + LineEnd;
   WorkBuffer:=WorkBuffer + 'echo ======================Start of Build Script======================' + LineEnd;

   {Check ARMv6}
   if PlatformARMv6 then
    begin
     {Check RTL}
     if BuildRTL then
      begin
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo .' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo Building ARMv6 RTL' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo ==================' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo .' + LineEnd;

       {RTL Clean}
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'make rtl_clean CROSSINSTALL=1 OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv6';
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/' + ARMCompiler;
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV6 -CfVFPV2 -CIARM -CaEABIHF -OoFASTMATH"';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/' + ARMCompiler + LineEnd;

       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'IF %errorlevel% NEQ 0 GOTO Error' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo .' + LineEnd;

       Inc(Progress);
       WorkBuffer:=WorkBuffer + 'echo ' + MarkerProgress + IntToStr(Progress * Percent) + LineEnd;

       {RTL}
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'make rtl OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv6';
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/' + ARMCompiler;
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV6 -CfVFPV2 -CIARM -CaEABIHF -OoFASTMATH"';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/' + ARMCompiler + LineEnd;

       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'IF %errorlevel% NEQ 0 GOTO Error' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo .' + LineEnd;

       Inc(Progress);
       WorkBuffer:=WorkBuffer + 'echo ' + MarkerProgress + IntToStr(Progress * Percent) + LineEnd;

       {RTL Install}
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'make rtl_install CROSSINSTALL=1';
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/' + ARMCompiler;
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV6 -CfVFPV2 -CIARM -CaEABIHF -OoFASTMATH" OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv6';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/' + ARMCompiler;
       WorkBuffer:=WorkBuffer + ' INSTALL_BASEDIR=' + StripTrailingSlash(CompilerPath);
       WorkBuffer:=WorkBuffer + ' INSTALL_UNITDIR=' + StripTrailingSlash(CompilerPath) + '/units/armv6-ultibo/rtl' + LineEnd;

       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'IF %errorlevel% NEQ 0 GOTO Error' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo .' + LineEnd;

       Inc(Progress);
       WorkBuffer:=WorkBuffer + 'echo ' + MarkerProgress + IntToStr(Progress * Percent) + LineEnd;

       WorkBuffer:=WorkBuffer + '' + LineEnd;
      end;

     {Check Packages}
     if BuildPackages then
      begin
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo .' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo Building ARMv6 packages' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo =======================' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo .' + LineEnd;

       {RTL Clean (To remove units from \rtl\units)}
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'make rtl_clean CROSSINSTALL=1 OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv6';
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/' + ARMCompiler;
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV6 -CfVFPV2 -CIARM -CaEABIHF -OoFASTMATH"';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/' + ARMCompiler + LineEnd;

       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'IF %errorlevel% NEQ 0 GOTO Error' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo .' + LineEnd;

       Inc(Progress);
       WorkBuffer:=WorkBuffer + 'echo ' + MarkerProgress + IntToStr(Progress * Percent) + LineEnd;

       {Packages Clean}
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'make packages_clean CROSSINSTALL=1 OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv6';
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/' + ARMCompiler;
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV6 -CfVFPV2 -CIARM -CaEABIHF -OoFASTMATH"';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/' + ARMCompiler + LineEnd;

       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'IF %errorlevel% NEQ 0 GOTO Error' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo .' + LineEnd;

       Inc(Progress);
       WorkBuffer:=WorkBuffer + 'echo ' + MarkerProgress + IntToStr(Progress * Percent) + LineEnd;

       {Packages}
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'make packages OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv6';
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/' + ARMCompiler;
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV6 -CfVFPV2 -CIARM -CaEABIHF -OoFASTMATH -Fu' + StripTrailingSlash(CompilerPath) + '\units\armv6-ultibo\rtl"';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/' + ARMCompiler + LineEnd;

       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'IF %errorlevel% NEQ 0 GOTO Error' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo .' + LineEnd;

       Inc(Progress);
       WorkBuffer:=WorkBuffer + 'echo ' + MarkerProgress + IntToStr(Progress * Percent) + LineEnd;

       {Packages Install}
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'make packages_install CROSSINSTALL=1';
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/' + ARMCompiler;
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV6 -CfVFPV2 -CIARM -CaEABIHF -OoFASTMATH" OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv6';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/' + ARMCompiler;
       WorkBuffer:=WorkBuffer + ' INSTALL_BASEDIR=' + StripTrailingSlash(CompilerPath);
       WorkBuffer:=WorkBuffer + ' INSTALL_UNITDIR=' + StripTrailingSlash(CompilerPath) + '/units/armv6-ultibo/packages' + LineEnd;

       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'IF %errorlevel% NEQ 0 GOTO Error' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo .' + LineEnd;

       Inc(Progress);
       WorkBuffer:=WorkBuffer + 'echo ' + MarkerProgress + IntToStr(Progress * Percent) + LineEnd;

       WorkBuffer:=WorkBuffer + '' + LineEnd;
      end;
    end;

   {Check ARMv7}
   if PlatformARMv7 then
    begin
     {Check RTL}
     if BuildRTL then
      begin
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo .' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo Building ARMv7 RTL' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo ==================' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo .' + LineEnd;

       {RTL Clean}
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'make rtl_clean CROSSINSTALL=1 OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv7a';
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/' + ARMCompiler;
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV7A -CfVFPV3 -CIARM -CaEABIHF -OoFASTMATH"';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/' + ARMCompiler + LineEnd;

       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'IF %errorlevel% NEQ 0 GOTO Error' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo .' + LineEnd;

       Inc(Progress);
       WorkBuffer:=WorkBuffer + 'echo ' + MarkerProgress + IntToStr(Progress * Percent) + LineEnd;

       {RTL}
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'make rtl OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv7a';
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/' + ARMCompiler;
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV7A -CfVFPV3 -CIARM -CaEABIHF -OoFASTMATH"';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/' + ARMCompiler + LineEnd;

       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'IF %errorlevel% NEQ 0 GOTO Error' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo .' + LineEnd;

       Inc(Progress);
       WorkBuffer:=WorkBuffer + 'echo ' + MarkerProgress + IntToStr(Progress * Percent) + LineEnd;

       {RTL Install}
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'make rtl_install CROSSINSTALL=1';
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/' + ARMCompiler;
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV7A -CfVFPV3 -CIARM -CaEABIHF -OoFASTMATH" OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv7a';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/' + ARMCompiler;
       WorkBuffer:=WorkBuffer + ' INSTALL_BASEDIR=' + StripTrailingSlash(CompilerPath);
       WorkBuffer:=WorkBuffer + ' INSTALL_UNITDIR=' + StripTrailingSlash(CompilerPath) + '/units/armv7-ultibo/rtl' + LineEnd;

       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'IF %errorlevel% NEQ 0 GOTO Error' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo .' + LineEnd;

       Inc(Progress);
       WorkBuffer:=WorkBuffer + 'echo ' + MarkerProgress + IntToStr(Progress * Percent) + LineEnd;

       WorkBuffer:=WorkBuffer + '' + LineEnd;
      end;

     {Check Packages}
     if BuildPackages then
      begin
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo .' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo Building ARMv7 Packages' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo =======================' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo .' + LineEnd;

       {RTL Clean (To remove units from \rtl\units)}
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'make rtl_clean CROSSINSTALL=1 OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv7a';
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/' + ARMCompiler;
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV7A -CfVFPV3 -CIARM -CaEABIHF -OoFASTMATH"';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/' + ARMCompiler + LineEnd;

       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'IF %errorlevel% NEQ 0 GOTO Error' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo .' + LineEnd;

       Inc(Progress);
       WorkBuffer:=WorkBuffer + 'echo ' + MarkerProgress + IntToStr(Progress * Percent) + LineEnd;

       {Packages Clean}
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'make packages_clean CROSSINSTALL=1 OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv7a';
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/' + ARMCompiler;
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV7A -CfVFPV3 -CIARM -CaEABIHF -OoFASTMATH"';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/' + ARMCompiler + LineEnd;

       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'IF %errorlevel% NEQ 0 GOTO Error' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo .' + LineEnd;

       Inc(Progress);
       WorkBuffer:=WorkBuffer + 'echo ' + MarkerProgress + IntToStr(Progress * Percent) + LineEnd;

       {Packages}
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'make packages OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv7a';
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/' + ARMCompiler;
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV7A -CfVFPV3 -CIARM -CaEABIHF -OoFASTMATH -Fu' + StripTrailingSlash(CompilerPath) + '\units\armv7-ultibo\rtl"';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/' + ARMCompiler + LineEnd;

       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'IF %errorlevel% NEQ 0 GOTO Error' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo .' + LineEnd;

       Inc(Progress);
       WorkBuffer:=WorkBuffer + 'echo ' + MarkerProgress + IntToStr(Progress * Percent) + LineEnd;

       {Packages Install}
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'make packages_install CROSSINSTALL=1';
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/' + ARMCompiler;
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV7A -CfVFPV3 -CIARM -CaEABIHF -OoFASTMATH" OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv7a';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/' + ARMCompiler;
       WorkBuffer:=WorkBuffer + ' INSTALL_BASEDIR=' + StripTrailingSlash(CompilerPath);
       WorkBuffer:=WorkBuffer + ' INSTALL_UNITDIR=' + StripTrailingSlash(CompilerPath) + '/units/armv7-ultibo/packages' + LineEnd;

       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'IF %errorlevel% NEQ 0 GOTO Error' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo .' + LineEnd;

       Inc(Progress);
       WorkBuffer:=WorkBuffer + 'echo ' + MarkerProgress + IntToStr(Progress * Percent) + LineEnd;

       WorkBuffer:=WorkBuffer + '' + LineEnd;
      end;
    end;

   {Check ARMv8}
   if PlatformARMv8 then
    begin
     {Check RTL}
     if BuildRTL then
      begin
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo .' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo Building ARMv8 RTL' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo ==================' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo .' + LineEnd;

       {RTL Clean}
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'make rtl_clean CROSSINSTALL=1 OS_TARGET=ultibo CPU_TARGET=aarch64 SUBARCH=armv8';
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/' + AARCH64Compiler;
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV8 -CfVFP -OoFASTMATH"';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/' + AARCH64Compiler + LineEnd;

       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'IF %errorlevel% NEQ 0 GOTO Error' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo .' + LineEnd;

       Inc(Progress);
       WorkBuffer:=WorkBuffer + 'echo ' + MarkerProgress + IntToStr(Progress * Percent) + LineEnd;

       {RTL}
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'make rtl OS_TARGET=ultibo CPU_TARGET=aarch64 SUBARCH=armv8';
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/' + AARCH64Compiler;
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV8 -CfVFP -OoFASTMATH"';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/' + AARCH64Compiler + LineEnd;

       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'IF %errorlevel% NEQ 0 GOTO Error' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo .' + LineEnd;

       Inc(Progress);
       WorkBuffer:=WorkBuffer + 'echo ' + MarkerProgress + IntToStr(Progress * Percent) + LineEnd;

       {RTL Install}
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'make rtl_install CROSSINSTALL=1';
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/' + AARCH64Compiler;
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV8 -CfVFP -OoFASTMATH" OS_TARGET=ultibo CPU_TARGET=aarch64 SUBARCH=armv8';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/' + AARCH64Compiler;
       WorkBuffer:=WorkBuffer + ' INSTALL_BASEDIR=' + StripTrailingSlash(CompilerPath);
       WorkBuffer:=WorkBuffer + ' INSTALL_UNITDIR=' + StripTrailingSlash(CompilerPath) + '/units/armv8-ultibo/rtl' + LineEnd;

       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'IF %errorlevel% NEQ 0 GOTO Error' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo .' + LineEnd;

       Inc(Progress);
       WorkBuffer:=WorkBuffer + 'echo ' + MarkerProgress + IntToStr(Progress * Percent) + LineEnd;

       WorkBuffer:=WorkBuffer + '' + LineEnd;
      end;

     {Check Packages}
     if BuildPackages then
      begin
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo .' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo Building ARMv8 Packages' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo =======================' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo .' + LineEnd;

       {RTL Clean (To remove units from \rtl\units)}
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'make rtl_clean CROSSINSTALL=1 OS_TARGET=ultibo CPU_TARGET=aarch64 SUBARCH=armv8';
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/' + AARCH64Compiler;
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV8 -CfVFP -OoFASTMATH"';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/' + AARCH64Compiler + LineEnd;

       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'IF %errorlevel% NEQ 0 GOTO Error' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo .' + LineEnd;

       Inc(Progress);
       WorkBuffer:=WorkBuffer + 'echo ' + MarkerProgress + IntToStr(Progress * Percent) + LineEnd;

       {Packages Clean}
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'make packages_clean CROSSINSTALL=1 OS_TARGET=ultibo CPU_TARGET=aarch64 SUBARCH=armv8';
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/' + AARCH64Compiler;
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV8 -CfVFP -OoFASTMATH"';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/' + AARCH64Compiler + LineEnd;

       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'IF %errorlevel% NEQ 0 GOTO Error' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo .' + LineEnd;

       Inc(Progress);
       WorkBuffer:=WorkBuffer + 'echo ' + MarkerProgress + IntToStr(Progress * Percent) + LineEnd;

       {Packages}
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'make packages OS_TARGET=ultibo CPU_TARGET=aarch64 SUBARCH=armv8';
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/' + AARCH64Compiler;
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV8 -CfVFP -OoFASTMATH -Fu' + StripTrailingSlash(CompilerPath) + '\units\armv8-ultibo\rtl"';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/' + AARCH64Compiler + LineEnd;

       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'IF %errorlevel% NEQ 0 GOTO Error' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo .' + LineEnd;

       Inc(Progress);
       WorkBuffer:=WorkBuffer + 'echo ' + MarkerProgress + IntToStr(Progress * Percent) + LineEnd;

       {Packages Install}
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'make packages_install CROSSINSTALL=1';
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/' + AARCH64Compiler;
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV8 -CfVFP -OoFASTMATH" OS_TARGET=ultibo CPU_TARGET=aarch64 SUBARCH=armv8';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/' + AARCH64Compiler;
       WorkBuffer:=WorkBuffer + ' INSTALL_BASEDIR=' + StripTrailingSlash(CompilerPath);
       WorkBuffer:=WorkBuffer + ' INSTALL_UNITDIR=' + StripTrailingSlash(CompilerPath) + '/units/armv8-ultibo/packages' + LineEnd;

       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'IF %errorlevel% NEQ 0 GOTO Error' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo .' + LineEnd;

       Inc(Progress);
       WorkBuffer:=WorkBuffer + 'echo ' + MarkerProgress + IntToStr(Progress * Percent) + LineEnd;

       WorkBuffer:=WorkBuffer + '' + LineEnd;
      end;
    end;

   WorkBuffer:=WorkBuffer + 'echo ' + MarkerProgress + IntToStr(100) + LineEnd;

   {Add Footer}
   WorkBuffer:=WorkBuffer + '' + LineEnd;
   WorkBuffer:=WorkBuffer + 'echo .' + LineEnd;
   WorkBuffer:=WorkBuffer + 'echo Build RTL completed successfully' + LineEnd;
   WorkBuffer:=WorkBuffer + 'GOTO End' + LineEnd;

   {Add Error}
   WorkBuffer:=WorkBuffer + '' + LineEnd;
   WorkBuffer:=WorkBuffer + ':Error' + LineEnd;
   WorkBuffer:=WorkBuffer + 'echo .' + LineEnd;
   WorkBuffer:=WorkBuffer + 'echo Build RTL failed, see above for errors' + LineEnd;

   {Add End}
   WorkBuffer:=WorkBuffer + '' + LineEnd;
   WorkBuffer:=WorkBuffer + ':End' + LineEnd;
   WorkBuffer:=WorkBuffer + 'echo .' + LineEnd;
   WorkBuffer:=WorkBuffer + 'echo =======================End of Build Script=======================' + LineEnd;
   WorkBuffer:=WorkBuffer + 'echo ' + MarkerEnd + LineEnd;
   WorkBuffer:=WorkBuffer + '' + LineEnd;
   {$ENDIF}
   {$IFDEF LINUX}
   {Add Header}
   WorkBuffer:='';
   WorkBuffer:=WorkBuffer + '#!/bin/bash' + LineEnd;
   WorkBuffer:=WorkBuffer + '' + LineEnd;
   WorkBuffer:=WorkBuffer + 'export PATH=' + PathPrefix + StripTrailingSlash(CompilerPath) + '/bin:$PATH' + LineEnd;
   WorkBuffer:=WorkBuffer + '' + LineEnd;

   {Add Error}
   WorkBuffer:=WorkBuffer + 'function exitFailure() {' + LineEnd;
   WorkBuffer:=WorkBuffer + '    if [ $? -ne 0 ]; then' + LineEnd;
   WorkBuffer:=WorkBuffer + '        echo "."' + LineEnd;
   WorkBuffer:=WorkBuffer + '        echo "Build RTL failed, see above for errors"' + LineEnd;
   WorkBuffer:=WorkBuffer + '        echo "."' + LineEnd;
   WorkBuffer:=WorkBuffer + '        echo "=======================End of Build Script======================="' + LineEnd;
   WorkBuffer:=WorkBuffer + '        echo "' + MarkerEnd + '"' + LineEnd;
   WorkBuffer:=WorkBuffer + '        exit 1' + LineEnd;
   WorkBuffer:=WorkBuffer + '    fi' + LineEnd;
   WorkBuffer:=WorkBuffer + '}' + LineEnd;

   {Add Start}
   WorkBuffer:=WorkBuffer + 'echo "."' + LineEnd;
   WorkBuffer:=WorkBuffer + 'echo "======================Start of Build Script======================"' + LineEnd;

   {Check ARMv6}
   if PlatformARMv6 then
    begin
     {Check RTL}
     if BuildRTL then
      begin
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo "."' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo "Building ARMv6 RTL"' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo "=================="' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo "."' + LineEnd;

       {RTL Clean}
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'make rtl_clean CROSSINSTALL=1 OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv6 BINUTILSPREFIX=arm-none-eabi-';
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/' + ARMCompiler;
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV6 -CfVFPV2 -CIARM -CaEABIHF -OoFASTMATH"';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/' + ARMCompiler + LineEnd;
       WorkBuffer:=WorkBuffer + 'exitFailure' + LineEnd;

       Inc(Progress);
       WorkBuffer:=WorkBuffer + 'echo "' + MarkerProgress + IntToStr(Progress * Percent) + '"' + LineEnd;

       {RTL}
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'make rtl OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv6 BINUTILSPREFIX=arm-none-eabi-';
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/' + ARMCompiler;
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV6 -CfVFPV2 -CIARM -CaEABIHF -OoFASTMATH"';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/' + ARMCompiler + LineEnd;
       WorkBuffer:=WorkBuffer + 'exitFailure' + LineEnd;

       Inc(Progress);
       WorkBuffer:=WorkBuffer + 'echo "' + MarkerProgress + IntToStr(Progress * Percent) + '"' + LineEnd;

       {RTL Install}
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'make rtl_install CROSSINSTALL=1 BINUTILSPREFIX=arm-none-eabi-';
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/' + ARMCompiler;
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV6 -CfVFPV2 -CIARM -CaEABIHF -OoFASTMATH" OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv6';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/' + ARMCompiler;
       WorkBuffer:=WorkBuffer + ' INSTALL_PREFIX=' + StripTrailingSlash(CompilerPath);
       WorkBuffer:=WorkBuffer + ' INSTALL_UNITDIR=' + StripTrailingSlash(CompilerPath) + '/units/armv6-ultibo/rtl' + LineEnd;
       WorkBuffer:=WorkBuffer + 'exitFailure' + LineEnd;

       Inc(Progress);
       WorkBuffer:=WorkBuffer + 'echo "' + MarkerProgress + IntToStr(Progress * Percent) + '"' + LineEnd;

       WorkBuffer:=WorkBuffer + '' + LineEnd;
      end;

     {Check Packages}
     if BuildPackages then
      begin
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo "."' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo "Building ARMv6 packages"' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo "======================="' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo "."' + LineEnd;

       {RTL Clean (To remove units from /rtl/units)}
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'make rtl_clean CROSSINSTALL=1 OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv6 BINUTILSPREFIX=arm-none-eabi-';
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/' + ARMCompiler;
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV6 -CfVFPV2 -CIARM -CaEABIHF -OoFASTMATH"';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/' + ARMCompiler + LineEnd;
       WorkBuffer:=WorkBuffer + 'exitFailure' + LineEnd;

       Inc(Progress);
       WorkBuffer:=WorkBuffer + 'echo "' + MarkerProgress + IntToStr(Progress * Percent) + '"' + LineEnd;

       {Packages Clean}
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'make packages_clean CROSSINSTALL=1 OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv6 BINUTILSPREFIX=arm-none-eabi-';
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/' + ARMCompiler;
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV6 -CfVFPV2 -CIARM -CaEABIHF -OoFASTMATH"';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/' + ARMCompiler + LineEnd;
       WorkBuffer:=WorkBuffer + 'exitFailure' + LineEnd;

       Inc(Progress);
       WorkBuffer:=WorkBuffer + 'echo "' + MarkerProgress + IntToStr(Progress * Percent) + '"' + LineEnd;

       {Packages}
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'make packages OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv6 BINUTILSPREFIX=arm-none-eabi-';
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/' + ARMCompiler;
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV6 -CfVFPV2 -CIARM -CaEABIHF -OoFASTMATH -Fu' + StripTrailingSlash(CompilerPath) + '/units/armv6-ultibo/rtl"';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/' + ARMCompiler + LineEnd;
       WorkBuffer:=WorkBuffer + 'exitFailure' + LineEnd;

       Inc(Progress);
       WorkBuffer:=WorkBuffer + 'echo "' + MarkerProgress + IntToStr(Progress * Percent) + '"' + LineEnd;

       {Packages Install}
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'make packages_install CROSSINSTALL=1 BINUTILSPREFIX=arm-none-eabi-';
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/' + ARMCompiler;
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV6 -CfVFPV2 -CIARM -CaEABIHF -OoFASTMATH" OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv6';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/' + ARMCompiler;
       WorkBuffer:=WorkBuffer + ' INSTALL_PREFIX=' + StripTrailingSlash(CompilerPath);
       WorkBuffer:=WorkBuffer + ' INSTALL_UNITDIR=' + StripTrailingSlash(CompilerPath) + '/units/armv6-ultibo/packages' + LineEnd;
       WorkBuffer:=WorkBuffer + 'exitFailure' + LineEnd;

       Inc(Progress);
       WorkBuffer:=WorkBuffer + 'echo "' + MarkerProgress + IntToStr(Progress * Percent) + '"' + LineEnd;

       WorkBuffer:=WorkBuffer + '' + LineEnd;
      end;
    end;

   {Check ARMv7}
   if PlatformARMv7 then
    begin
     {Check RTL}
     if BuildRTL then
      begin
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo "."' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo "Building ARMv7 RTL"' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo "=================="' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo "."' + LineEnd;

       {RTL Clean}
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'make rtl_clean CROSSINSTALL=1 OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv7a BINUTILSPREFIX=arm-none-eabi-';
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/' + ARMCompiler;
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV7A -CfVFPV3 -CIARM -CaEABIHF -OoFASTMATH"';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/' + ARMCompiler + LineEnd;
       WorkBuffer:=WorkBuffer + 'exitFailure' + LineEnd;

       Inc(Progress);
       WorkBuffer:=WorkBuffer + 'echo "' + MarkerProgress + IntToStr(Progress * Percent) + '"' + LineEnd;

       {RTL}
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'make rtl OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv7a BINUTILSPREFIX=arm-none-eabi-';
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/' + ARMCompiler;
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV7A -CfVFPV3 -CIARM -CaEABIHF -OoFASTMATH"';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/' + ARMCompiler + LineEnd;
       WorkBuffer:=WorkBuffer + 'exitFailure' + LineEnd;

       Inc(Progress);
       WorkBuffer:=WorkBuffer + 'echo "' + MarkerProgress + IntToStr(Progress * Percent) + '"' + LineEnd;

       {RTL Install}
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'make rtl_install CROSSINSTALL=1 BINUTILSPREFIX=arm-none-eabi-';
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/' + ARMCompiler;
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV7A -CfVFPV3 -CIARM -CaEABIHF -OoFASTMATH" OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv7a';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/' + ARMCompiler;
       WorkBuffer:=WorkBuffer + ' INSTALL_PREFIX=' + StripTrailingSlash(CompilerPath);
       WorkBuffer:=WorkBuffer + ' INSTALL_UNITDIR=' + StripTrailingSlash(CompilerPath) + '/units/armv7-ultibo/rtl' + LineEnd;
       WorkBuffer:=WorkBuffer + 'exitFailure' + LineEnd;

       Inc(Progress);
       WorkBuffer:=WorkBuffer + 'echo "' + MarkerProgress + IntToStr(Progress * Percent) + '"' + LineEnd;

       WorkBuffer:=WorkBuffer + '' + LineEnd;
      end;

     {Check Packages}
     if BuildPackages then
      begin
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo "."' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo "Building ARMv7 Packages"' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo "======================="' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo "."' + LineEnd;

       {RTL Clean (To remove units from /rtl/units)}
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'make rtl_clean CROSSINSTALL=1 OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv7a BINUTILSPREFIX=arm-none-eabi-';
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/' + ARMCompiler;
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV7A -CfVFPV3 -CIARM -CaEABIHF -OoFASTMATH"';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/' + ARMCompiler + LineEnd;
       WorkBuffer:=WorkBuffer + 'exitFailure' + LineEnd;

       Inc(Progress);
       WorkBuffer:=WorkBuffer + 'echo "' + MarkerProgress + IntToStr(Progress * Percent) + '"' + LineEnd;

       {Packages Clean}
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'make packages_clean CROSSINSTALL=1 OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv7a BINUTILSPREFIX=arm-none-eabi-';
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/' + ARMCompiler;
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV7A -CfVFPV3 -CIARM -CaEABIHF -OoFASTMATH"';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/' + ARMCompiler + LineEnd;
       WorkBuffer:=WorkBuffer + 'exitFailure' + LineEnd;

       Inc(Progress);
       WorkBuffer:=WorkBuffer + 'echo "' + MarkerProgress + IntToStr(Progress * Percent) + '"' + LineEnd;

       {Packages}
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'make packages OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv7a BINUTILSPREFIX=arm-none-eabi-';
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/' + ARMCompiler;
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV7A -CfVFPV3 -CIARM -CaEABIHF -OoFASTMATH -Fu' + StripTrailingSlash(CompilerPath) + '/units/armv7-ultibo/rtl"';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/' + ARMCompiler + LineEnd;
       WorkBuffer:=WorkBuffer + 'exitFailure' + LineEnd;

       Inc(Progress);
       WorkBuffer:=WorkBuffer + 'echo "' + MarkerProgress + IntToStr(Progress * Percent) + '"' + LineEnd;

       {Packages Install}
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'make packages_install CROSSINSTALL=1 BINUTILSPREFIX=arm-none-eabi-';
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/' + ARMCompiler;
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV7A -CfVFPV3 -CIARM -CaEABIHF -OoFASTMATH" OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv7a';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/' + ARMCompiler;
       WorkBuffer:=WorkBuffer + ' INSTALL_PREFIX=' + StripTrailingSlash(CompilerPath);
       WorkBuffer:=WorkBuffer + ' INSTALL_UNITDIR=' + StripTrailingSlash(CompilerPath) + '/units/armv7-ultibo/packages' + LineEnd;
       WorkBuffer:=WorkBuffer + 'exitFailure' + LineEnd;

       Inc(Progress);
       WorkBuffer:=WorkBuffer + 'echo "' + MarkerProgress + IntToStr(Progress * Percent) + '"' + LineEnd;

       WorkBuffer:=WorkBuffer + '' + LineEnd;
      end;
    end;

   {Check ARMv8}
   if PlatformARMv8 then
    begin
     {Check RTL}
     if BuildRTL then
      begin
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo "."' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo "Building ARMv8 RTL"' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo "=================="' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo "."' + LineEnd;

       //To Do //BINUTILSPREFIX for AARCH64 ?

       {RTL Clean}
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'make rtl_clean CROSSINSTALL=1 OS_TARGET=ultibo CPU_TARGET=aarch64 SUBARCH=armv8';
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/' + AARCH64Compiler;
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV8 -CfVFP -OoFASTMATH"';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/' + AARCH64Compiler + LineEnd;
       WorkBuffer:=WorkBuffer + 'exitFailure' + LineEnd;

       Inc(Progress);
       WorkBuffer:=WorkBuffer + 'echo "' + MarkerProgress + IntToStr(Progress * Percent) + '"' + LineEnd;

       {RTL}
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'make rtl OS_TARGET=ultibo CPU_TARGET=aarch64 SUBARCH=armv8';
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/' + AARCH64Compiler;
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV8 -CfVFP -OoFASTMATH"';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/' + AARCH64Compiler + LineEnd;
       WorkBuffer:=WorkBuffer + 'exitFailure' + LineEnd;

       Inc(Progress);
       WorkBuffer:=WorkBuffer + 'echo "' + MarkerProgress + IntToStr(Progress * Percent) + '"' + LineEnd;

       {RTL Install}
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'make rtl_install CROSSINSTALL=1';
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/' + AARCH64Compiler;
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV8 -CfVFP -OoFASTMATH" OS_TARGET=ultibo CPU_TARGET=aarch64 SUBARCH=armv8';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/' + AARCH64Compiler;
       WorkBuffer:=WorkBuffer + ' INSTALL_PREFIX=' + StripTrailingSlash(CompilerPath);
       WorkBuffer:=WorkBuffer + ' INSTALL_UNITDIR=' + StripTrailingSlash(CompilerPath) + '/units/armv8-ultibo/rtl' + LineEnd;
       WorkBuffer:=WorkBuffer + 'exitFailure' + LineEnd;

       Inc(Progress);
       WorkBuffer:=WorkBuffer + 'echo "' + MarkerProgress + IntToStr(Progress * Percent) + '"' + LineEnd;

       WorkBuffer:=WorkBuffer + '' + LineEnd;
      end;

     {Check Packages}
     if BuildPackages then
      begin
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo "."' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo "Building ARMv8 Packages"' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo "======================="' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo "."' + LineEnd;

       //To Do //BINUTILSPREFIX for AARCH64 ?

       {RTL Clean (To remove units from /rtl/units)}
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'make rtl_clean CROSSINSTALL=1 OS_TARGET=ultibo CPU_TARGET=aarch64 SUBARCH=armv8';
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/' + AARCH64Compiler;
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV8 -CfVFP -OoFASTMATH"';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/' + AARCH64Compiler + LineEnd;
       WorkBuffer:=WorkBuffer + 'exitFailure' + LineEnd;

       Inc(Progress);
       WorkBuffer:=WorkBuffer + 'echo "' + MarkerProgress + IntToStr(Progress * Percent) + '"' + LineEnd;

       {Packages Clean}
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'make packages_clean CROSSINSTALL=1 OS_TARGET=ultibo CPU_TARGET=aarch64 SUBARCH=armv8';
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/' + AARCH64Compiler;
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV8 -CfVFP -OoFASTMATH"';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/' + AARCH64Compiler + LineEnd;
       WorkBuffer:=WorkBuffer + 'exitFailure' + LineEnd;

       Inc(Progress);
       WorkBuffer:=WorkBuffer + 'echo "' + MarkerProgress + IntToStr(Progress * Percent) + '"' + LineEnd;

       {Packages}
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'make packages OS_TARGET=ultibo CPU_TARGET=aarch64 SUBARCH=armv8';
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/' + AARCH64Compiler;
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV8 -CfVFP -OoFASTMATH -Fu' + StripTrailingSlash(CompilerPath) + '/units/armv8-ultibo/rtl"';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/' + AARCH64Compiler + LineEnd;
       WorkBuffer:=WorkBuffer + 'exitFailure' + LineEnd;

       Inc(Progress);
       WorkBuffer:=WorkBuffer + 'echo "' + MarkerProgress + IntToStr(Progress * Percent) + '"' + LineEnd;

       {Packages Install}
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'make packages_install CROSSINSTALL=1';
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/' + AARCH64Compiler;
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV8 -CfVFP -OoFASTMATH" OS_TARGET=ultibo CPU_TARGET=aarch64 SUBARCH=armv8';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/' + AARCH64Compiler;
       WorkBuffer:=WorkBuffer + ' INSTALL_PREFIX=' + StripTrailingSlash(CompilerPath);
       WorkBuffer:=WorkBuffer + ' INSTALL_UNITDIR=' + StripTrailingSlash(CompilerPath) + '/units/armv8-ultibo/packages' + LineEnd;
       WorkBuffer:=WorkBuffer + 'exitFailure' + LineEnd;

       Inc(Progress);
       WorkBuffer:=WorkBuffer + 'echo "' + MarkerProgress + IntToStr(Progress * Percent) + '"' + LineEnd;

       WorkBuffer:=WorkBuffer + '' + LineEnd;
      end;
    end;

   WorkBuffer:=WorkBuffer + 'echo "' + MarkerProgress + IntToStr(100) + '"' + LineEnd;

   {Add Footer}
   WorkBuffer:=WorkBuffer + '' + LineEnd;
   WorkBuffer:=WorkBuffer + 'echo "."' + LineEnd;
   WorkBuffer:=WorkBuffer + 'echo "Build RTL completed successfully"' + LineEnd;

   {Add End}
   WorkBuffer:=WorkBuffer + '' + LineEnd;
   WorkBuffer:=WorkBuffer + 'echo "."' + LineEnd;
   WorkBuffer:=WorkBuffer + 'echo "=======================End of Build Script======================="' + LineEnd;
   WorkBuffer:=WorkBuffer + 'echo "' + MarkerEnd + '"' + LineEnd;
   WorkBuffer:=WorkBuffer + '' + LineEnd;
   {$ENDIF}

   {Set Size}
   FileStream.Size:=Length(WorkBuffer);

   {Write File}
   FileStream.Position:=0;
   FileStream.WriteBuffer(PChar(WorkBuffer)^,Length(WorkBuffer));

   Result:=True;
  finally
   FileStream.Free;

   {$IFDEF LINUX}
   if Result then
    begin
     FpChmod(Filename,S_IRWXU or S_IRGRP or S_IXGRP or S_IROTH or S_IXOTH);
    end;
   {$ENDIF}
  end;
 except
  on E: Exception do
   begin
    mmoMain.Lines.Add(' Error: Exception creating RTL build script - Message: ' + E.Message);
   end;
 end;
end;

{==============================================================================}

function TfrmMain.ExecuteBuildFile:Boolean;
var
 Filename:String;
 {$IFDEF LINUX}
 Parameters:TStringList;
 {$ENDIF}
begin
 {}
 Result:=False;
 try
  if Length(SourcePath) = 0 then Exit;
  if Length(InstallPath) = 0 then Exit;
  if Length(CompilerName) = 0 then Exit;
  if Length(CompilerPath) = 0 then Exit;
  if Length(CompilerVersion) = 0 then Exit;
  if not(BuildRTL) and not(BuildPackages) then Exit;
  if not(PlatformARMv6) and not(PlatformARMv7) and not(PlatformARMv8) then Exit;

  {Get Filename}
  Filename:=AddTrailingSlash(SourcePath) + BuildScript;
  mmoMain.Lines.Add('  Build Script is ' + Filename);
  mmoMain.Lines.Add('');

  {Check File}
  if not FileExists(Filename) then Exit;

  {Execute Process}
  {$IFDEF WINDOWS}
  if not ExecuteConsoleProcessEx('cmd.exe /c',Filename,SourcePath,MarkerEnd,MarkerProgress,mmoMain,progressMain) then Exit;
  {$ENDIF}
  {$IFDEF LINUX}
  Parameters:=TStringList.Create;
  try
   Parameters.Add('-c');
   Parameters.Add(Filename);
   if not ExecuteShellProcess('/bin/bash',Parameters,SourcePath,MarkerEnd,MarkerProgress,mmoMain,progressMain) then Exit;
  finally
   Parameters.Free;
  end;
  {$ENDIF}

  Result:=True;
 except
  on E: Exception do
   begin
    mmoMain.Lines.Add(' Error: Exception executing RTL build script - Message: ' + E.Message);
   end;
 end;
end;

{==============================================================================}
{==============================================================================}

end.
