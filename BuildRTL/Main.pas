{
Ultibo RTL Builder Tool.

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


RTL Builder
===========

 The RTL builder creates a Windows batch file which compiles the RTL and Packages
 for any of the supported architectures and displays the output in a window during
 the compile.

 While the tool is designed to determine the correct paths and locations without
 configuration it does support creating a BuildRTL.ini file in the same directory
 and setting a number of parameters to adjust the behavior.

 The format of the INI file is:

 [BuildRTL]
 PathPrefix=
 InstallPath=
 CompilerPath=
 CompilerVersion=
 SourcePath=

 BuildRTL=
 BuildPackages=

 PlatformARMv6=
 PlatformARMv7=

 A brief explanation of each parameter along with the standard default value:

 PathPrefix - A text value to prepend to the path variable in the batch file (Default: <Blank>)

 InstallPath - The path where Ultibo core is installed (Default: C:\Ultibo\Core) (Detected from the application path)

 CompilerPath - The path where the Ultibo version of FPC is installed (Default: <InstallPath>\fpc\<CompilerVersion>)

 CompilerVersion - The version of the FPC compiler (Default: 3.1.1)

 SourcePath - The path to RTL and Packages source code (Default: <CompilerPath>\source)

 BuildRTL - Enable or disable building the RTL (0=Disable / 1=Enable) (Default: 1)

 BuildPackages - Enable or disable building the Packages (0=Disable / 1=Enable) (Default: 1)

 PlatformARMv6 - Build the RTL and Packages for ARMv6 architecture (0=Disable / 1=Enable) (Default: 1)

 PlatformARMv7 - Build the RTL and Packages for ARMv7 architecture (0=Disable / 1=Enable) (Default: 1)

}

unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, IniFiles;

const
 LineEnd = Chr(13) + Chr(10); {CR LF}
 MarkerEnd = '!!!EndOfBuild!!!';

type
  TfrmMain = class(TForm)
    mmoMain: TMemo;
    sbMain: TStatusBar;
    pnlMain: TPanel;
    cmdExit: TButton;
    lblCompiler: TLabel;
    lblPlatforms: TLabel;
    cmdBuild: TButton;
    chkARMv6: TCheckBox;
    chkARMv7: TCheckBox;
    lblMain: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure cmdExitClick(Sender: TObject);
    procedure cmdBuildClick(Sender: TObject);
    procedure chkARMv6Click(Sender: TObject);
    procedure chkARMv7Click(Sender: TObject);
  private
    { Private declarations }
    PathPrefix:String;
    InstallPath:String;
    SourcePath:String;
    CompilerPath:String;
    CompilerVersion:String;

    BuildRTL:Boolean;
    BuildPackages:Boolean;

    PlatformARMv6:Boolean;
    PlatformARMv7:Boolean;
  public
    { Public declarations }
    function LoadConfig:Boolean;

    function CreateBuildFile:Boolean;
    function ExecuteBuildFile:Boolean;
  end;

var
  frmMain: TfrmMain;

function GetDosOutput(CommandLine: string; Work: string = 'C:\'): string;
procedure CaptureConsoleOutput(const ACommand, AParameters: String; AMemo: TMemo);

function ExecuteConsoleProcess(const ACommand,AParameters,AWorking:String;AMemo:TMemo):Boolean;
function ExecuteConsoleProcessEx(const ACommand,AParameters,AWorking,AMarker:String;AMemo:TMemo):Boolean;

{For details on how to capture command prompt output see:

 http://www.delphidabbler.com/tips/61

 http://stackoverflow.com/questions/9119999/getting-output-from-a-shell-dos-app-into-a-delphi-app

 See also JclSysUtils unit in the Jedi JCL

}

function AddTrailingSlash(const FilePath:String):String;
function StripTrailingSlash(const FilePath:String):String;

implementation

{$R *.DFM}

{==============================================================================}

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

function ExecuteConsoleProcessEx(const ACommand,AParameters,AWorking,AMarker:String;AMemo:TMemo):Boolean;
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
              {Check Marker}
              if Length(AMarker) <> 0 then
               begin
                if ReadData = AMarker then Break;
               end;

              {Add Output}
              AMemo.Lines.Add(ReadData);
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

 {Assume that the compiler path will be \fpc\<CompilerVersion> under the InstallPath}
 CompilerPath:=InstallPath + '\fpc\' + CompilerVersion;

 {Assume that the source path will be \source under the CompilerPath}
 SourcePath:=CompilerPath + '\source';

 BuildRTL:=True;
 BuildPackages:=True;

 PlatformARMv6:=True;
 PlatformARMv7:=True;

 LoadConfig;
end;

{==============================================================================}

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
 {}

end;

{==============================================================================}

procedure TfrmMain.FormShow(Sender: TObject);
begin
 {}
 chkARMv6.Checked:=PlatformARMv6;
 chkARMv7.Checked:=PlatformARMv7;
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

procedure TfrmMain.cmdExitClick(Sender: TObject);
begin
 {}
 Application.Terminate;
end;

{==============================================================================}

procedure TfrmMain.cmdBuildClick(Sender: TObject);
begin
 {}
 lblMain.Enabled:=False;
 lblPlatforms.Enabled:=False;
 chkARMv6.Enabled:=False;
 chkARMv7.Enabled:=False;
 cmdBuild.Enabled:=False;
 cmdExit.Enabled:=False;
 try
  {Clear Memo}
  mmoMain.Lines.Clear;

  {Add Banner}
  mmoMain.Lines.Add('Building Ultibo RTL');
  mmoMain.Lines.Add('');

  {Add Path Prefix}
  mmoMain.Lines.Add(' Path Prefix is ' + PathPrefix);
  mmoMain.Lines.Add('');
  {Add Install Path}
  mmoMain.Lines.Add(' Install Path is ' + InstallPath);
  mmoMain.Lines.Add('');
  {Add Compiler Path}
  mmoMain.Lines.Add(' Compiler Path is ' + CompilerPath);
  mmoMain.Lines.Add('');
  {Add Source Path}
  mmoMain.Lines.Add(' Source Path is ' + SourcePath);
  mmoMain.Lines.Add('');

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
  cmdBuild.Enabled:=True;
  cmdExit.Enabled:=True;
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
     {Get CompilerPath}
     CompilerPath:=IniFile.ReadString(Section,'CompilerPath',CompilerPath);
     {Get CompilerVersion}
     CompilerVersion:=IniFile.ReadString(Section,'CompilerVersion',CompilerVersion);
     {Get SourcePath}
     SourcePath:=IniFile.ReadString(Section,'SourcePath',SourcePath);

     {Get BuildRTL}
     BuildRTL:=IniFile.ReadBool(Section,'BuildRTL',BuildRTL);
     {Get BuildPackages}
     BuildPackages:=IniFile.ReadBool(Section,'BuildPackages',BuildPackages);

     {Get PlatformARMv6}
     PlatformARMv6:=IniFile.ReadBool(Section,'PlatformARMv6',PlatformARMv6);
     {Get PlatformARMv7}
     PlatformARMv7:=IniFile.ReadBool(Section,'PlatformARMv7',PlatformARMv7);
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

function TfrmMain.CreateBuildFile:Boolean;
var
 Filename:String;
 WorkBuffer:String;
 FileStream:TFileStream;
begin
 {}
 Result:=False;
 try
  if Length(SourcePath) = 0 then Exit;
  if Length(InstallPath) = 0 then Exit;
  if Length(CompilerPath) = 0 then Exit;
  if Length(CompilerVersion) = 0 then Exit;
  if not(BuildRTL) and not(BuildPackages) then Exit;
  if not(PlatformARMv6) and not(PlatformARMv7) then Exit;

  {Get Filename}
  Filename:=AddTrailingSlash(SourcePath) + '__buildrtl.bat';
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
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/ppcrossarm.exe';
       //WorkBuffer:=WorkBuffer + ' CROSSBINDIR=' + StripTrailingSlash(CompilerPath) + '/bin/win32-arm-none'; //Not required due to arm-ultibo- prefix on as/ld etc
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV6 -CfVFPV2 -CIARM -OoFASTMATH"';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/ppcrossarm.exe' + LineEnd;

       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'IF %errorlevel% NEQ 0 GOTO Error' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo .' + LineEnd;

       {RTL}
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'make rtl OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv6';
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/ppcrossarm.exe';
       //WorkBuffer:=WorkBuffer + ' CROSSBINDIR=' + StripTrailingSlash(CompilerPath) + '/bin/win32-arm-none'; //Not required due to arm-ultibo- prefix on as/ld etc
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV6 -CfVFPV2 -CIARM -OoFASTMATH"';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/ppcrossarm.exe' + LineEnd;

       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'IF %errorlevel% NEQ 0 GOTO Error' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo .' + LineEnd;

       {RTL Install}
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'make rtl_install CROSSINSTALL=1';
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/ppcrossarm.exe';
       //WorkBuffer:=WorkBuffer + ' CROSSBINDIR=' + StripTrailingSlash(CompilerPath) + '/bin/win32-arm-none'; //Not required due to arm-ultibo- prefix on as/ld etc
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV6 -CfVFPV2 -CIARM -OoFASTMATH" OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv6';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/ppcrossarm.exe';
       WorkBuffer:=WorkBuffer + ' INSTALL_BASEDIR=' + StripTrailingSlash(CompilerPath);
       WorkBuffer:=WorkBuffer + ' INSTALL_UNITDIR=' + StripTrailingSlash(CompilerPath) + '/units/armv6-ultibo/rtl' + LineEnd;

       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'IF %errorlevel% NEQ 0 GOTO Error' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo .' + LineEnd;

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
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/ppcrossarm.exe';
       //WorkBuffer:=WorkBuffer + ' CROSSBINDIR=' + StripTrailingSlash(CompilerPath) + '/bin/win32-arm-none'; //Not required due to arm-ultibo- prefix on as/ld etc
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV6 -CfVFPV2 -CIARM -OoFASTMATH"';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/ppcrossarm.exe' + LineEnd;

       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'IF %errorlevel% NEQ 0 GOTO Error' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo .' + LineEnd;

       {Packages Clean}
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'make packages_clean CROSSINSTALL=1 OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv6';
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/ppcrossarm.exe';
       //WorkBuffer:=WorkBuffer + ' CROSSBINDIR=' + StripTrailingSlash(CompilerPath) + '/bin/win32-arm-none'; //Not required due to arm-ultibo- prefix on as/ld etc
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV6 -CfVFPV2 -CIARM -OoFASTMATH"';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/ppcrossarm.exe' + LineEnd;

       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'IF %errorlevel% NEQ 0 GOTO Error' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo .' + LineEnd;

       {Packages}
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'make packages OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv6';
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/ppcrossarm.exe';
       //WorkBuffer:=WorkBuffer + ' CROSSBINDIR=' + StripTrailingSlash(CompilerPath) + '/bin/win32-arm-none'; //Not required due to arm-ultibo- prefix on as/ld etc
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV6 -CfVFPV2 -CIARM -OoFASTMATH -Fu' + StripTrailingSlash(CompilerPath) + '\units\armv6-ultibo\rtl"';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/ppcrossarm.exe' + LineEnd;

       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'IF %errorlevel% NEQ 0 GOTO Error' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo .' + LineEnd;

       {Packages Install}
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'make packages_install CROSSINSTALL=1';
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/ppcrossarm.exe';
       //WorkBuffer:=WorkBuffer + ' CROSSBINDIR=' + StripTrailingSlash(CompilerPath) + '/bin/win32-arm-none'; //Not required due to arm-ultibo- prefix on as/ld etc
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV6 -CfVFPV2 -CIARM -OoFASTMATH" OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv6';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/ppcrossarm.exe';
       WorkBuffer:=WorkBuffer + ' INSTALL_BASEDIR=' + StripTrailingSlash(CompilerPath);
       WorkBuffer:=WorkBuffer + ' INSTALL_UNITDIR=' + StripTrailingSlash(CompilerPath) + '/units/armv6-ultibo/packages' + LineEnd;

       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'IF %errorlevel% NEQ 0 GOTO Error' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo .' + LineEnd;

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
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/ppcrossarm.exe';
       //WorkBuffer:=WorkBuffer + ' CROSSBINDIR=' + StripTrailingSlash(CompilerPath) + '/bin/win32-arm-none'; //Not required due to arm-ultibo- prefix on as/ld etc
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV7A -CfVFPV3 -CIARM -OoFASTMATH"';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/ppcrossarm.exe' + LineEnd;

       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'IF %errorlevel% NEQ 0 GOTO Error' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo .' + LineEnd;

       {RTL}
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'make rtl OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv7a';
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/ppcrossarm.exe';
       //WorkBuffer:=WorkBuffer + ' CROSSBINDIR=' + StripTrailingSlash(CompilerPath) + '/bin/win32-arm-none'; //Not required due to arm-ultibo- prefix on as/ld etc
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV7A -CfVFPV3 -CIARM -OoFASTMATH"';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/ppcrossarm.exe' + LineEnd;

       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'IF %errorlevel% NEQ 0 GOTO Error' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo .' + LineEnd;

       {RTL Install}
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'make rtl_install CROSSINSTALL=1';
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/ppcrossarm.exe';
       //WorkBuffer:=WorkBuffer + ' CROSSBINDIR=' + StripTrailingSlash(CompilerPath) + '/bin/win32-arm-none'; //Not required due to arm-ultibo- prefix on as/ld etc
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV7A -CfVFPV3 -CIARM -OoFASTMATH" OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv7a';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/ppcrossarm.exe';
       WorkBuffer:=WorkBuffer + ' INSTALL_BASEDIR=' + StripTrailingSlash(CompilerPath);
       WorkBuffer:=WorkBuffer + ' INSTALL_UNITDIR=' + StripTrailingSlash(CompilerPath) + '/units/armv7-ultibo/rtl' + LineEnd;

       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'IF %errorlevel% NEQ 0 GOTO Error' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo .' + LineEnd;

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
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/ppcrossarm.exe';
       //WorkBuffer:=WorkBuffer + ' CROSSBINDIR=' + StripTrailingSlash(CompilerPath) + '/bin/win32-arm-none'; //Not required due to arm-ultibo- prefix on as/ld etc
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV7A -CfVFPV3 -CIARM -OoFASTMATH"';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/ppcrossarm.exe' + LineEnd;

       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'IF %errorlevel% NEQ 0 GOTO Error' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo .' + LineEnd;

       {Packages Clean}
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'make packages_clean CROSSINSTALL=1 OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv7a';
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/ppcrossarm.exe';
       //WorkBuffer:=WorkBuffer + ' CROSSBINDIR=' + StripTrailingSlash(CompilerPath) + '/bin/win32-arm-none'; //Not required due to arm-ultibo- prefix on as/ld etc
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV7A -CfVFPV3 -CIARM -OoFASTMATH"';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/ppcrossarm.exe' + LineEnd;

       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'IF %errorlevel% NEQ 0 GOTO Error' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo .' + LineEnd;

       {Packages}
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'make packages OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv7a';
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/ppcrossarm.exe';
       //WorkBuffer:=WorkBuffer + ' CROSSBINDIR=' + StripTrailingSlash(CompilerPath) + '/bin/win32-arm-none'; //Not required due to arm-ultibo- prefix on as/ld etc
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV7A -CfVFPV3 -CIARM -OoFASTMATH -Fu' + StripTrailingSlash(CompilerPath) + '\units\armv7-ultibo\rtl"';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/ppcrossarm.exe' + LineEnd;

       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'IF %errorlevel% NEQ 0 GOTO Error' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo .' + LineEnd;

       {Packages Install}
       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'make packages_install CROSSINSTALL=1';
       WorkBuffer:=WorkBuffer + ' FPCFPMAKE=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/ppcrossarm.exe';
       //WorkBuffer:=WorkBuffer + ' CROSSBINDIR=' + StripTrailingSlash(CompilerPath) + '/bin/win32-arm-none'; //Not required due to arm-ultibo- prefix on as/ld etc
       WorkBuffer:=WorkBuffer + ' CROSSOPT="-CpARMV7A -CfVFPV3 -CIARM -OoFASTMATH" OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv7a';
       WorkBuffer:=WorkBuffer + ' FPC=' + StripTrailingSlash(CompilerPath) + '/bin/i386-win32/ppcrossarm.exe';
       WorkBuffer:=WorkBuffer + ' INSTALL_BASEDIR=' + StripTrailingSlash(CompilerPath);
       WorkBuffer:=WorkBuffer + ' INSTALL_UNITDIR=' + StripTrailingSlash(CompilerPath) + '/units/armv7-ultibo/packages' + LineEnd;

       WorkBuffer:=WorkBuffer + '' + LineEnd;
       WorkBuffer:=WorkBuffer + 'IF %errorlevel% NEQ 0 GOTO Error' + LineEnd;
       WorkBuffer:=WorkBuffer + 'echo .' + LineEnd;

       WorkBuffer:=WorkBuffer + '' + LineEnd;
      end;
    end;

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

   {Set Size}
   FileStream.Size:=Length(WorkBuffer);

   {Write File}
   FileStream.Position:=0;
   FileStream.WriteBuffer(PChar(WorkBuffer)^,Length(WorkBuffer));

   Result:=True;
  finally
   FileStream.Free;
  end;
 except
  on E: Exception do
   begin
    mmoMain.Lines.Add(' Error: Exception creating build script - Message: ' + E.Message);
   end;
 end;
end;

{==============================================================================}

function TfrmMain.ExecuteBuildFile:Boolean;
var
 Filename:String;
begin
 {}
 Result:=False;
 try
  if Length(SourcePath) = 0 then Exit;
  if Length(InstallPath) = 0 then Exit;
  if Length(CompilerPath) = 0 then Exit;
  if Length(CompilerVersion) = 0 then Exit;
  if not(BuildRTL) and not(BuildPackages) then Exit;
  if not(PlatformARMv6) and not(PlatformARMv7) then Exit;

  {Get Filename}
  Filename:=AddTrailingSlash(SourcePath) + '__buildrtl.bat';
  mmoMain.Lines.Add('  Build Script is ' + Filename);
  mmoMain.Lines.Add('');

  {Check File}
  if not FileExists(Filename) then Exit;

  {Execute Process}
  if not ExecuteConsoleProcessEx('cmd.exe /c',Filename,SourcePath,MarkerEnd,mmoMain) then Exit;

  Result:=True;
 except
  on E: Exception do
   begin
    mmoMain.Lines.Add(' Error: Exception executing build script - Message: ' + E.Message);
   end;
 end;
end;

{==============================================================================}
{==============================================================================}

end.
