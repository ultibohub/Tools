{
Ultibo Configure RTL Tool.

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


Configure RTL
=============

 The Configure RTL tool is used by the Ultibo core installer to update the configuration
 files used by FPC to determine unit locations etc.

 The tool only runs once at the end of the install and is not required again, the
 files updated are:

  FP.CFG
  FPC.CFG
  RPI.CFG
  RPI2.CFG
  RPI3.CFG

}

unit Main;

interface

uses Windows,SysUtils,Classes;

{==============================================================================}
const
 LineEnd = Chr(13) + Chr(10); {CR LF}

{==============================================================================}

function GetTempDirectoryString:String;

function AddTrailingSlash(const FilePath:String):String;
function StripTrailingSlash(const FilePath:String):String;

function EditConfiguration(const AName,APath,ASearch,AReplace:String):Boolean;

function CreateOptions(const AName,AInstallPath,ACompilerPath,ALazarusVersion,ALazarusVersionNo:String):Boolean;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}

function GetTempDirectoryString:String;
var
 szTempPath:array[0..(MAX_PATH - 1)] of char;
begin
 {}
 Result:='';
 szTempPath:='';
 if GetTempPath(MAX_PATH,szTempPath) > 0 then
  begin
   Result:=szTempPath;
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

function EditConfiguration(const AName,APath,ASearch,AReplace:String):Boolean;
var
 PosIdx:Integer;
 WorkBuffer:String;
 LeftBuffer:String;
 RightBuffer:String;
 FileStream:TFileStream;
begin
 {}
 Result:=False;
 try
  if Length(AName) = 0 then Exit;
  if Length(APath) = 0 then Exit;
  if Length(ASearch) = 0 then Exit;

  {Check File}
  if not FileExists(APath + AName) then Exit;

  {Open File}
  FileStream:=TFileStream.Create(APath + AName,fmOpenReadWrite);
  try
   {Get Size}
   SetLength(WorkBuffer,FileStream.Size);

   {Read File}
   FileStream.ReadBuffer(PChar(WorkBuffer)^,Length(WorkBuffer));

   {Find}
   PosIdx:=Pos(ASearch,WorkBuffer);
   while PosIdx <> 0 do
    begin
     LeftBuffer:=Copy(WorkBuffer,1,PosIdx - 1);
     RightBuffer:=Copy(WorkBuffer,PosIdx + Length(ASearch),Length(WorkBuffer));
     WorkBuffer:=LeftBuffer + AReplace + RightBuffer;

     PosIdx:=Pos(ASearch,WorkBuffer);
    end;

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
  {}
 end;
end;

{==============================================================================}

function CreateOptions(const AName,AInstallPath,ACompilerPath,ALazarusVersion,ALazarusVersionNo:String):Boolean;
var
 WorkBuffer:String;
 FileStream:TFileStream;
begin
 {}
 Result:=False;
 try
  if Length(AName) = 0 then Exit;
  if Length(AInstallPath) = 0 then Exit;
  if Length(ACompilerPath) = 0 then Exit;
  if Length(ALazarusVersion) = 0 then Exit;
  if Length(ALazarusVersionNo) = 0 then Exit;

  {Check File}
  if FileExists(AInstallPath + AName) then Exit;

  {Create File}
  FileStream:=TFileStream.Create(AInstallPath + AName,fmCreate);
  try
   {Create Content}
   WorkBuffer:='';
   WorkBuffer:=WorkBuffer + '<?xml version="1.0"?>' + LineEnd;
   WorkBuffer:=WorkBuffer + '<CONFIG>' + LineEnd;
   WorkBuffer:=WorkBuffer + '  <EnvironmentOptions>' + LineEnd;
   WorkBuffer:=WorkBuffer + '    <Version Value="' + ALazarusVersionNo + '" Lazarus="' + ALazarusVersion + '"/>' + LineEnd;
   WorkBuffer:=WorkBuffer + '    <LazarusDirectory Value="' + StripTrailingSlash(AInstallPath) + '">' + LineEnd;
   WorkBuffer:=WorkBuffer + '    </LazarusDirectory>' + LineEnd;
   WorkBuffer:=WorkBuffer + '    <CompilerFilename Value="' + StripTrailingSlash(ACompilerPath) + '\bin\i386-win32\fpc.exe">' + LineEnd;
   WorkBuffer:=WorkBuffer + '    </CompilerFilename>' + LineEnd;
   WorkBuffer:=WorkBuffer + '    <FPCSourceDirectory Value="$(LazarusDir)fpc\$(FPCVer)\source">' + LineEnd;
   WorkBuffer:=WorkBuffer + '    </FPCSourceDirectory>' + LineEnd;
   WorkBuffer:=WorkBuffer + '    <MakeFilename Value="' + StripTrailingSlash(ACompilerPath) + '\bin\i386-win32\make.exe">' + LineEnd;
   WorkBuffer:=WorkBuffer + '    </MakeFilename>' + LineEnd;
   WorkBuffer:=WorkBuffer + '    <TestBuildDirectory Value="' + AddTrailingSlash(GetTempDirectoryString) + '">' + LineEnd;
   WorkBuffer:=WorkBuffer + '    </TestBuildDirectory>' + LineEnd;
   WorkBuffer:=WorkBuffer + '    <Debugger Class="TGDBMIDebugger"/>' + LineEnd;
   WorkBuffer:=WorkBuffer + '    <DebuggerFilename Value="' + StripTrailingSlash(ACompilerPath) + '\bin\i386-win32\gdb.exe">' + LineEnd;
   WorkBuffer:=WorkBuffer + '    </DebuggerFilename>' + LineEnd;
   WorkBuffer:=WorkBuffer + '  </EnvironmentOptions>' + LineEnd;
   WorkBuffer:=WorkBuffer + '</CONFIG>' + LineEnd;

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
  {}
 end;
end;

{==============================================================================}
{==============================================================================}

end.
