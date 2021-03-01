{
Ultibo Configure RTL Tool.

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


Configure RTL
=============

 The Configure RTL tool is used by the Ultibo core installer to update the configuration
 files used by FPC to determine unit locations etc and the environment options file for
 Lazarus.

 The tool only runs once at the end of the install and is not required again, the
 files updated are:

  FP.CFG
  FPC.CFG
  RPI.CFG
  RPI2.CFG
  RPI3.CFG
  RPI4.CFG
  QEMUVPB.CFG

  environmentoptions.xml
  
}

program ConfigureRTL;

{$MODE Delphi}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  SysUtils,
  FileCtrl,
  Main in 'Main.pas';

{$R *.res}

var
 InstallPath:String;
 CompilerPath:String;
 CompilerVersion:String;
 LazarusVersion:String;
 LazarusVersionNo:String;
 ConfigurationPath:String;
 
begin
 {Install will pass 4 parameters:

  1. The APP directory where the files were installed

  2. The FPC version which is defined in the script

  3. The Lazarus version which is defined in the script

  4. The Lazarus version no which is defined in the script}

 {Check Parameters}
 if ParamCount >=4 then
  begin
   {Get Install Path}
   InstallPath:=StripTrailingSlash(ParamStr(1));

   {Check Install Path}
   if DirectoryExists(InstallPath) then
    begin
     {Get FPC Version}
     CompilerVersion:=ParamStr(2);

     {Get Lazarus Version}
     LazarusVersion:=ParamStr(3);

     {Get Lazarus Version No}
     LazarusVersionNo:=ParamStr(4);

     {Get Compiler Path}
     {$IFDEF WINDOWS}
     CompilerPath:=InstallPath + '\fpc\' + CompilerVersion;
     {$ENDIF}
     {$IFDEF LINUX}
     CompilerPath:=InstallPath + '/fpc';
     {$ENDIF}

     {Get Configuration Path}
     {$IFDEF WINDOWS}
     ConfigurationPath:=InstallPath + '\fpc\' + CompilerVersion + '\bin\i386-win32';
     {$ENDIF}
     {$IFDEF LINUX}
     ConfigurationPath:=InstallPath + '/fpc/bin';
     {$ENDIF}

     {Check Configuration Path}
     if DirectoryExists(ConfigurationPath) then
      begin
       {$IFDEF WINDOWS}
       {Edit FP.CFG}
       EditConfiguration('FP.CFG',AddTrailingSlash(ConfigurationPath),'%INSTALLDIRECTORY%',CompilerPath);

       {Edit FPC.CFG}
       EditConfiguration('FPC.CFG',AddTrailingSlash(ConfigurationPath),'%INSTALLDIRECTORY%',CompilerPath);

       {Edit RPI.CFG}
       EditConfiguration('RPI.CFG',AddTrailingSlash(ConfigurationPath),'%INSTALLDIRECTORY%',CompilerPath);

       {Edit RPI2.CFG}
       EditConfiguration('RPI2.CFG',AddTrailingSlash(ConfigurationPath),'%INSTALLDIRECTORY%',CompilerPath);

       {Edit RPI3.CFG}
       EditConfiguration('RPI3.CFG',AddTrailingSlash(ConfigurationPath),'%INSTALLDIRECTORY%',CompilerPath);

       {Edit RPI4.CFG}
       EditConfiguration('RPI4.CFG',AddTrailingSlash(ConfigurationPath),'%INSTALLDIRECTORY%',CompilerPath);

       {Edit QEMUVPB.CFG}
       EditConfiguration('QEMUVPB.CFG',AddTrailingSlash(ConfigurationPath),'%INSTALLDIRECTORY%',CompilerPath);

       {Create environmentoptions.xml}
       CreateOptions('environmentoptions.xml',AddTrailingSlash(InstallPath),AddTrailingSlash(CompilerPath),LazarusVersion,LazarusVersionNo);
       {$ENDIF}
       {$IFDEF LINUX}
       {Don't Edit FP.CFG or FPC.CFG}

       {Edit RPI.CFG}
       EditConfiguration('RPI.CFG',AddTrailingSlash(ConfigurationPath),'%INSTALLDIRECTORY%',CompilerPath);

       {Edit RPI2.CFG}
       EditConfiguration('RPI2.CFG',AddTrailingSlash(ConfigurationPath),'%INSTALLDIRECTORY%',CompilerPath);

       {Edit RPI3.CFG}
       EditConfiguration('RPI3.CFG',AddTrailingSlash(ConfigurationPath),'%INSTALLDIRECTORY%',CompilerPath);

       {Edit RPI4.CFG}
       EditConfiguration('RPI4.CFG',AddTrailingSlash(ConfigurationPath),'%INSTALLDIRECTORY%',CompilerPath);

       {Edit QEMUVPB.CFG}
       EditConfiguration('QEMUVPB.CFG',AddTrailingSlash(ConfigurationPath),'%INSTALLDIRECTORY%',CompilerPath);

       //To Do //environmentoptions.xml
       {$ENDIF}
      end;
    end;
  end;
end.
