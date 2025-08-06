{
Ultibo QEMU Launcher Tool.

Copyright (C) 2022 - SoftOz Pty Ltd.

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

program QEMULauncher;

{$MODE Delphi}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Forms,
  Interfaces,
  Main in 'Main.pas' {frmMain};

{$R *.res}

var
 Launch:TQEMULaunch;

begin
 {}
 RequireDerivedFormResource:=True;

 if ParamCount >= 4 then
  begin
   Launch:=TQEMULaunch.Create;
   Launch.LoadConfig;
   Launch.LoadParams;
   Launch.LoadProjectConfig;
   Launch.Launch;
   Launch.Free;
  end
 else
  begin
   Application.Initialize;
   Application.Title := 'Ultibo QEMU Launcher';
   Application.CreateForm(TfrmMain, frmMain);
   Application.Run;
  end;
end.
