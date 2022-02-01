{
Ultibo RTL Builder Tool.

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


RTL Builder
===========

 The Ultibo RTL builder provides the capability to update the RTL and Packages to
 the latest version available from GitHub and supports options to check for, download,
 install and rebuild the RTL without user intervention.
 
 You can also perform any one of the above steps individually if required.
 
 To provide consistent and reliable functionality the RTL builder now also supports
 downloading the latest tested versions of the Raspberry Pi firmware and extracting
 them to the firmware folder of your Ultibo installation.
  
 The RTL builder creates a Windows batch file or Linux shell script which compiles
 the RTL and Packages for any of the supported architectures and displays the output
 in a window during the compile.

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
 FirmwarePath=
 BranchName=

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

 CompilerVersion - The version of the FPC compiler (Default: 3.2.2)


 SourcePath - The path to RTL and Packages source code (Default Windows: <CompilerPath>\source)
                                                       (        Linux: <CompilerPath>/source)

 FirmwarePath - The path where firmware files are located (Default Windows: <InstallPath>\firmware)
                                                          (        Linux: <InstallPath>/firmware)

 BranchName - The name of the Git branch to use when checking for and downloading updates


 ARMCompiler - The name of the Free Pascal ARM Compiler or Cross Compiler (Default: <Blank>)


 AARCH64Compiler - The name of the Free Pascal AARCH64 Compiler or Cross Compiler (Default: <Blank>)


 BuildRTL - Enable or disable building the RTL (0=Disable / 1=Enable) (Default: 1)


 BuildPackages - Enable or disable building the Packages (0=Disable / 1=Enable) (Default: 1)


 PlatformARMv6 - Build the RTL and Packages for ARMv6 architecture (0=Disable / 1=Enable) (Default: 1)


 PlatformARMv7 - Build the RTL and Packages for ARMv7 architecture (0=Disable / 1=Enable) (Default: 1)


 PlatformARMv8 - Build the RTL and Packages for ARMv8 architecture (0=Disable / 1=Enable) (Default: 1)


 Please note that compiling BuildRTL requires Lazarus 2.0.10 (with FPC 3.2.0) or above.
}

program BuildRTL;

{$MODE Delphi}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Forms,
  Interfaces,
  Main in 'Main.pas' {frmMain};

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.Title := 'Ultibo RTL Builder';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
