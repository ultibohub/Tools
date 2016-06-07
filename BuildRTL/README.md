Ultibo RTL Builder tool

This is the source for the Ultibo RTL Builder which can be found under the Tools menu in Lazarus.

The RTL builder creates a Windows batch file which compiles the RTL and Packages for any of the supported architectures and displays the output in a window during the compile.

While the tool is designed to determine the correct paths and locations without configuration it does support creating a BuildRTL.ini file in the same directory and setting a number of parameters to adjust the behavior.


The format of the INI file is:

[BuildRTL]
PathPrefix=VALUE

InstallPath=VALUE

CompilerPath=VALUE

CompilerVersion=VALUE

SourcePath=VALUE

BuildRTL=VALUE

BuildPackages=VALUE

PlatformARMv6=VALUE

PlatformARMv7=VALUE


A brief explanation of each parameter along with the standard default value:

PathPrefix - A text value to prepend to the path variable in the batch file (Default: BLANK)

InstallPath - The path where Ultibo core is installed (Default: C:\Ultibo\Core) (Detected from the application path)

CompilerPath - The path where the Ultibo version of FPC is installed (Default: INSTALLPATH\fpc\COMPILERVERSION)

CompilerVersion - The version of the FPC compiler (Default: 3.1.1)

SourcePath - The path to RTL and Packages source code (Default: COMPILERPATH\source)

BuildRTL - Enable or disable building the RTL (0=Disable / 1=Enable) (Default: 1)

BuildPackages - Enable or disable building the Packages (0=Disable / 1=Enable) (Default: 1)

PlatformARMv6 - Build the RTL and Packages for ARMv6 architecture (0=Disable / 1=Enable) (Default: 1)

PlatformARMv7 - Build the RTL and Packages for ARMv7 architecture (0=Disable / 1=Enable) (Default: 1)


Compiled with Delphi (Convertable to FPC/Lazarus)

