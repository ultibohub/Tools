Ultibo RTL Builder tool

This is the source for the Ultibo RTL Builder which can be found under the Tools menu in Lazarus.

The RTL builder creates a Windows batch file which compiles the RTL and Packages for any of the supported architectures and displays the output in a window during the compile.

While the tool is designed to determine the correct paths and locations without configuration it does support creating a BuildRTL.ini file in the same directory and setting a number of parameters to adjust the behavior.

See the header of the BuildRTL.dpr project file for details of the INI file format and available parameters.


Compiled with Delphi (Convertable to FPC/Lazarus)

