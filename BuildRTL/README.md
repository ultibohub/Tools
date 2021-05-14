## Ultibo RTL Builder tool

This is the source for the Ultibo RTL Builder which can be found under the Tools menu in Lazarus.

The Ultibo RTL builder provides the capability to update the RTL and Packages to the latest version available from GitHub and supports options to check for, download, install and rebuild the RTL without user intervention.
 
You can also perform any one of the above steps individually if required.
 
To provide consistent and reliable functionality the RTL builder now also supports downloading the latest tested versions of the Raspberry Pi firmware and extracting them to the firmware folder of your Ultibo installation.

The RTL builder creates a Windows batch file or Linux shell script which compiles the RTL and Packages for any of the supported architectures and displays the output in a window during the compile.

While the tool is designed to determine the correct paths and locations without configuration it does support creating a BuildRTL.ini file in the same directory and setting a number of parameters to adjust the behavior.

See the header of the BuildRTL.lpr project file for details of the INI file format and available parameters.


Compiled with FPC/Lazarus for Windows or Linux targets

