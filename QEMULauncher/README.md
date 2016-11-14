Ultibo QEMU Launcher tool

This is the source for the Ultibo QEMU Launcher which is used when you select "Run in QEMU ..." from the Tools menu in Lazarus.

The QEMU Launcher provides a convenient way to launch the QEMU machine emulator with a compiled Ultibo project by using parameters directly from the project itself. The tool will run without an interface when called by the Lazarus IDE but can also be run standalone and will provide a dialog to enter or select the necessary options.

Note that while QEMU supports a range of emulations the Ultibo run time current only supports the Versatile Platform Baseboard (Versatile PB) emulation in ARM 32-bit mode with experimental support for ARM 64-bit (AARCH64). Over time support for more emulations will be added based on community interest and support.

While the tool is designed to determine the required information without configuration it does support creating a QEMULauncher.ini file in the same directory and setting a number of parameters to adjust the behavior.

See the header of the QEMULauncher.dpr project file for details of the INI file format and available parameters.


Compiled with Delphi (Convertable to FPC/Lazarus)
