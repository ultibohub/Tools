## Ultibo core installer scripts

### Linux

The ultiboinstaller.sh script in the Linux folder is designed to download the sources for Ultibo core, FPC and Lazarus and build them using the correct parameters for the current platform.

To use the installer script simply download the [ultiboinstaller.sh](https://github.com/ultibohub/Tools/releases/latest/download/ultiboinstaller.sh) file and perform the following two steps:

```
chmod +x ultiboinstaller.sh
./ultiboinstaller.sh
```

The script will ask several questions and check for the required dependencies before commencing the installation. If you are reinstalling and have an existing Ultibo installation in the folder selected the installer will prompt to remove the folder before continuing, please ensure you have not saved any additional files to this location as this removal cannot be undone. If you are unsure please take a backup of your existing installation before commencing.

The script has been tested on a range of Linux distributions including Debian, Ubuntu, Raspberry Pi OS, Arch and various other derivatives for x86, x86_64, arm and aarch64 architectures. The key item to be aware of when using a different distribution is the availability of the various tools and libraries required, however these are all quite standard and will be readily available on most distributions.
 
**_Installing MAIN or FIXES branch_**

By default the script will build and install the current stable versions of FPC and Lazarus however if you prefer to use the main (trunk) or fixes branches you can specify that on the command line when launching the script.

_To select the MAIN (trunk) branch use:

```
./ultiboinstaller.sh main
```

_or for the FIXES branch use:

```
./ultiboinstaller.sh fixes
```
 
**_Swap size_**

Debian installation requires at least 2048MB of RAM, anything less may result in an error while building Lazarus. If you do not have 2048MB of physical RAM you may need to increase the swap file size as per the Raspbian information below.

Because all Raspberry Pi models prior to the Pi 4 have 1024MB of memory at most, installation on Raspbian requires a swap file size of at least 1000MB, anything less may result in an error while building Lazarus.

To increase the size of the swap file follow these simple instructions courtesy of our good friend Hans Otten:

```
sudo nano /etc/dphys-swapfile
```

_Edit the CONF_SWAPSIZE value to be 1000 or greater_
 
```
sudo /etc/init.d/dphys-swapfile stop
sudo /etc/init.d/dphys-swapfile start
ls -lh /var
```

_And check that the file "swap" is now the size specified above_

**_Other scripts_**

The build-fpc-minimal.sh script is only used to create minimal versions of the stable FPC release which can be used to bootstrap the Ultibo version, it is not required for the installation.