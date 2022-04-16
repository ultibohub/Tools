## Ultibo core installer scripts

### Linux

The ultiboinstaller.sh script in the Linux folder is designed to download the sources for Ultibo core, FPC and Lazarus and build them using the correct parameters for the current platform.

To use the installer script simply download the [ultiboinstaller.sh](https://github.com/ultibohub/Tools/releases/download/1.0.6/ultiboinstaller.sh) file and perform the following two steps:

```
chmod +x ultiboinstaller.sh
./ultiboinstaller.sh
```

**_Notes_**

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

The build-fpc-minimal.sh script is only used to create minimal versions of the stable FPC release which can be used to bootstrap the Ultibo version, it is not required for the installation.