#!/bin/bash
# Linux script to create zip files containing a minimal FPC stable
#
# Based on the setup.sh script from http://getlazarus.org

# The full version number of the stable compiler
FPC_STABLE=3.0.2

# Prevent this script from running as root 
if [ "$(id -u)" = "0" ]; then
   echo "This script should not be run as root"
   exit 1
fi

# function require(program) 
function require() {
	if ! type "$1" > /dev/null; then
		echo 
		echo "An error occurred"
		echo 
		echo "This installation requires the package $1 but it was not found on your system"
		echo 
		echo "On Debian based distributions type the following to install it"
		echo 
		echo "sudo apt-get install $2"
		echo 
		echo "Then re-run the installation"
		echo 
		echo "For other distributions refer to the documentation for your"
        echo "package manager"
		echo 
		exit 1
	fi	
	echo "$1 found"
}

# Require the following programs 
require "zip" "zip"

echo 
echo "Starting Build Minimal FPC $FPC_STABLE"
echo 

# Determine operating system architecture
CPU=$(uname -m)

if [ "$CPU" = "i686" ]; then
	CPU="i386"
fi

if [ "$CPU" = "i386" ]; then
    COMPILER="ppc386"
fi

if [ "$CPU" = "x86_64" ]; then
    COMPILER="ppcx64"
fi

if [ "$CPU" = "armv6l" ]; then
	CPU="arm"
    COMPILER="ppcarm"
fi

if [ "$CPU" = "armv7l" ]; then
	CPU="arm"
    COMPILER="ppcarm"
fi

# Setup the paths
FPC_DIR=$HOME/fpc-$FPC_STABLE
FPC_STABLE_DIR=$HOME/fpc-$FPC_STABLE.$CPU-linux

# Create the destination folder
mkdir -p $FPC_STABLE_DIR
mkdir -p $FPC_STABLE_DIR/bin
mkdir -p $FPC_STABLE_DIR/lib/fpc/$FPC_STABLE
mkdir -p $FPC_STABLE_DIR/lib/fpc/$FPC_STABLE/msg
mkdir -p $FPC_STABLE_DIR/lib/fpc/$FPC_STABLE/units/$CPU-linux
mkdir -p $FPC_STABLE_DIR/lib/fpc/$FPC_STABLE/units/$CPU-linux/pthreads
mkdir -p $FPC_STABLE_DIR/lib/fpc/$FPC_STABLE/units/$CPU-linux/rtl
mkdir -p $FPC_STABLE_DIR/lib/fpc/$FPC_STABLE/units/$CPU-linux/rtl-console
mkdir -p $FPC_STABLE_DIR/lib/fpc/$FPC_STABLE/units/$CPU-linux/rtl-extra
mkdir -p $FPC_STABLE_DIR/lib/fpc/$FPC_STABLE/units/$CPU-linux/rtl-objpas
mkdir -p $FPC_STABLE_DIR/lib/fpc/$FPC_STABLE/units/$CPU-linux/rtl-unicode

# Copy the files in bin
cp $FPC_DIR/bin/fpc $FPC_STABLE_DIR/bin/fpc
cp $FPC_DIR/bin/fpcmake $FPC_STABLE_DIR/bin/fpcmake
cp $FPC_DIR/bin/fpcmkcfg $FPC_STABLE_DIR/bin/fpcmkcfg
cp $FPC_DIR/lib/fpc/$FPC_STABLE/$COMPILER $FPC_STABLE_DIR/bin/$COMPILER

# Copy the files in lib
cp $FPC_DIR/lib/fpc/$FPC_STABLE/$COMPILER $FPC_STABLE_DIR/lib/fpc/$FPC_STABLE/$COMPILER

# Copy the files in lib/../msg
cp $FPC_DIR/lib/fpc/$FPC_STABLE/msg/* $FPC_STABLE_DIR/lib/fpc/$FPC_STABLE/msg

# Copy the files in lib/../units
cp $FPC_DIR/lib/fpc/$FPC_STABLE/units/$CPU-linux/pthreads/* $FPC_STABLE_DIR/lib/fpc/$FPC_STABLE/units/$CPU-linux/pthreads
cp $FPC_DIR/lib/fpc/$FPC_STABLE/units/$CPU-linux/rtl/* $FPC_STABLE_DIR/lib/fpc/$FPC_STABLE/units/$CPU-linux/rtl
cp $FPC_DIR/lib/fpc/$FPC_STABLE/units/$CPU-linux/rtl-console/* $FPC_STABLE_DIR/lib/fpc/$FPC_STABLE/units/$CPU-linux/rtl-console
cp $FPC_DIR/lib/fpc/$FPC_STABLE/units/$CPU-linux/rtl-extra/* $FPC_STABLE_DIR/lib/fpc/$FPC_STABLE/units/$CPU-linux/rtl-extra
cp $FPC_DIR/lib/fpc/$FPC_STABLE/units/$CPU-linux/rtl-objpas/* $FPC_STABLE_DIR/lib/fpc/$FPC_STABLE/units/$CPU-linux/rtl-objpas
cp $FPC_DIR/lib/fpc/$FPC_STABLE/units/$CPU-linux/rtl-unicode/* $FPC_STABLE_DIR/lib/fpc/$FPC_STABLE/units/$CPU-linux/rtl-unicode

# Zip the files
cd
zip -r $HOME/fpc-$FPC_STABLE.$CPU-linux.zip fpc-$FPC_STABLE.$CPU-linux/*

echo 
echo "Completed Build Minimal FPC $FPC_STABLE"
echo 
