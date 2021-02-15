{
Ultibo Font Builder Tool.

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

  PC Screen Fonts (PSF) - https://en.wikipedia.org/wiki/PC_Screen_Font

  A number of PSF format fonts in various stlyes and sizes are available from:

   http://v3.sk/~lkundrak/fonts/kbd/

  Note that this site also lists a number of other fonts in raw format (no header)
  which contain the font character data in the same format as the PSF files but
  cannot currently be converted by this tool.

   http://v3.sk/~lkundrak/fonts/

Font Builder
============

 The fonts supported by Ultibo are a bitmap format that contains a block of data
 where each character is represented by a number of consecutive bytes.

 Fonts can either be statically compiled as a pascal unit and loaded during startup
 or can be dynamically loaded by passing a header and data block to the FontLoad()
 function.

 For an 8x16 (8 pixels wide and 16 pixels high) font the data contains 8 bits (1 byte)
 for each of the 16 rows that make up a character and each character would be
 16 bytes long.

 For a 12x22 font the data contains 12 bits padded to 16 bits (2 bytes) for each of
 the 22 rows that make up a character. Therefore each character would be 44 bytes
 in length.

 The font unit can support any size font from 8x6 to 32x64 including every combination
 in between.

 For fonts where the bits per row is greater than one byte both little endian and big
 endian format is supported.

 This tool currently supports converting PC Screen Fonts (PSF) into a pascal unit
 suitable for including in an Ultibo project. Additional formats will be supported
 in future.

}

program FontBuilder;

{$MODE Delphi}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Forms,
  Interfaces,
  Main in 'Main.pas' {frmMain},
  DlgExport in 'DlgExport.pas' {frmExport};

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.Title := 'Ultibo Font Builder';
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmExport, frmExport);
  Application.Run;
end.
