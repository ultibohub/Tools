{
Ultibo Text to Binary Tool.

Copyright (C) 2023 - SoftOz Pty Ltd.

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


Text to Binary
==============

 This tool takes plain text in the form of hex values seperated by spaces and
 converts them to a binary output file.

 The text to be converted should be in the form:

 00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F
 1F 1E 1D 1C ...

 Each pair of characters represents a byte in the binary output.

}

program Text2Bin;

{$mode Delphi}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  Main;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

