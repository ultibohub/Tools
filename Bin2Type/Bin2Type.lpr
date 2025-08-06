{
Ultibo Binary to Type Tool.

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


Binary to Type
==============

 This tool takes any binary file and converts it to an array type and variable
 that can be embedded into a Pascal project. You can optionally specify a starting
 offset and length for the conversion (defaults to the entire file).

}

program Bin2Type;

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
  Application.Title := 'Binary to Type Definition';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
