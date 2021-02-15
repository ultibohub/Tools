{
Ultibo Timezone Builder Tool.

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


Timezone Builder
================

 This tool reads all available timezones from the registry on the local computer
 and converts them to a TTimezoneList structure to be copied and pasted directly
 into the Ultibo Timezone unit.

}

program TimezoneBuilder;

{$MODE Delphi}

uses
  Forms,
  Interfaces,
  Main in 'Main.pas' {frmMain};

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.Title := 'Ultibo Timezone Builder';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
