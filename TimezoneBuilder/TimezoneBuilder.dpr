{
Ultibo Timezone Builder Tool.

Copyright (C) 2016 - SoftOz Pty Ltd.

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

uses
  Forms,
  Main in 'Main.pas' {frmMain};

{$R TimezoneBuilderManifest.RES}  
{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Ultibo Timezone Builder';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
