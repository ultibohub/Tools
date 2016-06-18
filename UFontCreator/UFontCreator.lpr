{
Ultibo Custom Font Creator.

Copyright (C) 2016 - Kerry Shipman.


Licence
=======

 LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)

Credits
=======

 Parts and pieces for this program were found on many blogs and forum posts.
 Many thanks to the great Delphi and ObjectPascal community.

Windows Fonts
=============

Please note this program will export any Windows font in any selected size as a
fixed width font in a simple text based file format with a .ufnt extension.
The file may be parsed using the standard TIniFiles interface. These are
intended to be used with the Ultibo.org Raspberry Pi project.

NOTE:  Please respect all font copyright restrictions when exporting fonts for
embedded use in your Ultibo application.  Not all fonts, including some free ones, allow
for use in embedded systems.

}
program UFontCreator;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Unit1
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

