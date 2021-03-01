{
Ultibo Font Builder Tool.

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

unit DlgExport;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TfrmExport = class(TForm)
    lblFileName: TLabel;
    saveMain: TSaveDialog;
    txtFileName: TEdit;
    cmdFileName: TButton;
    chkIncludeUnicode: TCheckBox;
    cmdCancel: TButton;
    cmdExport: TButton;
    lblName: TLabel;
    txtName: TEdit;
    lblDescription: TLabel;
    txtDescription: TEdit;
    lblUnitName: TLabel;
    txtUnitName: TEdit;
    procedure cmdFileNameClick(Sender: TObject);
    procedure txtFileNameChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function CheckUnitName(const UnitName:String):Boolean;
    function NormalizeUnitName(const UnitName:String):String;

    function ShowExport(var Name,Description,UnitName,FileName:String;var IncludeUnicode:Boolean):Boolean;
  end;

var
  frmExport: TfrmExport;

implementation

{$R *.lfm}

{==============================================================================}

function TfrmExport.CheckUnitName(const UnitName:String):Boolean;
var
 Count:Integer;
begin
 {}
 Result:=False;

 if Length(UnitName) <> 0 then
  begin
   for Count:=1 to Length(UnitName) do
    begin
     if not(UnitName[Count] in ['a'..'z','A'..'Z','0'..'9','_','.']) then Exit;
    end;

   Result:=True;
  end;
end;

{==============================================================================}

function TfrmExport.NormalizeUnitName(const UnitName:String):String;
var
 Count:Integer;
begin
 {}
 Result:=UnitName;

 if Length(Result) <> 0 then
  begin
   for Count:=1 to Length(Result) do
    begin
     if not(Result[Count] in ['a'..'z','A'..'Z','0'..'9','_','.']) then
      begin
       Result[Count]:='_';
      end;
    end;
  end;
end;

{==============================================================================}

function TfrmExport.ShowExport(var Name,Description,UnitName,FileName:String;var IncludeUnicode:Boolean):Boolean;
begin
 {}
 Result:=False;

 txtName.Text:=Name;
 txtDescription.Text:=Description;
 txtUnitName.Text:=UnitName;
 txtFileName.Text:=FileName;
 chkIncludeUnicode.Checked:=IncludeUnicode;

 if ShowModal = mrOk then
  begin
   Name:=txtName.Text;
   Description:=txtDescription.Text;
   UnitName:=txtUnitName.Text;
   FileName:=txtFileName.Text;
   IncludeUnicode:=chkIncludeUnicode.Checked;

   Result:=True;
  end;
end;

{==============================================================================}

procedure TfrmExport.FormShow(Sender: TObject);
begin
 {}
 {Adjust Labels}
 lblName.Top:=txtName.Top + ((txtName.Height - lblName.Height) div 2);
 lblDescription.Top:=txtDescription.Top + ((txtDescription.Height - lblDescription.Height) div 2);
 lblUnitName.Top:=txtUnitName.Top + ((txtUnitName.Height - lblUnitName.Height) div 2);
 lblFileName.Top:=txtFileName.Top + ((txtFileName.Height - lblFileName.Height) div 2);

 {Adjust Buttons}
 if txtFileName.Height > cmdFileName.Height then
  begin
   cmdFileName.Height:=txtFileName.Height;
   cmdFileName.Width:=txtFileName.Height;
   cmdFileName.Top:=txtFileName.Top + ((txtFileName.Height - cmdFileName.Height) div 2);
  end
 else
  begin
   cmdFileName.Height:=txtFileName.Height + 2;
   cmdFileName.Width:=txtFileName.Height + 2;
   cmdFileName.Top:=txtFileName.Top - 1;
  end;

 if cmdFileName.Height > cmdExport.Height then
  begin
   cmdExport.Height:=cmdFileName.Height;
   cmdCancel.Height:=cmdFileName.Height;
  end;

 {Check PixelsPerInch}
 if PixelsPerInch > 96 then
  begin
   {Adjust Button}
   {cmdFileName.Top:=txtFileName.Top;
   cmdFileName.Height:=txtFileName.Height;
   cmdFileName.Width:=cmdFileName.Height;}
 end;
end;

{==============================================================================}

procedure TfrmExport.txtFileNameChange(Sender: TObject);
begin
 {}
 txtUnitName.Text:=ExtractFileName(ChangeFileExt(txtFileName.Text,''));
end;

{==============================================================================}

procedure TfrmExport.cmdFileNameClick(Sender: TObject);
begin
 {}
 saveMain.Title:='Export font';
 {$IFDEF WINDOWS}
 saveMain.Filter:='Pascal source files|*.pas||All files|*.*';
 {$ENDIF}
 {$IFDEF LINUX}
 saveMain.Filter:='Pascal source files|*.pas||All files|*';
 {$ENDIF}
 saveMain.Filename:=txtFileName.Text;
 if Length(saveMain.InitialDir) = 0 then
  begin
   saveMain.InitialDir:=ExtractFilePath(Application.ExeName);
  end;
 if saveMain.Execute then
  begin
   txtFileName.Text:=saveMain.FileName;
  end;
end;

{==============================================================================}
{==============================================================================}

end.
