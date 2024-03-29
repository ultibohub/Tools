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

unit Main;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    buttonOutput: TButton;
    buttonConvert: TButton;
    buttonClose: TButton;
    editOutput: TLabeledEdit;
    memoMain: TMemo;
    panelMain: TPanel;
    saveMain: TSaveDialog;
    statusMain: TStatusBar;
    procedure editOutputChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure buttonCloseClick(Sender: TObject);
    procedure buttonConvertClick(Sender: TObject);
    procedure buttonOutputClick(Sender: TObject);
  private
    { Private declarations }
    FOutput:String;
  public
    { Public declarations }
    function Convert:Boolean;
  end;

var
  frmMain: TfrmMain;

implementation

{==============================================================================}
{==============================================================================}

{$R *.lfm}

{ TfrmMain }

function TfrmMain.Convert:Boolean;
var
  Data:Byte;
  Value:String;
  Offset:Integer;
  Source:String;
  Dest:TMemoryStream;
begin
  Result:=False;

  if Length(Trim(editOutput.Text)) = 0 then
    Exit;

  try
    Source:=Trim(memoMain.Text);
    if Length(Source) > 0 then
    begin
      Dest:=TMemoryStream.Create;
      try
        Value:='';
        Offset:=1;

        while Offset <= Length(Source) do
        begin
          if Source[Offset] = ' ' then
            Inc(Offset)
          else if Source[Offset] = #10 then
            Inc(Offset)
          else if Source[Offset] = #13 then
            Inc(Offset)
          else if Source[Offset] in ['0'..'9','a'..'f','A'..'F'] then
          begin
            Value:=Value + Source[Offset];
            Inc(Offset);

            if Length(Value) = 2 then
            begin
              Data:=StrToInt('$' + Value);

              Dest.WriteByte(Data);

              Value:='';
            end;
          end
          else
            Exit;
        end;

        Dest.SaveToFile(Trim(editOutput.Text));

        Result:=True;
      finally
        Dest.Free;
      end;
    end;
  except
    // Failed
  end;
end;

{==============================================================================}
{==============================================================================}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FOutput:='';
end;

{==============================================================================}

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  // Nothing
end;

{==============================================================================}

procedure TfrmMain.FormShow(Sender: TObject);
var
 Scale:Double;
begin
  editOutput.Text:=FOutput;

  // Adjust Controls
  if editOutput.Height > buttonOutput.Height then
  begin
    buttonOutput.Height:=editOutput.Height;
    buttonOutput.Width:=editOutput.Height;
    buttonOutput.Top:=editOutput.Top + ((editOutput.Height - buttonOutput.Height) div 2);
  end
  else
  begin
    buttonOutput.Height:=editOutput.Height + 2;
    buttonOutput.Width:=editOutput.Height + 2;
    buttonOutput.Top:=editOutput.Top - 1;
  end;

  if editOutput.Height > buttonConvert.Height then
   begin
    buttonConvert.Height:=editOutput.Height;
    buttonConvert.Top:=editOutput.Top + ((editOutput.Height - buttonConvert.Height) div 2);
   end
  else
   begin
    buttonConvert.Height:=editOutput.Height + 2;
    buttonConvert.Top:=editOutput.Top - 1;
   end;

  if editOutput.Height > buttonClose.Height then
   begin
    buttonClose.Height:=editOutput.Height;
    buttonClose.Top:=editOutput.Top + ((editOutput.Height - buttonClose.Height) div 2);
   end
  else
   begin
    buttonClose.Height:=editOutput.Height + 2;
    buttonClose.Top:=editOutput.Top - 1;
   end;

  // Check PixelsPerInch
  if PixelsPerInch > 96 then
  begin
    // Calculate Scale
    Scale:=(PixelsPerInch / 96);

    // Disable Anchors}
    editOutput.Anchors:=[akLeft,akTop];
    buttonOutput.Anchors:=[akLeft,akTop];
    buttonConvert.Anchors:=[akLeft,akTop];
    buttonClose.Anchors:=[akLeft,akTop];

    // Resize Form
    Width:=Trunc(Width * Scale);
    Height:=Trunc(Height * Scale);

    // Move Controls
    editOutput.Left:=panelMain.Width - Trunc(570 * Scale); {650 - 80 = 570}
    buttonOutput.Left:=panelMain.Width - Trunc(261 * Scale); {650 - 389 = 261}
    buttonConvert.Left:=panelMain.Width - Trunc(222 * Scale); {650 - 428 = 222}
    buttonClose.Left:=panelMain.Width - Trunc(137 * Scale); {650 - 513 = 137}

    // Enable Anchors
    editOutput.Anchors:=[akLeft,akRight,akBottom];
    buttonOutput.Anchors:=[akRight,akBottom];
    buttonConvert.Anchors:=[akRight,akBottom];
    buttonClose.Anchors:=[akRight,akBottom];
  end;
end;

{==============================================================================}

procedure TfrmMain.FormHide(Sender: TObject);
begin
  // Nothing
end;

{==============================================================================}

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // Nothing
end;

{==============================================================================}
{==============================================================================}

procedure TfrmMain.editOutputChange(Sender: TObject);
begin
  FOutput:=editOutput.Text;
end;

{==============================================================================}

procedure TfrmMain.buttonOutputClick(Sender: TObject);
begin
  saveMain.FileName:=FOutput;
  saveMain.InitialDir:=ExtractFileDir(Application.ExeName);
  {$IFDEF WINDOWS}
  saveMain.Filter:='All Files (*.*)|*.*';
  {$ENDIF}
  {$IFDEF LINUX}
  saveMain.Filter:='All Files (*.*)|*';
  {$ENDIF}
  if saveMain.Execute then
  begin
    editOutput.Text:=saveMain.FileName;
  end;
end;

{==============================================================================}

procedure TfrmMain.buttonConvertClick(Sender: TObject);
begin
  if Convert then
  begin
    MessageDlg('Conversion Successful',mtInformation,[mbOk],0);
  end
  else
  begin
    MessageDlg('Conversion Failed',mtInformation,[mbOk],0);
  end;
end;

{==============================================================================}

procedure TfrmMain.buttonCloseClick(Sender: TObject);
begin
  Application.Terminate;
end;

{==============================================================================}
{==============================================================================}

end.

