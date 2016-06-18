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
unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Windows, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Spin;

type
TFontType = (ftRaster, ftDevice, ftTrueType);


TFontInfo = class
  private
  FShortName : string;
  FFullName : string;
  FStyle : string;
  FLF : TLogFont;
  FFontType : TFontType;
  FTM : TNewTextMetric;
  public
  property FullName : string read FFullName ;
  property ShortName : string read FShortName;
  property Style : string read FStyle ;
  property FontType : TFontType read FFontType ;
  property TM : TNewTextMetric read FTM ;
  property LF : TLogFont read FLF ;
 end;


TFontList = class
  private
  procedure ClearList;
  procedure AddFont(EnumLogFont: TEnumLogFont; TextMetric: TNewTextMetric; FontType: Integer);
  public
  List : TStringList;
  constructor Create;
  destructor Destroy; override;
  procedure RefreshFontInfo;
  end;
{ TFontList }

  { TForm1 }

  TForm1 = class(TForm)
    btnExport: TButton;
    cbFont: TComboBox;
    cbUFontMode: TComboBox;
    cbUFontType: TComboBox;
    edMaxFWidth: TEdit;
    edFHeight: TEdit;
    Image1: TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lbH: TLabel;
    lbH1: TLabel;
    lbW: TLabel;
    lbChar: TLabel;
    lbW1: TLabel;
    Memo1: TMemo;
    mmoData: TMemo;
    mmoSample: TMemo;
    seFontSize: TSpinEdit;
    seChar: TSpinEdit;
    Timer1: TTimer;
    Timer2: TTimer;
    Timer3: TTimer;
    procedure btnDrawCharClick(Sender: TObject);
    procedure btnExportClick(Sender: TObject);
    procedure cbFontChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure seCharChange(Sender: TObject);
    procedure seFontSizeChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure Timer3Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure GetFontDim;
    procedure DrawChar(code: byte);
    procedure GetCharData;
    procedure DrawCharExp(code: byte);
    procedure GetCharDataExp;
    procedure CharExport;
  end;


var
  Form1: TForm1;
  TexWidth, TexHeight: integer;
  FileName: string;
  ExpChar: byte;
  doExport, doSave, inExport: boolean;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.GetCharData;
var
  s: string;
  x, y, w, h, z: integer;

begin
  w := lbChar.Width - 1;
  h := lbChar.Height - 1;
  s := 'w = ' + IntToStr(lbChar.Width) + ', h = ' + IntToStr(lbChar.Height);
  mmoData.Lines.Add(s);
  s := '';

  for y := 0 to h do
  begin
    for x := 0 to w do
    begin
      z := lbChar.Canvas.Pixels[x, y];
      if z = clWhite then s := s + '0' else s := s + '1';
    end;
    mmoData.Lines.Add(s);
    s := '';
  end;
end;

procedure TForm1.GetCharDataExp;
var
  s: string;
  x, y, w, h, z: integer;

begin
  w := lbChar.Width - 1;
  h := lbChar.Height - 1;
  s:='[S' + IntToStr(ExpChar) + ']';
  mmoData.Lines.AddStrings(s);
  s := 'Width=' + IntToStr(lbChar.Width);
  mmoData.Lines.Add(s);
  s := 'Height=' + IntToStr(lbChar.Height);
  mmoData.Lines.Add(s);

  s:='[C' + IntToStr(ExpChar) + ']';
  mmoData.Lines.Add(s);
  s:='';

  for y := 0 to h do
  begin
    for x := 0 to w do
    begin
      z := lbChar.Canvas.Pixels[x, y];
      if z = clWhite then s := s + '0' else s := s + '1';
    end;
    mmoData.Lines.Add(s);
    s := '';
  end;
end;

procedure TForm1.DrawChar(code: byte);
Var
  S:string;
begin
  S := Chr(code);
  lbChar.Width := TexWidth;
  lbChar.Height := TexHeight;
  lbChar.Caption:=S;
  lbW.Caption:=IntToStr(lbChar.Width);
  lbH.Caption:=IntToStr(lbChar.Height);
  mmoData.Clear;
  Timer1.Enabled:=true;
end;
procedure TForm1.DrawCharExp(code: byte);
Var
  S:string;
begin
  S := Chr(code);
  lbChar.Width := TexWidth;
  lbChar.Height := TexHeight;
  lbChar.Caption:=S;
  lbW.Caption:=IntToStr(lbChar.Width);
  lbH.Caption:=IntToStr(lbChar.Height);
  Timer2.Enabled:=true;
end;

procedure TForm1.GetFontDim;
var
  w, h, maxw, maxh: integer;
  x: byte;
  s: string;
begin
  maxw := 0;
  maxh := 0;
  w := 0;
  h := 0;

  with Image1.Canvas do
    begin
      for x := 0 to 255 do
      begin
        s := Chr(x);
        w := TextWidth(s);
        h := TextHeight(s);
        if w > maxw then maxw := w;
        if h > maxh then maxh := h;
      end;

    end;

  edMaxFWidth.Text:= IntToStr(maxw);
  edFHeight.Text:= IntToStr(maxh);
  TexWidth := maxw;
  TexHeight := maxh;
  lbChar.Width:= maxw;
  lbChar.Height:= maxh;
  Image1.Width:= maxw;
  Image1.Height:= maxh;

end;

procedure TForm1.FormCreate(Sender: TObject);
var
  FontList : TFontList;
begin
  cbFont.Clear;
  FontList := TFontList.Create;
  try
    FontList.RefreshFontInfo;
    cbFont.Items.AddStrings(FontList.List);
  finally
    FontList.Free;
  end;
  cbFont.ItemIndex:= cbFont.Items.IndexOf('Arial');
  mmoSample.Font.Name:=cbFont.Text;
  mmoSample.Font.Size:=seFontSize.Value;
  mmoSample.Repaint;
  Image1.Canvas.Font.Name:=cbFont.Text;
  Image1.Canvas.Font.Size:=seFontSize.Value;
  lbChar.Font.Name:=cbFont.Text;
  lbChar.Font.Size:=seFontSize.Value;
  lbChar.Font.Quality:=fqNonAntialiased;

  GetFontDim;
  lbChar.Width:= TexWidth;
  lbChar.Height:= TexHeight;

  Image1.Width:=TexWidth;
  Image1.Height:=TexHeight;
  Image1.Canvas.Font.Color:=clWhite;

  DrawChar(seChar.Value);

end;

procedure TForm1.seCharChange(Sender: TObject);
var
  c: byte;
begin
  if inExport = false then
  begin
    c := byte(seChar.Value);
    DrawChar(c);
  end;
end;

procedure TForm1.seFontSizeChange(Sender: TObject);
begin
  mmoSample.Font.Name:=cbFont.Text;
  mmoSample.Font.Size:=seFontSize.Value;
  mmoSample.Repaint;
  Image1.Canvas.Font.Name:=cbFont.Text;
  Image1.Canvas.Font.Size:=seFontSize.Value;
  GetFontDim;
  lbChar.Font.Name:=cbFont.Text;
  lbChar.Font.Size:=seFontSize.Value;
  lbChar.Width:= TexWidth;
  lbChar.Height:= TexHeight;

  DrawChar(seChar.Value);

end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
    GetCharData;
    Timer1.Enabled:=false;
end;

procedure TForm1.Timer2Timer(Sender: TObject);
begin
  Timer2.Enabled:=false;
  GetCharDataExp;

end;

procedure TForm1.Timer3Timer(Sender: TObject);
begin
  Timer3.Enabled:=false;
  CharExport;
end;

procedure TForm1.cbFontChange(Sender: TObject);
begin
  mmoSample.Font.Name:=cbFont.Text;
  mmoSample.Font.Size:=seFontSize.Value;
  mmoSample.Repaint;
  Image1.Canvas.Font.Name:=cbFont.Text;
  Image1.Canvas.Font.Size:=seFontSize.Value;
  GetFontDim;
  lbChar.Font.Name:=cbFont.Text;
  lbChar.Font.Size:=seFontSize.Value;
  lbChar.Font.Quality:= fqNonAntialiased;
  lbChar.Width:= TexWidth;
  lbChar.Height:= TexHeight;

  DrawChar(seChar.Value);

end;

procedure TForm1.btnDrawCharClick(Sender: TObject);
var
  c: byte;
begin
  c := byte(seChar.Value);
  DrawChar(c);
end;

procedure TForm1.CharExport;
begin
  if ((doExport) or (doSave)) and (ExpChar < 255) then ExpChar:= ExpChar + 1;
  if (doExport = false) and (ExpChar = 0) then doExport := true;
    Timer3.Enabled:=false;
    if doSave then
    begin
      doSave := false;
      mmoData.Lines.SaveToFile(FileName);
      inExport := false;
      cbFont.Enabled := true;
      seFontSize.Enabled := true;
      exit;
    end;
    if ExpChar = 255 then
    begin
      doExport := false;
      doSave := true;
    end;
    seChar.Value:= ExpChar;
    DrawCharExp(ExpChar);
    if doExport or doSave then Timer3.Enabled:=true;

end;

procedure TForm1.btnExportClick(Sender: TObject);
var
  s: string;
begin

  inExport := true;

  cbFont.Enabled:=false;
  seFontSize.Enabled:=false;

  doSave := false;
  FileName := Application.Location + cbFont.Text + '_' + seFontSize.Text + '.ufnt';
  mmoData.Clear;
  mmoData.Lines.Add('[Font]');
  s := 'FontName=' + cbFont.Text + '_' + seFontSize.Text;
  mmoData.Lines.Add(s);
  mmoData.Lines.Add('Mode=1Bpp');
  mmoData.Lines.Add('Type=Fixed');
  s := 'Width=' + IntToStr(TexWidth);
  mmoData.Lines.Add(s);
  s := 'Height=' + IntToStr(TexHeight);
  mmoData.Lines.Add(s);

  ExpChar := 0;
  Timer3.Enabled:=true;

end;


constructor TFontList.Create;
begin
inherited Create;
List := TStringList.Create;
List.Sorted := True;
end;

destructor TFontList.Destroy;
begin
ClearList;
inherited Destroy;
end;

procedure TFontList.ClearList;
begin
while List.Count > 0 do
begin
TFontInfo(List.Objects[0]).Free;
List.Delete(0);
end;
end;

function EnumFontsProc(var EnumLogFont: TEnumLogFont; var TextMetric: TNewTextMetric; FontType: Integer; Data: LPARAM): Integer; stdcall;
var
FontList : TFontList;
begin
FontList := TFontList(Data);
FontList.AddFont(EnumLogFont, TextMetric, FontType);
Result := 1;
end;

procedure TFontList.AddFont(EnumLogFont: TEnumLogFont; TextMetric: TNewTextMetric; FontType: Integer);
var
FI : TFontInfo;
begin
FI := TFontInfo.Create;

FI.FShortName := StrPas(EnumLogFont.elfLogFont.lfFaceName);
FI.FFullName := StrPas(EnumLogFont.elfFullName);
FI.FStyle := StrPas(EnumLogFont.elfStyle);
FI.FLF := EnumLogFont.elfLogFont;

case FontType of
RASTER_FONTTYPE : FI.FFontType := ftRaster;
DEVICE_FONTTYPE : FI.FFontType := ftDevice;
TRUETYPE_FONTTYPE : FI.FFontType := ftTrueType;
end;

FI.FTM := TextMetric;

List.AddObject(FI.FShortName, FI);
end;

procedure TFontList.RefreshFontInfo;
var
DC: HDC;
begin
ClearList;
DC := GetDC(0);
try
EnumFontFamilies(DC, nil, @EnumFontsProc,  PtrUInt(Self));
finally
ReleaseDC(0, DC);
end;
end;

end.

