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

unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, StdCtrls;

const
 {Format constants}
 FONT_FORMAT_PSF = 0;
 FONT_FORMAT_CPI = 1;
 FONT_FORMAT_CP  = 2;
 FONT_FORMAT_RAW = 3;

 FONT_FORMAT_MAX = 3;
 FONT_FORMAT_DISPLAY_MAX = 0;

 FONT_FORMAT_NAMES:array[0..FONT_FORMAT_MAX] of String = (
  'PC Screen Font (PSF)',
  'Code Page Information (CPI)',
  'Code Page Entry (CP)',
  'Raw Font');

  //To Do //Raw font files and .fnt etc

 FONT_FORMAT_FILTERS:array[0..FONT_FORMAT_MAX] of String = (
  'PC Screen Font|*.psf?|All files|*.*',
  'Code Page Information|*.cpi|All files|*.*',
  'Code Page Entry|*.cp|All files|*.*',
  'All files|*.*');

 {Version constants}
 FONT_VERSION_PSF1 = 0;
 FONT_VERSION_PSF2 = 1;

const
 {PSF1 constants}
 PSF1_MAGIC0     = $36; {PSF1 magic numbers}
 PSF1_MAGIC1     = $04;

 PSF1_MODE512    = $01; {512 glyphs in the font instead of 256}
 PSF1_MODEHASTAB = $02; {Has unicode table following the font data}
 PSF1_MODEHASSEQ = $04;
 PSF1_MAXMODE    = $05;

 PSF1_SEPARATOR  = $FFFF;
 PSF1_STARTSEQ   = $FFFE;

type
 {PSF1 types}
 PPSF1Header = ^TPSF1Header;
 TPSF1Header = packed record
  Magic:array[0..1] of Byte;  {Magic number}
  Mode:Byte;                  {PSF font mode}
  CharSize:Byte;              {Character size (Bytes) (Width is always 8 so CharSize equals Height)}
 end;

const
 {PSF2 constants}
 PSF2_MAGIC0    = $72; {PSF2 magic numbers}
 PSF2_MAGIC1    = $b5;
 PSF2_MAGIC2    = $4a;
 PSF2_MAGIC3    = $86;

 {bits used in flags}
 PSF2_HAS_UNICODE_TABLE = $01;

 {max version recognized so far}
 PSF2_MAXVERSION = 0;

 {UTF8 separators}
 PSF2_SEPARATOR = $FF;
 PSF2_STARTSEQ  = $FE;

type
 {PSF2 types}
 PPSF2Header = ^TPSF2Header;
 TPSF2Header = packed record
  Magic:array[0..3] of Byte;
  Version:Integer;
  HeaderSize:Integer;    {Offset of bitmaps in file}
  Flags:Integer;
  Length:Integer;        {Number of glyphs}
  CharSize:Integer;      {Number of bytes for each character (CharSize = Height * ((Width + 7) div 8))}
  Height:Integer;        {Max height of glyphs}
  Width:Integer;         {Max width of glyphs}
 end;

type
  TfrmMain = class(TForm)
    openMain: TOpenDialog;
    StatusBar1: TStatusBar;
    pnlMain: TPanel;
    lblSource: TLabel;
    txtSource: TEdit;
    cmdSource: TButton;
    cmdOpen: TButton;
    cmdExport: TButton;
    cmbFormat: TComboBox;
    lblFormat: TLabel;
    imgMain: TImage;
    lblPreview: TLabel;
    saveMain: TSaveDialog;
    lblWidth: TLabel;
    lblHeight: TLabel;
    lblInformation: TLabel;
    lblCharSize: TLabel;
    lblLength: TLabel;
    lblUnicode: TLabel;
    lblVersion: TLabel;
    chkAllowPartial: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure txtSourceChange(Sender: TObject);
    procedure cmdSourceClick(Sender: TObject);
    procedure cmdOpenClick(Sender: TObject);
    procedure cmdExportClick(Sender: TObject);
    procedure cmbFormatChange(Sender: TObject);
    procedure chkAllowPartialClick(Sender: TObject);
  private
    { Private declarations }
    FSource:String;
    FFormat:Integer;
    FVersion:Integer;
    FAllowPartial:Boolean;
    
    FPSF1Header:TPSF1Header;
    FPSF2Header:TPSF2Header;
    FPSFFontData:Pointer;
    FPSFUnicodeData:Pointer;

    FBitmap:TBitmap;

    function WriteStream(AStream:TStream;const AText:String):Boolean;
  public
    { Public declarations }
    function OpenPSF(const AFileName:String):Boolean;
    function OpenCPI(const AFileName:String):Boolean;
    function OpenCP(const AFileName:String):Boolean;

    function ExportPSF(const AName,ADescription,AUnitName,AFileName:String;AUnicode:Boolean):Boolean;
    function ExportCPI(const AName,ADescription,AUnitName,AFileName:String;AUnicode:Boolean):Boolean;
    function ExportCP(const AName,ADescription,AUnitName,AFileName:String;AUnicode:Boolean):Boolean;
    function ExportHeader(AStream:TStream;const AUnitName:String):Boolean;
    function ExportFooter(AStream:TStream;const AUnitName:String):Boolean;
    function ExportBitmap(AStream:TStream;AData:Pointer;AWidth,AHeight,ACharSize,ALength:Integer):Boolean;

    function DisplayPSF:Boolean;
    function DisplayCPI:Boolean;
    function DisplayCP:Boolean;
    function DisplayBitmap(Data:Pointer;Width,Height,CharSize,Length:Integer):Boolean;
  end;

var
  frmMain: TfrmMain;

implementation

uses DlgExport;

{$R *.DFM}

{==============================================================================}

function TfrmMain.WriteStream(AStream:TStream;const AText:String):Boolean;
var
 WorkBuffer:String;
begin
 {}
 Result:=False;

 if AStream = nil then Exit;

 WorkBuffer:=AText + Chr(13) + Chr(10);

 AStream.Seek(AStream.Size,soFromBeginning);
 AStream.WriteBuffer(WorkBuffer[1],Length(WorkBuffer));
end;

{==============================================================================}

function TfrmMain.OpenPSF(const AFileName:String):Boolean;
var
 DataSize:Integer;
 FileStream:TFileStream;
begin
 {}
 Result:=False;
 try
  if FileExists(AFileName) then
   begin
    {Open File}
    FileStream:=TFileStream.Create(AFileName,fmOpenRead or fmShareDenyNone);
    try
     {Check Size}
     if FileStream.Size < SizeOf(TPSF1Header) then Exit;

     {Read PSF1 Header}
     FileStream.Position:=0;
     FileStream.ReadBuffer(FPSF1Header,SizeOf(TPSF1Header));

     {Check Magic}
     if (FPSF1Header.Magic[0] = PSF1_MAGIC0) and (FPSF1Header.Magic[1] = PSF1_MAGIC1) then
      begin
       {PSF1 File}
       FVersion:=FONT_VERSION_PSF1;

       {Get Data Size}
       if (FPSF1Header.Mode and PSF1_MODE512) = 0 then
        begin
         DataSize:=FPSF1Header.CharSize * 256;
        end
       else
        begin
         DataSize:=FPSF1Header.CharSize * 512;
        end;

       {Allocate Font Data}
       GetMem(FPSFFontData,DataSize);

       {Read Font Data}
       FileStream.Position:=SizeOf(TPSF1Header);
       FileStream.ReadBuffer(FPSFFontData^,DataSize);

       {Check Unicode}
       //To Do //Allocate Unicode Data / Read Unicode Data

       Result:=True;
      end
     else
      begin
       {Read PSF2 Header}
       FileStream.Position:=0;
       FileStream.ReadBuffer(FPSF2Header,SizeOf(TPSF2Header));

       {Check Magic}
       if (FPSF2Header.Magic[0] = PSF2_MAGIC0) and (FPSF2Header.Magic[1] = PSF2_MAGIC1) and (FPSF2Header.Magic[2] = PSF2_MAGIC2) and (FPSF2Header.Magic[3] = PSF2_MAGIC3) then
        begin
         {Check Version}
         if FPSF2Header.Version <> PSF2_MAXVERSION then Exit;

         {Check Size}
         if FPSF2Header.CharSize <> (FPSF2Header.Height * ((FPSF2Header.Width + 7) div 8)) then Exit;

         {Check Length}
         if (FPSF2Header.Length < 256) and not(FAllowPartial) then Exit;
         
         {PSF2 File}
         FVersion:=FONT_VERSION_PSF2;

         {Allocate Font Data}
         GetMem(FPSFFontData,FPSF2Header.CharSize * FPSF2Header.Length);

         {Read Font Data}
         FileStream.Position:=FPSF2Header.HeaderSize;
         FileStream.ReadBuffer(FPSFFontData^,FPSF2Header.CharSize * FPSF2Header.Length);

         {Check Unicode}
         //To Do //Allocate Unicode Data / Read Unicode Data

         Result:=True;
        end;
      end;
    finally
     FileStream.Free;
    end;
   end;
 except
  {}
 end;
end;

{==============================================================================}

function TfrmMain.OpenCPI(const AFileName:String):Boolean;
begin
 {}
 Result:=False;

 //To Do
end;

{==============================================================================}

function TfrmMain.OpenCP(const AFileName:String):Boolean;
begin
 {}
 Result:=False;

 //To Do
end;

{==============================================================================}

function TfrmMain.ExportPSF(const AName,ADescription,AUnitName,AFileName:String;AUnicode:Boolean):Boolean;
var
 Count:Integer;
 FileStream:TFileStream;
begin
 {}
 Result:=False;
 try
  if Length(AName) = 0 then Exit;
  if Length(AUnitName) = 0 then Exit;
  if Length(AFileName) = 0 then Exit;

  if FFormat <> FONT_FORMAT_PSF then Exit;

  if FPSFFontData <> nil then
   begin
    {Check File}
    if FileExists(AFileName) then
     begin
      DeleteFile(AFileName);
      if FileExists(AFileName) then Exit;
     end;

    {Open File}
    FileStream:=TFileStream.Create(AFileName,fmCreate);
    try
     {Header}
     if not ExportHeader(FileStream,AUnitName) then Exit;

     {Check Version}
     case FVersion of
      FONT_VERSION_PSF1:begin
        {Get Count}
        if (FPSF1Header.Mode and PSF1_MODE512) = 0 then
         begin
          Count:=256;
         end
        else
         begin
          Count:=512;
         end;

        {Header}
        WriteStream(FileStream,' ' + AUnitName + 'FontHeader:TFontHeader = (');
        WriteStream(FileStream,'  Width:' + IntToStr(8) + ';');
        WriteStream(FileStream,'  Height:' + IntToStr(FPSF1Header.CharSize) + ';');
        WriteStream(FileStream,'  Count:' + IntToStr(Count) + ';');
        WriteStream(FileStream,'  Mode:FONT_MODE_PIXEL;');
        WriteStream(FileStream,'  Flags:FONT_FLAG_NONE;'); //To Do //Unicode
        WriteStream(FileStream,'  Mask:0;');
        WriteStream(FileStream,'  CodePage:CP_ACP;');
        WriteStream(FileStream,'  Name:(''' + AName + ''');');
        WriteStream(FileStream,'  Description:(''' + ADescription + ''')');
        WriteStream(FileStream,'  );');
        WriteStream(FileStream,'');

        {Data}
        WriteStream(FileStream,' ' + AUnitName + 'FontData:array[0..' + IntToStr(Count - 1) + ',0..' + IntToStr(FPSF1Header.CharSize - 1) + '] of Byte = (');

        {Bitmap}
        if not ExportBitmap(FileStream,FPSFFontData,8,FPSF1Header.CharSize,FPSF1Header.CharSize,Count) then Exit;

        {Unicode}
        if AUnicode then
         begin
          //To Do //ExportUnicode
         end;
       end;
      FONT_VERSION_PSF2:begin
        {Header}
        WriteStream(FileStream,' ' + AUnitName + 'FontHeader:TFontHeader = (');
        WriteStream(FileStream,'  Width:' + IntToStr(FPSF2Header.Width) + ';');
        WriteStream(FileStream,'  Height:' + IntToStr(FPSF2Header.Height) + ';');
        WriteStream(FileStream,'  Count:' + IntToStr(FPSF2Header.Length) + ';');
        WriteStream(FileStream,'  Mode:FONT_MODE_PIXEL;');
        WriteStream(FileStream,'  Flags:FONT_FLAG_NONE;'); //To Do //Unicode
        WriteStream(FileStream,'  Mask:0;');
        WriteStream(FileStream,'  CodePage:CP_ACP;');
        WriteStream(FileStream,'  Name:(''' + AName + ''');');
        WriteStream(FileStream,'  Description:(''' + ADescription + ''')');
        WriteStream(FileStream,'  );');
        WriteStream(FileStream,'');

        {Data}
        case FPSF2Header.Width of
         1..8:begin
           WriteStream(FileStream,' ' + AUnitName + 'FontData:array[0..' + IntToStr(FPSF2Header.Length - 1) + ',0..' + IntToStr(FPSF2Header.Height - 1) + '] of Byte = (');
          end;
         9..16:begin
           WriteStream(FileStream,' ' + AUnitName + 'FontData:array[0..' + IntToStr(FPSF2Header.Length - 1) + ',0..' + IntToStr(FPSF2Header.Height - 1) + '] of Word = (');
          end;
         17..32:begin
           WriteStream(FileStream,' ' + AUnitName + 'FontData:array[0..' + IntToStr(FPSF2Header.Length - 1) + ',0..' + IntToStr(FPSF2Header.Height - 1) + '] of LongWord = (');
          end;
        end;

        {Bitmap}
        if not ExportBitmap(FileStream,FPSFFontData,FPSF2Header.Width,FPSF2Header.Height,FPSF2Header.CharSize,FPSF2Header.Length) then Exit;

        {Unicode}
        if AUnicode then
         begin
          //To Do //ExportUnicode
         end;
       end;
     end;

     {Footer}
     if not ExportFooter(FileStream,AUnitName) then Exit;

     Result:=True;
    finally
     FileStream.Free;
    end;
   end;
 except
  {}
 end;
end;

{==============================================================================}

function TfrmMain.ExportCPI(const AName,ADescription,AUnitName,AFileName:String;AUnicode:Boolean):Boolean;
begin
 {}
 Result:=False;

 //To Do
end;

{==============================================================================}

function TfrmMain.ExportCP(const AName,ADescription,AUnitName,AFileName:String;AUnicode:Boolean):Boolean;
begin
 {}
 Result:=False;

 //To Do
end;

{==============================================================================}

function TfrmMain.ExportHeader(AStream:TStream;const AUnitName:String):Boolean;
begin
 {}
 Result:=False;

 if AStream = nil then Exit;

 if Length(AUnitName) = 0 then Exit;

 WriteStream(AStream,'{');
 WriteStream(AStream,'Ultibo ' + AUnitName + ' font unit.');
 WriteStream(AStream,'');
 WriteStream(AStream,'Arch');
 WriteStream(AStream,'====');
 WriteStream(AStream,'');
 WriteStream(AStream,' <All>');
 WriteStream(AStream,'');
 WriteStream(AStream,'Boards');
 WriteStream(AStream,'======');
 WriteStream(AStream,'');
 WriteStream(AStream,' <All>');
 WriteStream(AStream,'');
 WriteStream(AStream,'Licence');
 WriteStream(AStream,'=======');
 WriteStream(AStream,'');
 WriteStream(AStream,' LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)');
 WriteStream(AStream,'');
 WriteStream(AStream,'Credits');
 WriteStream(AStream,'=======');
 WriteStream(AStream,'');
 WriteStream(AStream,' This font was originally from the file ' + ExtractFileName(FSource));
 WriteStream(AStream,'');
 WriteStream(AStream,'}');
 WriteStream(AStream,'');
 WriteStream(AStream,'{$mode delphi} {Default to Delphi compatible syntax}');
 WriteStream(AStream,'{$H+}          {Default to AnsiString}');
 WriteStream(AStream,'{$inline on}   {Allow use of Inline procedures}');
 WriteStream(AStream,'');
 WriteStream(AStream,'unit ' + AUnitName + ';');
 WriteStream(AStream,'');
 WriteStream(AStream,'interface');
 WriteStream(AStream,'');
 WriteStream(AStream,'uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Font;');
 WriteStream(AStream,'');
 WriteStream(AStream,'{==============================================================================}');
 WriteStream(AStream,'{const}');
 WriteStream(AStream,' {' + AUnitName + ' specific constants}');
 WriteStream(AStream,'');
 WriteStream(AStream,'{==============================================================================}');
 WriteStream(AStream,'{type}');
 WriteStream(AStream,' {' + AUnitName + ' specific types}');
 WriteStream(AStream,'');
 WriteStream(AStream,'{==============================================================================}');
 WriteStream(AStream,'{var}');
 WriteStream(AStream,' {' + AUnitName + ' specific variables}');
 WriteStream(AStream,'');
 WriteStream(AStream,'{==============================================================================}');
 WriteStream(AStream,'{Initialization Functions}');
 WriteStream(AStream,'');
 WriteStream(AStream,'{==============================================================================}');
 WriteStream(AStream,'{==============================================================================}');
 WriteStream(AStream,'');
 WriteStream(AStream,'implementation');
 WriteStream(AStream,'');
 WriteStream(AStream,'{==============================================================================}');
 WriteStream(AStream,'{==============================================================================}');
 WriteStream(AStream,'var');
 WriteStream(AStream,' {' + AUnitName + ' specific variables}');
 WriteStream(AStream,' ' + AUnitName + 'Initialized:Boolean;');
 WriteStream(AStream,'');

 Result:=True;
end;

{==============================================================================}

function TfrmMain.ExportFooter(AStream:TStream;const AUnitName:String):Boolean;
begin
 {}
 Result:=False;

 if AStream = nil then Exit;

 if Length(AUnitName) = 0 then Exit;

 WriteStream(AStream,'');
 WriteStream(AStream,'{==============================================================================}');
 WriteStream(AStream,'{==============================================================================}');
 WriteStream(AStream,'{Initialization Functions}');
 WriteStream(AStream,'procedure ' + AUnitName + 'Init;');
 WriteStream(AStream,'begin');
 WriteStream(AStream,' {}');
 WriteStream(AStream,' {Check Initialized}');
 WriteStream(AStream,' if ' + AUnitName + 'Initialized then Exit;');
 WriteStream(AStream,'');
 WriteStream(AStream,' {Load ' + AUnitName + '}');
 WriteStream(AStream,' if FontLoad(@' + AUnitName + 'FontHeader,@' + AUnitName + 'FontData,SizeOf(' + AUnitName + 'FontData)) = INVALID_HANDLE_VALUE then');
 WriteStream(AStream,'  begin');
 WriteStream(AStream,'   if PLATFORM_LOG_ENABLED then PlatformLogError(''Failed to load ' + AUnitName + ' font'');');
 WriteStream(AStream,'  end;');
 WriteStream(AStream,'');
 WriteStream(AStream,' ' + AUnitName + 'Initialized:=True;');
 WriteStream(AStream,'end;');
 WriteStream(AStream,'');
 WriteStream(AStream,'{==============================================================================}');
 WriteStream(AStream,'{==============================================================================}');
 WriteStream(AStream,'');
 WriteStream(AStream,'initialization');
 WriteStream(AStream,' ' + AUnitName + 'Init;');
 WriteStream(AStream,'');
 WriteStream(AStream,'{==============================================================================}');
 WriteStream(AStream,'');
 WriteStream(AStream,'finalization');
 WriteStream(AStream,' {Nothing}');
 WriteStream(AStream,'');
 WriteStream(AStream,'{==============================================================================}');
 WriteStream(AStream,'{==============================================================================}');
 WriteStream(AStream,'');
 WriteStream(AStream,'end.');

 Result:=True;
end;

{==============================================================================}

function TfrmMain.ExportBitmap(AStream:TStream;AData:Pointer;AWidth,AHeight,ACharSize,ALength:Integer):Boolean;
var
 Row:Integer;
 Count:Integer;
 Offset:LongWord;
 Current:LongWord;

 Mask:LongWord;
 Digits:Integer;
 Increment:Integer;

 WorkBuffer:String;
begin
 {}
 Result:=False;

 if AStream = nil then Exit;
 if AData = nil then Exit;

 {Get Increment}
 Increment:=((AWidth + 7) div 8);

 {Get Digits}
 Digits:=Increment * 2;

 {Get Mask}
 Mask:=(1 shl (Increment * 8)) - 1;

 WorkBuffer:='   (';

 Offset:=0;
 for Count:=1 to ALength do
  begin
   for Row:=0 to AHeight - 1 do
    begin
     Current:=(PLongWord(LongWord(AData) + Offset + (Row * Increment))^ and Mask);

     if Row = 0 then
      begin
       WorkBuffer:=WorkBuffer + '$' + IntToHex(Current,Digits);
      end
     else
      begin
       WorkBuffer:=WorkBuffer + ', $' + IntToHex(Current,Digits);
      end;
    end;

   if Count < ALength then
    begin
     WorkBuffer:=WorkBuffer + '),';
    end
   else
    begin
     WorkBuffer:=WorkBuffer + ')';
    end;
   WriteStream(AStream,WorkBuffer);

   WorkBuffer:='   (';

   Inc(Offset,ACharSize);
  end;

 WriteStream(AStream,'  );');


 Result:=True;
end;

{==============================================================================}

function TfrmMain.DisplayPSF:Boolean;
begin
 {}
 Result:=False;

 if FFormat <> FONT_FORMAT_PSF then Exit;

 if FPSFFontData <> nil then
  begin
   {Check Version}
   case FVersion of
    FONT_VERSION_PSF1:begin
      {Version}
      lblVersion.Caption:='Version: FONT_VERSION_PSF1';
      {Width}
      lblWidth.Caption:='Character Width: 8';
      {Height}
      lblHeight.Caption:='Character Height: ' + IntToStr(FPSF1Header.CharSize);
      {CharSize}
      lblCharSize.Caption:='Character Size: ' + IntToStr(FPSF1Header.CharSize);
      {Length}
      if (FPSF1Header.Mode and PSF1_MODE512) = 0 then
       begin
        lblLength.Caption:='Number of Glyphs: 256';
       end
      else
       begin
        lblLength.Caption:='Number of Glyphs: 512';
       end;
      {Unicode}
      if (FPSF1Header.Mode and PSF1_MODEHASTAB) = 0 then
       begin
        lblUnicode.Caption:='Has Unicode Table: No';
       end
      else
       begin
        lblUnicode.Caption:='Has Unicode Table: Yes';
       end;

      {Bitmap}
      if (FPSF1Header.Mode and PSF1_MODE512) = 0 then
       begin
        DisplayBitmap(FPSFFontData,8,FPSF1Header.CharSize,FPSF1Header.CharSize,256);
       end
      else
       begin
        DisplayBitmap(FPSFFontData,8,FPSF1Header.CharSize,FPSF1Header.CharSize,512);
       end;
     end;
    FONT_VERSION_PSF2:begin
      {Version}
      lblVersion.Caption:='Version: FONT_VERSION_PSF2 (' + IntToStr(FPSF2Header.Version) + ')';
      {Width}
      lblWidth.Caption:='Character Width: ' + IntToStr(FPSF2Header.Width);
      {Height}
      lblHeight.Caption:='Character Height: ' + IntToStr(FPSF2Header.Height);
      {CharSize}
      lblCharSize.Caption:='Character Size: ' + IntToStr(FPSF2Header.CharSize);
      {Length}
      lblLength.Caption:='Number of Glyphs: ' + IntToStr(FPSF2Header.Length);
      {Unicode}
      if (FPSF2Header.Flags and PSF2_HAS_UNICODE_TABLE) = 0 then
       begin
        lblUnicode.Caption:='Has Unicode Table: No';
       end
      else
       begin
        lblUnicode.Caption:='Has Unicode Table: Yes';
       end;

      {Bitmap}
      DisplayBitmap(FPSFFontData,FPSF2Header.Width,FPSF2Header.Height,FPSF2Header.CharSize,FPSF2Header.Length);
     end;
   end;
  end
 else
  begin
   {Version}
   lblVersion.Caption:='Version: 0';
   {Width}
   lblWidth.Caption:='Character Width: 0';
   {Height}
   lblHeight.Caption:='Character Height: 0';
   {CharSize}
   lblCharSize.Caption:='Character Size: 0';
   {Length}
   lblLength.Caption:='Number of Glyphs: 0';
   {Unicode}
   lblUnicode.Caption:='Has Unicode Table: No';

   {Bitmap}
   imgMain.Canvas.Brush.Color:=clBtnFace;
   imgMain.Canvas.Brush.Style:=bsSolid;
   imgMain.Canvas.FillRect(imgMain.ClientRect);
  end;

 Result:=True;
end;

{==============================================================================}

function TfrmMain.DisplayCPI:Boolean;
begin
 {}
 Result:=False;

 //To Do
end;

{==============================================================================}

function TfrmMain.DisplayCP:Boolean;
begin
 {}
 Result:=False;

 //To Do
end;

{==============================================================================}

function TfrmMain.DisplayBitmap(Data:Pointer;Width,Height,CharSize,Length:Integer):Boolean;
var
 X:Integer;
 Y:Integer;
 Row:Integer;
 Column:Integer;
 Count:Integer;
 Offset:LongWord;
 Current:LongWord;

 Rows:Integer;
 Columns:Integer;
 Shift:Integer;
 Increment:Integer;
begin
 {}
 Result:=False;

 if Data = nil then Exit;

 {Fill Background}
 imgMain.Canvas.Brush.Color:=clWhite;
 imgMain.Canvas.Brush.Style:=bsSolid;
 imgMain.Canvas.FillRect(imgMain.ClientRect);

 {Draw Characters}
 imgMain.Canvas.Pen.Color:=clBlack;
 imgMain.Canvas.Pen.Style:=psSolid;

 {Get Rows}
 Rows:=(imgMain.ClientHeight div Height) - 2;

 {Get Columns}
 Columns:=(imgMain.ClientWidth div Width) - 2;

 {Get Increment}
 Increment:=((Width + 7) div 8);

 {Get Shift}
 Shift:=(Increment * 8) - Width;

 Count:=0;
 Offset:=0;
 for Row:=1 to Rows do
  begin
   for Column:=1 to Columns do
    begin
     {Draw Character}
     for Y:=0 to Height - 1 do
      begin
       {Get Row}
       Current:=PLongWord(LongWord(Data) + Offset + (Y * Increment))^;

       if Increment > 1 then
        begin
         Current:=Swap(Current);
         Current:=Current shr Shift;
        end;

       for X:=Width - 1 downto 0 do
        begin
         if (Current and 1) = 1 then
          begin
           imgMain.Canvas.Pixels[(Column * Width) + X,(Row * Height) + Y]:=clBlack;
          end;

         Current:=Current shr 1;
        end;
      end;

     {Update Count}
     Inc(Count);
     if Count = Length then Break;

     {Update Offset}
     Inc(Offset,CharSize);
    end;

   if Count = Length then Break;
  end;

 Result:=True;
end;

{==============================================================================}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
 {}
 FSource:='';
 FFormat:=FONT_FORMAT_PSF;
 FVersion:=FONT_VERSION_PSF2;
 FAllowPartial:=False;

 FPSFFontData:=nil;
 FPSFUnicodeData:=nil;

 FBitmap:=TBitmap.Create;
 FBitmap.Width:=imgMain.ClientWidth;
 FBitmap.Height:=imgMain.ClientHeight;
end;

{==============================================================================}

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
 {}
 if FPSFFontData <> nil then FreeMem(FPSFFontData);
 FPSFFontData:=nil;

 if FPSFUnicodeData <> nil then FreeMem(FPSFUnicodeData);
 FPSFUnicodeData:=nil;
end;

{==============================================================================}

procedure TfrmMain.FormShow(Sender: TObject);
var
 Count:Integer;
begin
 {}
 cmbFormat.Clear;
 for Count:=0 to FONT_FORMAT_DISPLAY_MAX do
  begin
   cmbFormat.Items.Add(FONT_FORMAT_NAMES[Count]);
  end;

 txtSource.Text:=FSource;
 cmbFormat.ItemIndex:=FFormat;
 chkAllowPartial.Checked:=FAllowPartial;
 
 cmdOpen.Enabled:=False;
 cmdExport.Enabled:=False;

 imgMain.Picture.Bitmap:=FBitmap;
 imgMain.Canvas.Brush.Color:=clBtnFace;
 imgMain.Canvas.Brush.Style:=bsSolid;
 imgMain.Canvas.FillRect(imgMain.ClientRect);
end;

{==============================================================================}

procedure TfrmMain.FormHide(Sender: TObject);
begin
 {}

end;

{==============================================================================}
{==============================================================================}

procedure TfrmMain.txtSourceChange(Sender: TObject);
begin
 {}
 FSource:=txtSource.Text;
 cmdOpen.Enabled:=Length(FSource) <> 0;
end;

{==============================================================================}

procedure TfrmMain.cmdSourceClick(Sender: TObject);
begin
 {}
 openMain.Title:='Select font';
 openMain.Filter:=FONT_FORMAT_FILTERS[FFormat];
 if Length(openMain.InitialDir) = 0 then
  begin
   openMain.InitialDir:=ExtractFilePath(Application.ExeName);
  end;
 if openMain.Execute then
  begin
   txtSource.Text:=openMain.FileName;
  end;
end;

{==============================================================================}

procedure TfrmMain.cmbFormatChange(Sender: TObject);
begin
 {}
 if cmbFormat.ItemIndex = -1 then Exit;

 FFormat:=cmbFormat.ItemIndex;
end;

{==============================================================================}

procedure TfrmMain.chkAllowPartialClick(Sender: TObject);
begin
 {}
 FAllowPartial:=chkAllowPartial.Checked;
end;

{==============================================================================}

procedure TfrmMain.cmdOpenClick(Sender: TObject);
begin
 {}
 {Check Data}
 if FPSFFontData <> nil then FreeMem(FPSFFontData);
 FPSFFontData:=nil;

 {Check Unicode}
 if FPSFUnicodeData <> nil then FreeMem(FPSFUnicodeData);
 FPSFUnicodeData:=nil;

 {Disable Export}
 cmdExport.Enabled:=False;

 {Check Source}
 if FileExists(FSource) then
  begin
   case FFormat of
    FONT_FORMAT_PSF:begin
      if OpenPSF(FSource) then
       begin
        DisplayPSF;
        cmdExport.Enabled:=True;
       end
      else
       begin
        DisplayPSF;
        MessageDlg('Unable to open ' + FSource + ' as a PSF format font',mtInformation,[mbOk],0);
       end;
     end;
    FONT_FORMAT_CPI:begin
      if OpenCPI(FSource) then
       begin
        DisplayCPI;
        cmdExport.Enabled:=True;
       end
      else
       begin
        DisplayCPI;
        MessageDlg('Unable to open ' + FSource + ' as a CPI format font',mtInformation,[mbOk],0);
       end;
     end;
    FONT_FORMAT_CP:begin
      if OpenCP(FSource) then
       begin
        DisplayCP;
        cmdExport.Enabled:=True;
       end
      else
       begin
        DisplayCP;
        MessageDlg('Unable to open ' + FSource + ' as a CP format font',mtInformation,[mbOk],0);
       end;
     end;
   end;
  end
 else
  begin
   MessageDlg('The file ' + FSource + ' does not exist',mtInformation,[mbOk],0);
  end;
end;

{==============================================================================}

procedure TfrmMain.cmdExportClick(Sender: TObject);
var
 Name:String;
 Description:String;
 UnitName:String;
 FileName:String;
 IncludeUnicode:Boolean;
begin
 {}
 Name:=ExtractFileName(ChangeFileExt(FSource,''));
 Description:='';
 UnitName:=frmExport.NormalizeUnitName(Name);
 FileName:=ExtractFilePath(FSource) + UnitName + '.pas';
 IncludeUnicode:=True;

 if frmExport.ShowExport(Name,Description,UnitName,FileName,IncludeUnicode) then
  begin
   if Length(Name) = 0 then
    begin
     MessageDlg('Font name cannot be blank',mtInformation,[mbOk],0);
     Exit;
    end;

   if Length(UnitName) = 0 then
    begin
     MessageDlg('Unit name cannot be blank',mtInformation,[mbOk],0);
     Exit;
    end;

   if Length(FileName) = 0 then
    begin
     MessageDlg('File name cannot be blank',mtInformation,[mbOk],0);
     Exit;
    end;

   if not frmExport.CheckUnitName(UnitName) then
    begin
     MessageDlg('Unit name must contain only letters, numbers, underscores and dots',mtInformation,[mbOk],0);
     Exit;
    end;

   case FFormat of
    FONT_FORMAT_PSF:begin
      if not ExportPSF(Name,Description,UnitName,FileName,IncludeUnicode) then
       begin
        MessageDlg('Unable to export PSF format font to file ' + FileName,mtInformation,[mbOk],0);
       end;
     end;
    FONT_FORMAT_CPI:begin
      if not ExportPSF(Name,Description,UnitName,FileName,IncludeUnicode) then
       begin
        MessageDlg('Unable to export CPI format font to file ' + FileName,mtInformation,[mbOk],0);
       end;
     end;
    FONT_FORMAT_CP:begin
      if not ExportPSF(Name,Description,UnitName,FileName,IncludeUnicode) then
       begin
        MessageDlg('Unable to export CP format font to file ' + FileName,mtInformation,[mbOk],0);
       end;
     end;
   end;
  end; 
end;

{==============================================================================}
{==============================================================================}

end.
