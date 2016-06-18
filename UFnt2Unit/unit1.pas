{
Program donated to the Ultibo project by Kerry Shipman  10 June 2016
Free to use for any Ultibo purpose
}
unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, IniFiles, StrUtils;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    OpenDialog1: TOpenDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { private declarations }
  public
    FontIni:TIniFile;
    FileStream:TFileStream;
    FontName:string;
    FontMode:string;
    FontType:string;
    FontWidth:integer;
    FontHeight:integer;
    function ExportFooter(AStream:TStream;const AUnitName:String):Boolean;
    function ExportHeader(AStream:TStream;const AUnitName:String):Boolean;
    function ExportData(AStream:TStream;AFont:TIniFile):Boolean;
    procedure doError(ErrorText:string);
    function WriteStream(AStream:TStream;const AText:String):Boolean;
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button2Click(Sender: TObject);
var
 s: string;
 b: boolean;
begin
 try
  { Export UFnt file to .pas unit compatible with Ultibo FontBuilder program }
  if Edit1.Text = '' then exit;
  FontIni := nil;

  FontIni := TIniFile.Create(Edit1.Text, false);

  if FontIni = nil then
   begin
    doError('Error loading Font Ini File');
    exit;
   end;

  if FontIni <> nil then
    begin
       FontName := FontIni.ReadString('Font', 'FontName', '');
       FontMode := FontIni.ReadString('Font', 'Mode', '');
       FontType := FontIni.ReadString('Font', 'Type', '');
       FontWidth := FontIni.ReadInteger('Font', 'Width', 0);
       FontHeight := FontIni.ReadInteger('Font', 'Height', 0);
       if (FontName = '') or (FontMode = '') or (FontType = '') or (FontWidth = 0) or (FontHeight = 0) then
        begin
         doError('Error Reading Font Header');
         exit;
        end;
       s := Edit1.Text;
       s := LeftStr(s,Length(s) - 4);
       s := s + 'pas';
       FileStream := TFileStream.Create(s,fmCreate);
       if FileStream = nil then
        begin
         doError('Error creating export unit file');
         exit;
        end;

       b :=  ExportHeader(FileStream,FontName);
       if not b then
         begin
            doError('Export Header Failed');
            exit;
         end;
       b :=  ExportData(FileStream,FontIni);
       if not b then
         begin
            doError('Export Data Failed');
            exit;
         end;
       b :=  ExportFooter(FileStream,FontName);
       if not b then
         begin
            doError('Export Footer Failed');
            exit;
         end;
       ShowMessage('Export Done');
    end;
 finally

    FileStream.Free;
 end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  b: boolean;
begin
  b := OpenDialog1.Execute;
  if b then
    Edit1.Text := OpenDialog1.FileName;
end;

function TForm1.WriteStream(AStream:TStream;const AText:String):Boolean;
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

procedure TForm1.doError(ErrorText:string);
begin
  { Error Popup }
  ShowMessage(ErrorText);
end;


{==============================================================================}

function TForm1.ExportHeader(AStream:TStream;const AUnitName:String):Boolean;
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
 WriteStream(AStream,' This font was originally from the file ' + AUnitName + '.ufnt');
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

function TForm1.ExportFooter(AStream:TStream;const AUnitName:String):Boolean;
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

function TForm1.ExportData(AStream:TStream;AFont:TIniFile):Boolean;
var
 Row:Integer;
 Count:Integer;
 Current:LongWord;

 Digits:Integer;
 Increment:Integer;
 WorkBuffer:String;
 s,s2: string;
 sl: TStringList;

begin
 {}
 Result:=False;
 sl := TStringList.Create;

 if AStream = nil then Exit;
 if AFont = nil then Exit;

 {Get Increment}
 Increment:=((FontWidth + 7) div 8);

 {Get Digits}
 Digits:=Increment * 2;

 WriteStream(FileStream,' ' + FontName + 'FontHeader:TFontHeader = (');
 WriteStream(FileStream,'  Width:' + IntToStr(FontWidth) + ';');
 WriteStream(FileStream,'  Height:' + IntToStr(FontHeight) + ';');
 WriteStream(FileStream,'  Count:' + IntToStr(256) + ';');
 WriteStream(FileStream,'  Mode:FONT_MODE_PIXEL;');
 WriteStream(FileStream,'  Flags:FONT_FLAG_BIGENDIAN or FONT_FLAG_RIGHTALIGN;');
 WriteStream(FileStream,'  Mask:0;');
 WriteStream(FileStream,'  CodePage:CP_ACP;');
 WriteStream(FileStream,'  Name:(''' + FontName + ''');');
 WriteStream(FileStream,'  Description:(''' + FontName + ''')');
 WriteStream(FileStream,'  );');
 WriteStream(FileStream,'');

 case FontWidth of
   1..8:begin
     WriteStream(FileStream,' ' + FontName + 'FontData:array[0..' + IntToStr(255) + ',0..' + IntToStr(FontHeight - 1) + '] of Byte = (');
    end;
   9..16:begin
     WriteStream(FileStream,' ' + FontName + 'FontData:array[0..' + IntToStr(255) + ',0..' + IntToStr(FontHeight - 1) + '] of Word = (');
    end;
   17..32:begin
     WriteStream(FileStream,' ' + FontName + 'FontData:array[0..' + IntToStr(255) + ',0..' + IntToStr(FontHeight - 1) + '] of LongWord = (');
    end;
  end;
 WorkBuffer:='   (';

 for Count:=0 to 255 do
  begin
   s := 'C' + IntToStr(Count);
   AFont.ReadSectionRaw(s,sl);
   for Row:=0 to sl.Count - 1 do
    begin

     s2 := '%' + sl.Strings[Row];
     Current := StrToInt(s2);
     if Row = 0 then
      begin
       WorkBuffer:=WorkBuffer + '$' + IntToHex(Current,Digits);
      end
     else
      begin
       WorkBuffer:=WorkBuffer + ', $' + IntToHex(Current,Digits);
      end;
    end;

   if Count < 255 then
    begin
     WorkBuffer:=WorkBuffer + '),';
    end
   else
    begin
     WorkBuffer:=WorkBuffer + ')';
    end;
   WriteStream(AStream,WorkBuffer);

   WorkBuffer:='   (';

  end;

 WriteStream(AStream,'  );');


 Result:=True;
end;

end.

