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

unit Main;

{$MODE Delphi}

interface

uses
  LCLIntf,
  LCLType,
  LMessages,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    chkStandalone: TCheckBox;
    cmdConvert: TButton;
    cmdExit: TButton;
    lblSource: TLabel;
    txtSource: TEdit;
    lblDest: TLabel;
    txtDest: TEdit;
    lblOffset: TLabel;
    txtOffset: TEdit;
    lblSize: TLabel;
    txtSize: TEdit;
    lblSize2: TLabel;
    cmdSource: TButton;
    cmdDest: TButton;
    openMain: TOpenDialog;
    saveMain: TSaveDialog;
    lblName: TLabel;
    txtName: TEdit;
    procedure chkStandaloneClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure txtSourceChange(Sender: TObject);
    procedure cmdSourceClick(Sender: TObject);
    procedure txtDestChange(Sender: TObject);
    procedure cmdDestClick(Sender: TObject);
    procedure txtOffsetChange(Sender: TObject);
    procedure txtSizeChange(Sender: TObject);
    procedure cmdConvertClick(Sender: TObject);
    procedure cmdExitClick(Sender: TObject);
    procedure txtNameChange(Sender: TObject);
  private
    { Private declarations }
    FSource:String;
    FDest:String;
    FStandalone:Boolean;
    FName:String;
    FOffset:LongWord;
    FSize:LongWord;
  public
    { Public declarations }
    function Convert:Boolean;

    procedure WriteBuffer(AStream:TStream;const ABuffer:String);
  end;

const
  DefaultName = 'BinaryData';

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{==============================================================================}
{==============================================================================}

function TfrmMain.Convert:Boolean;
var
 ByteCount:LongWord;
 LineCount:LongWord;
 SourceFile:TFileStream;
 DestFile:TFileStream;
 WorkByte:Byte;
 WorkBuffer:String;
 ReadStart:LongWord;
 ReadSize:LongWord;
begin
 {}
 Result:=False;
 try
  Screen.Cursor:=crHourGlass;
  try
   {Check Name}
   if Length(FName) = 0 then FName:=DefaultName;

   {Check Source}
   if not FileExists(FSource) then Exit;

   {Check Dest}
   if FileExists(FDest) then
    begin
     if MessageDlg('Destination file already exists, overwrite ?',mtConfirmation,[mbYes,mbNo],0) <> mrYes then Exit;
     FileSetAttr(FDest,0);
    end;

   {Open Source}
   SourceFile:=TFileStream.Create(FSource,fmOpenRead or fmShareDenyNone);
   try
    {Open Dest}
    DestFile:=TFileStream.Create(FDest,fmCreate or fmShareDenyWrite);
    try
     ReadStart:=FOffset;
     if ReadStart > LongWord(SourceFile.Size - 1) then ReadStart:=(SourceFile.Size - 1);

     ReadSize:=SourceFile.Size;
     if FSize > 0 then ReadSize:=ReadStart + FSize;
     if ReadSize > LongWord(SourceFile.Size) then ReadSize:=SourceFile.Size;

     SourceFile.Position:=ReadStart;
     DestFile.Position:=0;

     if FStandalone then
      begin
       {Create Interface}
       WorkBuffer:='unit ' + ChangeFileExt(ExtractFileName(FDest),'') + ';';
       WriteBuffer(DestFile,WorkBuffer);

       WorkBuffer:='';
       WriteBuffer(DestFile,WorkBuffer);

       WorkBuffer:='interface';
       WriteBuffer(DestFile,WorkBuffer);

       WorkBuffer:='';
       WriteBuffer(DestFile,WorkBuffer);
      end;

     {Create Type and Var}
     WorkBuffer:='type';
     WriteBuffer(DestFile,WorkBuffer);

     WorkBuffer:=' T' + FName + ' = array[0..' + IntToStr(ReadSize - 1) + '] of Byte;';
     WriteBuffer(DestFile,WorkBuffer);

     WorkBuffer:='';
     WriteBuffer(DestFile,WorkBuffer);

     WorkBuffer:='var';
     WriteBuffer(DestFile,WorkBuffer);

     WorkBuffer:=' ' + FName + ':T' + FName + ' = (';
     WriteBuffer(DestFile,WorkBuffer);

     LineCount:=1;
     WorkBuffer:='  ';
     for ByteCount:=ReadStart to ReadSize - 1 do
      begin
       SourceFile.ReadBuffer(WorkByte,1);
       WorkBuffer:=WorkBuffer + '$' + IntToHex(WorkByte,2);

       {Check for Last Byte}
       if ByteCount < (ReadSize - 1) then
        begin
         WorkBuffer:=WorkBuffer + ',';
        end;

       Inc(LineCount);
       if LineCount > 16 then
        begin
         {Move to Next Line}
         WriteBuffer(DestFile,WorkBuffer);
         LineCount:=1;
         WorkBuffer:='  ';
        end
       else
        begin
         if ByteCount = (ReadSize - 1) then
          begin
           {Write Last Line}
           WriteBuffer(DestFile,WorkBuffer);
          end;
        end;
      end;

     WorkBuffer:=' );';
     WriteBuffer(DestFile,WorkBuffer);

     if FStandalone then
      begin
       {Create Implementation}
       WorkBuffer:='';
       WriteBuffer(DestFile,WorkBuffer);

       WorkBuffer:='implementation';
       WriteBuffer(DestFile,WorkBuffer);

       WorkBuffer:='';
       WriteBuffer(DestFile,WorkBuffer);

       WorkBuffer:=' // Nothing';
       WriteBuffer(DestFile,WorkBuffer);

       WorkBuffer:='';
       WriteBuffer(DestFile,WorkBuffer);

       WorkBuffer:='end.';
       WriteBuffer(DestFile,WorkBuffer);
      end;

     Result:=True;
    finally
     DestFile.Free;
    end;
   finally
    SourceFile.Free;
   end;
  finally
   Screen.Cursor:=crDefault;
  end;
 except
  {}
 end;
end;

{==============================================================================}

procedure TfrmMain.WriteBuffer(AStream:TStream;const ABuffer:String);
var
 WorkBuffer:String;
begin
 {}
 if AStream = nil then Exit;
 WorkBuffer:=ABuffer + Chr(13) + Chr(10);
 AStream.Seek(AStream.Size,soFromBeginning);
 AStream.WriteBuffer(WorkBuffer[1],Length(WorkBuffer));
end;

{==============================================================================}
{==============================================================================}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
 {}
 FSource:='';
 FDest:='';
 FStandalone:=False;
 FName:=DefaultName;
 FOffset:=0;
 FSize:=0;
end;

{==============================================================================}

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
 {}

end;

{==============================================================================}

procedure TfrmMain.FormShow(Sender: TObject);
var
 Scale:Double;
begin
 {}
 txtSource.Text:=FSource;
 txtDest.Text:=FDest;
 chkStandalone.Checked:=FStandalone;
 txtName.Text:=FName;
 txtOffset.Text:=IntToStr(FOffset);
 txtSize.Text:=IntToStr(FSize);

 {Adjust Labels}
 lblSource.Top:=txtSource.Top + ((txtSource.Height - lblSource.Height) div 2);
 lblDest.Top:=txtDest.Top + ((txtDest.Height - lblDest.Height) div 2);
 lblName.Top:=txtName.Top + ((txtName.Height - lblName.Height) div 2);
 lblOffset.Top:=txtOffset.Top + ((txtOffset.Height - lblOffset.Height) div 2);
 lblSize.Top:=txtSize.Top + ((txtSize.Height - lblSize.Height) div 2);
 lblSize2.Top:=txtSize.Top + ((txtSize.Height - lblSize2.Height) div 2);

 {Adjust Buttons}
 if txtSource.Height > cmdSource.Height then
  begin
   cmdSource.Height:=txtSource.Height;
   cmdSource.Width:=txtSource.Height;
   cmdSource.Top:=txtSource.Top + ((txtSource.Height - cmdSource.Height) div 2);
  end
 else
  begin
   cmdSource.Height:=txtSource.Height + 2;
   cmdSource.Width:=txtSource.Height + 2;
   cmdSource.Top:=txtSource.Top - 1;
  end;

 if txtDest.Height > cmdDest.Height then
  begin
   cmdDest.Height:=txtDest.Height;
   cmdDest.Width:=txtDest.Height;
   cmdDest.Top:=txtDest.Top + ((txtDest.Height - cmdDest.Height) div 2);
  end
 else
  begin
   cmdDest.Height:=txtDest.Height + 2;
   cmdDest.Width:=txtDest.Height + 2;
   cmdDest.Top:=txtDest.Top - 1;
  end;

 if cmdSource.Height > cmdConvert.Height then
  begin
   cmdConvert.Height:=cmdSource.Height;
   cmdExit.Height:=cmdSource.Height;
  end;

 {Check PixelsPerInch}
 if PixelsPerInch > 96 then
  begin
   {Calculate Scale}
   Scale:=(PixelsPerInch / 96);

   {Disable Anchors (Not required for dialogs)}
   {txtSource.Anchors:=[akLeft,akTop];
   cmdSource.Anchors:=[akLeft,akTop];
   txtDest.Anchors:=[akLeft,akTop];
   cmdDest.Anchors:=[akLeft,akTop];
   txtName.Anchors:=[akLeft,akTop];
   cmdConvert.Anchors:=[akLeft,akTop];
   cmdExit.Anchors:=[akLeft,akTop];}

   {Resize Form (Not required for dialogs)}
   {Width:=Trunc(Width * Scale);
   Height:=Trunc(Height * Scale);}

   {Adjust Buttons}
   {cmdSource.Top:=txtSource.Top;
   cmdSource.Height:=txtSource.Height;
   cmdSource.Width:=cmdSource.Height;
   cmdDest.Top:=txtDest.Top;
   cmdDest.Height:=txtDest.Height;
   cmdDest.Width:=cmdDest.Height;}

   {Enable Anchors (Not required for dialogs)}
   {txtSource.Anchors:=[akLeft,akTop,akRight];
   cmdSource.Anchors:=[akTop,akRight];
   txtDest.Anchors:=[akLeft,akTop,akRight];
   cmdDest.Anchors:=[akTop,akRight];
   txtName.Anchors:=[akLeft,akTop,akRight];
   cmdConvert.Anchors:=[akRight,akBottom];
   cmdExit.Anchors:=[akRight,akBottom];}
  end;
end;

{==============================================================================}

procedure TfrmMain.FormHide(Sender: TObject);
begin
 {}

end;

{==============================================================================}

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 {}

end;

{==============================================================================}
{==============================================================================}

procedure TfrmMain.txtSourceChange(Sender: TObject);
begin
 {}
 FSource:=txtSource.Text;
end;

{==============================================================================}

procedure TfrmMain.cmdSourceClick(Sender: TObject);
begin
 {}
 openMain.FileName:=FSource;
 openMain.InitialDir:=ExtractFileDir(Application.ExeName);
 {$IFDEF WINDOWS}
 openMain.Filter:='All Files (*.*)|*.*';
 {$ENDIF}
 {$IFDEF LINUX}
 openMain.Filter:='All Files (*.*)|*';
 {$ENDIF}
 if openMain.Execute then
  begin
   txtSource.Text:=openMain.FileName;
  end;
end;

{==============================================================================}

procedure TfrmMain.txtDestChange(Sender: TObject);
begin
 {}
 FDest:=txtDest.Text;
end;

{==============================================================================}

procedure TfrmMain.cmdDestClick(Sender: TObject);
begin
 {}
 saveMain.FileName:=FDest;
 saveMain.InitialDir:=ExtractFileDir(Application.ExeName);
 {$IFDEF WINDOWS}
 saveMain.Filter:='All Files (*.*)|*.*';
 {$ENDIF}
 {$IFDEF LINUX}
 saveMain.Filter:='All Files (*.*)|*';
 {$ENDIF}
 if saveMain.Execute then
  begin
   txtDest.Text:=saveMain.FileName;
  end;
end;

{==============================================================================}

procedure TfrmMain.chkStandaloneClick(Sender: TObject);
begin
 {}
 FStandalone:=chkStandalone.Checked;
end;

{==============================================================================}

procedure TfrmMain.txtNameChange(Sender: TObject);
begin
 {}
 FName:=txtName.Text;
end;

{==============================================================================}

procedure TfrmMain.txtOffsetChange(Sender: TObject);
begin
 {}
 try
  FOffset:=StrToInt(txtOffset.Text);
 except
  txtOffset.Text:=IntToStr(FOffset);
 end;
end;

{==============================================================================}

procedure TfrmMain.txtSizeChange(Sender: TObject);
begin
 {}
 try
  FSize:=StrToInt(txtSize.Text);
 except
  txtSize.Text:=IntToStr(FSize);
 end;
end;

{==============================================================================}

procedure TfrmMain.cmdConvertClick(Sender: TObject);
begin
 {}
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

procedure TfrmMain.cmdExitClick(Sender: TObject);
begin
 {}
 Application.Terminate;
end;

{==============================================================================}
{==============================================================================}

end.
