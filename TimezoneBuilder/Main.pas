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
  StdCtrls,
  ExtCtrls,
  Registry;

type
  TfrmMain = class(TForm)
    mmoMain: TMemo;
    pnlMain: TPanel;
    cmdBuild: TButton;
    cmdExit: TButton;
    procedure cmdBuildClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cmdExitClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

type
 TTimezoneRec = record  {Represents the structure of the TZI value in the Registry}
  Bias:LongInt;
  StandardBias:LongInt;
  DaylightBias:LongInt;
  StandardStart:TSystemTime;
  DaylightStart:TSystemTime;
 end;

const
  keyTimezonesNT = '\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Time Zones';

var
  frmMain: TfrmMain;

function ConvertSingleQuotes(const AValue:String):String;

implementation

{$R *.lfm}

{==============================================================================}
{==============================================================================}

function ConvertSingleQuotes(const AValue:String):String;
var
 Count:Integer;
begin
 {}
 Result:='';

 for Count:=1 to Length(AValue) do
  begin
   {Check for Single Quotes}
   if AValue[Count] = '''' then
    begin
     {Duplicate Single Quotes}
     Result:=Result + AValue[Count] + AValue[Count];
    end
   else
    begin
     Result:=Result + AValue[Count];
    end;
  end;
end;

{==============================================================================}
{==============================================================================}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
 {}
 mmoMain.Text:='Click the build button to build Ultibo timezone information from the current computer';
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
 {Check PixelsPerInch}
 if PixelsPerInch > 96 then
  begin
   {Calculate Scale}
   Scale:=(PixelsPerInch / 96);

   {Disable Anchors}
   cmdBuild.Anchors:=[akLeft,akTop];
   cmdExit.Anchors:=[akLeft,akTop];

   {Resize Form}
   Width:=Trunc(Width * Scale);
   Height:=Trunc(Height * Scale);

   {Move Buttons}
   cmdBuild.Left:=pnlMain.Width - Trunc(171 * Scale); {663 - 492 = 171}
   cmdExit.Left:=pnlMain.Width - Trunc(87 * Scale);  {663 - 576 = 87}

   {Enable Anchors}
   cmdBuild.Anchors:=[akRight,akBottom];
   cmdExit.Anchors:=[akRight,akBottom];
  end;
end;

{==============================================================================}

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 {}

end;

{==============================================================================}

procedure TfrmMain.cmdBuildClick(Sender: TObject);
var
 Count:Integer;
 WorkBuffer:String;
 Registry:TRegistry;
 TimezoneRec:TTimezoneRec;
 TimezoneNames:TStringList;
begin
 {}
 cmdBuild.Enabled:=False;
 cmdExit.Enabled:=False;

 Registry:=TRegistry.Create;
 try
  {Clear Text}
  mmoMain.Text:='';

  {Get Timezone Names}
  TimezoneNames:=TStringList.Create;
  try
   Registry.RootKey:=HKEY_LOCAL_MACHINE;

   {Open Key}
   if Registry.OpenKeyReadOnly(keyTimezonesNT) then
    begin
     {Get Names}
     Registry.GetKeyNames(TimezoneNames);

     {Close Key}
     Registry.CloseKey;

     {Add Constants}
     mmoMain.Lines.Add('const');
     mmoMain.Lines.Add(' {Timezone count}');
     mmoMain.Lines.Add(' TIMEZONE_COUNT = ' + IntToStr(TimezoneNames.Count) + ';');
     mmoMain.Lines.Add('');

     {Add Types}
     mmoMain.Lines.Add('type');
     mmoMain.Lines.Add(' {Timezone List}');
     mmoMain.Lines.Add(' PTimezoneList = ^TTimezoneList;');
     mmoMain.Lines.Add(' TTimezoneList = record');
     mmoMain.Lines.Add('  TimezoneCount:LongWord;');
     mmoMain.Lines.Add('  TimezoneData:array[0..(TIMEZONE_COUNT - 1)] of TTimezoneData;');
     mmoMain.Lines.Add(' end;');
     mmoMain.Lines.Add('');

     {Add Header}
     mmoMain.Lines.Add('var');
     mmoMain.Lines.Add(' {Timezone List}');
     mmoMain.Lines.Add(' TimezoneList:TTimezoneList = (');
     mmoMain.Lines.Add('  TimezoneCount:' + IntToStr(TimezoneNames.Count) + ';');
     mmoMain.Lines.Add('  TimezoneData:(');
     mmoMain.Lines.Add('');

     {Check each Key}
     for Count:=0 to TimezoneNames.Count - 1 do
      begin
       {Open Subkey}
       if Registry.OpenKeyReadOnly(keyTimezonesNT + '\' + TimezoneNames.Strings[Count]) then
        begin
         {Check Display}
         if Registry.ValueExists('Display') then
          begin
           {Check TZI}
           if Registry.ValueExists('TZI') and (Registry.GetDataSize('TZI') >= SizeOf(TTimezoneRec)) then
            begin
             {Read Timezone Record}
             Registry.ReadBinaryData('TZI',TimezoneRec,SizeOf(TTimezoneRec));

             {Add Timezone}
             mmoMain.Lines.Add('   {' + TimezoneNames.Strings[Count] + '}');
             mmoMain.Lines.Add('   (Name:(''' + ConvertSingleQuotes(TimezoneNames.Strings[Count]) + ''');');
             mmoMain.Lines.Add('    Description:(''' + ConvertSingleQuotes(Registry.ReadString('Display')) + ''');');
             mmoMain.Lines.Add('    Bias:' + IntToStr(TimezoneRec.Bias) + ';');
             mmoMain.Lines.Add('    StandardName:(''' + ConvertSingleQuotes(Registry.ReadString('Std')) + ''');');
             mmoMain.Lines.Add('    StandardBias:' + IntToStr(TimezoneRec.StandardBias) + ';');

             WorkBuffer:='    StandardStart:(wYear:' + IntToStr(TimezoneRec.StandardStart.wYear);
             WorkBuffer:=WorkBuffer + ';wMonth:' + IntToStr(TimezoneRec.StandardStart.wMonth);
             WorkBuffer:=WorkBuffer + ';wDayOfWeek:' + IntToStr(TimezoneRec.StandardStart.wDayOfWeek);
             WorkBuffer:=WorkBuffer + ';wDay:' + IntToStr(TimezoneRec.StandardStart.wDay);
             WorkBuffer:=WorkBuffer + ';wHour:' + IntToStr(TimezoneRec.StandardStart.wHour);
             WorkBuffer:=WorkBuffer + ';wMinute:' + IntToStr(TimezoneRec.StandardStart.wMinute);
             WorkBuffer:=WorkBuffer + ';wSecond:' + IntToStr(TimezoneRec.StandardStart.wSecond);
             WorkBuffer:=WorkBuffer + ';wMilliseconds:' + IntToStr(TimezoneRec.StandardStart.wMilliseconds) + ');';
             mmoMain.Lines.Add(WorkBuffer);

             mmoMain.Lines.Add('    DaylightName:(''' + ConvertSingleQuotes(Registry.ReadString('Dlt')) + ''');');
             mmoMain.Lines.Add('    DaylightBias:' + IntToStr(TimezoneRec.DaylightBias) + ';');

             WorkBuffer:='    DaylightStart:(wYear:' + IntToStr(TimezoneRec.DaylightStart.wYear);
             WorkBuffer:=WorkBuffer + ';wMonth:' + IntToStr(TimezoneRec.DaylightStart.wMonth);
             WorkBuffer:=WorkBuffer + ';wDayOfWeek:' + IntToStr(TimezoneRec.DaylightStart.wDayOfWeek);
             WorkBuffer:=WorkBuffer + ';wDay:' + IntToStr(TimezoneRec.DaylightStart.wDay);
             WorkBuffer:=WorkBuffer + ';wHour:' + IntToStr(TimezoneRec.DaylightStart.wHour);
             WorkBuffer:=WorkBuffer + ';wMinute:' + IntToStr(TimezoneRec.DaylightStart.wMinute);
             WorkBuffer:=WorkBuffer + ';wSecond:' + IntToStr(TimezoneRec.DaylightStart.wSecond);
             WorkBuffer:=WorkBuffer + ';wMilliseconds:' + IntToStr(TimezoneRec.DaylightStart.wMilliseconds) + ');';
             mmoMain.Lines.Add(WorkBuffer);

             if Count = TimezoneNames.Count - 1 then
              begin
               mmoMain.Lines.Add('   )');
              end
             else
              begin
               mmoMain.Lines.Add('   ),');
              end;
             mmoMain.Lines.Add('');
            end;
          end;
         Registry.CloseKey;
        end;
      end;

     {Add Footer}
     mmoMain.Lines.Add('   )');
     mmoMain.Lines.Add('  );');
     mmoMain.Lines.Add('');
    end;
  finally
   TimezoneNames.Free;
  end;

  {Add Completed}
  mmoMain.Lines.Add('Build completed, select all of the above text and paste it into the Ultibo timezone module');
 finally
  Registry.Free;

  cmdBuild.Enabled:=True;
  cmdExit.Enabled:=True;
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
