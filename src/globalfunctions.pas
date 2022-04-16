unit globalfunctions;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms;

function IntegerToString(value: integer): String;
function StringToInteger(value: String): Integer;
function GetApplicationFolder():String;
function GetApplicationFile(FileName: String):String;
function LoadApplicationTextFile(FileName:String):String;

implementation

function IntegerToString(value: integer): String;
begin
  Result:= '';
  try
   Result:= IntToStr(value);
  except
  end;
end;

function StringToInteger(value: String): Integer;
begin
  Result:= 0;
  try
     Result:= StrToInt(value);
  except
  end;
end;

function GetApplicationFolder: String;
var
  FolderName: String;
begin
  FolderName:= ExtractFilePath(Application.ExeName);
  Result:= FolderName;
end;

function GetApplicationFile(FileName: String): String;
var
  AppFolder, FullPath: String;
begin
  AppFolder:= GetApplicationFolder;
  FullPath:= ConcatPaths([AppFolder, FileName]);
  Result:= FullPath;
end;

function LoadApplicationTextFile(FileName: String): String;
var
  FullPath: String;
  StringList: TStringList;
begin
  FullPath:= GetApplicationFile(FileName);
  Result:= '';

  if (FileExists(FullPath)) then
  begin
    StringList:= TStringList.Create;
    StringList.LoadFromFile(FullPath);
    Result:= StringList.Text;
  end;
end;

end.

