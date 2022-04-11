unit globalfunctions;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

function IntegerToString(value: integer): String;
function StringToInteger(value: String): Integer;

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

end.

