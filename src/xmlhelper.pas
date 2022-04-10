unit XMLHelper;

{$mode objfpc}{$H+}

interface

uses
  Classes, StrUtils, SysUtils, laz2_DOM, lconvencoding;

type

  { TXMLHelper }

TXMLHelper = class
 public
   class function CreateXmlNode(Doc: TXMLDocument; Name: String): TDOMNode;
   class function CreateXmlNode(Doc: TXMLDocument; Parent: TDOMNode; Name: String; Content: String): TDOMNode;
   class function GetXML(Tag : String; Node :TDOMNode):String;overload;
   class function GetXML(Tag : String; Node :TDOMNode; default : string):String;overload;
   class function GetXMLBool(Tag : String; Node :TDOMNode):boolean;
end;


implementation

{ TXMLHelper }

class function TXMLHelper.CreateXmlNode(Doc: TXMLDocument; Name: String): TDOMNode;
var
   Node: TDOMNode;
begin
   Node:= Doc.CreateElement(Name);
   Result:= Node;
end;

class function TXMLHelper.CreateXmlNode(Doc: TXMLDocument; Parent: TDOMNode;
  Name: String; Content: String): TDOMNode;
var
   Node: TDOMNode;
begin
     Node:= CreateXmlNode(Doc, Name);
     Node.TextContent:= Content;
     Parent.AppendChild(Node);
     Result:= Node;
end;

class function TXMLHelper.GetXML(Tag : String; Node :TDOMNode):String;
var temp : string;
begin
  Result := '';

  if (Node.FindNode(Tag) <> nil) and (Node.FindNode(Tag).FirstChild <> nil) then
  begin
    temp := Node.FindNode(Tag).FirstChild.NodeValue;
    Result := AnsiReplaceStr(temp,#10,#13#10);
  end;

  Result:= Result;
end;


class function TXMLHelper.GetXML(Tag : String; Node :TDOMNode; default : string):String;
begin
  Result := default;

  if (Node.FindNode(Tag) <> nil) and (Node.FindNode(Tag).FirstChild <> nil) then
  begin
    Result := AnsiReplaceStr(Node.FindNode(Tag).FirstChild.NodeValue,#10#10,#13#10);
  end;

  Result:= Result;
end;


class function TXMLHelper.GetXMLBool(Tag : String; Node :TDOMNode):boolean;
begin
 Result := false;
  if (Node.FindNode(Tag) <> nil) and (Node.FindNode(Tag).FirstChild <> nil) then
  begin
    Result := StrToBool(Node.FindNode(Tag).FirstChild.NodeValue);
  end;
end;

end.



