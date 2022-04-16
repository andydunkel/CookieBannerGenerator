unit udatamodel;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, contnrs, XMLHelper, laz2_XMLRead, laz2_XmlWrite, laz2_DOM, fgl;

type

  { TGuiOptions }

  TGuiOptions = class
    public
      layout: String;
      position: String;
      position1: String;
      transition: String;
      procedure Save(Doc: TXMLDocument; Node: TDOMNode);
      procedure Load(Node: TDOMNode);
  end;

type

  { TConsentModal }

  TConsentModal = class
    public
      title:String;
      description:String;
      primary_btn_text: String;
      secondary_btn_text: String;
      procedure Save(Doc: TXMLDocument; Node: TDOMNode);
      procedure Load(Node: TDOMNode);
  end;

type

  { TToggle }

  TToggle = class
    public
    isToggle: boolean;
    value: string;
    enabled: boolean;
    readonly: boolean;
    procedure Save(Doc: TXMLDocument; Node: TDOMNode);
    procedure Load(Node: TDOMNode);
  end;

type

  { TCookieTableEntry }

  TCookieTableEntry = class
  public
    col1: String;
    col2: String;
    col3: String;
    col4: String;
    is_regex: boolean;
    procedure Save(Doc: TXMLDocument; Node: TDOMNode);
    procedure Load(Node: TXMLDocument);
  end;


type
  TCookieTableList = class(specialize TFPGObjectList<TCookieTableEntry>);

type

  { TBlock }

  TBlock = class
    title: string;
    description: string;
    Toggle: TToggle;
    CookieTable: TCookieTableList;
    constructor Create;
    procedure Save(Doc: TXMLDocument; Node: TDOMNode);
    procedure Load(Node: TDOMNode);
  end;

type
  TBlockList = class(specialize TFPGObjectList<TBlock>);

type

  { TSettingsModal }

  TSettingsModal = class
    public
    title: String;
    save_settings_btn: String;
    accept_all_btn: String;
    reject_all_btn: String;
    close_btn_label: String;
    cookie_table_header1: String;
    cookie_table_header2: String;
    cookie_table_header3: String;
    cookie_table_header4: String;
    Blocks: TBlockList;
    procedure Save(Doc: TXMLDocument; Node: TDOMNode);
    procedure Load(Node: TDOMNode);
    constructor Create;
  end;

type

  { TDataModel }

  TDataModel = class
  public
    current_lang: String;
    autoclear: boolean;
    cookie_name: String;
    page_scripts: boolean;
    cookie_expiration: Integer;
    delay: Integer;
    hide_from_bots: boolean;
    remove_cookie_tables: boolean;
    GuiOptionConsentModal : TGuiOptions;
    GuiOptionSettingsModal: TGuiOptions;
    ConsentModal: TConsentModal;
    SettingsModal: TSettingsModal;

    CurrentFileName : string;

    procedure Save;
    procedure Load(FileName: String);
    constructor Create;
  end;


implementation

{ TCookieTableEntry }

procedure TCookieTableEntry.Save(Doc: TXMLDocument; Node: TDOMNode);
begin
  TXMLHelper.CreateXmlNode(Doc, Node, 'col1', Self.col1);
  TXMLHelper.CreateXmlNode(Doc, Node, 'col2', Self.col2);
  TXMLHelper.CreateXmlNode(Doc, Node, 'col3', Self.col3);
  TXMLHelper.CreateXmlNode(Doc, Node, 'col4', Self.col4);
  TXMLHelper.CreateXmlNode(Doc, Node, 'is_regex', BoolToStr(Self.is_regex));
end;

procedure TCookieTableEntry.Load(Node: TXMLDocument);
begin
  Self.col1:= TXMLHelper.GetXML('col1', Node);
  Self.col2:= TXMLHelper.GetXML('col2', Node);
  Self.col3:= TXMLHelper.GetXML('col3', Node);
  Self.col4:= TXMLHelper.GetXML('col4', Node);
  Self.is_regex:= TXMLHelper.GetXMLBool('is_regex', Node);
end;

{ TToggle }

procedure TToggle.Save(Doc: TXMLDocument; Node: TDOMNode);
begin
  TXMLHelper.CreateXmlNode(Doc, Node, 'enabled', BoolToStr(Self.enabled));
  TXMLHelper.CreateXmlNode(Doc, Node, 'isToggle', BoolToStr(Self.isToggle));
  TXMLHelper.CreateXmlNode(Doc, Node, 'readonly', BoolToStr(Self.readonly));
  TXMLHelper.CreateXmlNode(Doc, Node, 'value', Self.value);
end;

procedure TToggle.Load(Node: TDOMNode);
begin
  Self.value:= TXMLHelper.GetXML('value', Node);
  Self.readonly:= TXMLHelper.GetXMLBool('readonly', Node);
  Self.enabled:= TXMLHelper.GetXMLBool('enabled', Node);
  Self.isToggle:= TXMLHelper.GetXMLBool('isToggle', Node);
end;

{ TConsentModal }

procedure TConsentModal.Save(Doc: TXMLDocument; Node: TDOMNode);
begin
  TXMLHelper.CreateXmlNode(Doc, Node, 'title', Self.title);
  TXMLHelper.CreateXmlNode(Doc, Node, 'description', Self.description);
  TXMLHelper.CreateXmlNode(Doc, Node, 'primary_btn_text', Self.primary_btn_text);
  TXMLHelper.CreateXmlNode(Doc, Node, 'secondary_btn_text', Self.secondary_btn_text);
end;

procedure TConsentModal.Load(Node: TDOMNode);
begin
  Self.title:= TXMLHelper.GetXML('title', Node);
  Self.description:= TXMLHelper.GetXML('description', Node);
  Self.primary_btn_text:= TXMLHelper.GetXML('primary_btn_text', Node);
  Self.secondary_btn_text:= TXMLHelper.GetXML('secondary_btn_text', Node);
end;

{ TGuiOptions }

procedure TGuiOptions.Save(Doc: TXMLDocument; Node: TDOMNode);
begin
  TXMLHelper.CreateXmlNode(Doc, Node, 'layout', Self.layout);
  TXMLHelper.CreateXmlNode(Doc, Node, 'position', Self.position);
  TXMLHelper.CreateXmlNode(Doc, Node, 'position1', Self.position1);
  TXMLHelper.CreateXmlNode(Doc, Node, 'transition', Self.transition);
end;

procedure TGuiOptions.Load(Node: TDOMNode);
begin
  Self.layout:= TXMLHelper.GetXML('layout', Node, 'bar');
  Self.position:= TXMLHelper.GetXML('position', Node, '');
  Self.position1:= TXMLHelper.GetXML('position1', Node, 'left');
  Self.transition:= TXMLHelper.GetXML('transition', Node, 'slide');
end;

{ TSettingsModal }

procedure TSettingsModal.Save(Doc: TXMLDocument; Node: TDOMNode);
var
  BlockNode: TDOMNode;
  count: Integer;
  i: Integer;
begin
  TXMLHelper.CreateXmlNode(doc, Node, 'title', title);
  TXMLHelper.CreateXmlNode(doc, Node, 'save_settings_btn', save_settings_btn);
  TXMLHelper.CreateXmlNode(doc, Node, 'accept_all_btn', accept_all_btn);
  TXMLHelper.CreateXmlNode(doc, Node, 'reject_all_btn', reject_all_btn);
  TXMLHelper.CreateXmlNode(doc, Node, 'close_btn_label', close_btn_label);
  TXMLHelper.CreateXmlNode(doc, Node, 'cookie_table_header1', cookie_table_header1);
  TXMLHelper.CreateXmlNode(doc, Node, 'cookie_table_header2', cookie_table_header2);
  TXMLHelper.CreateXmlNode(doc, Node, 'cookie_table_header3', cookie_table_header3);
  TXMLHelper.CreateXmlNode(doc, Node, 'cookie_table_header4', cookie_table_header4);

  BlockNode:= TXMLHelper.CreateXmlNode(doc, 'Blocks');
  Node.AppendChild(BlockNode);

  count:= Blocks.Count;

  for i:= 0 to count - 1 do
  begin
    Blocks[i].Save(Doc, BlockNode);
  end;
end;

procedure TSettingsModal.Load(Node: TDOMNode);
var
  BlockNode: TDOMNode;
  Block: TBlock;
  i : Integer;
begin
  Self.title:= TXMLHelper.GetXML('title', Node);
  Self.save_settings_btn:= TXMLHelper.GetXML('save_settings_btn', Node);
  Self.accept_all_btn:= TXMLHelper.GetXML('accept_all_btn', Node);
  Self.reject_all_btn:= TXMLHelper.GetXML('reject_all_btn', Node);
  Self.close_btn_label:= TXMLHelper.GetXML('close_btn_label', Node);
  Self.cookie_table_header1:= TXMLHelper.GetXML('cookie_table_header1', Node);
  Self.cookie_table_header2:= TXMLHelper.GetXML('cookie_table_header2', Node);
  Self.cookie_table_header3:= TXMLHelper.GetXML('cookie_table_header3', Node);
  Self.cookie_table_header4:= TXMLHelper.GetXML('cookie_table_header4', Node);

  BlockNode:= Node.FindNode('Blocks');
  Blocks.Clear;

  if (BlockNode <> nil) then begin
     for i:= 0 to BlockNode.GetChildCount do
     begin
       Block:= TBlock.Create;
       Block.Load(BlockNode.ChildNodes[i]);
       Blocks.Add(Block);
     end;

  end;
end;

constructor TSettingsModal.Create;
begin
  Self.Blocks:= TBlockList.Create;
end;

{ TBlock }

constructor TBlock.Create;
begin
  Toggle:= TToggle.Create;
  CookieTable:= TCookieTableList.Create;
end;

procedure TBlock.Save(Doc: TXMLDocument; Node: TDOMNode);
begin

end;

procedure TBlock.Load(Node: TDOMNode);
begin

end;

procedure TDataModel.Save;
var
  doc : TXMLDocument;
  mainNode, GuiOptionConsentModalNode,GuiOptionSettingsModalNode : TDOMNode;
  ConsentModalNode, SettingsModalNode: TDOMNode;
begin
  doc:= TXMLDocument.Create;

  mainNode:= TXMLHelper.CreateXmlNode(doc, 'xml');
  doc.AppendChild(mainNode);

  TXMLHelper.CreateXmlNode(doc, mainNode, 'current_lang', current_lang);
  TXMLHelper.CreateXmlNode(doc, mainNode, 'cookie_name', cookie_name);
  TXMLHelper.CreateXmlNode(doc, mainNode, 'autoclear', BoolToStr(autoclear));
  TXMLHelper.CreateXmlNode(doc, mainNode, 'page_scripts', BoolToStr(page_scripts));
  TXMLHelper.CreateXmlNode(doc, mainNode, 'hide_from_bots', BoolToStr(hide_from_bots));
  TXMLHelper.CreateXmlNode(doc, mainNode, 'remove_cookie_tables', BoolToStr(remove_cookie_tables));
  TXMLHelper.CreateXmlNode(doc, mainNode, 'delay', IntToStr(delay));

  GuiOptionConsentModalNode:= TXMLHelper.CreateXmlNode(doc, 'GuiOptionConsentModal');
  Self.GuiOptionConsentModal.Save(doc, GuiOptionConsentModalNode);

  GuiOptionSettingsModalNode:= TXMLHelper.CreateXmlNode(doc, 'GuiOptionSettingsModal');
  Self.GuiOptionSettingsModal.Save(doc, GuiOptionSettingsModalNode);

  mainNode.AppendChild(GuiOptionSettingsModalNode);
  mainNode.AppendChild(GuiOptionConsentModalNode);

  ConsentModalNode:= TXMLHelper.CreateXmlNode(doc, 'ConsentModal');
  Self.ConsentModal.Save(doc, ConsentModalNode);
  mainNode.AppendChild(ConsentModalNode);

  SettingsModalNode:= TXMLHelper.CreateXmlNode(doc, 'SettingsModal');
  SettingsModal.Save(doc, SettingsModalNode);
  mainNode.AppendChild(SettingsModalNode);

  WriteXMLFile(doc, Self.CurrentFileName);
end;

procedure TDataModel.Load(FileName: String);
var
  Doc: TXMLDocument;
  MainNode: TDOMNode;
begin
  ReadXMLFile(Doc, FileName);
  MainNode:= Doc.DocumentElement;

  Self.current_lang:= TXMLHelper.GetXML('current_lang', MainNode);
  Self.cookie_name:= TXMLHelper.GetXML('cookie_name', MainNode);
  Self.autoclear:= TXMLHelper.GetXMLBool('autoclear', MainNode);
  Self.hide_from_bots:= TXMLHelper.GetXMLBool('hide_from_bots', MainNode);
  Self.remove_cookie_tables:= TXMLHelper.GetXMLBool('remove_cookie_tables', MainNode);
  Self.delay:= StrToInt(TXMLHelper.GetXML('delay', MainNode));



end;

constructor TDataModel.Create;
begin
  Self.GuiOptionConsentModal:= TGuiOptions.Create;
  Self.GuiOptionSettingsModal:= TGuiOptions.Create;
  Self.ConsentModal:= TConsentModal.Create;
  Self.SettingsModal:= TSettingsModal.Create;
end;

end.

