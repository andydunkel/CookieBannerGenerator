unit udatamodel;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, contnrs, XMLHelper, laz2_XMLRead, laz2_XmlWrite, laz2_DOM;

type
  TGuiOptions = class
    public
      layout: String;
      position: String;
      transition: String;
  end;

type
  TConsentModal = class
    public
      title:String;
      description:String;
      primary_btn_text: String;
      secondary_btn_text: String;
  end;

type
  TToggle = class
    public
    isToggle: boolean;
    value: string;
    enabled: boolean;
    readonly: boolean;
  end;

type
  TCookieTableEntry = class
  public
    col1: String;
    col2: String;
    col3: String;
    col4: String;
    is_regex: boolean;
  end;


type

  { TBlock }

  TBlock = class
    title: string;
    description: string;
    Toggle: TToggle;
    CookieTable: TObjectList;
    constructor Create;
  end;

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
    Blocks: TObjectList;
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

    FileName : string;

    procedure Save;
    procedure Load;
    constructor Create;
  end;


implementation

{ TSettingsModal }

constructor TSettingsModal.Create;
begin
  Self.Blocks:= TObjectList.Create;
  Self.Blocks.OwnsObjects:= true;
end;

{ TBlock }

constructor TBlock.Create;
begin
  Toggle:= TToggle.Create;
  CookieTable:= TObjectList.Create;
  CookieTable.OwnsObjects:= true;
end;

procedure TDataModel.Save;
var
  doc : TXMLDocument;
  mainNode: TDOMNode;
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

  WriteXMLFile(doc, Self.FileName);
end;

procedure TDataModel.Load;
begin

end;

constructor TDataModel.Create;
begin
  Self.GuiOptionConsentModal:= TGuiOptions.Create;
  Self.GuiOptionSettingsModal:= TGuiOptions.Create;
  Self.ConsentModal:= TConsentModal.Create;
  Self.SettingsModal:= TSettingsModal.Create;
end;

end.

