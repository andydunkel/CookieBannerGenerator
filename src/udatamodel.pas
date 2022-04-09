unit udatamodel;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;


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
    value: string;
    enabled: boolean;
    readonly: boolean;
  end;



type
  TBlock = class
    title: string;
    description: string;
    Toggle: TToggle;


  end;

type
  TSettingsModal = class
    title: String;
    save_settings_btn: String;
    accept_all_btn: String;
    reject_all_btn: String;
    close_btn_label: String;
    cookie_table_header1: String;
    cookie_table_header2: String;
    cookie_table_header3: String;
    cookie_table_header4: String;
  end;

type
  TDataModel = class
  public
    current_lang: String;
    autoclear: boolean;
    cookie_name: String;
    page_scripts: boolean;
    delay: Integer;
    hide_from_bots: boolean;
    remove_cookie_tables: boolean;
    GuiOptionConsentModal : TGuiOptions;
    GuiOptionSettingsModal: TGuiOptions;
    ConsentModal: TConsentModal;
    SettingsModal: TSettingsModal;
  end;


implementation

constructor TDataModel.Create
begin
  Self.GuiOptionConsentModal:= TGuiOptions.Create;
  Self.GuiOptionSettingsModal:= TGuiOptions.Create;
  Self.ConsentModal:= TConsentModal.Create;
  Self.SettingsModal:= TConsentModal.Create;
end;

end.

