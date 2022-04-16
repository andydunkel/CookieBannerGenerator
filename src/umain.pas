unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls,
  ActnList, StdCtrls, udatamodel, globalfunctions;

type

  { TMainForm }

  TMainForm = class(TForm)
    ActionSave: TAction;
    ActionNew: TAction;
    ActionOpen: TAction;
    ActionList1: TActionList;
    cboSettingsLayout: TComboBox;
    cboSettingsPosition: TComboBox;
    cboSettingsTransition: TComboBox;
    cboLanguage: TComboBox;
    chkHideBots: TCheckBox;
    chkRemoveCookieTables: TCheckBox;
    cboConsentLayout: TComboBox;
    cboContentPosition1: TComboBox;
    cboContentPosition2: TComboBox;
    cboContentTransition: TComboBox;
    edtCookieName: TEdit;
    edtDelay: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupLayout: TGroupBox;
    ImageListToolbar: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    MainMenu: TMainMenu;
    MenuFile: TMenuItem;
    MenuExit: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    PageControl1: TPageControl;
    SaveDialog: TSaveDialog;
    Separator1: TMenuItem;
    StatusBar1: TStatusBar;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButtonNew: TToolButton;
    ToolButtonOpen: TToolButton;
    procedure ActionNewExecute(Sender: TObject);
    procedure ActionSaveExecute(Sender: TObject);
    procedure edtCookieNameChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuExitClick(Sender: TObject);
  private
    DataModel: TDataModel;
    procedure DialogToModel;
    procedure ModelToDialog;

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.MenuExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TMainForm.DialogToModel;
begin
  DataModel.current_lang:= cboLanguage.Text;
  DataModel.delay:= StringToInteger(edtDelay.Text);
  DataModel.cookie_name:= edtCookieName.Text;
  DataModel.hide_from_bots:= chkHideBots.Checked;
  DataModel.remove_cookie_tables:= chkRemoveCookieTables.Checked;

  DataModel.GuiOptionConsentModal.layout:= cboConsentLayout.Text;
  DataModel.GuiOptionConsentModal.position:= cboContentPosition1.Text;
  DataModel.GuiOptionConsentModal.position1:= cboContentPosition1.Text;
  DataModel.GuiOptionConsentModal.transition:= cboContentTransition.Text;

  DataModel.GuiOptionSettingsModal.layout:= cboSettingsLayout.Text;
  DataModel.GuiOptionSettingsModal.position:= cboSettingsPosition.Text;
  DataModel.GuiOptionSettingsModal.transition:= cboContentTransition.Text;
end;

procedure TMainForm.ModelToDialog;
begin
  cboLanguage.Text:= DataModel.current_lang;
  edtDelay.Text:= IntegerToString(DataModel.delay);
  edtCookieName.Text:= DataModel.cookie_name;
  chkHideBots.Checked:= DataModel.hide_from_bots;
  chkRemoveCookieTables.Checked:= DataModel.remove_cookie_tables;

  cboConsentLayout.Text:= DataModel.GuiOptionConsentModal.layout;
  cboContentPosition1.Text:= DataModel.GuiOptionConsentModal.position;
  cboContentPosition1.Text:= DataModel.GuiOptionConsentModal.position1;
  cboContentTransition.Text:= DataModel.GuiOptionConsentModal.transition;

  cboSettingsLayout.Text:= DataModel.GuiOptionSettingsModal.layout;
  cboSettingsPosition.Text:= DataModel.GuiOptionSettingsModal.position;
  cboSettingsTransition.Text:= DataModel.GuiOptionSettingsModal.transition;
end;

procedure TMainForm.ActionNewExecute(Sender: TObject);
begin

end;

procedure TMainForm.ActionSaveExecute(Sender: TObject);
begin
  if SaveDialog.Execute then
  begin
    DialogToModel;
    DataModel.CurrentFileName:= SaveDialog.FileName;
    DataModel.Save;
  end;
end;

procedure TMainForm.edtCookieNameChange(Sender: TObject);
begin

end;

procedure TMainForm.FormShow(Sender: TObject);
var
  DefaultFileName: String;
  Content: String;
begin
  DataModel:= TDataModel.Create;
  DefaultFileName:= GetApplicationFile('res\default.cookie');
  DataModel.Load(DefaultFileName);
  ModelToDialog;
end;

end.

