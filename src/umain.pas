unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls,
  ActnList;

type

  { TMainForm }

  TMainForm = class(TForm)
    ActionNew: TAction;
    ActionOpen: TAction;
    ActionList1: TActionList;
    ImageListToolbar: TImageList;
    MainMenu: TMainMenu;
    MenuFile: TMenuItem;
    MenuExit: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    PageControl1: TPageControl;
    Separator1: TMenuItem;
    StatusBar1: TStatusBar;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    ToolBar1: TToolBar;
    ToolButtonNew: TToolButton;
    ToolButtonOpen: TToolButton;
    procedure ActionNewExecute(Sender: TObject);
    procedure MenuExitClick(Sender: TObject);
  private

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

procedure TMainForm.ActionNewExecute(Sender: TObject);
begin

end;

end.

