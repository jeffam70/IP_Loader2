unit Advanced;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.StrUtils,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.ListView.Types, FMX.Edit, FMX.ListView,
  IdStack;

type
  TAdvancedSearchForm = class(TForm)
    SearchedPanel: TPanel;
    LastSearchedLabel: TLabel;
    LastSearchedListView: TListView;
    CustomPanel: TPanel;
    CustomSearchLabel: TLabel;
    EditLabel: TLabel;
    Edit: TEdit;
    AddButton: TButton;
    RemoveButton: TButton;
    CustomListView: TListView;
    CloseButton: TButton;
    procedure FormActivate(Sender: TObject);
    procedure AddButtonClick(Sender: TObject);
    procedure RemoveButtonClick(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure CustomListViewItemClick(const Sender: TObject; const AItem: TListViewItem);
    procedure EditChangeTracking(Sender: TObject);
    procedure EditKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
  private
    { Private declarations }
    procedure UpdateButtons;
    procedure AddOrUpdateItem;
    function ItemIsValid: Boolean;
  public
    { Public declarations }
  end;

var
  AdvancedSearchForm: TAdvancedSearchForm;

implementation

{$R *.fmx}

{----------------------------------------------------------------------------------------------------}
{----------------------------------------------------------------------------------------------------}
{------------------------------------------- Event Methods ------------------------------------------}
{----------------------------------------------------------------------------------------------------}
{----------------------------------------------------------------------------------------------------}

procedure TAdvancedSearchForm.FormActivate(Sender: TObject);
begin
  Edit.SetFocus;
end;

{----------------------------------------------------------------------------------------------------}

procedure TAdvancedSearchForm.AddButtonClick(Sender: TObject);
begin
  AddOrUpdateItem;
end;

{----------------------------------------------------------------------------------------------------}

procedure TAdvancedSearchForm.RemoveButtonClick(Sender: TObject);
begin
  CustomListView.Items.Delete(CustomListView.ItemIndex);
  CustomListView.ItemIndex := -1;
  Edit.Text := '';
  Edit.Tag := -1;
  AddButton.Enabled := False;
  RemoveButton.Enabled := False;
  Edit.SetFocus;
end;

{----------------------------------------------------------------------------------------------------}

procedure TAdvancedSearchForm.CloseButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

{----------------------------------------------------------------------------------------------------}

procedure TAdvancedSearchForm.CustomListViewItemClick(const Sender: TObject; const AItem: TListViewItem);
begin
  Edit.Text := CustomListView.Selected.Text;
  Edit.Tag := CustomListView.ItemIndex;
  Edit.SetFocus;
  UpdateButtons;
end;

{----------------------------------------------------------------------------------------------------}

procedure TAdvancedSearchForm.EditChangeTracking(Sender: TObject);
begin
  UpdateButtons;
end;

{----------------------------------------------------------------------------------------------------}

procedure TAdvancedSearchForm.EditKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
{Filter Edit entries to '0'..'9', '.', and cursor navigation only}
begin
  Caption := Key.ToString + ' - ' + KeyChar;
  {Allow only dotted decimal and backspace, end/home, left/right cursor, and delete keys}
  if not ((KeyChar in ['.', '0'..'9']) or (Key in [8, 35, 36, 37, 39, 46])) then
    begin
    {If Esc pressed, clear edit and deselect item from list}
    if Key = 27 then
      begin
      Edit.Text := '';
      Edit.Tag := -1;
      CustomListView.ItemIndex := -1;
      end;
    {If Enter, accept changes if applicable}
    if Key = 13 then AddOrUpdateItem;
    {Veto unacceptable key presses}
    Key := 0;
    KeyChar := chr(0);
    end;
  UpdateButtons;
end;

{----------------------------------------------------------------------------------------------------}
{----------------------------------------------------------------------------------------------------}
{------------------------------------------ Private Methods -----------------------------------------}
{----------------------------------------------------------------------------------------------------}
{----------------------------------------------------------------------------------------------------}

procedure TAdvancedSearchForm.UpdateButtons;
begin
  AddButton.Text := ifthen(Edit.Tag = -1, 'Add', 'Apply');
  AddButton.Enabled := ItemIsValid;
  RemoveButton.Enabled := Edit.Tag <> -1;
end;

{----------------------------------------------------------------------------------------------------}

procedure TAdvancedSearchForm.AddOrUpdateItem;
{Verify item validity and add or update it as necesary}
begin
  if not ItemIsValid then
    begin
      beep;
      exit;
    end;
  if Edit.Tag = -1 then
    begin
    CustomListView.Items.Add.Text := Edit.Text;
    CustomListView.ScrollTo(CustomListView.Items.Count-1);
    end
  else
    begin
    CustomListView.Items[Edit.Tag].Text := Edit.Text;
    CustomListView.ScrollTo(Edit.Tag);
    end;
  CustomListView.ItemIndex := -1;
  Edit.Text := '';
  Edit.Tag := -1;
  UpdateButtons;
  Edit.SetFocus;
end;

{----------------------------------------------------------------------------------------------------}

function TAdvancedSearchForm.ItemIsValid: Boolean;
{Returns true if item in Edit is valid (is IP address, is not blank, and is unique)}
var
  Idx : Integer;
begin
  Idx := 0;
  while (Idx < CustomListView.Items.Count) and (Edit.Text <> CustomListView.Items[Idx].Text) do inc(Idx);
  Result := (Edit.Text <> '') and (Idx = CustomListView.Items.Count) and GStack.IsIP(Edit.Text);
end;

end.
