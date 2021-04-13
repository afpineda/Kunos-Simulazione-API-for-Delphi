unit ACPressureTest_CopyPasteOptions;

{ *******************************************************

  Automated pressure test for AC/ACC

  Computes correct cold pressures to achieve target
  hot pressures

  *******************************************************

  (C) 2021. Ángel Fernández Pineda. Madrid. Spain.

  This work is licensed under the Creative Commons
  Attribution-ShareAlike 3.0 Unported License. To
  view a copy of this license,
  visit http://creativecommons.org/licenses/by-sa/3.0/
  or send a letter to Creative Commons,
  444 Castro Street, Suite 900,
  Mountain View, California, 94041, USA.

  *******************************************************

  [2021-03-17] First implementation

  [2021-03-30] Added "PSI" to some field's name

  [2021-03-30] Fixed bug on wheel pressure strings

  ******************************************************* }

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.CheckLst,
  Vcl.ExtCtrls;

type
  TForm_CopyPasteOptions = class(TForm)
    Btn_up: TButton;
    Btn_down: TButton;
    Panel_fields: TPanel;
    Lbl_fields: TLabel;
    List_fields: TCheckListBox;
    procedure List_fieldsClick(Sender: TObject);
    procedure Btn_upClick(Sender: TObject);
    procedure Btn_downClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure MoveListItem(const delta: integer);
  public
    { Public declarations }
    function NextField(var idx: integer; out fieldName: string): boolean;
    procedure LoadFields(fieldStr: string);
  end;

var
  Form_CopyPasteOptions: TForm_CopyPasteOptions;

implementation

uses
  I18NUtils,
  ACPressureTest_main,
  System.StrUtils, System.Types,
  ACPressureTest_strings;

{$R *.dfm}

procedure TForm_CopyPasteOptions.Btn_downClick(Sender: TObject);
begin
  MoveListItem(1);
end;

procedure TForm_CopyPasteOptions.Btn_upClick(Sender: TObject);
begin
  MoveListItem(-1);
end;

procedure TForm_CopyPasteOptions.FormCreate(Sender: TObject);
var
  i: integer;
begin
  I18NUtils.Translate(self);
  List_fields.Items.Clear;
  List_fields.Items.Add(str_carModel);
  List_fields.Items.Add(str_trackName);
  List_fields.Items.Add(str_weather_options);
  List_fields.Items.Add(str_compound);
  i := List_fields.Items.Add(str_airTemp);
  List_fields.Checked[i] := true;
  i := List_fields.Items.Add(str_roadTemp);
  List_fields.Checked[i] := true;
  i := List_fields.Items.Add(str_FL);
  List_fields.Checked[i] := true;
  i := List_fields.Items.Add(str_FR);
  List_fields.Checked[i] := true;
  i := List_fields.Items.Add(str_RL);
  List_fields.Checked[i] := true;
  i := List_fields.Items.Add(str_RR);
  List_fields.Checked[i] := true;
  LoadFields(Form_main.savedCopyPasteFields);
end;

procedure TForm_CopyPasteOptions.LoadFields(fieldStr: string);
var
  fields: TStringDynArray;
  i, idx: integer;
begin
  if (fieldStr <> '') then
  begin
    fields := SplitString(fieldStr, ';');
    // fields := fieldStr.Split([';']);
    for i := 0 to List_fields.Items.Count - 1 do
      List_fields.Checked[i] := false;
    for i := Low(fields) to High(fields) do
    begin
      idx := List_fields.Items.IndexOf(fields[i]);
      if (idx >= 0) then
      begin
        List_fields.Items.Move(idx, i);
        List_fields.Checked[i] := true;
      end;
    end;
  end;
end;

procedure TForm_CopyPasteOptions.MoveListItem(const delta: integer);
var
  idx, newIdx: integer;
begin
  idx := List_fields.ItemIndex;
  newIdx := idx + delta;
  if (newIdx >= 0) and (newIdx < List_fields.Items.Count) then
  begin
    List_fields.Items.Move(idx, newIdx);
    List_fields.ItemIndex := newIdx;
  end;
  List_fieldsClick(List_fields);
end;

procedure TForm_CopyPasteOptions.List_fieldsClick(Sender: TObject);
begin
  Btn_up.Enabled := (List_fields.ItemIndex > 0);
  Btn_down.Enabled := (List_fields.ItemIndex < List_fields.Items.Count - 1);
end;

function TForm_CopyPasteOptions.NextField(var idx: integer;
  out fieldName: string): boolean;
begin
  fieldName := '';
  if (idx >= 0) and (idx < List_fields.Items.Count) then
  begin
    while (idx < List_fields.Items.Count) and (not List_fields.Checked[idx]) do
      inc(idx);
    Result := (idx < List_fields.Items.Count);
    if (Result) then
    begin
      fieldName := List_fields.Items[idx];
      inc(idx);
    end;
  end
  else
    Result := false;
end;

end.
