unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Memo, FMX.Edit, duck;

type
  {$M+}
  IAdder = interface(IInterface)
    ['{0195DEEB-2F6A-44EC-993E-5CE88349CD84}']
    function Add(A,B : integer) : Integer;
  end;

  IFloatAdder = interface(IInterface)
    ['{8EFB6202-7A54-4DE6-959C-B1D0744CE55B}']
    function Add(A,B : single) : single;
  end;

  TMyAdder = class(TObject) // does not indicate support for IAdder or IFloatAdder but implements both
  public
    //function Add(A,B : integer) : integer; overload;
    function add(A,B : single) : single; overload;
  end;

  TForm1 = class(TForm)
    Edit1: TEdit;
    Memo1: TMemo;
    btnAdd: TButton;
    btnImper: TButton;
    btnImprFloat: TButton;
    btnSoft: TButton;
    btnFont: TButton;
    btnHide: TButton;
    tmrHide: TTimer;
    btnClear: TButton;
    CheckBox1: TCheckBox;
    RadioButton1: TRadioButton;
    Label1: TLabel;
    procedure btnAddClick(Sender: TObject);
    procedure btnImperClick(Sender: TObject);
    procedure btnImprFloatClick(Sender: TObject);
    procedure btnSoftClick(Sender: TObject);
    procedure btnFontClick(Sender: TObject);
    procedure btnHideClick(Sender: TObject);
    procedure tmrHideTimer(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

{ TMyAdder }

{function TMyAdder.Add(A, B: integer): integer;
begin
  Result := A+B;
end;}

function TMyAdder.Add(A, B: single): single;
begin
  Result := A+B;
end;

{ TForm1 }

procedure TForm1.btnAddClick(Sender: TObject);
var
  obj : TObject;
  iResult : integer;
  eResult : Extended;
begin
  obj := TMyAdder.Create;
  try
    if obj.impersonates<IAdder> then
    begin
      iResult := obj.duck.call('Add',[2,5]).AsInteger;
      Edit1.Text := IntToStr(iResult);
    end;
    if obj.impersonates<IFloatAdder> then
    begin
      eResult := obj.duck.call('Add',[2.03,5.5]).AsExtended;
      Memo1.Text := FloatToStr(eResult);
    end;
  finally
    obj.Free;
  end;
end;

procedure TForm1.btnClearClick(Sender: TObject);
begin
  // Use duck typing to clear contents of the edits.
  // NOTE, Firemonkey does not have a clear method like the VCL so we
  // have to clear contents using selection.
  duck.all.can('SelectAll').each(
    procedure(obj : TObject)
    begin
      obj.duck.call('SelectAll');
      obj.duck.call('DeleteSelection');
    end
  );
end;

procedure TForm1.btnFontClick(Sender: TObject);
var
  i : integer;
begin
  // This example will use the each method to show a hint for each control
  // and a fake filter that allows all objects through followed by the "even"
  // selector to make sure the fonts only get set on ever other control.
  i := 0;
  duck.all.filter(function(obj : TObject) : boolean
  begin
    result := obj.ToString <> 'Not going to match.';
  end).even.on('TextSettings').setTo('FontColor',TAlphaColorRec.Red).on('Font').setTo('Size',24);
end;

procedure TForm1.btnHideClick(Sender: TObject);
begin
  // Use duck typing to hide all controls with a visible property.  Start a timer
  // to show it again after 3 seconds.
  Self.Duck.all.has('Visible').setTo(false);
  tmrHide.Enabled := True;
end;

procedure TForm1.btnImperClick(Sender: TObject);
var
  obj : TObject;
  iResult : integer;
  eResult : Extended;
begin
  obj := TMyAdder.Create;
  try
    if obj.impersonates<IAdder> then
    begin
      iResult := obj.asA<IAdder>.Add(2,7);
      Edit1.Text := IntToStr(iResult);
    end;
  finally
    obj.Free;
  end;
end;

procedure TForm1.btnImprFloatClick(Sender: TObject);
var
  obj : TObject;
  iResult : integer;
  eResult : Extended;
begin
  obj := TMyAdder.Create;
  try
    if obj.impersonates<IFloatAdder> then
    begin
      eResult := obj.asA<IFloatAdder>.Add(2.22,7.44);
      Edit1.Text := FloatToStr(eResult);
    end;
  finally
    obj.Free;
  end;

end;

procedure TForm1.btnSoftClick(Sender: TObject);
var
  obj : TObject;
  iResult : integer;
  ia : iadder;
  fa : ifloatadder;
  eResult : Extended;
begin
  obj := TMyAdder.Create;
  try
    ia := (obj.duck<IAdder> as IAdder);
    iResult := ia.Add(12,7);
    Edit1.Text := IntToStr(iResult);
  finally
    obj.Free;
  end;
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
begin
  // Each time the checkbox is clicked, text will be added to each control that
  // has a text property.
  CheckBox1.IsChecked := False;
  Self.duck.all.has('text').add(CheckBox1.Text);
end;

procedure TForm1.tmrHideTimer(Sender: TObject);
begin
  // Use duck typing to show all controls with a visible property.
  tmrHide.Enabled := False;
  Self.duck.all.has('Visible').setTo(True);
end;

end.
