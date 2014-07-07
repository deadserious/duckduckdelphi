unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Menus, duck,
  Vcl.ExtCtrls;

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
    function Add(A,B : integer) : integer; overload;
    function add(A,B : single) : single; overload;
  end;

  TForm1 = class(TForm)
    Label1: TLabel;
    Memo1: TMemo;
    Edit1: TEdit;
    Button1: TButton;
    CheckBox1: TCheckBox;
    RadioButton1: TRadioButton;
    Timer1: TTimer;
    Button2: TButton;
    Label2: TLabel;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  // Use duck typing to hide all controls with a visible property.  Start a timer
  // to show it again after 3 seconds.
  Self.Duck.all.has('Visible').setTo(false);
  Timer1.Enabled := True;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  // Use duck typing to execute the Clear method of any object that supports it.
  duck.all.can('Clear').go;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  i : integer;
begin
  // This example will use the each method to show a hint for each control
  // and a fake filter that allows all objects through followed by the "even"
  // selector to make sure the fonts only get set on ever other control.
  i := 0;
  duck.all.each(procedure(obj : TObject)
  begin
    obj.duck.setTo('Hint','Happy fun day');
    obj.duck.setTo('showHint',True);
  end).filter(function(obj : TObject) : boolean
  begin
    result := obj.ToString <> 'Not going to match.';
  end).even.on('Font').setTo('color',clRed).setTo('Size',24);
end;

procedure TForm1.Button4Click(Sender: TObject);
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

function AddWithAdder(adder : IAdder; A, B : integer) : integer;
begin
  result := adder.add(A,B);
end;

procedure TForm1.Button5Click(Sender: TObject);
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

procedure TForm1.Button6Click(Sender: TObject);
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

procedure TForm1.Button7Click(Sender: TObject);
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

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  // Each time the checkbox is clicked, text will be added to each control that
  // has a text property.
  CheckBox1.Checked := False;
  Self.duck.all.has('text').add(CheckBox1.Caption);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  // Use duck typing to show all controls with a visible property.
  Timer1.Enabled := False;
  Self.duck.all.has('Visible').setTo(True);
end;

{ TMyAdder }

function TMyAdder.add(A, B: integer): integer;
begin
  Result := A+B;
end;

function TMyAdder.add(A, B: single): single;
begin
  Result := a+b;
end;

end.
