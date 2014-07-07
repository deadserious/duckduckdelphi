object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 300
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 312
    Top = 160
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object Label2: TLabel
    Left = 392
    Top = 40
    Width = 31
    Height = 13
    Caption = 'Label2'
  end
  object Memo1: TMemo
    Left = 232
    Top = 128
    Width = 185
    Height = 89
    Lines.Strings = (
      'Memo1')
    TabOrder = 0
  end
  object Edit1: TEdit
    Left = 424
    Top = 128
    Width = 121
    Height = 21
    TabOrder = 1
    Text = 'Edit1'
  end
  object Button1: TButton
    Left = 64
    Top = 155
    Width = 75
    Height = 25
    Caption = 'Hide for 3s'
    TabOrder = 2
    OnClick = Button1Click
  end
  object CheckBox1: TCheckBox
    Left = 64
    Top = 64
    Width = 145
    Height = 17
    Caption = 'Add this text to all'
    TabOrder = 3
    OnClick = CheckBox1Click
  end
  object RadioButton1: TRadioButton
    Left = 216
    Top = 24
    Width = 113
    Height = 17
    Caption = 'RadioButton1'
    TabOrder = 4
  end
  object Button2: TButton
    Left = 64
    Top = 208
    Width = 75
    Height = 25
    Caption = 'Clear All'
    TabOrder = 5
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 512
    Top = 64
    Width = 75
    Height = 25
    Caption = 'Font Update'
    TabOrder = 6
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 424
    Top = 176
    Width = 75
    Height = 25
    Caption = 'Add'
    TabOrder = 7
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 424
    Top = 216
    Width = 121
    Height = 25
    Caption = 'Add Impersonate'
    TabOrder = 8
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 424
    Top = 256
    Width = 121
    Height = 25
    Caption = 'Add Soft Interface'
    TabOrder = 9
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 551
    Top = 216
    Width = 65
    Height = 25
    Caption = 'Float'
    TabOrder = 10
    OnClick = Button7Click
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 3000
    OnTimer = Timer1Timer
    Left = 352
    Top = 72
  end
end
