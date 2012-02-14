// *****************************************************************************
//      DUCK DUCK DELPHI
// *****************************************************************************
//
//  Original Author:
//    Jason Southwell
//
//  Description:
//    This unit adds simple duck typing to Delphi objects with RTTI and
//    provides an RTTI helper class to simplify many common RTTI tasks.
//
//  For more information on Duck Typing, please check out this article on
//  wikipedia:
//    http://en.wikipedia.org/wiki/Duck_typing
//
//  For more information on this project, please visit:
//    http:/arcana.sivv.com
//
// *****************************************************************************
//      DUCK TYPING
// *****************************************************************************
//
//  Instead of:
//    if obj is TControl then
//      TCoontrol(obj).Visible := True
//
//  You can simply call
//    obj.duck.sett('Visible',True);
//
//  If the property does not exist on the object, the call fails silently.
//
//  If you must know that the property can be set:
//    if obj.duck.has('Visible') then
//      obj.duck.sett('Visible',True);
//
//  "obj" in the above examples can be any object with RTTI.
//
// *****************************************************************************
//      ANNONYMOUS EVENTS
// *****************************************************************************
//
//  Instead of:
//    obj.OnChange := MyObjectOnChange;
//
//  You can now define event handlers inline with anonymous methods
//    obj.OnChange := Events.Notify(procedure(Sender : TObject)
//    begin
//      DoSomethingHere();
//    end);
//
//  To release the memory used by the anonymous method, instead of setting the
//  property to nil, you must release the event:
//    Event.NotifyRelease(obj.OnChange);
//
//  If the event is of a type that does not have an explicit helper, you can
//  use the From method as follows:
//    TKeyEventReference = reference to procedure(Sender: TObject; var Key: Word; Shift: TShiftState);
//    obj.OnKeyUp := Events.From<TKeyEvent, TKeyEventReference>(procedure(Sender: TObject; var Key: Word; Shift: TShiftState)
//    begin
//      DoSomethingHere();
//    end);
//
// *****************************************************************************
//      LICENSE
// *****************************************************************************
//
//  The MIT License (MIT)
//  Copyright (c) 2012 by Sivv LLC, All Rights Reseved
//
//  Permission is hereby granted, free of charge, to any person obtaining a copy
//  of this software and associated documentation files (the "Software"), to
//  deal in the Software without restriction, including without limitation the
//  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
//  sell copies of the Software, and to permit persons to whom the Software is
//  furnished to do so, subject to the following conditions:
//
//  The above copyright notice and this permission notice shall be included in
//  all copies or substantial portions of the Software.
//
//  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
//  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
//  IN THE SOFTWARE.
//
// *****************************************************************************

unit duck;

interface

uses System.SysUtils, System.Classes, System.Types, System.Rtti,
  System.Generics.Collections;

type
  TNotifyReference = reference to procedure(Sender : TObject);

  TRTTI = class
  public
    class function isSame(const Value1, Value2 : TValue) : boolean; static;
    class function isA<T>(ctrl: TObject; const PropertyName: string) : boolean; overload; static;
    class function get<T>(ctrl: TObject; const PropertyName: string) : T; overload; static;
    class function tryGet<T>(ctrl: TObject; const PropertyName: string; var AResult : T) : boolean; overload; static;
    class procedure sett<T>(ctrl: TObject; const PropertyName: string; Value: T); overload; static;
    class function trySet<T>(ctrl: TObject; const PropertyName: string; Value: T) : boolean; overload; static;

    class function isA<T>(ctrl: TObject; const PropertyName: string; args : TArray<TValue>) : boolean; overload; static;
    class function get<T>(ctrl: TObject; const PropertyName: string; args : TArray<TValue>) : T; overload; static;
    class function tryGet<T>(ctrl: TObject; const PropertyName: string; args : TArray<TValue>; var AResult : T) : boolean; overload; static;
    class procedure sett<T>(ctrl: TObject; const PropertyName: string; args : TArray<TValue>; Value: T); overload; static;
    class function trySet<T>(ctrl: TObject; const PropertyName: string; args : TArray<TValue>; Value: T) : boolean; overload; static;

    class function exists(ctrl: TObject; const name: string) : boolean; static;
    class function getValue(ctrl: TObject; const PropertyName: string) : TValue; static;
    class procedure setValue(ctrl: TObject; const PropertyName: string; Value : TValue); static;

    class function call(ctrl : TObject; const MethodName : string; args : TArray<TValue>) : TValue;
    class function tryCall(ctrl : TObject; const MethodName : string; args : TArray<TValue>; var AResult : TValue) : boolean;

    class function isProperty(ctrl: TObject; const PropertyName: string) : boolean; static;
    class function isMethod(ctrl: TObject; const MethodName: string) : boolean; static;
  end;

  TEvent = class
  private
    FEvents : TDictionary<TPair<Pointer,Pointer>,IInterface>;
  protected
    procedure MakeEvent(const ameth; var evt);  virtual;
    procedure ParseMethod(const meth; var p1, p2 : Pointer); virtual;
    function IsSameEvent(const ameth; const evt) : boolean; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function From<TEvent, TReference>(proc: TReference): TEvent;
    procedure Release<TEvent>(var event : TEvent);
    function Notify(proc : TNotifyReference) : TNotifyEvent; virtual;
    procedure NotifyRelease(var event : TNotifyEvent); virtual;
  end;

  TPropValue = record
    PropertyName : string;
    Value: TValue;
    constructor Create(const APropertyName : string; const AValue: TValue);
  end;

  ISelector = interface(IInterface)
    // chained selection refinement methods
    function has(PropertyName : string) : ISelector; overload;
    function has(PropertyNames : TArray<string>) : ISelector; overload;
    function has(NameValues : TArray<TPropValue>) : ISelector; overload;
    function has(PropertyName : string; Value : TValue) : ISelector; overload;
    function can(MethodName : string) : ISelector;
    function isa(ClassType : TClass) : ISelector;

    // retrieves items enumerator
    function count : integer;
    function items : TArray<TObject>;

    // Property Actions upon the entire selection
    function sett(Value : TValue) : ISelector; overload;
    function sett(propertyName : string; Value : TValue) : ISelector; overload;
    function add(Value : TValue) : ISelector; overload;
    function add(propertyName : string; Value : TValue) : ISelector; overload;
    function subtract(Value : TValue) : ISelector; overload;
    function subtract(propertyName : string; Value : TValue) : ISelector; overload;

    // Method Actions upon the entire selection
    function go : ISelector; overload;
    function go(methodName : string) : ISelector; overload;
    function go(args : TArray<TValue>) : ISelector; overload;
    function go(methodName : string; args : TArray<TValue>) : ISelector; overload;
    function call : TArray<TValue>; overload;
    function call(methodName : string) : TArray<TValue>; overload;
    function call(args : TArray<TValue>) : TArray<TValue>; overload;
    function call(methodName : string; args : TArray<TValue>) : TArray<TValue>; overload;
  end;

  TSelectorImpl = class(TInterfacedObject, ISelector)
  private
    FResults : TList<TObject>;
    FObject : TObject;
    FContext : string;
  protected
    procedure RequireContext; virtual;
  public
    constructor Create(AObject : TObject; AContext : string; AResults : TList<TObject>); virtual;
    destructor Destroy; override;

    // chained selection refinement methods
    function has(PropertyName : string) : ISelector; overload;
    function has(PropertyNames : TArray<string>) : ISelector; overload;
    function has(NameValues : TArray<TPropValue>) : ISelector; overload;
    function has(PropertyName : string; Value : TValue) : ISelector; overload;
    function can(MethodName : string) : ISelector;
    function isa(ClassType : TClass) : ISelector;

    // retrieves items array
    function count : integer;
    function items : TArray<TObject>;

    // Property Actions upon the entire selection
    function sett(Value : TValue) : ISelector; overload;
    function sett(propertyName : string; Value : TValue) : ISelector; overload;
    function add(Value : TValue) : ISelector; overload;
    function add(propertyName : string; Value : TValue) : ISelector; overload;
    function subtract(Value : TValue) : ISelector; overload;
    function subtract(propertyName : string; Value : TValue) : ISelector; overload;

    // Method Actions upon the entire selection
    function go : ISelector; overload;
    function go(methodName : string) : ISelector; overload;
    function go(args : TArray<TValue>) : ISelector; overload;
    function go(methodName : string; args : TArray<TValue>) : ISelector; overload;
    function call : TArray<TValue>; overload;
    function call(methodName : string) : TArray<TValue>; overload;
    function call(args : TArray<TValue>) : TArray<TValue>; overload;
    function call(methodName : string; args : TArray<TValue>) : TArray<TValue>; overload;
  end;

  IDuck = interface
    procedure sett(propertyName : string; Value : TValue);
    function get(propertyName : string) : TValue;
  	function has(propertyName : string) : boolean;
	  function can(methodName : string) : boolean;
    function call(methodName : string) : TValue; overload;
    function call(methodName : string; args : TArray<TValue>) : TValue; overload;
    function call(methodName : string; var Exists : boolean) : TValue; overload;
    function call(methodName : string; args : TArray<TValue>; var Exists : boolean) : TValue; overload;
    function call(methodName : string; var AResult : TValue) : boolean; overload;
    function call(methodName : string; args : TArray<TValue>; var AResult : TValue) : boolean; overload;
    function all : ISelector;
    function obj : TObject;
  end;

  TDuckImpl = class(TInterfacedObject, IDuck)
  private
    FOwner : TObject;
    function IndexParam(idx : integer) : TArray<TValue>;
  public
    constructor Create(AOwner: TObject); virtual;
    procedure EnumerateAssociatedObjects(AOwner : TObject; List : TList<TObject>); virtual;

    // IDuck
    procedure sett(propertyName : string; Value : TValue);
    function get(propertyName : string) : TValue;
  	function has(propertyName : string) : boolean;
	  function can(methodName : string) : boolean;
    function call(methodName : string) : TValue; overload;
    function call(methodName : string; args : TArray<TValue>) : TValue; overload;
    function call(methodName : string; var Exists : boolean) : TValue; overload;
    function call(methodName : string; args : TArray<TValue>; var Exists : boolean) : TValue; overload;
    function call(methodName : string; var AResult : TValue) : boolean; overload;
    function call(methodName : string; args : TArray<TValue>; var AResult : TValue) : boolean; overload;
    function obj : TObject;
    function all : ISelector;
  end;

  TDuckHelper = class helper for TObject
  public
    function duck : IDuck;
  end;

  TDuck = class(TObject)
  public
    function duck : IDuck;
  end;

var
  Event : TEvent;


function PV(const APropertyName : string; const AValue : TValue) : TPropValue;
procedure RegisterPropertyEnumerator(const IndexedProperty : string; const CountProperty : string);

resourcestring
  S_MISSINGCONTEXT = 'Context missing for opperation on selector. Try explicitly stating identifier in call to method.';

implementation

var
  enumprops : TArray<TArray<string>>;

function PV(const APropertyName : string; const AValue : TValue) : TPropValue;
begin
  Result.Create(APropertyName, AValue);
end;

procedure RegisterPropertyEnumerator(const IndexedProperty : string; const CountProperty : string);
begin
  setLength(enumprops,length(enumprops)+1);
  setLength(enumprops[length(enumprops)-1],2);
  enumprops[length(enumprops)-1][0] := IndexedProperty;
  enumprops[length(enumprops)-1][1] := CountProperty;
end;

procedure InitializeEnumProps;
begin
  RegisterPropertyEnumerator('Children','ChildCount');
  RegisterPropertyEnumerator('Components','ComponentCount');
  RegisterPropertyEnumerator('Items','Count');
  RegisterPropertyEnumerator('Items','ItemCount');
  RegisterPropertyEnumerator('Objects','Count');
  RegisterPropertyEnumerator('Objects','ObjectCount');
  RegisterPropertyEnumerator('Commands','CommandCount');
  RegisterPropertyEnumerator('DataSets','Dataset');
  RegisterPropertyEnumerator('Fields','FieldCount');
  RegisterPropertyEnumerator('FieldDefs','FieldDefCount');
  RegisterPropertyEnumerator('IndexFields','IndexFieldCount');
  RegisterPropertyEnumerator('Errors','ErrorCount');
  RegisterPropertyEnumerator('Sessions','SessionCount');
  RegisterPropertyEnumerator('Databases','DatabaseCount');
  RegisterPropertyEnumerator('BindComps','BindCompCount');
  RegisterPropertyEnumerator('Nodes','NodeCount');
  RegisterPropertyEnumerator('Forms','FormCount');
  RegisterPropertyEnumerator('DataModules','DataModuleCount');
  RegisterPropertyEnumerator('Columns','ColumnCount');
  RegisterPropertyEnumerator('ListItems','Count');
  RegisterPropertyEnumerator('Printers','Count');
  RegisterPropertyEnumerator('Points','Count');
  RegisterPropertyEnumerator('Rows','RowCount');
end;


{ TRTTI }

class function TRTTI.call(ctrl: TObject; const MethodName: string; args: TArray<TValue>): TValue;
var
  cxt : TRTTIContext;
  meth : TRttiMethod;
begin
  Result := nil;
  meth := cxt.GetType(ctrl.ClassInfo).GetMethod(MethodName);
  if meth <> nil then
  begin
    Result := meth.Invoke(ctrl,args);
  end;
end;

class function TRTTI.get<T>(ctrl: TObject; const PropertyName: string): T;
var
  cxt : TRTTIContext;
  prop : TRttiProperty;
begin
  Result := T(nil);
  prop := cxt.GetType(ctrl.ClassInfo).GetProperty(PropertyName);
  if prop <> nil then
  begin
    Result := prop.GetValue(ctrl).AsType<T>;
  end;
end;

class function TRTTI.get<T>(ctrl: TObject; const PropertyName: string; args : TArray<TValue>): T;
var
  cxt : TRTTIContext;
  prop : TRttiIndexedProperty;
begin
  Result := T(nil);
  prop := cxt.GetType(ctrl.ClassInfo).GetIndexedProperty(PropertyName);
  if prop <> nil then
  begin
    Result := prop.GetValue(ctrl, args).AsType<T>;
  end;
end;

class function TRTTI.getValue(ctrl: TObject; const PropertyName: string): TValue;
var
  cxt : TRTTIContext;
  prop : TRttiProperty;
begin
  Result := nil;
  prop := cxt.GetType(ctrl.ClassInfo).GetProperty(PropertyName);
  if prop <> nil then
  begin
    Result := prop.GetValue(ctrl);
  end;
end;

class function TRTTI.exists(ctrl: TObject; const Name: string): boolean;
begin
  Result := isProperty(ctrl, Name) or isMethod(ctrl, Name);
end;

class function TRTTI.isProperty(ctrl: TObject; const PropertyName: string): boolean;
var
  cxt : TRTTIContext;
begin
  Result := (cxt.GetType(ctrl.ClassInfo).GetProperty(PropertyName) <> nil) or
            (cxt.GetType(ctrl.ClassInfo).GetIndexedProperty(PropertyName) <> nil);
end;

class function TRTTI.isSame(const Value1, Value2: TValue): boolean;
begin
  result :=
    (Value1.TypeInfo = Value2.TypeInfo) and
    (  ((Value1.DataSize = Value2.DataSize) and (Value1.GetReferenceToRawData = Value2.GetReferenceToRawData)) or
       (Value1.IsObject and Value2.IsObject and (Value1.AsObject = Value2.AsObject)) or
       (Value1.IsClass and Value2.IsClass and (Value1.AsClass = Value2.AsClass)) or
       (Value1.IsOrdinal and Value2.IsOrdinal and (Value1.AsOrdinal = Value2.AsOrdinal)) or
       (Value1.IsClass and Value2.IsClass and (Value1.AsClass = Value2.AsClass)) or
       (Value1.IsType<string> and Value2.IsType<string> and (Value1.AsString = Value2.AsString)) or
       (Value1.IsType<boolean> and Value2.IsType<boolean> and (Value1.AsBoolean = Value2.AsBoolean)) or
       (Value1.IsType<extended> and Value2.IsType<string> and (Value1.AsExtended = Value2.AsExtended)) or
       (Value1.IsType<IInterface> and Value2.IsType<IInterface> and (Value1.AsInterface = Value2.AsInterface)) or
       (Value1.IsType<Currency> and Value2.IsType<string> and (Value1.AsCurrency = Value2.AsCurrency)) or
       (Value1.IsType<Variant> and Value2.IsType<Variant> and (Value1.AsVariant = Value2.AsVariant))
    );
end;

class function TRTTI.isA<T>(ctrl: TObject; const PropertyName: string): boolean;
var
  cxt : TRTTIContext;
  prop : TRttiProperty;
begin
  Result := False;
  prop := cxt.GetType(ctrl.ClassInfo).GetProperty(PropertyName);
  if prop <> nil then
  begin
    Result := prop.GetValue(ctrl).IsType(TypeInfo(T));
  end;
end;

class function TRTTI.isA<T>(ctrl: TObject; const PropertyName: string;
  args: TArray<TValue>): boolean;
var
  cxt : TRTTIContext;
  prop : TRttiIndexedProperty;
begin
  Result := False;
  prop := cxt.GetType(ctrl.ClassInfo).GetIndexedProperty(PropertyName);
  if prop <> nil then
  begin
    Result := prop.GetValue(ctrl, args).IsType(TypeInfo(T));
  end;
end;

class function TRTTI.isMethod(ctrl: TObject; const MethodName: string): boolean;
var
  cxt : TRTTIContext;
begin
  Result := cxt.GetType(ctrl.ClassInfo).GetMethod(MethodName) <> nil;
end;

class procedure TRTTI.sett<T>(ctrl: TObject; const PropertyName: string; Value: T);
var
  cxt : TRTTIContext;
  prop : TRttiProperty;
begin
  prop := cxt.GetType(ctrl.ClassInfo).GetProperty(PropertyName);
  if prop <> nil then
  begin
    prop.SetValue(ctrl, TValue.From<T>(Value));
  end;
end;

class procedure TRTTI.sett<T>(ctrl: TObject; const PropertyName: string;
  args : TArray<TValue>; Value: T);
var
  cxt : TRTTIContext;
  prop : TRttiIndexedProperty;
begin
  prop := cxt.GetType(ctrl.ClassInfo).GetIndexedProperty(PropertyName);
  if prop <> nil then
  begin
    prop.SetValue(ctrl, args, TValue.From<T>(Value));
  end;
end;

class procedure TRTTI.setValue(ctrl: TObject; const PropertyName: string; Value: TValue);
var
  cxt : TRTTIContext;
  prop : TRttiProperty;
begin
  prop := cxt.GetType(ctrl.ClassInfo).GetProperty(PropertyName);
  if prop <> nil then
  begin
    prop.SetValue(ctrl, Value);
  end;
end;

class function TRTTI.tryCall(ctrl: TObject; const MethodName: string; args: TArray<TValue>; var AResult: TValue): boolean;
begin
  result := isMethod(ctrl, MethodName);
  if result then
    AResult := call(ctrl, MethodName, args);
end;

class function TRTTI.tryGet<T>(ctrl: TObject; const PropertyName: string;
  args : TArray<TValue>; var AResult: T): boolean;
begin
  result := isProperty(ctrl, PropertyName);
  if result then
    AResult := get<T>(ctrl, PropertyName, args);
end;

class function TRTTI.tryGet<T>(ctrl: TObject; const PropertyName: string; var AResult: T): boolean;
begin
  result := isProperty(ctrl, PropertyName);
  if result then
    AResult := get<T>(ctrl, PropertyName);
end;

class function TRTTI.trySet<T>(ctrl: TObject; const PropertyName: string; Value: T): boolean;
begin
  result := isProperty(ctrl,propertyName);
  if result then
    sett<T>(ctrl, propertyName, Value);
end;

class function TRTTI.trySet<T>(ctrl: TObject; const PropertyName: string;
  args : TArray<TValue>; Value: T): boolean;
begin
  result := isProperty(ctrl,propertyName);
  if result then
    sett<T>(ctrl, propertyName, args, Value);
end;

{ TEvent }

constructor TEvent.Create;
begin
  inherited Create;
  FEvents := TDictionary<TPair<Pointer,Pointer>,IInterface>.Create;
end;

destructor TEvent.Destroy;
begin
  FEvents.Free;
  inherited;
end;

procedure TEvent.MakeEvent(const ameth; var evt);
type
  TVtable = array[0..3] of Pointer;
  PVtable = ^TVtable;
  PPVtable = ^PVtable;
begin
  // 3 is offset of Invoke, after QI, AddRef, Release
  TMethod(evt).Code := PPVtable(ameth)^^[3];
  TMethod(evt).Data := Pointer(ameth);
end;

procedure TEvent.ParseMethod(const meth; var p1, p2 : Pointer);
type
  TVtable = array[0..3] of Pointer;
  PVtable = ^TVtable;
  PPVtable = ^PVtable;
begin
  // 3 is offset of Invoke, after QI, AddRef, Release
  p1 := TMethod(meth).Code;
  p2 := TMethod(meth).Data;
end;

function TEvent.From<TEvent, TReference>(proc: TReference): TEvent;
type
  PInterface = ^IInterface;
var
  intf : PInterface;
  p1, p2 : Pointer;
begin
  TMonitor.Enter(Self);
  try
    MakeEvent(proc,Result);
    intf := PInterface(@proc);

    ParseMethod(Result, p1, p2);
    FEvents.Add(TPair<Pointer,Pointer>.Create(p1, p2),intf^);
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TEvent.Release<TEvent>(var event: TEvent);
type
  PInterface = ^IInterface;
var
  intf : IInterface;
  p1, p2 : Pointer;
begin
  TMonitor.Enter(Self);
  try
    ParseMethod(event, p1, p2);
    FEvents.Remove(TPair<Pointer,Pointer>.Create(p1, p2));
  finally
    TMonitor.Exit(Self);
  end;
  event := TEvent(nil);
end;

function TEvent.Notify(proc: TNotifyReference): TNotifyEvent;
begin
  Result := From<TNotifyEvent, TNotifyReference>(proc);
end;

procedure TEvent.NotifyRelease(var event: TNotifyEvent);
begin
  Release<TNotifyEvent>(event);
end;

function TEvent.IsSameEvent(const ameth, evt): boolean;
type
  TVtable = array[0..3] of Pointer;
  PVtable = ^TVtable;
  PPVtable = ^PVtable;
begin
  // 3 is offset of Invoke, after QI, AddRef, Release
  result := (TMethod(evt).Code = PPVtable(ameth)^^[3]) and
            (TMethod(evt).Data = Pointer(ameth));
end;

{ TDuckImpl }

constructor TDuckImpl.Create(AOwner: TObject);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TDuckImpl.EnumerateAssociatedObjects(AOwner: TObject; List : TList<TObject>);
  procedure DoIt(const IndexProp : string; const CountProp : string);
  var
    i, iCnt : integer;
    obj : TObject;
  begin
    if TRTTI.tryGet<integer>(FOwner,CountProp, iCnt) then
      for i := 0 to iCnt-1 do
      begin
        if TRTTI.isA<TObject>(FOwner,IndexProp,IndexParam(i)) and
           TRTTI.tryGet<TObject>(FOwner,IndexProp,IndexParam(i), obj) then
        begin
          List.Add(obj);
        end;
      end;
  end;
var
  i : integer;
begin
  for i := 0 to Length(enumprops)-1 do
    DoIt(enumprops[i][0], enumprops[i][1]);
end;

function TDuckImpl.call(methodName: string; args: TArray<TValue>): TValue;
begin
  call(methodName,args,Result);
end;

function TDuckImpl.call(methodName: string; args: TArray<TValue>;
  var Exists: boolean): TValue;
begin
  Exists := call(methodName,args,Result);
end;

function TDuckImpl.call(methodName: string; var Exists: boolean): TValue;
var
  args : TArray<TValue>;
begin
  Exists := call(methodName,args,Result);
end;

function TDuckImpl.call(methodName: string; args: TArray<TValue>;
  var AResult: TValue): boolean;
var
  cxt : TRTTIContext;
begin
  Result := TRTTI.isMethod(FOwner,methodName);
  if Result then
  begin
    AResult := cxt.GetType(FOwner.ClassInfo).GetMethod(methodName).Invoke(FOwner,Args);
  end;
end;

function TDuckImpl.call(methodName: string; var AResult: TValue): boolean;
var
  args : TArray<TValue>;
begin
  Result := call(methodName,args,AResult);
end;

function TDuckImpl.call(methodName: string): TValue;
var
  args : TArray<TValue>;
begin
  call(methodName, args, Result);
end;

function TDuckImpl.get(propertyName: string): TValue;
begin
  Result := TRTTI.getValue(FOwner,propertyName);
end;

function TDuckImpl.all: ISelector;
var
  lst : TList<TObject>;
begin
  lst := TList<TObject>.Create;
  try
    EnumerateAssociatedObjects(FOwner, lst);
    Result := TSelectorImpl.Create(FOwner, '', lst);
  finally
    lst.Free;
  end;
end;

procedure TDuckImpl.sett(propertyName: string; Value: TValue);
begin
  TRTTI.setValue(FOwner,propertyName, Value);
end;

function TDuckImpl.has(propertyName : string) : boolean;
begin
  Result := TRTTI.isProperty(FOwner, propertyName);
end;

function TDuckImpl.IndexParam(idx: integer): TArray<TValue>;
begin
  SetLength(Result,1);
  Result[0] := idx;
end;

function TDuckImpl.obj: TObject;
begin
  Result := Self;
end;

function TDuckImpl.can(methodName : string) : boolean;
begin
  Result := TRTTI.isMethod(FOwner, methodName);
end;

{ TDuckHelper }

function TDuckHelper.Duck: IDuck;
begin
  Result := TDuckImpl.Create(Self);
end;

{ TDuck }

function TDuck.Duck: IDuck;
begin
  Result := TDuckImpl.Create(Self);
end;

{ TSelectorImpl }

constructor TSelectorImpl.Create(AObject: TObject; AContext : string; AResults : TList<TObject>);
var
  i : integer;
begin
  inherited Create;
  FObject := AObject;
  FContext := AContext;
  FResults := TList<TObject>.Create;
  for i := 0 to AResults.Count-1 do
  begin
    if not FResults.Contains(AResults[i]) then
      FResults.Add(AResults[i]);
  end;
end;

destructor TSelectorImpl.Destroy;
begin
  FResults.Free;
  inherited;
end;

function TSelectorImpl.go(args: TArray<TValue>): ISelector;
begin
  call(args);
  Result := Self;
end;

function TSelectorImpl.go(methodName: string): ISelector;
begin
  call(methodName);
  result := Self;
end;

function TSelectorImpl.go: ISelector;
begin
  call;
  result := self;
end;

function TSelectorImpl.go(methodName: string; args: TArray<TValue>): ISelector;
begin
  call(methodName, args);
  result := Self;
end;

function TSelectorImpl.call(methodName: string): TArray<TValue>;
var
  i : integer;
  args : TArray<TValue>;
begin
  setLength(Result,FResults.Count);
  for i := 0 to FResults.Count-1 do
  begin
    result[i] := TRTTI.call(FResults.Items[i],methodName, args);
  end;
end;

function TSelectorImpl.add(propertyName: string; Value: TValue): ISelector;
var
  i : integer;
begin
  for i := 0 to FResults.Count-1 do
  begin
    TRTTI.setValue(FResults.Items[i],propertyName, TValue.FromVariant(TRTTI.getValue(FResults.Items[i],propertyName).AsVariant + Value.AsVariant));
  end;
  Result := Self;
end;

function TSelectorImpl.add(Value: TValue): ISelector;
var
  i : integer;
begin
  RequireContext;
  for i := 0 to FResults.Count-1 do
  begin
    TRTTI.setValue(FResults.Items[i],FContext, TValue.FromVariant(TRTTI.getValue(FResults.Items[i],FContext).AsVariant + Value.AsVariant));
  end;
  Result := Self;
end;

function TSelectorImpl.call(methodName: string;
  args: TArray<TValue>): TArray<TValue>;
var
  i : integer;
begin
  setLength(Result,FResults.Count);
  for i := 0 to FResults.Count-1 do
  begin
    result[i] := TRTTI.call(FResults.Items[i],methodName, args);
  end;
end;

function TSelectorImpl.call: TArray<TValue>;
var
  i : integer;
  args : TArray<TValue>;
begin
  RequireContext;
  setLength(Result,FResults.Count);
  for i := 0 to FResults.Count-1 do
  begin
    result[i] := TRTTI.call(FResults.Items[i],FContext, args);
  end;
end;

function TSelectorImpl.call(args: TArray<TValue>): TArray<TValue>;
var
  i : integer;
begin
  RequireContext;
  setLength(Result,FResults.Count);
  for i := 0 to FResults.Count-1 do
  begin
    result[i] := TRTTI.call(FResults.Items[i],FContext, args);
  end;
end;

function TSelectorImpl.can(MethodName: string): ISelector;
var
  lst : TList<TObject>;
  i : integer;
begin
  lst := TList<TObject>.Create;
  try
    for i := 0 to FResults.Count-1 do
    begin
      if TRTTI.isMethod(FResults.Items[i],methodName) then
      begin
        lst.Add(FResults.Items[i]);
      end;
    end;
    Result := TSelectorImpl.Create(FObject, MethodName, lst);
  finally
    lst.Free;
  end;
end;

function TSelectorImpl.count: integer;
begin
  Result := FResults.Count;
end;

function TSelectorImpl.has(NameValues: TArray<TPropValue>): ISelector;
var
  lst : TList<TObject>;
  i, j : integer;
  b : boolean;
begin
  lst := TList<TObject>.Create;
  try
    for i := 0 to FResults.Count-1 do
    begin
      b := True;

      for j := 0 to length(NameValues)-1 do
      begin
        if (not TRTTI.isProperty(FResults.Items[i],NameValues[j].PropertyName)) or
           (not TRTTI.isSame(TRTTI.getValue(FResults.Items[i],NameValues[j].PropertyName), NameValues[j].Value)) then
        begin
          b := False;
          break;
        end;
      end;
      if b then
      begin
        lst.Add(FResults.Items[i]);
      end;
    end;
    Result := TSelectorImpl.Create(FObject, '', lst);
  finally
    lst.Free;
  end;
end;

function TSelectorImpl.has(PropertyNames: TArray<string>): ISelector;
var
  lst : TList<TObject>;
  i, j : integer;
begin
  lst := TList<TObject>.Create;
  try
    for i := 0 to FResults.Count-1 do
    begin
      for j := 0 to Length(PropertyNames)-1 do
        if TRTTI.isProperty(FResults.Items[i],PropertyNames[j]) then
        begin
          lst.Add(FResults.Items[i]);
        end;
    end;
    Result := TSelectorImpl.Create(FObject,'',lst);
  finally
    lst.Free;
  end;
end;

function TSelectorImpl.has(PropertyName: string): ISelector;
var
  lst : TList<TObject>;
  i : integer;
begin
  lst := TList<TObject>.Create;
  try
    for i := 0 to FResults.Count-1 do
    begin
      if TRTTI.isProperty(FResults.Items[i],propertyName) then
      begin
        lst.Add(FResults.Items[i]);
      end;
    end;
    Result := TSelectorImpl.Create(FObject, PropertyName, lst);
  finally
    lst.Free;
  end;
end;

function TSelectorImpl.isa(ClassType: TClass): ISelector;
begin

end;

function TSelectorImpl.items : TArray<TObject>;
begin
  Result := FResults.ToArray;
end;

procedure TSelectorImpl.RequireContext;
begin
  if FContext = '' then
    raise Exception.Create(S_MISSINGCONTEXT);
end;

function TSelectorImpl.sett(Value: TValue) : ISelector;
var
  i : integer;
begin
  RequireContext;

  for i := 0 to FResults.Count-1 do
  begin
    TRTTI.setValue(FResults.Items[i],FContext, Value);
  end;
  Result := Self;
end;

function TSelectorImpl.sett(propertyName: string; Value: TValue) : ISelector;
var
  i : integer;
begin
  for i := 0 to FResults.Count-1 do
  begin
    TRTTI.setValue(FResults.Items[i],propertyName, Value);
  end;
  Result := Self;
end;

function TSelectorImpl.subtract(Value: TValue): ISelector;
var
  i : integer;
begin
  RequireContext;
  for i := 0 to FResults.Count-1 do
  begin
    TRTTI.setValue(FResults.Items[i],FContext, TValue.FromVariant(TRTTI.getValue(FResults.Items[i],FContext).AsVariant - Value.AsVariant));
  end;
  Result := Self;
end;

function TSelectorImpl.subtract(propertyName: string; Value: TValue): ISelector;
var
  i : integer;
begin
  for i := 0 to FResults.Count-1 do
  begin
    TRTTI.setValue(FResults.Items[i],propertyName, TValue.FromVariant(TRTTI.getValue(FResults.Items[i],propertyName).AsVariant - Value.AsVariant));
  end;
  Result := Self;
end;

function TSelectorImpl.has(PropertyName: string; Value: TValue): ISelector;
var
  lst : TList<TObject>;
  i : integer;
begin
  lst := TList<TObject>.Create;
  try
    for i := 0 to FResults.Count-1 do
    begin
      if TRTTI.isProperty(FResults.Items[i],propertyName) then
      begin
        if TRTTI.isSame(TRTTI.getValue(FResults.Items[i],propertyName),Value) then
          lst.Add(FResults.Items[i]);
      end;
    end;
    Result := TSelectorImpl.Create(FObject,PropertyName, lst);
  finally
    lst.Free;
  end;
end;

{ TPropValue }

constructor TPropValue.Create(const APropertyName: string; const AValue: TValue);
begin
  Self.PropertyName := APropertyName;
  Self.Value := AValue;
end;

initialization
  Event := TEvent.Create;
  InitializeEnumProps;

finalization
  Event.Free;

end.
