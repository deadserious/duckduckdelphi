// *****************************************************************************
//      DUCK DUCK DELPHI
// *****************************************************************************
//
//  Description:
//    This unit adds simple duck typing to Delphi TPersistent descendants and
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
//  "obj" in the above examples can be any TPersistent descendant.
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

uses System.SysUtils, System.Classes, System.Types, System.Rtti, System.Generics.Collections;

type
  TNotifyReference = reference to procedure(Sender : TObject);

  TRTTI = class
  public
    class function get<T>(ctrl: TPersistent; const PropertyName: string) : T; static;
    class function tryGet<T>(ctrl: TPersistent; const PropertyName: string; var AResult : T) : boolean; static;
    class procedure sett<T>(ctrl: TPersistent; const PropertyName: string; Value: T); static;
    class function trySet<T>(ctrl: TPersistent; const PropertyName: string; Value: T) : boolean; static;
    class function exists(ctrl: TPersistent; const name: string) : boolean; static;
    class function getValue(ctrl: TPersistent; const PropertyName: string) : TValue; static;
    class procedure setValue(ctrl: TPersistent; const PropertyName: string; Value : TValue); static;

    class function call(ctrl : TPersistent; const MethodName : string; args : TArray<TValue>) : TValue;
    class function tryCall(ctrl : TPersistent; const MethodName : string; args : TArray<TValue>; var AResult : TValue) : boolean;

    class function isProperty(ctrl: TPersistent; const PropertyName: string) : boolean; static;
    class function isMethod(ctrl: TPersistent; const MethodName: string) : boolean; static;
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
  end;

  TDuckImpl = class(TInterfacedObject, IDuck)
  private
    FOwner : TPersistent;
  public
    constructor Create(AOwner: TPersistent); virtual;
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
  end;

  TDuckHelper = class helper for TPersistent
  public
    function Duck : IDuck;
  end;

  TDuck = class(TPersistent)
  public
    function Duck : IDuck;
  end;

var
  Event : TEvent;

implementation

{ TRTTI }

class function TRTTI.call(ctrl: TPersistent; const MethodName: string; args: TArray<TValue>): TValue;
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

class function TRTTI.get<T>(ctrl: TPersistent; const PropertyName: string): T;
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

class function TRTTI.getValue(ctrl: TPersistent; const PropertyName: string): TValue;
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

class function TRTTI.exists(ctrl: TPersistent; const Name: string): boolean;
begin
  Result := isProperty(ctrl, Name) or isMethod(ctrl, Name);
end;

class function TRTTI.isProperty(ctrl: TPersistent; const PropertyName: string): boolean;
var
  cxt : TRTTIContext;
begin
  Result := cxt.GetType(ctrl.ClassInfo).GetProperty(PropertyName) <> nil;
end;

class function TRTTI.isMethod(ctrl: TPersistent; const MethodName: string): boolean;
var
  cxt : TRTTIContext;
begin
  Result := cxt.GetType(ctrl.ClassInfo).GetMethod(MethodName) <> nil;
end;

class procedure TRTTI.sett<T>(ctrl: TPersistent; const PropertyName: string; Value: T);
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

class procedure TRTTI.setValue(ctrl: TPersistent; const PropertyName: string; Value: TValue);
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

class function TRTTI.tryCall(ctrl: TPersistent; const MethodName: string; args: TArray<TValue>; var AResult: TValue): boolean;
begin
  result := isMethod(ctrl, MethodName);
  if result then
    AResult := call(ctrl, MethodName, args);
end;

class function TRTTI.tryGet<T>(ctrl: TPersistent; const PropertyName: string; var AResult: T): boolean;
begin
  result := isProperty(ctrl, PropertyName);
  if result then
    AResult := get<T>(ctrl, PropertyName);
end;

class function TRTTI.trySet<T>(ctrl: TPersistent; const PropertyName: string; Value: T): boolean;
begin
  result := isProperty(ctrl,propertyName);
  if result then
    sett(ctrl, propertyName, Value);
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

constructor TDuckImpl.Create(AOwner: TPersistent);
begin
  inherited Create;
  FOwner := AOwner;
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

procedure TDuckImpl.sett(propertyName: string; Value: TValue);
begin
  TRTTI.setValue(FOwner,propertyName, Value);
end;

function TDuckImpl.has(propertyName : string) : boolean;
begin
  Result := TRTTI.isProperty(FOwner, propertyName);
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

initialization
  Event := TEvent.Create;

finalization
  Event.Free;

end.
