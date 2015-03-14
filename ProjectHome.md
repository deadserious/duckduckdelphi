**DUCK DUCK DELPHI**

> Description:
> > This unit adds simple duck typing to Delphi Objects and
> > provides an RTTI helper class to simplify many common RTTI tasks.


> Compatibility:
> > Requires Delphi XE2.  Older versions **may** be compatible but not supported.


> For more information on this project, please visit:
> > http:/arcana.sivv.com

With DuckDuckDelphi you can take advantage of duck typing.  Instead of needing to know what an object is, you can often accomplish tasks by knowing what it looks like and what it can do.

With DuckDuckDelphi you can now inspect any object using it's Duck property.  for example...
```
Edit1.duck.has('visible') 
```
...obviously this would return true as TEdits have visible properties...  but what if you didn't want to know each object's type and operate only on those objects that have "Visible" properties... say you wanted to hide all objects on a form...
```
Form1.duck.all.has('visible').sett(false);
```
This code iterates through all objects on the form and sets the ones that have a "visible" property to false, regardless their type.

What if you want to clear all memos, text edits, etc on a form via a buttonclick?
```
procedure TForm1.Button2Click(Sender: TObject);
begin
  duck.all.can('Clear').go;
end;
```
pretty simple, eh?

And this obviously is not limited to visual components or objects of a form. Lets say you have an object list that you want to update en-mass...
```
var list : TList<TObject>
.... create and fill list ...
list.duck.all.has('timestamp').set(Now);
```
DuckDuckDelphi knows about common object colletions as used through the RTL, but if you have custom collection properties, you can register them easily using...

Suppose you had a class with the following properties:
```
TMyType = class(TObject)
  property CustomObjects[idx : integer] : TObject ...
  property ObjectCount : integer ... 
end;
```
Then you could register that collection as follows:
```
RegisterPropertyEnumerator('CustomObjects', 'ObjectCount');
```
Duck typing in a strongly typed language such as Delphi has never been easy.  Now you can have the best of both worlds.

Please give DuckDuckDelphi a try and I'm sure you will find yourself using it in nearly every Delphi project very soon.