(*
		Pascal Physics Engine - Copyright(c) 2011

	Author:
    	Lionel Parreaux
    
    Abstract:
    	A very nice and useful tool to display informations in any memo
        You just have to set 'traceMemo := myForm.myMemo' and use the syntax
        'trace(anything)' and your messages will be displayed in the memo
    
    Please see 'copyright.txt' for details about the copyright.
*)

unit ppe_uTrace; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls; 

procedure trace(str:String);
procedure trace(n:integer);
procedure trace(x:real);
procedure trace(b:Boolean);
procedure trace(obj:TObject);

var traceMemo:tMemo;

implementation

procedure trace(str:String);
begin
     traceMemo.append(str);
end;

procedure trace(n:integer);
begin
     traceMemo.append(intToStr(n));
end;

procedure trace(x:real);
begin
     traceMemo.append(floatToStr(x));
end;

procedure trace(b:Boolean);
begin
     if b then trace('true') else trace('false');
end;

procedure trace(obj:TObject);
begin
	 if obj = NIL then trace('NIL')
     else trace(Concat('[Object ',obj.ClassName,']'));
     
     //Form3.Memo1.append(String(obj));
     //Form3.Memo1.append(obj.toString);
end;

initialization

traceMemo := tMemo.Create(NIL);

end.

