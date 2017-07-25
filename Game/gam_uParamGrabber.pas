(*
		Liquid Story - Copyright(c) 2011

	Authors:
    	Lionel Parreaux
    
    Abstract:
    	Retrieving parameters in the 'config.ini' file
    
    Please see 'copyright.txt' for details about the copyright.
*)

unit gam_uParamGrabber; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ppe_uTrace;

var cantReadParamFile:Boolean;

function paramExists(name:String):Boolean;
function getStr(name:String):String;
function getBool(name:String):Boolean;
function getInt(name:String):Integer;

implementation

type CString = Class(tObject)
  str:String;
  constructor Create(s:String);
end;

var
  paramList, objects:tStringList;
  i:integer; strList:TStringList;

constructor CString.Create(s:String); begin str := s end;

function paramExists(name:String):Boolean;
begin paramExists := (objects.IndexOf(name)<>-1) end;

function getStr(name:String):String; var index:Integer;
begin
    index := objects.IndexOf(name);
    if index=-1 then getStr := ''
    else getStr := (CString(objects.Objects[index])).str;
end;

function getBool(name:String):Boolean; var s:String;
begin
    s := getStr(name);
    if s='' then getBool := false else getBool := strToBool(s);
end;

function getInt(name:String):Integer; var s:String;
begin
    s := getStr(name);
    if s='' then getInt := 0 else getInt := strToInt(s);
end;

procedure newParam(name,val:String);
begin
	trace(concat(name+' : ',val));
    objects.AddObject(name,CString.Create(val));
end;

procedure Split
   (const Delimiter: Char;
    Input: string;
    const Strings: TStrings) ;
begin
   Assert(Assigned(Strings)) ;
   Strings.Clear;
   Strings.Delimiter := Delimiter;
   Strings.DelimitedText := Input;
end;

initialization

trace('Loading parameters from file');

paramList := tStringList.Create;
try
	paramList.LoadFromFile('config.ini');
except
	trace('Unable to read parameters in file config.ini');
    //MessageDLG('Warning!', 'Unable to read parameters in file config.ini', mtWarning, [mbClose],0)
    cantReadParamFile := true;
end;

objects := tStringList.Create;
strList := tStringList.Create;

for i := 0 to paramList.Count-1 do begin
    Split(' ', paramList[i], strList);
	if(strList.Count>1) then newParam(strList[0],strList[1])
    else if(strList.Count>0) then trace('Invalid initialization: '+strList[0]);
end;

paramList.Free;
strList.Free;

end.




