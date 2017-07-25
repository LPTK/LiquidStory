(*
		Pascal Physics Engine - Copyright(c) 2011

	Author:
    	Lionel Parreaux
    
    Abstract:
    	Just a basic class representing points with real-valued coordinates
    
    Please see 'copyright.txt' for details about the copyright.
*)

unit ppe_uPoint;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type cPoint = Class
     public
     x,y:real;
     constructor Create(_x,_y:real);
     destructor Destroy; Override;
     procedure init; virtual;
end;

implementation

procedure cPoint.init(); begin end;

constructor cPoint.Create(_x,_y:real);
begin
     x := _x; y := _y;
	 init;
end;

destructor cPoint.destroy;
begin
     inherited;
end;

end.
       
