(*
		Pascal Physics Engine - Copyright(c) 2011

	Author:
    	Lionel Parreaux
    
    Abstract:
    	Grounds are supposed to be fixed line segments to which particles
        collide, but I haven't implemented them in this pascal version of the
        engine
    
    Please see 'copyright.txt' for details about the copyright.
*)

unit ppe_uGround; // Note that grounds were not implemented because we realized we didn't need them!

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ppe_upoint;

type cGround = Class
     public
     p1,p2:cPoint;
     constructor Create(_p1,_p2:cPoint);
     constructor Create(x1,y1,x2,y2:real);
     destructor Destroy; override;
end;

implementation

constructor cGround.Create(_p1,_p2:cPoint);
begin
     p1 := _p1; p2 := _p2;
end;

constructor cGround.Create(x1,y1,x2,y2:real);
begin
     Create(cPoint.Create(x1,y1),cPoint.Create(x2,y2));
end;

destructor cGround.Destroy;
begin
     inherited;
     p1.Free; p2.Free;
end;

end.

