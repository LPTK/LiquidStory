(*
		Pascal Physics Engine - Copyright(c) 2011

	Author:
    	Lionel Parreaux
    
    Abstract:
    	Particles are the main object of the engine
        They have a size plus some x/y positions and speeds and can be
        submitted to the cLink constraints
    
    Please see 'copyright.txt' for details about the copyright.
*)

unit ppe_uparticle;

{$mode objfpc}{$H+}

interface

            
uses
  Classes, SysUtils, ppe_uDisplayPoint, ppe_uTrace;

type cParticle = Class(cDisplayPoint)
     public
     id:integer;
     xSave,ySave,sx,sy:real;
     fixed,draggable:Boolean; mass:real;
     ppeNextPart,ppePrevPart:cParticle;
     constructor Create(_x,_y:real);
     constructor Create(ax,ay,asize:real);
     procedure init; override;
     destructor Destroy; Override;
end;

var nbTotPts:integer;

implementation

procedure cParticle.init;
begin
	inherited;
	id := nbTotPts;
	nbTotPts := nbTotPts+1;
	mass := 1; draggable := true;
end;

constructor cParticle.Create(ax,ay,asize:real);
begin
     x := ax; y := ay; size := asize;
     inherited Create(ax,ay);
end;

constructor cParticle.Create(_x,_y:real);
begin
	trace('Warning: a particle has size=0.');
     Create(_x,_y,20);
end;

destructor cParticle.destroy;
begin
     inherited;
end;


end.

