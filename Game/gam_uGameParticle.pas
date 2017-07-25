(*
		Liquid Story - Copyright(c) 2011

	Author:
    	Lionel Parreaux
    
    Abstract:
    	Game particles are the particles we use in the game
    
    Please see 'copyright.txt' for details about the copyright.
*)

unit gam_uGameParticle;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  ppe_uEngine,
  ppe_uParticle,
  ppe_uLink,
  ppe_uTrace;

type tPartType = (ptLiquid, ptIce, ptVapor, ptCloud);

type cGameParticle = Class(cParticle)
     public
     connected:Boolean;
     wind:real;
     partType:tPartType;
     procedure enterFrame(motionSpeed:real); virtual;
     procedure init; override;
end;

var gameParts:Array[1..maxPts] of cGameParticle; nbGamPts:integer;

implementation

uses gam_uGame;

procedure cGameParticle.init;
var i:integer; l:cLink;
begin
	inherited;
    //wind:=0.1+random(50)/1000;
    wind:=0.001;
    if gam_uGame.interParticleCollisions then begin
	    {if ClassName<>'cCloudParticle' then for i:=1 to nbGamPts do
	    if gameParts[i].ClassName<>'cCloudParticle' then begin}
        if partType<>ptCloud then for i:=1 to nbGamPts do
	    if gameParts[i].partType<>ptCloud then begin
    	    l:=cLink.Create(gameParts[i],self,size/2+gameParts[i].size/2,100000000);
    	    l.hide;
    	    game.engine.addLink(l);
        end;
    end;

	if nbGamPts <= maxPts then begin
		nbGamPts := nbGamPts+1;
		gameParts[nbGamPts] := self;
	end;
	connected := false;
end;

procedure cGameParticle.enterFrame(motionSpeed:real);
begin
    sx:=sx+wind*motionspeed/mass;
end;

initialization
nbGamPts := 0;

end.

