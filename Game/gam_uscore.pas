(*
		Liquid Story - Copyright(c) 2011

	Authors:
    	Jordan Vincent
    
    Abstract:
    	Manages the score and particles count
    
    Please see 'copyright.txt' for details about the copyright.
*)

unit gam_uScore;

{$mode objfpc}

interface

uses
  Classes, SysUtils, Graphics, ppe_uparticle,gam_uGameParticle,ppe_uengine;

type cScore = Class
nbLiq, nbSol, nbVap, maxLiq, maxSol, maxVap, points:integer;
maxheight:real;

constructor Create();
procedure Counter(engine:cengine);
procedure Display(engine:cengine; canv:tCanvas);
procedure Bonus(plusSol, plusLiq, plusVap, plusPoints:integer);
function noLiq():boolean;
function noVap():boolean;
function noSol():boolean;
function getMaxHeight:real;
procedure allTimesMaxH;
end;

var mainScore:cScore;

implementation

constructor cScore.Create();
begin
     inherited;
     maxLiq:=57;
     maxsol:=30;
     maxVap:=15;
     maxheight:=0;
     mainScore := self;
end;


procedure cScore.Counter(engine:cengine);
var i,s,l,v:integer;
begin
     s:=0; l:=0; v:=0;
     for i:=1 to nbGamPts do
     case Gameparts[i].partType of
     ptIce : s:=s+1;
     ptLiquid : l:=l+1;
     PtVapor : v:=v+1;
     end;
     nbLiq:=l; nbSol:=s; nbVap:=v;
     allTimesMaxH;
     points:=nbLiq*2+nbSol*3+nbVap*4+round(maxheight/3);
end;

procedure cScore.Bonus(plusSol, plusLiq, plusVap, plusPoints:integer);
begin
     maxSol:=maxSol+plusSol;
     maxliq:=maxliq+plusliq;
     maxVap:=maxVap+plusVap;
     points:=points+plusPoints;
end;

procedure cScore.Display(engine:cengine; canv:tCanvas);
var t1,t2,t3,t4:string;
begin
     Counter(engine);
     canv.brush.color:= clwhite;
     t1:= Concat(' ',floatToStr(points),'Pts');
     t2:= Concat(' ',floatToStr(maxSol-nbSol));
     t3:= Concat(' ',floatToStr(maxLiq-nbLiq));
     t4:= Concat(' ',floatToStr(maxVap-nbVap));
     canv.pen.color:= clblack;
     canv.TextOut(730,15,t1);
     canv.TextOut(755,49,t4);
     canv.TextOut(755,83,t3);
     canv.TextOut(755,117,t2);
end;

function cScore.getMaxHeight:real;
var i:integer; p:cGameParticle;
begin
    p := gameParts[1];
    for i:=1 to nbGamPts do
    	if gameParts[i].connected
        and (gameParts[i].y-gameParts[i].size/2 < p.y-p.size/2)
          then p := gameParts[i];
    getMaxHeight := 600-p.y+p.size/2;
end;

procedure cScore.allTimesMaxH;
var m:real;
begin
    m:=getMaxHeight;
    if maxheight <= m then maxheight:= m;
end;

function cScore.noLiq():boolean;
begin
     noLiq:=false;
     if maxLiq-nbLiq<=0 then noLiq:=true;
end;

function cScore.noVap():boolean;
begin
     noVap:=false;
     if maxVap-nbVap<=0 then noVap:=true;
end;

function cScore.noSol():boolean;
begin
     noSol:=false;
     if maxSol-nbSol<=0 then noSol:=true;
end;

end.


