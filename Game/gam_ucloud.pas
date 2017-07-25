(*
		Liquid Story - Copyright(c) 2011

	Authors:
    	Lionel Parreaux
        Pierre *CURIS*
    
    Abstract:
    	The clouds class
    
    Please see 'copyright.txt' for details about the copyright.
*)

unit gam_uCloud;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  ppe_uengine,
  ppe_uLink,
  ppe_uparticle,
  ppe_uDisplayer,
  ppe_uDisplayPoint,
  gam_uCloudParticle
  ;

type cCloud=class
  public
  constructor create(x,y:real;engine:cEngine);


end;

implementation

constructor cCloud.create(x,y:real;engine:cEngine);
 const size=20; nbCloudParts = 10;
 var i,j,posX,posY:integer; p1:cParticle; p2:cCloudParticle; lc:cLink; pts:Array[1..nbCloudParts] of cCloudParticle;
 begin
  for i:=1 to nbCloudParts do
   begin
   posX := random(size*6)-size*3;
   posY := random(size*2)-size;
   
   p1 := cParticle.Create(x+posX,y+posY,0);
   p1.fixed := true;
   p1.color := clblack;
   engine.addParticle(p1);
   
   p2 := cCloudParticle.Create(x+posX,y+posY,size/nbCloudParts*30);
   engine.addParticle(p2);
   p2.color := clwhite;
   //lc:=cLink.Create(p1,p2,11,11);
   lc := cLink.Create(p1,p2);
   lc.elasticity := 0.005;//0.03;
   lc.hide;
   engine.addLink(lc);
   
   for j:=1 to i-1 do begin
   	   lc := cLink.Create(p2,pts[j]);
   	   lc.elasticity := 0.03;
   	   lc.hide;
       engine.addLink(lc);
   end;
   
   pts[i] := p2;
   
 end;
end;

end.

