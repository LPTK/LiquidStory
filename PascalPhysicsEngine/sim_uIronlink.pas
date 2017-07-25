(*
		Pascal Physics Engine - Copyright(c) 2011

	Author:
    	Lionel Parreaux
    
    Abstract:
    	Some links that explode into many other links when broken, giving
        a nice sensation of rupture
    
    Please see 'copyright.txt' for details about the copyright.
*)

unit sim_uIronLink; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ppe_uLink, ppe_uEngine, ppe_uParticle, ppe_uPropContainer, ppe_uTrace; 

const breakingBitsNb = 2;

type cIronLink = Class(cLink)
     //breakingBitsNb:integer;
     procedure refresh(computeSpeeds:Boolean; motionSpeed:real); override;
     procedure breakLink(); override;
     //constructor Create();
end;

implementation

{constructor cIronLink.Create;
begin
     breakingBitsNb := 2;
end;}

procedure cIronLink.refresh(computeSpeeds:Boolean; motionSpeed:real);
var br:Boolean; //i,j,nbP:integer; parts:array[0..30] of cParticle; p:cParticle; l:cLink; co:real; pc:cPropContainer;
begin
     br := broken;
     inherited;
     if not br and broken then begin
     
     	breakLink;
     
     	{//trace('ouch!');
     	//p := cParticle.Create(p1.x+10,p2.y);	   p.sx := 0; p.sy := 0;
     	//mainEngine.addParticle(p);
     	//trace(mainEngine.addParticle(p));
     	nbP := Random(breakingBitsNb)+1;
     	//nbP := 1;
     	for i := 0 to nbP+1 do begin
     	    co := i/(nbP+1);
     	    {if i <> 0 then begin parts[i*2] := cParticle.Create(p1.x*co+p2.x*(1-co), p1.y*co+p2.y*(1-co), p1.size*co+p2.size*(1-co));
     	       mainEngine.addParticle(parts[i*2]); parts[i].mass := (p1.mass*co+p2.mass*(1-co))/2; end;
     	    if i <> nbP+1 then begin parts[i*2+1] := cParticle.Create(p1.x*co+p2.x*(1-co), p1.y*co+p2.y*(1-co), p1.size*co+p2.size*(1-co));
     	       mainEngine.addParticle(parts[i*2+1]); parts[i+1].mass := (p1.mass*co+p2.mass*(1-co))/2; end;}
     	    for j := 0 to 1 do begin
     	    	p := cParticle.Create(p1.x*co+p2.x*(1-co), p1.y*co+p2.y*(1-co), p1.size*co+p2.size*(1-co));
     	        mainEngine.addParticle(p); p.mass := (p1.mass*co+p2.mass*(1-co))/2;
     	        p.sx := p1.sx*co+p2.sx*(1-co); p.sy := p1.sy*co+p2.sy*(1-co);
     	        parts[i*2+j] := p;
     	    end;
     	end;
	
	//parts[1].sx := p1.sx; parts[1].sy := p1.sy;
	//parts[nbP].sx := p2.sx; parts[nbP].sy := p2.sy;
	
	//cp := cPropContainer.Create(self);
	
	{mainEngine.addLink(cIronLink.Create(p1,parts[1],cp));
	for i := 2 to nbP-1 do begin mainEngine.addLink(cIronLink.Create(parts[i],parts[i+1],cp));
	mainEngine.addLink(cIronLink.Create(parts[1],p2,cp));}
	
	//parts[0] := p1; parts[i+1] := p2;
	//for i := 0 to nbP do mainEngine.addLink(cIronLink.Create(parts[i],parts[i+1],cp));
	//for i := 0 to nbP do mainEngine.addLink(cIronLink.Create(parts[i],parts[i+1],getPropContainer()));
     	{for i := 0 to nbP do begin
     	    l := cIronLink.Create(parts[i],parts[i+1]);
     	    mainEngine.addLink(l);
     	end;}
     	pc := getPropContainer(); pc.resistance := -1; if length1=0 then pc.length1 := 0;
     	for i := 0 to nbP do mainEngine.addLink(cIronLink.Create(parts[i*2+1],parts[i*2+2],pc));
     	}
     end;
end;

procedure cIronLink.breakLink();
var i,j,nbP:integer; parts:array[0..30] of cParticle; p:cParticle; l:cLink; co:real; pc:cPropContainer;
begin
     inherited;
     //trace('ouch!');
     	//p := cParticle.Create(p1.x+10,p2.y);	   p.sx := 0; p.sy := 0;
     	//mainEngine.addParticle(p);
     	//trace(mainEngine.addParticle(p));
     	nbP := Random(breakingBitsNb)+1;
     	//nbP := 1;
     	for i := 0 to nbP+1 do begin
     	    co := i/(nbP+1);
     	    {if i <> 0 then begin parts[i*2] := cParticle.Create(p1.x*co+p2.x*(1-co), p1.y*co+p2.y*(1-co), p1.size*co+p2.size*(1-co));
     	       mainEngine.addParticle(parts[i*2]); parts[i].mass := (p1.mass*co+p2.mass*(1-co))/2; end;
     	    if i <> nbP+1 then begin parts[i*2+1] := cParticle.Create(p1.x*co+p2.x*(1-co), p1.y*co+p2.y*(1-co), p1.size*co+p2.size*(1-co));
     	       mainEngine.addParticle(parts[i*2+1]); parts[i+1].mass := (p1.mass*co+p2.mass*(1-co))/2; end;}
     	    for j := 0 to 1 do begin
     	    	p := cParticle.Create(p1.x*co+p2.x*(1-co), p1.y*co+p2.y*(1-co), p1.size*co+p2.size*(1-co));
     	        mainEngine.addParticle(p); p.mass := (p1.mass*co+p2.mass*(1-co))/2;
     	        p.sx := p1.sx*co+p2.sx*(1-co); p.sy := p1.sy*co+p2.sy*(1-co);
     	        parts[i*2+j] := p;
     	    end;
     	end;
	
	//parts[1].sx := p1.sx; parts[1].sy := p1.sy;
	//parts[nbP].sx := p2.sx; parts[nbP].sy := p2.sy;
	
	//cp := cPropContainer.Create(self);
	
	{mainEngine.addLink(cIronLink.Create(p1,parts[1],cp));
	for i := 2 to nbP-1 do begin mainEngine.addLink(cIronLink.Create(parts[i],parts[i+1],cp));
	mainEngine.addLink(cIronLink.Create(parts[1],p2,cp));}
	
	//parts[0] := p1; parts[i+1] := p2;
	//for i := 0 to nbP do mainEngine.addLink(cIronLink.Create(parts[i],parts[i+1],cp));
	//for i := 0 to nbP do mainEngine.addLink(cIronLink.Create(parts[i],parts[i+1],getPropContainer()));
     	{for i := 0 to nbP do begin
     	    l := cIronLink.Create(parts[i],parts[i+1]);
     	    mainEngine.addLink(l);
     	end;}
     	pc := getPropContainer(); pc.resistance := -1; if length1=0 then pc.length1 := 0;
     	for i := 0 to nbP do mainEngine.addLink(cIronLink.Create(parts[i*2+1],parts[i*2+2],pc));
     	
end;

end.

