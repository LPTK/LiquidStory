(*
		Pascal Physics Engine - Copyright(c) 2011

	Author:
    	Lionel Parreaux
    
    Abstract:
    	The main class of the physics engine
        You may use the 'refresh' procedure at each frame so that it calculates
        every constraints and displacements
        It will also automaticallly refresh the last cDisplayer object
        that used the engine for its creation
    
    Please see 'copyright.txt' for details about the copyright.
*)

unit ppe_uengine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ppe_uparticle, ppe_uLink, ppe_uGround, ppe_uTrace;

const maxPts = 10000; maxLks = 100000; maxGnds = 1000; version = '0.134';
  // Note that grounds were not implemented because we realized we didn't need them!
  // I wanted to use linked lists instead of these stupid arrays but I didn't find the time for this

type cEngine = Class
     public
     refreshRate,linksRefreshRate:integer; gravity, motionSpeed, airFriction, groundFriction:real;
     time:real; topGround,bottomGround,rightGround,leftGround:integer;
     parts:Array[1..maxPts] of cParticle; links:Array[1..maxLks] of cLink;
     //grounds:Array[1..maxLks] of cGround; // We chose not to use grounds
     activeDisplayer:tObject;
     nbPts,nbLks,nbGnds:integer;
     //startPart,endPart:cParticle;

     constructor Create();
     procedure refresh(forcedIterations:integer=-1);
     function addParticle(p:cParticle):boolean;
     function addLink(l:cLink):boolean;
     function removeParticle(p:cParticle; removeLinks:Boolean=true):boolean;
     function removeLink(l:cLink):boolean;
     //function addGround(g:cGround):boolean;
     procedure mixLinks;
     // ^ This procedure is used to prevent resonance effects that happen when a big
     // structure is constructed with links created from a side to another in order.
     // This is due to the fact that the engine refreshes all the links in the
     // order they were put into it.
	 
     destructor Destroy; Override;

end;

var mainEngine:cEngine;

implementation

// !! TODO: Use linked lists instead of arrays!

uses ppe_uDisplayer;

constructor cEngine.Create();
begin
     trace('Creation of PPE');
     mainEngine := self; activeDisplayer := NIL;
     nbTotPts := 0;
     refreshRate := 1; linksRefreshRate := 1;
     time := 0;
     nbPts := 0; nbLks := 0; nbGnds := 0;
     gravity := 0.1;
     motionSpeed := 1;
     airFriction := 0.001; groundFriction := 0.1;
     topGround := -99999; bottomGround := 500; leftGround := 0; rightGround := 600;
     //startPart := NIL; endPart := NIL;
     inherited Create;
end;
      

destructor cEngine.destroy;
var i:integer; begin
     trace('Destruction of PPE');
     for i := 1 to nbPts do parts[i].Free;
     for i := 1 to nbLks do links[i].Free;
inherited; end;

procedure cEngine.refresh(forcedIterations:integer=-1);
var i,n,k:integer; p:cParticle; c,s2,_motionSpeed,slipping:real; disp:cDisplayer;
//linkRefreshSens:Boolean;
begin
 if forcedIterations=-1 then forcedIterations := refreshRate;
 _motionSpeed := motionSpeed/refreshRate; //linkRefreshSens := true;
 for k := 1 to forcedIterations do begin
	 
	 //if activeDisplayer<>NIL then (activeDisplayer as cDisplayer).refresh(k);
  	 if activeDisplayer<>NIL then begin
        disp := cDisplayer(activeDisplayer); disp.refresh(k);
        	 	  	 // ^ this conversion is necessary because of cycle unit reference bullshit
     end;
     
     // Manage the particles: speeds, gravity...
     for i := 1 to nbPts do begin
         p := parts[i];
         if p.fixed then begin p.sx := 0; p.sy := 0; end;
         if not p.fixed then begin
            p.sy := p.sy+gravity*_motionSpeed;
            p.x += p.sx*_motionSpeed;
            p.y += p.sy*_motionSpeed;
            p.sx *= (1-airFriction);
            p.sy *= (1-airFriction);
         end;
         s2 := p.size/2;
         c := -0.8; slipping := 1-groundFriction;
         if(p.y>bottomGround-s2) then begin p.y := bottomGround-s2; p.sy := p.sy*c; p.sx*=slipping end;
         if(p.y<topGround+s2) then begin p.y := topGround+s2; p.sy := p.sy*c; p.sx*=slipping end;
         if(p.x<leftGround+s2) then begin p.x := leftGround+s2; p.sx := p.sx*c; p.sy*=slipping end;
         if(p.x>rightGround-s2) then begin p.x := rightGround-s2; p.sx := p.sx*c; p.sy*=slipping end;
         
     end;
     
     // Links constraints:
     {makes it all explode:} {if linkRefreshSens then for i := 1 to nbLks do links[i].refresh(true, _motionSpeed)
     else for i := nbLks downto 1 do links[i].refresh(true, _motionSpeed);}
     for i := 1 to nbLks do links[i].refresh(true, _motionSpeed);
     
     // Additional links constraints calculations for special refresh rate:
     for n := 2 to linksRefreshRate do for i := 1 to nbLks do links[i].refresh(false, _motionSpeed);
     
     // Saving the particles' coordinates:
     for i := 1 to nbPts do begin
         p := parts[i]; p.xSave := p.x; p.ySave := p.y;
     end;
     
     //linkRefreshSens := not linkRefreshSens;
     
     time += _motionSpeed;
 end;
end;

function cEngine.addParticle(p:cParticle):boolean;
begin
     if(nbPts<maxPts) then begin   		  //trace('particle added!');
       nbPts := nbPts+1;
       parts[nbPts] := p;          
       addParticle := true;
     end else begin   	   	   		  //trace('couldnt add particle!');
       addParticle := false;
     end;

     //test := nbPts;
end;

function cEngine.addLink(l:cLink):boolean;
begin
     if(nbLks<maxLks) then begin
       nbLks := nbLks+1;
       links[nbLks] := l;
       addLink := true;
     end else begin
       addLink := false;
     end;
end;

function cEngine.removeParticle(p:cParticle; removeLinks:Boolean=true):boolean; var i,j:Integer;
begin
	 removeParticle := false;
     
 	 if removeLinks then
     begin
        {for i := 1 to nbLks do if (links[i].p1.id = p.id) then trace(links[i].p1);
        for i := 1 to nbLks do if (links[i].p2.id = p.id) then trace(links[i].p2);}
        
        for i := 1 to nbLks do
          if (links[i].p1.id = p.id)
          or (links[i].p2.id = p.id)
        	  then removeLink(links[i]);
     end;
     
     for i := 1 to nbPts do if parts[i].id = p.id then
     begin
        nbPts -= 1;
        for j := i to nbPts do parts[j] := parts[j+1];
        removeParticle := true;
        //nbPts -= 1;
     	p.Free;
        break;
     end;
end;

function cEngine.removeLink(l:cLink):boolean; var i,j:Integer;
begin
     removeLink := false;
     for i := 1 to nbLks do if links[i].id = l.id then
     begin
        for j := i to nbLks-1 do links[j] := links[j+1];
        removeLink := true;
        nbLks -= 1;
        l.Free;
        break;
     end;
end;

{function cEngine.addGround(g:cGround):boolean;
begin
     if(nbGnds<maxGnds) then begin
       nbGnds := nbGnds+1;
       grounds[nbGnds] := g;          
       addGround := true;
     end else begin
       addGround := false;
     end;
end;}

procedure cEngine.mixLinks();
var k,i:integer; l:cLink;
begin
     for k:= 1 to trunc(nbLks/4) do begin
       i := k*2;
       l := links[i];
       links[i] := links[nbLks-i+1];
       links[nbLks-i+1] := l;
     end;
end;

end.

