(*
		Pascal Physics Engine - Copyright(c) 2011

	Author:
    	Lionel Parreaux
    
    Abstract:
    	Points with display properties
    
    Please see 'copyright.txt' for details about the copyright.
*)

unit ppe_uDisplayPoint;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, ppe_uPoint;

type cDisplayPoint = Class(cPoint)
     public
     size:real; color,contourColor:tColor; noContour:Boolean;
     constructor Create(_x,_y:real);
     procedure init; override;
     procedure display(_x,_y:real; var canv:tCanvas); virtual;
     destructor Destroy; Override;
     
     protected
     procedure drawBody(_x,_y:real; var canv:tCanvas); virtual;
end;

var defPen:tPen; defBrush:tBrush;
procedure defCanv(canv:tCanvas);

implementation

procedure cDisplayPoint.init;
begin
	inherited;
    color := clRed;
    noContour := false;
end;

constructor cDisplayPoint.Create(_x,_y:real);
begin
     inherited;
end;

procedure cDisplayPoint.display(_x,_y:real; var canv:tCanvas);
begin
     drawBody(_x,_y,canv);
end;

procedure cDisplayPoint.drawBody(_x,_y:real; var canv:tCanvas);
begin
	defCanv(canv); //canv.Pen.Style:=psClear;
	canv.Brush.Color := color;
    if noContour then canv.Pen.Style := psClear
    else canv.Pen.Style := psSolid;
    canv.Pen.Color := contourColor;
	//canv.Pen.Width := round(size/15); canv.Pen.Color := round(color*0.5);
	canv.Ellipse(round(_x-size/2),round(_y-size/2),
     		   round(_x+size/2),round(_y+size/2));
end;

destructor cDisplayPoint.destroy;
begin
     inherited;
end;

procedure defCanv(canv:tCanvas); begin canv.pen := defPen; canv.brush := defBrush; end;

initialization
defPen := tPen.Create; defBrush := tBrush.Create;

end.

