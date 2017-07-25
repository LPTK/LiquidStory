(*
		Liquid Story - Copyright(c) 2011

	Authors:
    	Jordan Vincent
        Lionel Parreaux
    
    Abstract:
    	Where all background images are handled
    
    Please see 'copyright.txt' for details about the copyright.
*)

unit gam_uGraphic;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, uSplashScreen,
  gam_uParamGrabber;

type cGraphic = Class
    x: real;
    y: real;       // repère absolu de la position des images dans le coin en bas à gauche sans prendre en compte les dimensions de l'image
    layer:integer;
    scrolling:real;
    image: TGraphic;
    isPNG:Boolean;
    constructor create(ax,ay:real; alayer:integer; ascrolling:real; filename: string);
end;

implementation

constructor cGraphic.create(ax,ay:real; alayer:integer; ascrolling:real; filename: string);
begin
     //SplashScreenForm.Label1.Caption:='Loading '+filename; // doesn't work
     //if loadImages then begin
     if not paramExists('loadImages') or getBool('loadImages') then begin
	//if ascrolling=0 then ascrolling := 99999 else ascrolling := 1/ascrolling*2;
     x:=ax; y:=ay; layer:=alayer; scrolling:=ascrolling;
     if Pos('.png', filename)<> 0 then
     begin
          image := TPortableNetworkGraphic.create;
          image.loadFromFile (filename);
          isPNG := true;
     end
     else if Pos('.bmp', filename)<> 0 then
     begin
          image := TBitmap.create;
          image.loadFromFile (filename);
          isPNG := false;
     end;   // Caution if not png nor bmp !!!
     
     //end else begin image := TBitmap.create; image.Width:=0; image.Height:=0; end;
     end else begin image := TBitmap.create; image.loadFromFile('img/stupid_part.bmp'); end;
     
end;

end.

