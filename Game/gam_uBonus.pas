(*
		Liquid Story - Copyright(c) 2011

	Authors:
		Najoua El Ouadie
    	Lionel Parreaux
    
    Abstract:
    	The bonuses
    
    Please see 'copyright.txt' for details about the copyright.
*)

unit gam_uBonus;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, ppe_uDisplayPoint,
  ppe_uDisplayer,
  ppe_uMaths,
  ppe_uTrace,
  gam_uGameParticle,
  gam_uscore,
  ppe_uengine;

type cBonus = Class(cGameParticle)
     public
         nbonus : integer;
         catched : boolean;
         procedure init; override;
         procedure drawBody(_x,_y:real; var canv:tCanvas); override;
         procedure PartBonus(score:cscore);
         constructor create(ax,ay:real;engine:cengine;akind:byte);
     protected
         img:Tgraphic;
         kind : byte;
end;

implementation

uses gam_uGame;

var liqBonusImg, iceBonusImg, vapBonusImg:Tgraphic;

constructor cBonus.create(ax,ay:real;engine:cengine;akind:byte);
begin
    kind := akind;
    self.fixed := true;
    partType := ptCloud;
    engine.addParticle(self);
    inherited create(ax,ay,10);
end;

procedure cBonus.init;
 Begin
  inherited;
  nbonus:=10;
  case kind of
      1 : img := iceBonusImg;
      2 : img := liqBonusImg;
      3 : img := vapBonusImg;
  end;
 end;

procedure cBonus.PartBonus(score:cscore);
begin
    case kind of
        1 : score.bonus(nbonus,0,0,10*nbonus);
        2 : score.bonus(0,nbonus,0,10*nbonus);
        3 : score.bonus(0,0,nbonus,10*nbonus);
    end;
    //trace('remPart');
    //if not mainEngine.removeParticle(self) then trace('Error: Cannot remove bonus '+intToStr(id));
    if not gam_uGame.game.engine.removeParticle(self) then trace('Error: Cannot remove bonus '+intToStr(id));
end;

procedure cBonus.drawBody(_x,_y:real; var canv:tCanvas);
begin
	//inherited;
    canv.draw(round(_x)-(img.Width div 2),round(_y)-(img.Height div 2),img);
end;

{procedure cBonus.loadImage();
begin
 inherited;
    case kind of
     1 : begin
        GraphicBonus := TPortableNetworkGraphic.create;
        GraphicBonus.loadFromFile ('img/BonusVapl.png'); end;
     2 : begin
        GraphicBonus := TPortableNetworkGraphic.create;
        GraphicBonus.loadFromFile ('img/BonusLiq.png'); end;
     3 : begin
        GraphicBonus := TPortableNetworkGraphic.create;
        GraphicBonus.loadFromFile ('img/BonusSol.png'); end;
    end;
     //canv.draw(round(_x)-(GraphicBonus.Width div 2),round(_y)-(GraphicBonus.Height div 2),GraphicBonus);
end;}

{procedure cbonus.catchedBonus(score:cscore;engine:cengine);
var i: integer;
begin
    for i:=1 to engine.nblks do
    if ( engine.links[i].isVisible ) and (catched=false) and ( ( engine.links[i].p1=self ) or ( engine.links[i].p2=self ))   then
    begin
      catched:=true;
      PartBonus(score);
    end;
end;}

initialization

trace('Loading bonus images...');

vapBonusImg := TPortableNetworkGraphic.create;
vapBonusImg.loadFromFile ('img/BonusVapl.png');

liqBonusImg := TPortableNetworkGraphic.create;
liqBonusImg.loadFromFile ('img/BonusLiq.png');

iceBonusImg := TPortableNetworkGraphic.create;
iceBonusImg.loadFromFile ('img/BonusSol.png');

end.

