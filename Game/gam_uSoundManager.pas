(*
		Liquid Story - Copyright(c) 2011

	Authors:
    	Lionel Parreaux
    
    Abstract:
    	Manages the sounds playback and loading from files
    
    Please see 'copyright.txt' for details about the copyright.
*)

unit gam_uSoundManager; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ppe_uTrace, gam_uParamGrabber, soundlib3dll;

var {music:tSoundStream;} music:tSample; playSounds,playMusic:Boolean;
function getSound(sndName:String):tSoundStream;
procedure playSound(sndName:String);

implementation

var
  driver      : tSoundInterface;
  sound       : tSoundCollection;
  soundsList,soundFilesList:tStringList;
  i:integer;

function getSound(sndName:String):tSoundStream;
begin getSound := tSoundFile(soundsList.Objects[soundsList.IndexOf(sndName)]); end;

procedure playSound(sndName:String); var snd:tSoundStream;
begin if playSounds then begin
  snd := getSound(sndName);
  (*snd.stop;*) snd.play;
end; end;

procedure newSound(sndName:String);
begin
	trace(concat('Loading sound: ',sndName));
	try
	soundsList.AddObject(sndName,
		tSoundFile.create(sound,pchar(concat('snd/', sndName, '.wav')))
	);
	except
		trace(concat('Unable to load sound: ',sndName));
	end;
end;

initialization

try
  driver := tSoundInterface.create; // Selects best sound interface automatically
except
  //on E: Exception do error('Error: '+E.Message);
end;

// Initialize playback
try
  driver.startplayback(0,snd_auto,0,0,0); // Everything will be automatically set
except
  on E: Exception do
    //error('Could not initialize sound output ('+E.Message+')');
end;

// Create sound collection layer
sound := tSoundCollection.create(driver,1);

try
	trace('Loading music');
	//music := tSoundFile.create(sound,'snd/music.mod');	// MOD FILES DO NOT WORK!
	//music := tSoundFile.create(sound,'snd/create.wav');	// (on my computer at least)
	//music := tSoundFile.create(sound,'snd/music.flac');
	//music := tSoundFile.create(sound,'snd/music.wav');
	//music := tSample.create(sound,'snd/music.wav');
	music := tSample.create(sound,tSoundFile.create(sound,'snd/music.wav'));
except
	trace('Unable to load music.');
end;

//music.play;
music.setloop(0,music.getsize-1);

soundsList := tStringList.Create;

soundFilesList := TStringList.Create;
soundFilesList.LoadFromFile('snd/Sounds.txt');

for i := 0 to soundFilesList.Count-1 do newSound(soundFilesList[i]);

soundFilesList.Free;

(*playSounds := true;
playMusic := true;*)
playSounds := getBool('playSounds');
playMusic := getBool('playMusic');

end.




