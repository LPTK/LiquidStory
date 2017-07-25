unit soundlib3;

{****************************************************************************
 * SoundLib 3 - copyright (c) 1997-2007 by Stefan Goehler/Crossfire Designs *
 * DLL interface                                                            *
 *                                                                          *
 * Copyright agreement in license.txt - illegal copy without this file!     *
 * Any bug reports and suggestions are welcome                              *
 *                                                                          *
 * Supported compilers: Delphi5+, Free Pascal, Lazarus                      *
 * Tested platforms: Windows 98 SE, Windows 2000, Windows XP                *
 *                                                                          *
 * Complete package and new version always available here:                  *
 * http://www.crossfire-designs.de                                          *
 ****************************************************************************}

interface

uses sysutils,classes,vectors;


const
  stdbfsize       = 1024; { Standard mix chunk size for tSound class in samples }
  maxoutChannels  = 8;    { Maximum number of output channels  }
  maxinchannels   = 8;    { Maximum number of input channels (per stream) }
  maxvolume       = 512;
  minvolume       = 0;



type

{$IFDEF WIN32}
  tHandle = longword;
{$ELSE}
  tHandle = int64;
{$ENDIF}

  tChannelMap         = array[0..maxOutChannels-1] of longword;
  tChannelVolumeMap   = array[0..maxOutChannels-1] of integer;
  tVolumeMap          = array[0..maxInChannels-1] of tChannelVolumeMap;
  tvuval              = array[0..maxInChannels-1] of integer;

  tSoundDeviceInfo = packed record
    deviceName  : pchar;
    frequencies : record
      play : record
        min,max,preferred : longword;
      end;
    end;
    outputchannels : longword;
    lfe            : boolean;
    flags          : longword;
    version        : longword;
    wavetablesize  : longword;
    minbufsize,
    maxbufsize     : longword;
  end;

  tSoundInterfaceInfo = packed record
    interfaceName     : pchar;    // Name of interface
    interfaceVersion  : pchar;    // Version of SL interface driver
    priority          : longint;  // Priority of this interface (for auto selection)
    flags             : longword; // stores information about interface
  end;

  // tSoundInterface playback information
  tPlaybackInfo = packed record
    latency     : longword;  // Mean latency of playback
    frequency   : longword;  // Playback frequency
    resolution  : longword;  // Playback resolution
    samplesize  : longword;
    is_float    : boolean;   // Playback uses floating point values
    inchannels  : longword;  // Number of input channels for playback
    outchannels : longword;  // Number of output playback channels
    lfe         : boolean;   // If true, last in/outchannel is an LFE channel
    buffersize  : longword;  // Size of playback mixing buffer in samples
    buffers     : longword;  // Number of buffers for playback
  end;

  // Sound class information
  tSoundinfo = packed record
    name        : pchar;    { Name of the sound or song                 }
    creator     : pchar;    { Creator of that sound or song             }
    encoder     : pchar;    { Encoder/Creation Software string          }
    info        : pchar;    { Additional info field if available        }
    length      : int64;    { Length of sound or song in samples        }
    frequency   : longword; { Original playback frequency of that sound }
    resolution  : longword; { Resolution of sound data                  }
    channels    : longint;  { Number of channels this sound contains    }
  end;

  tSoundInterface = class
    constructor create; virtual;
    destructor  destroy; override;

    procedure   startplayback(freq,flags,channels,bufsize,buffers : longword); virtual;
    procedure   stopplayback; virtual;
    function    isPlaying : boolean;
    function    getPlaybackInfo : tPlaybackInfo;

    function    getInterfaceCount : longword; virtual;
    function    getCurrentInterface : longword; virtual;
    function    setInterface(device : longword) : tSoundInterface; virtual;
    function    getInterfaceInfo(device : longword) : tSoundInterfaceInfo; virtual;

    function    getDeviceCount : longword; virtual;
    function    getCurrentDevice : longword; virtual;
    function    setDevice(device : longword) : boolean; virtual;
    function    getDeviceInfo(device : longword) : tSoundDeviceInfo; virtual;

    function    getMixCount : cardinal; virtual;
    function    getVersionString : pchar; virtual;

    // 3D functions
    procedure   setListenerPosition(position : tSVector); virtual;
    procedure   setListenerOrientation(orientation : tSVector); virtual;
    procedure   setListenerVelocity(velocity : tSVector); virtual;
    procedure   updateListener(position, orientation, movement : tSVector); virtual;

    private
      handle : tHandle;

    property    getHandle : tHandle read handle;
  end;

  tSound = class
    constructor create(driver: tSoundInterface;sounds : longword); virtual;
    destructor  destroy; override;
    procedure   setBufferSize(samples : longword); virtual;
    procedure   setChannels(channels : longword); virtual;
    function    change : boolean; virtual;
    procedure   setVolume(channel : longint;volume : double); virtual;
    function    getVolume(channel : longword) : double; virtual;
    procedure   setInterpolation(intpoltype : longword); virtual;
    procedure   setRamping(active : boolean); virtual;
    function    getInfo : tSoundInfo; virtual;
    procedure   stopAll; virtual;
    procedure   pauseAll; virtual;
    procedure   resumeAll; virtual;

    private
      driverhandle : tHandle;
      handle       : tHandle;

    property    getHandle : tHandle read handle;
  end;

  tSoundStream = class
    private
      handle : tHandle;
      soundhandle : tHandle;

    public
    destructor  destroy; override;
    procedure   play; virtual;
    procedure   pause; virtual;
    procedure   stop; virtual;
    procedure   setvolume(channel : longint;volume : double); virtual;
    function    getvolume(channel : longword) : double; virtual;
    procedure   mute(active : boolean); virtual;
    procedure   setfrequency(frequency : longword); virtual;
    procedure   setInterpolation(intpoltype : longword); virtual;
    procedure   setramping(active : boolean); virtual;
    procedure   setvolumematrix(matrix : tVolumeMap); virtual;
    function    getvolumematrix : tVolumeMap; virtual;
    function    getpos : int64; virtual;
    function    getsize : int64; virtual;
    procedure   setpos(position : int64); virtual;
    procedure   setspeed(speed : double); virtual;
    function    getvu(inchannel : longword) : double; virtual;
    function    isplaying : boolean; virtual;
    function    ispaused : boolean; virtual;
    function    ismuted : boolean; virtual;
    function    getinfo : tSoundInfo; virtual;
    procedure   setPanning(channel : longint; lr,cs,tb,volume : double); virtual;

    // 3D methods
    procedure   setObjectPosition(channel : integer; position : tSVector; relative : boolean); virtual;
    procedure   setObjectOrientation(channel : integer; orientation : tSVector); virtual;
    procedure   setObjectVelocity(channel : integer; velocity : tSVector); virtual;
    procedure   setObjectSpreadAngle(channel : integer; angle : double); virtual;
    procedure   setObjectSize(channel : integer; size : tSVector); virtual;

    property    getHandle : tHandle read handle;
  end;


  tSample = class(tSoundStream)
    constructor create(dest : tSound; stream : tSoundStream); overload; virtual;
    constructor create(dest : tSound; s : pointer; samples, flags, frequency : longword); overload; virtual;
    destructor  destroy; override;

    procedure   setloop(strt,stp : int64); virtual;
    procedure   setpos(pos : int64);override;
    function    getpos : int64; override;
    function    getsize : int64; override;
  end;

  { SoundFile class - loads sound files from disk }
  tSoundFile = class(tSoundStream)
    constructor create(sound: tSound;name : pchar); overload;
    constructor create(sound: tSound;s : tstream); overload;
    destructor  destroy; override;
    private
      isstream : boolean;
  end;

  tModule = class(tSound)
    constructor create(driver : tSoundInterface;name: pchar); overload;
    constructor create(driver : tSoundInterface;s : tStream); overload;
    destructor  destroy; override;
    procedure   setvolume(channel : longint; volume : double); virtual;
    procedure   play; virtual;
    procedure   stop; virtual;
    procedure   pause; virtual;
    function    isplaying : boolean; virtual;
    function    ispaused : boolean; virtual;
    function    getvu(modchannel,inchannel : longword) : double; virtual;
    function    getpos : int64; virtual;
    function    getsize : int64; virtual;
    procedure   setpos(position : int64); virtual;
 end;



const
  sldllname = 'soundlib.dll';

  // Sound format flags
  snd_default       = $00000000;
  snd_signed        = $00000200; { Sound data is signed          }
  snd_nonsigned     = $00000000; { Sound data is non-signed      }
  snd_8bit          = $00000000; { Sound data uses 8 bits        }
  snd_16bit         = $00000400; { Sound data uses 16 bits       }
  snd_24bit         = $00000800; { Sound data uses 24 bits       }
  snd_32bit         = $00000100; { Sound data uses 32 bits       }
  snd_monosurround  = $00004000; { Dolby Pro Logic compatible playback      }
  snd_stereosurround= $00008000; { Dolby Pro Logic compatible II playback   }

  snd_auto          = $10000000;

  // Sound channel flags
  snd_mono      = $00000001;
  snd_stereo    = $00000002;
  snd_4chn      = $00000004; { 4 channel playback             }
  snd_5chn      = $00000005; { 5 channel playback             }
  snd_6chn      = $00000006; { 6 channel playback             }
  snd_7chn      = $00000007; { 7 channel playback             }
  snd_channels  = $0000000F; { Mask for channel number        }
  snd_lfe       = $10000000; { Include LFE channel            }


  // Interpolation types
  interpolation_none   = 0;
  interpolation_linear = 1;
  interpolation_cubic  = 2;


  // Channels
  chn_all         = -1;
  chn_left        = 0;
  chn_right       = 1;
  chn_center      = 2;
  chn_rear_left   = 3;
  chn_rear_right  = 4;
  chn_rear_center = 5; // Center rear for 6.1
  chn_side_left   = 5;
  chn_side_right  = 6;

  chn_sub_51      = 5;
  chn_sub_61      = 6;
  chn_sub_71      = 7;

  // Incoming channels for conversion matrix
  chn_in_first       = 0;

  chn_in_left        = 0;
  chn_in_right       = 1;
  chn_in_center      = 2;
  chn_in_rear_left   = 3;
  chn_in_rear_right  = 4;
  chn_in_side_left   = 5;
  chn_in_side_right  = 6;
  chn_in_sub         = 7;
  chn_in_rear_center = 8;
  chn_in_monosource  = 9;
  chn_in_ignore      = 10;

  chn_in_last        = 10;



// Read/Write callback functions

type
  tread_func  = function(const datasource; var ptr; size: int64) : int64; cdecl;
  twrite_func = function(const datasource; var ptr; size: int64) : int64; cdecl;
  tsize_func  = function(const datasource) : int64; cdecl;
  tpos_func   = function(const datasource)  : int64; cdecl;
  tseek_func  = function(const datasource; offset: int64) : int64; cdecl;
  tclose_func = function(const datasource) : int64; cdecl;

  stream_callbacks = ^tstream_callbacks;
  tstream_callbacks = packed record
    vread_func   : tread_func;
    vwrite_func  : twrite_func;
    vsize_func   : tsize_func;
    vpos_func    : tpos_func;
    vseek_func   : tseek_func;
    vclose_func  : tclose_func;
  end;



// DLL exported methods
function  tsoundinterface_create : tHandle; cdecl; external sldllname;
procedure tsoundinterface_startplayback(handle : tHandle;freq,flags,channels,bufsize,buffers : longword); cdecl; external sldllname;
procedure tsoundinterface_stopplayback(handle : tHandle); cdecl; external sldllname;
function  tsoundinterface_isplaying(handle : tHandle) : boolean; cdecl; external sldllname;
function  tsoundinterface_getplaybackinfo(handle : tHandle) : tPlaybackInfo; cdecl; external sldllname;
procedure tsoundinterface_destroy(handle : tHandle); cdecl; external sldllname;

function  tsoundinterface_getinterfacecount(handle : tHandle) : longword; cdecl; external sldllname;
function  tsoundinterface_getcurrentinterface(handle : tHandle) : longword; cdecl; external sldllname;
function  tsoundinterface_setinterface(handle : tHandle;device : longword) : tHandle; cdecl; external sldllname;
function  tsoundinterface_getinterfaceinfo(handle : tHandle;device : longword) : tSoundInterfaceInfo; cdecl; external sldllname;

function  tsoundinterface_getdevicecount(handle : tHandle) : longword; cdecl; external sldllname;
function  tsoundinterface_getcurrentdevice(handle : tHandle) : longword; cdecl; external sldllname;
function  tsoundinterface_setdevice(handle : tHandle;device : longword) : boolean; cdecl; external sldllname;
function  tsoundinterface_getdeviceinfo(handle : tHandle;device : longword) : tSoundDeviceInfo; cdecl; external sldllname;

function  tsoundinterface_getmixcount(handle : tHandle) : cardinal; cdecl;  external sldllname;
function  tsoundinterface_getversionstring(handle : tHandle) : pchar; cdecl; external sldllname;

procedure tsoundinterface_setlistenerposition(handle : tHandle; position: tSVector); cdecl;  external sldllname;
procedure tsoundinterface_setlistenerorientation(handle : tHandle; orientation : tSVector); cdecl;  external sldllname;
procedure tsoundinterface_setlistenervelocity(handle : tHandle; velocity: tSVector); cdecl;  external sldllname;
procedure tsoundinterface_updatelistener(handle : tHandle; position, orientation, speed: tSVector); cdecl;  external sldllname;

function  tsoundstream_create_delphi(soundhandle : tHandle;s : tStream) : tHandle; cdecl; external sldllname;
function  tsoundstream_create(soundhandle : tHandle;name: pchar) : tHandle; cdecl; external sldllname;
procedure tsoundstream_destroy(handle : tHandle); cdecl; external sldllname;
procedure tsoundstream_play(handle : tHandle); cdecl; external sldllname;
procedure tsoundstream_pause(handle : tHandle); cdecl; external sldllname;
procedure tsoundstream_stop(handle : tHandle); cdecl; external sldllname;
procedure tsoundstream_setvolume(handle : tHandle;channel : longint;volume : double);cdecl; external sldllname;
function  tsoundstream_getvolume(handle : tHandle;channel : longword) : double;cdecl; external sldllname;
function  tsoundstream_getpos(handle : tHandle) : int64; cdecl; external sldllname;
procedure tsoundstream_mute(handle : tHandle;active : boolean); cdecl; external sldllname;
procedure tsoundstream_setfrequency(handle : tHandle;frequency : longword);cdecl; external sldllname;
procedure tsoundstream_setinterpolation(handle : tHandle;intpoltype : longword); cdecl; external sldllname;
procedure tsoundstream_setramping(handle : tHandle; active : boolean); cdecl; external sldllname;
procedure tsoundstream_setvolumematrix(handle : tHandle; matrix : tVolumeMap); cdecl; external sldllname;
function  tsoundstream_getvolumematrix(handle : tHandle) : tVolumeMap; cdecl; external sldllname;
function  tsoundstream_getsize(handle : tHandle) : int64; cdecl; external sldllname;
procedure tsoundstream_setpos(handle : tHandle; position : int64); cdecl; external sldllname;
procedure tsoundstream_setspeed(handle : tHandle;speed : double);cdecl; external sldllname;
function  tsoundstream_getvu(handle,inchannel : longword) : double; cdecl; external sldllname;
function  tsoundstream_isplaying(handle : tHandle) : boolean; cdecl; external sldllname;
function  tsoundstream_ispaused(handle : tHandle) : boolean; cdecl; external sldllname;
function  tsoundstream_ismuted(handle : tHandle) : boolean; cdecl; external sldllname;
function  tsoundstream_getinfo(handle : tHandle) : tSoundInfo; cdecl; external sldllname;
procedure tsoundstream_setpanning(handle : tHandle;channel : longint; lr,cs,tb,volume : double); cdecl; external sldllname;

procedure tsoundstream_setobjectposition(handle : tHandle; channel : integer; position : tSVector; relative : boolean); cdecl; external sldllname;
procedure tsoundstream_setobjectorientation(handle : tHandle; channel : integer; orientation : tSVector); cdecl; external sldllname;
procedure tsoundstream_setobjectvelocity(handle : tHandle; channel : integer; velocity : tSVector); cdecl; external sldllname;
procedure tsoundstream_setobjectspreadangle(handle : tHandle; channel : integer; angle : double); cdecl; external sldllname;
procedure tsoundstream_setobjectsize(handle : tHandle; channel : integer; size : tSVector); cdecl; external sldllname;

function  tsample_create(soundhandle : tHandle;streamhandle : tHandle) : tHandle; cdecl; external sldllname;
procedure tsample_destroy(handle : tHandle); cdecl; external sldllname;
procedure tsample_play(handle : tHandle); cdecl; external sldllname;
procedure tsample_pause(handle : tHandle); cdecl; external sldllname;
procedure tsample_stop(handle : tHandle); cdecl; external sldllname;
procedure tsample_setvolume(handle : tHandle;channel : longint;volume : double);cdecl; external sldllname;
function  tsample_getpos(handle : tHandle) : int64; cdecl; external sldllname;
function  tsample_getsize(handle : tHandle) : int64; cdecl; external sldllname;
procedure tsample_setpos(handle : tHandle; position : int64); cdecl; external sldllname;
procedure tsample_setloop(handle : tHandle; startpos,endpos : int64); cdecl; external sldllname;
function  tsample_getvu(handle : tHandle;inchannel : longword) : double; cdecl; external sldllname;
procedure tsample_setpanning(handle : tHandle;channel : longword; lr,cs,tb,volume : double); cdecl; external sldllname;

function  tsample_create_memory(soundhandle : tHandle; s : pointer; samples, flags, frequency : longword) : tHandle; cdecl; external sldllname;

function  tsound_create(driverhandle : tHandle;sounds : longword) : tHandle;cdecl; external sldllname;
procedure tsound_destroy(handle : tHandle);cdecl; external sldllname;
procedure tsound_setbuffersize(handle : tHandle;samples : longword);cdecl; external sldllname;
procedure tsound_setchannels(handle : tHandle;channels : longword);cdecl; external sldllname;
function  tsound_change(handle : tHandle) : boolean;cdecl; external sldllname;
procedure tsound_setvolume(handle : tHandle;channel : longint;volume : double);cdecl; external sldllname;
function  tsound_getvolume(handle : tHandle;channel : longword) : double; cdecl; external sldllname;
procedure tsound_setinterpolation(handle : tHandle;intpoltype : longword); cdecl; external sldllname;
procedure tsound_setramping(handle : tHandle; active : boolean); cdecl; external sldllname;
function  tsound_getinfo(handle : tHandle) : tSoundInfo; cdecl; external sldllname;

procedure tsound_stopall(handle: tHandle); cdecl; external sldllname;
procedure tsound_pauseall(handle: tHandle); cdecl; external sldllname;
procedure tsound_resumeall(handle: tHandle); cdecl; external sldllname;


function  tsoundfile_create_stream(soundhandle : tHandle;datasource : tstream_callbacks; filehandle : int64) : tHandle; cdecl; external sldllname;
function  tsoundfile_create(soundhandle : tHandle;name: pchar) : tHandle; cdecl; external sldllname;
procedure tsoundfile_destroy_stream(handle : tHandle); cdecl; external sldllname;
procedure tsoundfile_destroy(handle : tHandle); cdecl; external sldllname;


function  tmodule_create_stream(driverhandle : tHandle;callbacks : tstream_callbacks; filehandle : int64) : tHandle;cdecl; external sldllname;
function  tmodule_create(driverhandle : tHandle;name: pchar) : tHandle;cdecl; external sldllname;
procedure tmodule_setvolume(handle : tHandle;channel : longint; volume : double);cdecl; external sldllname;
procedure tmodule_play(handle : tHandle);cdecl; external sldllname;
procedure tmodule_stop(handle : tHandle);cdecl; external sldllname;
procedure tmodule_pause(handle : tHandle);cdecl; external sldllname;
function  tmodule_getvu(handle : tHandle;modchannel,inchannel : longword) : double; cdecl; external sldllname;
function  tmodule_getpos(handle : tHandle) : int64; cdecl; external sldllname;
function  tmodule_getsize(handle : tHandle) : int64; cdecl; external sldllname;
function  tmodule_isplaying(handle : tHandle) : boolean; cdecl; external sldllname;
function  tmodule_ispaused(handle : tHandle) : boolean; cdecl; external sldllname;
procedure tmodule_setpos(handle : tHandle; position : int64); cdecl; external sldllname;
procedure tmodule_destroy(handle : tHandle);cdecl; external sldllname;



implementation


function read_func(const datasource; var ptr; size: int64) : int64; cdecl;
var
  s : tStream absolute datasource;
begin
  result := s.Read(ptr, size);
end;

function write_func(const datasource; var ptr; size: int64) : int64; cdecl;
var
  s : tStream absolute datasource;
begin
  result := s.Write(ptr, size);
end;

function size_func(const datasource): int64; cdecl;
var
  s : tStream absolute datasource;
begin
  result := s.Size;
end;

function pos_func(const datasource): int64; cdecl;
var
  s : tStream absolute datasource;
begin
  result := s.Position;
end;

function seek_func(const datasource; offset: int64): int64; cdecl;
var
  s : tStream absolute datasource;
begin
  result := s.Seek(offset, soFromBeginning);
end;

function close_func(const datasource): int64; cdecl;
var
  s : tStream absolute datasource;
begin
  s.Free;
  result := 0;
end;



constructor tSoundInterface.create;
begin
  handle := tsoundinterface_create;
//  if (handle = 0) then raise exception.Create('Could not initialize sound interface');
end;

destructor  tSoundInterface.destroy;
begin
  tsoundinterface_destroy(handle);
  inherited;
end;

procedure tSoundInterface.startplayback(freq,flags,channels,bufsize,buffers : longword);
begin
  tsoundinterface_startplayback(handle,freq,flags,channels,bufsize,buffers);
end;

procedure tSoundInterface.stopplayback;
begin
  tsoundinterface_stopplayback(handle);
end;


function tSoundInterface.isPlaying : boolean;
begin
  result := tsoundinterface_isplaying(handle);
end;

function tSoundInterface.getPlaybackInfo : tPlaybackInfo;
begin
  result := tsoundinterface_getplaybackinfo(handle);
end;



function  tSoundInterface.getDeviceCount : longword;
begin
  result := tsoundinterface_getdevicecount(handle);
end;

function  tSoundInterface.getCurrentDevice : longword;
begin
  result := tsoundinterface_getcurrentdevice(handle);
end;

function  tSoundInterface.setDevice(device : longword) : boolean;
begin
  result := tsoundinterface_setdevice(handle,device);
end;

function  tSoundInterface.getDeviceInfo(device : longword) : tSoundDeviceInfo;
begin
  result := tsoundinterface_getdeviceinfo(handle,device);
end;


function  tSoundInterface.getInterfaceCount : longword;
begin
  result := tsoundinterface_getinterfacecount(handle);
end;

function  tSoundInterface.getCurrentInterface : longword;
begin
  result := tsoundinterface_getcurrentinterface(handle);
end;

function  tSoundInterface.setInterface(device : longword) : tSoundInterface;
begin
  result := self;
  handle := tsoundinterface_setinterface(handle,device);
end;

function  tSoundInterface.getInterfaceInfo(device : longword) : tSoundInterfaceInfo;
begin
  result := tsoundinterface_getinterfaceinfo(handle,device);
end;


function  tSoundInterface.getMixCount : cardinal;
begin
  result := tsoundinterface_getmixcount(handle);
end;


function  tSoundInterface.getVersionString : pchar;
begin
  result := tsoundinterface_getversionstring(handle);
end;

procedure tSoundInterface.setListenerPosition(position : tSVector);
begin
  tsoundinterface_setlistenerposition(handle, position);
end;

procedure tSoundInterface.setListenerOrientation(orientation : tSVector);
begin
  tsoundinterface_setlistenerorientation(handle, orientation);
end;

procedure tSoundInterface.setListenerVelocity(velocity : tSVector);
begin
  tsoundinterface_setlistenervelocity(handle, velocity);
end;

procedure tSoundInterface.updateListener(position, orientation, movement : tSVector);
begin
  tsoundinterface_updatelistener(handle, position, orientation, movement);
end;


// ------


// tSound

constructor tSound.create(driver: tSoundInterface;sounds : longword);
begin
  handle := tsound_create(driver.getHandle,sounds);
  driverhandle := driver.getHandle;
end;

destructor   tSound.destroy;
begin
  tsound_destroy(handle);
  inherited;
end;

procedure   tSound.setBufferSize(samples : longword);
begin
  tsound_setbuffersize(handle,samples);
end;

procedure   tSound.setChannels(channels : longword);
begin
  tsound_setchannels(handle,channels);
end;

function    tSound.change : boolean;
begin
  result := tsound_change(handle);
end;

procedure   tSound.setVolume(channel : longint;volume : double);
begin
  tsound_setvolume(handle,channel,volume);
end;

function    tSound.getVolume(channel : longword) : double;
begin
  result := tsound_getvolume(handle,channel);
end;

procedure   tSound.setInterpolation(intpoltype : longword);
begin
  tsound_setinterpolation(getHandle,intpoltype);
end;

procedure   tSound.setRamping(active : boolean);
begin
  tsound_setramping(handle,active);
end;

function    tSound.getInfo : tSoundInfo;
begin
  result := tsound_getinfo(handle);
end;

procedure   tSound.stopAll;
begin
  tsound_stopall(handle);
end;

procedure   tSound.pauseAll;
begin
  tsound_pauseall(handle);
end;

procedure   tSound.resumeAll;
begin
  tsound_resumeall(handle);
end;


(*constructor tSoundStream.create(sound: tSound;name : pchar);
begin
//function tsoundstream_create_delphi(soundhandle : tHandle;s : tStream) : tHandle; cdecl; external sldllname;
//function tsoundstream_create(soundhandle : tHandle;name: pchar) : tHandle; cdecl; external sldllname;

  handle := tsoundstream_create(sound.getHandle,name);
  if (handle = 0) then raise Exception.Create('No known format');
end;*)

destructor  tSoundStream.destroy;
begin
  tsoundstream_destroy(handle);
  inherited;
end;

procedure tSoundStream.play;
begin
  tsoundstream_play(handle);
end;

procedure tSoundStream.pause;
begin
  tsoundstream_pause(handle);
end;

procedure tSoundStream.stop;
begin
  tsoundstream_stop(handle);
end;

procedure tSoundStream.setvolume(channel : longint;volume : double);
begin
  tsoundstream_setvolume(handle,channel,volume);
end;

function  tSoundStream.getvolume(channel : longword) : double;
begin
  result := tsoundstream_getvolume(handle,channel);
end;

procedure tSoundStream.mute(active : boolean);
begin
  tsoundstream_mute(handle,active);
end;

procedure tSoundStream.setfrequency(frequency : longword);
begin
  tsoundstream_setfrequency(handle,frequency);
end;

procedure tSoundStream.setramping(active : boolean);
begin
  tsoundstream_setramping(handle,active);
end;

procedure tSoundStream.setInterpolation(intpoltype : longword);
begin
  tsoundstream_setinterpolation(getHandle,intpoltype);
end;

procedure tSoundStream.setvolumematrix(matrix : tVolumeMap);
begin
  tsoundstream_setvolumematrix(handle,matrix);
end;

function  tSoundStream.getvolumematrix : tVolumeMap;
begin
  result := tsoundstream_getvolumematrix(handle);
end;

function  tSoundStream.getpos : int64;
begin
  result := tsoundstream_getpos(handle);
end;

function  tSoundStream.getsize : int64;
begin
  result := tsoundstream_getsize(handle);
end;

procedure tSoundStream.setpos(position : int64);
begin
  tsoundstream_setpos(handle,position);
end;

procedure tSoundStream.setspeed(speed : double);
begin
  tsoundstream_setspeed(handle,speed);
end;

function  tSoundStream.getvu(inchannel : longword) : double;
begin
  result := tsoundstream_getvu(handle,inchannel);
end;

function  tSoundStream.isplaying : boolean;
begin
  result := tsoundstream_isplaying(handle);
end;

function  tSoundStream.ispaused : boolean;
begin
  result := tsoundstream_ispaused(handle);
end;

function  tSoundStream.ismuted : boolean;
begin
  result := tsoundstream_ismuted(handle);
end;

function  tSoundStream.getinfo : tSoundInfo;
begin
  result := tsoundstream_getinfo(handle);
end;

procedure tSoundStream.setpanning(channel : longint; lr,cs,tb,volume : double);
begin
  tsoundstream_setpanning(handle,channel,lr,cs,tb,volume);
end;


// ----
// 3D functions

procedure tSoundStream.setObjectPosition(channel : integer; position : tSVector; relative : boolean);
begin
  tsoundstream_setobjectposition(handle, channel, position, relative);
end;

procedure tSoundStream.setObjectOrientation(channel : integer; orientation : tSVector);
begin
  tsoundstream_setobjectorientation(handle, channel,orientation);
end;

procedure tSoundStream.setObjectVelocity(channel : integer; velocity : tSVector);
begin
  tsoundstream_setobjectvelocity(handle, channel, velocity);
end;

procedure tSoundStream.setObjectSpreadAngle(channel : integer; angle : double);
begin
  tsoundstream_setobjectspreadangle(handle, channel, angle);
end;

procedure tSoundStream.setObjectSize(channel : integer; size : tSVector);
begin
  tsoundstream_setobjectsize(handle, channel, size);
end;


// tSample

constructor tSample.create(dest : tSound; stream : tSoundStream);
begin
  handle := tsample_create(dest.getHandle,stream.getHandle);
end;

constructor tSample.create(dest : tSound; s : pointer; samples, flags, frequency : longword);
begin
  handle := tsample_create_memory(dest.handle, s, samples, flags, frequency);
end;

destructor  tSample.destroy;
begin
  tsample_destroy(handle);
end;

procedure   tSample.setloop(strt,stp : int64);
begin
  tsample_setloop(handle,strt,stp);
end;

procedure   tSample.setpos(pos : int64);
begin
  tsample_setpos(handle,pos);
end;

function    tSample.getpos : int64;
begin
  result := tsample_getpos(handle);
end;

function    tSample.getsize : int64;
begin
  result := tsample_getsize(handle);
end;

// tSoundFile class methods
constructor tSoundFile.create(sound: tSound;name : pchar);
begin
  handle := tsoundfile_create(sound.handle, name);
  if (handle = 0) then exception.create('File type not recognized');
  isstream := false;
end;

constructor tSoundFile.create(sound: tSound;s : tstream);
var
  callbacks : tstream_callbacks;

begin
  // Set up callback functions for file actions
  with callbacks do
  begin
    vread_func   := @read_func;
    vwrite_func  := @write_func;
    vsize_func   := @size_func;
    vpos_func    := @pos_func;
    vseek_func   := @seek_func;
    vclose_func  := @close_func;
  end;

  handle := tsoundfile_create_stream(sound.handle, callbacks, longword(s));
  if (handle = 0) then raise exception.create('File type not recognized');
  isstream := true;
end;


destructor tSoundFile.destroy;
begin
  if (isstream) then tsoundfile_destroy_stream(handle)
  else tsoundfile_destroy(handle);
end;


// tModule


constructor  tModule.create(driver : tSoundInterface;name: pchar);
begin
  handle := tmodule_create(driver.getHandle,name);
  if (handle = 0) then raise Exception.Create('No Module');
end;


constructor tModule.create(driver : tSoundInterface;s : tstream);
var
  callbacks : tstream_callbacks;

begin
  with callbacks do
  begin
    vread_func   := @read_func;
    vwrite_func  := @write_func;
    vsize_func   := @size_func;
    vpos_func    := @pos_func;
    vseek_func   := @seek_func;
    vclose_func  := @close_func;
  end;


  handle := tmodule_create_stream(driver.handle, callbacks, longword(s));
  if (handle = 0) then raise exception.create('No module');
end;


destructor tModule.destroy;
begin
  tmodule_destroy(handle);
end;

procedure tModule.setvolume(channel : longint; volume : double);
begin
  tmodule_setvolume(handle,channel,volume);
end;

procedure tModule.play;
begin
  tmodule_play(handle);
end;

procedure tModule.stop;
begin
  tmodule_stop(handle);
end;

procedure tModule.pause;
begin
  tmodule_pause(handle);
end;

function  tModule.isplaying : boolean;
begin
  result := tmodule_isplaying(handle)
end;

function  tModule.ispaused : boolean;
begin
  result := tmodule_isplaying(handle)
end;


function  tModule.getvu(modchannel,inchannel : longword) : double;
begin
  result := tmodule_getvu(handle,modchannel,inchannel);
end;

function  tModule.getpos : int64;
begin
  result := tmodule_getpos(handle);
end;

function  tModule.getsize : int64;
begin
  result := tmodule_getsize(handle);
end;

procedure tModule.setpos(position : int64);
begin
  tmodule_setpos(handle,position);
end;



end.
