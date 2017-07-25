unit soundlib3dll;

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

type
  sl_int8       = shortint;
  sl_uint8      = byte;
  sl_int16      = smallint;
  sl_uint16     = word;
  sl_int32      = longint;
  sl_uint32     = longword;
  sl_int64      = int64;
  sl_uint64     = int64;
  sl_bool       = longbool;
  pointerval    = longword;

const
  sldllname = 'soundlib.dll';

  maxoutChannels  = 8;    { Maximum number of output channels  }
  maxinChannels   = 8;    { Maximum number of input channels (per stream) }
  maxvolume       = 2;
  minvolume       = 0;

  // Sound format flags
  snd_default       = $00000000;
  snd_signed        = $00000200; // Sound data is signed
  snd_nonsigned     = $00000000; // Sound data is non-signed
  snd_8bit          = $00000000; // Sound data uses 8 bits
  snd_16bit         = $00000400; // Sound data uses 16 bits
  snd_24bit         = $00000800; // Sound data uses 24 bits
  snd_32bit         = $00000100; // Sound data uses 32 bits
  snd_fp            = $00001000; // Data is floating point
  snd_monosurround  = $00004000; // Dolby Pro Logic compatible playback
  snd_stereosurround= $00008000; // Dolby Pro Logic II compatible playback

  snd_auto          = $10000000;

  // Sound channel flags
  channels_mono      = $00000001;
  channels_stereo    = $00000002;
  channels_4         = $00000004; { 4 channel playback             }
  channels_5         = $00000005; { 5 channel playback             }
  channels_6         = $00000006; { 6 channel playback             }
  channels_7         = $00000007; { 7 channel playback             }
  channels_mask      = $000000FF; { Mask for channels number       }
  channels_lfe       = $10000000; { Include LFE channel            }

  // Interpolation types
  interpolation_none    = 0;
  interpolation_linear  = 1;
  interpolation_cubic   = 2;
  interpolation_default = interpolation_linear;
  interpolation_fastest = interpolation_linear;
  interpolation_best    = 200;
  interpolation_average = 100;


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
        min,max,preferred : sl_uint32;
      end;
    end;
    outputchannels : sl_uint32;
    lfe            : sl_bool;
    flags          : sl_uint32;
    version        : sl_uint32;
    wavetablesize  : sl_uint32;
    minbufsize,
    maxbufsize     : sl_uint32;
  end;

  tSoundInterfaceInfo = packed record
    interfaceName     : pchar;      // Name of interface
    interfaceVersion  : pchar;      // Version of SL interface driver
    priority          : sl_int32;   // Priority of this interface (for auto selection)
    flags             : sl_uint32;  // stores information about interface
    physical          : sl_bool;    // True if physical device; false for null output
  end;

  // tSoundInterface playback information
  tPlaybackInfo = packed record
    frequency       : sl_uint32;  // Playback frequency
    resolution      : sl_uint32;  // Playback resolution
    outSamplesize   : sl_uint32;
    inSamplesize    : sl_uint32;
    is_fp           : sl_bool;    // Mixer uses floating point single values
    inchannels      : sl_uint32;  // Number of input channels for playback
    outchannels     : sl_uint32;  // Number of output playback channels
    lfe             : sl_bool;    // If true, last in/outchannel is an LFE channel
    buffersize      : sl_uint32;  // Size of playback mixing buffer in samples
    buffers         : sl_uint32;  // Number of buffers for playback
  end;

  // Sound class information
  tSoundInfo = packed record
    name        : pchar;      // Name of the sound or song
    creator     : pchar;      // Creator of that sound or song
    encoder     : pchar;      // Encoder/Creation Software string
    info        : pchar;      // Additional info field if available
    length      : sl_uint64;  // Length of sound or song in samples
    frequency   : sl_uint32;  // Original playback frequency of that sound
    is_fp       : sl_bool;    // Floating point single values
    resolution  : sl_uint32;  // Resolution of sound data
    channels    : sl_int32;   // Number of channels this sound contains
    samplesize  : sl_uint32;  // Size of a sample in bytes
    buffersize  : sl_uint32;  // Stream buffer request size in samples
  end;


// Callback functions
  trequestblock_direct16 = function(handle : tHandle; p : pointer; samples : longword) : longword; cdecl;
  trequestblock_directsingle = function(handle : tHandle; p : pointer; samples : longword) : longword; cdecl;
  tupdateMatrix = procedure(handle : tHandle); cdecl;

  tsoundstream_callbacks = packed record
    vrequestblock_direct16      : trequestblock_direct16;
    vrequestblock_directsingle  : trequestblock_directsingle;
    vupdateMatix                : tUpdateMatrix;
    handle                      : tHandle;
  end;

// Classes

  tSoundInterface = class
    constructor create; virtual;
    destructor  destroy; override;

    procedure   startplayback(freq : longword = 0;flags : longword = snd_auto;channels : longword = 0;bufsize : longword = 0;buffers : longword = 0); virtual;
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
    procedure   setMixManual(active : boolean); virtual;
    procedure   getMixData(dest : pointer); virtual;
    procedure   getOutputData(dest : pointer); virtual;
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

  tSoundCollection = class
    private
      parent       : tSoundInterface;

    public
    constructor create(driver: tSoundInterface;sounds : longword); reintroduce;
    destructor  destroy; reintroduce;
    property    getUpperNode : tSoundInterface read parent;

    procedure   setBufferSize(samples : longword); virtual;
    procedure   setChannels(channels : longword); virtual;
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
      handle    : tHandle;
      callbacks : tsoundstream_callbacks;
      cb_init   : boolean;
      parent    : tSoundCollection;

    protected  
    procedure   connect(collection : tSoundCollection); virtual;
    procedure   disconnect; virtual;
    procedure   setBufferSize(size : sl_uint32); virtual;

    public
    constructor create;
    destructor  destroy; override;
    property    getUpperNode : tSoundCollection read parent;

    procedure   play; virtual;
    procedure   pause; virtual;
    procedure   stop; virtual;

    procedure   setvolume(channel : longint;volume : double); virtual;
    function    getvolume(channel : longword) : double; virtual;

    procedure   setChannelMapping(map : tChannelVolumeMap); virtual;
    function    getChannelMapping : tChannelVolumeMap; virtual;

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
    function    isMuted : boolean; virtual;
    function    getInfo : tSoundInfo; virtual;
    procedure   setInfo(info : tSoundInfo); virtual;
    procedure   setPanning(channel : longint; lr,cs,tb,volume : double); virtual;

    // 3D methods
    procedure   setObjectPosition(channel : integer; position : tSVector; relative : boolean); virtual;
    procedure   setObjectOrientation(channel : integer; orientation : tSVector); virtual;
    procedure   setObjectVelocity(channel : integer; velocity : tSVector); virtual;
    procedure   setObjectSpreadAngle(channel : integer; angle : double); virtual;
    procedure   setObjectSize(channel : integer; size : tSVector); virtual;

    property    getHandle : tHandle read handle;

    protected
    function    requestblock_direct16(p : pointer;samples : longword) : longword; virtual;
    function    requestblock_directSingle(p : pointer;samples : longword) : longword; virtual;
  end;


  tSample = class(tSoundStream)
    constructor create(dest : tSoundCollection; stream : tSoundStream); overload; virtual;
    constructor create(dest : tSoundCollection; s : pointer; samples, flags, channels, frequency : longword); overload; virtual;
    destructor  destroy; override;

    procedure   setloop(strt,stp : int64); virtual;
    procedure   setpos(pos : int64);override;
    function    getpos : int64; override;
    function    getsize : int64; override;
  end;

  { SoundFile class - loads sound files from disk }
  tSoundFile = class(tSoundStream)
    constructor create(sound: tSoundCollection;name : pchar); overload;
    constructor create(sound: tSoundCollection;s : tstream); overload;
    destructor  destroy; override;
    private
      isstream : boolean;
  end;

  tModule = class(tSoundCollection)
    constructor create(driver : tSoundInterface;name: pchar); overload;
    constructor create(driver : tSoundInterface;s : tStream); overload;
    destructor  destroy; reintroduce;
    procedure   setvolume(channel : longint; volume : double); override;
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
procedure tsoundinterface_getplaybackinfo(handle : tHandle;var info : tPlaybackInfo); cdecl; external sldllname;
procedure tsoundinterface_destroy(handle : tHandle); cdecl; external sldllname;

function  tsoundinterface_getinterfacecount(handle : tHandle) : longword; cdecl; external sldllname;
function  tsoundinterface_getcurrentinterface(handle : tHandle) : longword; cdecl; external sldllname;
function  tsoundinterface_setinterface(handle : tHandle;device : longword) : tHandle; cdecl; external sldllname;
procedure tsoundinterface_getinterfaceinfo(handle : tHandle;device : longword;var info : tSoundInterfaceInfo); cdecl; external sldllname;

function  tsoundinterface_getdevicecount(handle : tHandle) : longword; cdecl; external sldllname;
function  tsoundinterface_getcurrentdevice(handle : tHandle) : longword; cdecl; external sldllname;
function  tsoundinterface_setdevice(handle : tHandle;device : longword) : boolean; cdecl; external sldllname;
procedure tsoundinterface_getdeviceinfo(handle : tHandle;device : longword;var info : tSoundDeviceInfo); cdecl; external sldllname;

function  tsoundinterface_getmixcount(handle : tHandle) : cardinal; cdecl;  external sldllname;
procedure tsoundinterface_setmixmanual(handle : tHandle; active : boolean); cdecl; external sldllname;
procedure tsoundinterface_getmixdata(handle : tHandle; dest : pointer); cdecl; external sldllname;
procedure tsoundinterface_getoutputdata(handle : tHandle; dest : pointer); cdecl; external sldllname;
function  tsoundinterface_getversionstring(handle : tHandle) : pchar; cdecl; external sldllname;

procedure tsoundinterface_setlistenerposition(handle : tHandle; position: tSVector); cdecl;  external sldllname;
procedure tsoundinterface_setlistenerorientation(handle : tHandle; orientation : tSVector); cdecl;  external sldllname;
procedure tsoundinterface_setlistenervelocity(handle : tHandle; velocity: tSVector); cdecl;  external sldllname;
procedure tsoundinterface_updatelistener(handle : tHandle; position, orientation, speed: tSVector); cdecl;  external sldllname;

function  tsoundstream_create : tHandle; cdecl; external sldllname;
procedure tsoundstream_destroy(handle : tHandle); cdecl; external sldllname;
procedure tsoundstream_setcallbacks(handle : tHandle; cb : tSoundStream_callbacks); cdecl; external sldllname;
procedure tsoundstream_play(handle : tHandle); cdecl; external sldllname;
procedure tsoundstream_pause(handle : tHandle); cdecl; external sldllname;
procedure tsoundstream_stop(handle : tHandle); cdecl; external sldllname;

procedure tsoundstream_connect(handle : tHandle;collection : tHandle);cdecl; external sldllname;
procedure tsoundstream_disconnect(handle : tHandle);cdecl; external sldllname;

procedure tsoundstream_setchannelmapping(handle : tHandle;map : tChannelVolumeMap);cdecl; external sldllname;
function  tsoundstream_getchannelmapping(handle : tHandle) : tChannelVolumeMap;cdecl; external sldllname;
procedure tsoundstream_setinfo(handle : tHandle; info : tSoundInfo);cdecl; external sldllname;
procedure tsoundstream_setbuffersize(handle : tHandle;size : sl_uint32);cdecl; external sldllname;

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
procedure tsoundstream_getinfo(handle : tHandle;var info : tSoundInfo); cdecl; external sldllname;
procedure tsoundstream_setpanning(handle : tHandle;channel : longint; lr,cs,tb,volume : double); cdecl; external sldllname;

procedure tsoundstream_setobjectposition(handle : tHandle; channel : integer; position : tSVector; relative : boolean); cdecl; external sldllname;
procedure tsoundstream_setobjectorientation(handle : tHandle; channel : integer; orientation : tSVector); cdecl; external sldllname;
procedure tsoundstream_setobjectvelocity(handle : tHandle; channel : integer; velocity : tSVector); cdecl; external sldllname;
procedure tsoundstream_setobjectspreadangle(handle : tHandle; channel : integer; angle : double); cdecl; external sldllname;
procedure tsoundstream_setobjectsize(handle : tHandle; channel : integer; size : tSVector); cdecl; external sldllname;

function  tsample_create(soundhandle : tHandle;streamhandle : tHandle) : tHandle; cdecl; external sldllname;
function  tsample_create_memory(soundhandle : tHandle; s : pointer; samples, flags, channels, frequency : longword) : tHandle; cdecl; external sldllname;
procedure tsample_destroy(handle : tHandle); cdecl; external sldllname;
function  tsample_getpos(handle : tHandle) : int64; cdecl; external sldllname;
function  tsample_getsize(handle : tHandle) : int64; cdecl; external sldllname;
procedure tsample_setpos(handle : tHandle; position : int64); cdecl; external sldllname;
procedure tsample_setloop(handle : tHandle; startpos,endpos : int64); cdecl; external sldllname;

(*procedure tsample_play(handle : tHandle); cdecl; external sldllname;
procedure tsample_pause(handle : tHandle); cdecl; external sldllname;
procedure tsample_stop(handle : tHandle); cdecl; external sldllname;
procedure tsample_setvolume(handle : tHandle;channel : longint;volume : double);cdecl; external sldllname;
function  tsample_getvu(handle : tHandle;inchannel : longword) : double; cdecl; external sldllname;
procedure tsample_setpanning(handle : tHandle;channel : longword; lr,cs,tb,volume : double); cdecl; external sldllname;*)


function  tsound_create(driverhandle : tHandle;sounds : longword) : tHandle;cdecl; external sldllname;
procedure tsound_destroy(handle : tHandle);cdecl; external sldllname;
procedure tsound_setbuffersize(handle : tHandle;samples : longword);cdecl; external sldllname;
procedure tsound_setchannels(handle : tHandle;channels : longword);cdecl; external sldllname;
procedure tsound_setvolume(handle : tHandle;channel : longint;volume : double);cdecl; external sldllname;
function  tsound_getvolume(handle : tHandle;channel : longword) : double; cdecl; external sldllname;
procedure tsound_setinterpolation(handle : tHandle;intpoltype : longword); cdecl; external sldllname;
procedure tsound_setramping(handle : tHandle; active : boolean); cdecl; external sldllname;
procedure tsound_getinfo(handle : tHandle; var info : tSoundInfo); cdecl; external sldllname;

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



// Callback functions
function frequestblock_direct16(handle : tHandle; p : pointer; samples : longword) : longword; cdecl;
function frequestblock_directsingle(handle : tHandle; p : pointer; samples : longword) : longword; cdecl;


implementation

// Callback functions for file handling
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


// Callback functions for sound streaming
function frequestblock_direct16(handle : tHandle; p : pointer; samples : longword) : longword; cdecl;
var
  s : tSoundStream absolute handle;
begin
  result := s.requestblock_direct16(p, samples);
end;

function frequestblock_directsingle(handle : tHandle; p : pointer; samples : longword) : longword; cdecl;
var
  s : tSoundStream absolute handle;
begin
  result := s.requestblock_directSingle(p, samples);
end;

// ---


// tSoundInterface methods
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
var info : tPlaybackInfo;
begin
  tsoundinterface_getplaybackinfo(handle, info);
  result := info;
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
var info : tSoundDeviceInfo;
begin
  tsoundinterface_getdeviceinfo(handle, device, info);
  result := info;
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
var info : tSoundInterfaceInfo;
begin
  tsoundinterface_getinterfaceinfo(handle, device, info);
  result := info;
end;


function  tSoundInterface.getMixCount : cardinal;
begin
  result := tsoundinterface_getmixcount(handle);
end;

procedure tSoundInterface.setMixManual(active : boolean);
begin
  tsoundinterface_setmixmanual(handle, active);
end;

procedure tSoundInterface.getMixData(dest : pointer);
begin
  tsoundinterface_getmixdata(handle, dest);
end;

procedure tSoundInterface.getOutputData(dest : pointer);
begin
  tsoundinterface_getoutputdata(handle, dest);
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


// tSoundCollection

constructor tSoundCollection.create(driver: tSoundInterface;sounds : longword);
begin
  handle := tsound_create(driver.getHandle,sounds);
  driverhandle := driver.getHandle;
  parent := driver;
end;

destructor   tSoundCollection.destroy;
begin
  tsound_destroy(handle);
  inherited;
end;

procedure   tSoundCollection.setBufferSize(samples : longword);
begin
  tsound_setbuffersize(handle,samples);
end;

procedure   tSoundCollection.setChannels(channels : longword);
begin
  tsound_setchannels(handle,channels);
end;

procedure   tSoundCollection.setVolume(channel : longint;volume : double);
begin
  tsound_setvolume(handle,channel,volume);
end;

function    tSoundCollection.getVolume(channel : longword) : double;
begin
  result := tsound_getvolume(handle,channel);
end;

procedure   tSoundCollection.setInterpolation(intpoltype : longword);
begin
  tsound_setinterpolation(getHandle,intpoltype);
end;

procedure   tSoundCollection.setRamping(active : boolean);
begin
  tsound_setramping(handle,active);
end;

function    tSoundCollection.getInfo : tSoundInfo;
var info : tSoundInfo;
begin
  tsound_getinfo(handle, info);
  result := info;
end;

procedure   tSoundCollection.stopAll;
begin
  tsound_stopall(handle);
end;

procedure   tSoundCollection.pauseAll;
begin
  tsound_pauseall(handle);
end;

procedure   tSoundCollection.resumeAll;
begin
  tsound_resumeall(handle);
end;
// ---


// --- tSoundStream ---

constructor  tSoundStream.create;
begin
  inherited;

  handle := tsoundstream_create;

  with callbacks do
  begin
    vrequestblock_direct16      := @frequestblock_direct16;
    vrequestblock_directsingle  := @frequestblock_directsingle;
    vupdateMatix                := nil;
    handle                      := tHandle(self);
  end;

  tsoundstream_setcallbacks(handle,callbacks);
end;

destructor  tSoundStream.destroy;
begin
  tsoundstream_destroy(handle);
  inherited;
end;

procedure tSoundStream.connect(collection : tSoundCollection);
begin
  parent := collection;
  tsoundstream_connect(handle,collection.getHandle);
end;

procedure tSoundStream.disconnect;
begin
  tsoundstream_disconnect(handle);
  parent := nil;
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

procedure tSoundStream.setBufferSize(size : sl_uint32);
begin
  tsoundstream_setbuffersize(handle,size);
end;

procedure tSoundStream.setvolume(channel : longint;volume : double);
begin
  tsoundstream_setvolume(handle,channel,volume);
end;

function  tSoundStream.getvolume(channel : longword) : double;
begin
  result := tsoundstream_getvolume(handle,channel);
end;

procedure tSoundStream.setChannelMapping(map : tChannelVolumeMap);
begin
  tsoundstream_setchannelmapping(handle,map);
end;

function  tSoundStream.getChannelMapping : tChannelVolumeMap;
begin
  result := tsoundstream_getchannelmapping(handle);
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
var info : tSoundInfo;
begin
  tsoundstream_getinfo(handle, info);
  result := info;
end;

procedure tSoundStream.setInfo(info : tSoundInfo);
begin
  tsoundstream_setinfo(handle,info);
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


function tSoundStream.requestblock_direct16(p : pointer;samples : longword) : longword;
begin
  result := 0;
end;

function tSoundStream.requestblock_directSingle(p : pointer;samples : longword) : longword;
begin
  result := 0;
end;


// tSample

constructor tSample.create(dest : tSoundCollection; stream : tSoundStream);
begin
  handle := tsample_create(dest.getHandle,stream.getHandle);
end;

constructor tSample.create(dest : tSoundCollection; s : pointer; samples, flags, channels, frequency : longword);
begin
  handle := tsample_create_memory(dest.handle, s, samples, flags, channels, frequency);
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
constructor tSoundFile.create(sound: tSoundCollection;name : pchar);
begin
  handle := tsoundfile_create(sound.handle, name);
  if (handle = 0) then exception.create('File type not recognized');
  isstream := false;
end;

constructor tSoundFile.create(sound: tSoundCollection;s : tstream);
var
  stream_callbacks : tstream_callbacks;

begin
  // Set up callback functions for file actions
  with stream_callbacks do
  begin
    vread_func   := @read_func;
    vwrite_func  := @write_func;
    vsize_func   := @size_func;
    vpos_func    := @pos_func;
    vseek_func   := @seek_func;
    vclose_func  := @close_func;
  end;

  handle := tsoundfile_create_stream(sound.handle, stream_callbacks, longword(s));
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
