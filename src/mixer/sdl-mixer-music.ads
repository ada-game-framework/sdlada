--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
--  SDL.Mixer.Music
--------------------------------------------------------------------------------------------------------------------
with SDL.RWops;

package SDL.Mixer.Music is
   --  These are types of music files (not libraries used to load them)
   type Music_Type_Type is  --  TODO: Fix this name.
     (None,
      CMD,
      WAV,
      Tracker,
      MIDI,
      OGG,
      MP3,
      MP3_Mad_Unused,
      Flac,
      MUS_MODPLUG_Unused,
      Opus);

   function Mod_C return Music_Type_Type renames Tracker;  --  Deprecated, terrible name.

   type Music_Type is private;

   --  Get a list of music decoders that this build of SDL2_mixer provides.
   --  This list can change between builds AND runs of the program, if external
   --  libraries that add functionality become available. You must successfully
   --  call Open before calling these functions.
   --
   --  usage...
   --  for Index in 1 .. Number_Of_Decoders loop
   --     Ada.Text_IO.Put_Line ("Supported music decoder: "
   --                           & Decoder_Name (index));
   --  end loop;
   --
   --  Appearing in this list doesn't promise your specific audio file will
   --  decode...but it's handy to know if you have, say, a functioning Timidity
   --  install.
   function Number_Of_Decoders return Natural with
     Inline;

   function Decoder_Name (Index : in Positive) return String with
     Inline;

   --  Load a music file from an SDL_RWop object (Ogg and MikMod specific currently)
   --  Matt Campbell (matt@campbellhome.dhs.org) April 2000
   procedure Load (Filename : in String; Music : out Music_Type);
   procedure Load_MUS (Filename : in String; Music : out Music_Type) renames Load;  --  Deprecated

   procedure Load (Source      : in out SDL.RWops.RWops;
                   Free_Source : in     Boolean;
                   Music       :    out Music_Type);
   procedure Load_MUS_RW (Source      : in out SDL.RWops.RWops;
                          Free_Source : in     Boolean;
                          Music       :    out Music_Type) renames Load;  --  Deprecated

   --  Load a music file from an SDL_RWop object assuming a specific format
   procedure Load (Source      : in out SDL.RWops.RWops;
                   Typ         : in     Music_Type_Type;
                   Free_Source : in     Boolean;
                   Music       :    out Music_Type);
   procedure Load_MUS_Type_RW (Source      : in out SDL.RWops.RWops;
                               Typ         : in     Music_Type_Type;
                               Free_Source : in     Boolean;
                               Music       :    out Music_Type) renames Load;  --  Deprecated

   --  Free an audio chunk previously loaded
   procedure Free (Music : in out Music_Type) with
     Inline;

   type Loop_Count is new Integer;

   Loop_Forever : constant Loop_Count := -1;

   --  Play Music.
   --  If Loops is greater than zero, loop the sound that many times.
   --  If Loops is Loop_Forever (-1), loop inifinitely (~65000 times).
   --  Returns which channel was used to play the sound.
   procedure Play (Music : in Music_Type; Loops : in Loop_Count) with
     Inline;

   --  Fade in music or a channel over "ms" milliseconds, same semantics as the
   --  Play functions
   procedure Fade_In (Music : in Music_Type;
                      Loops : in Loop_Count;
                      Ms    : in Integer) with
     Inline;

   procedure Fade_In (Music    : in Music_Type;
                      Loops    : in Loop_Count;
                      Ms       : in Integer;
                      Position : in Long_Float) with
     Inline;

   --  Set/get the volume in the range of 0-128 of a specific channel or chunk.
   --  If the specified channel is -1, set volume for all channels.
   --  Returns the original volume.
   --  If the specified volume is -1, just return the current volume.
   procedure Volume (New_Volume : in Volume_Type; Old_Volume : out Volume_Type) with
     Inline;

   procedure Volume (New_Volume : in Volume_Type) with
     Inline;

   function Get_Volume return Volume_Type with
     Inline;

   --  Pause/Resume the music stream
   procedure Pause with
     Import        => True,
     Convention    => C,
     External_Name => "Mix_PauseMusic";

   procedure Resume with
     Import        => True,
     Convention    => C,
     External_Name => "Mix_ResumeMusic";

   procedure Rewind with
     Import        => True,
     Convention    => C,
     External_Name => "Mix_RewindMusic";

   --  Set the current position in the music stream.
   --  This returns 0 if successful, or -1 if it failed or isn't implemented.
   --  This function is only implemented for MOD music formats (set pattern
   --  order number) and for OGG, FLAC, MP3_MAD, MP3_MPG and MODPLUG music
   --  (set position in seconds), at the moment.
   procedure Set_Position (Position : in Long_Float) with
     Inline;

   --  Stop music and set external music playback command
   procedure Set_Command (Command : in String) with
     Inline;

   procedure Set_CMD (Command : in String) renames Set_Command;  --  Deprecated

   --  Halt playing of a particular Channel
   procedure Halt with
     Inline;

   --  Halt a channel, fading it out progressively till it's silent
   procedure Fade_Out (Ms : in Integer) with
     Inline;

   --  Find out the music format of a mixer music, or the currently playing
   --  music, if 'music' is NULL.
   function Get_Type (Music : in Music_Type) return Music_Type_Type with
     Inline;

   function Is_Playing return Boolean with
     Inline;

   --  Check the status of a specific channel.
   --  If the specified channel is -1, check all channels.
   function Is_Paused return Boolean with
     Inline;

   --  Query the fading status of a channel
   function Fading return Fading_Type with
     Inline;
private
   type Music_Record is null record;
   type Music_Type is access all Music_Record;
end SDL.Mixer.Music;
