--------------------------------------------------------------------------------------------------------------------
--  Copyright (C) 1997-2018 Sam Lantinga <slouken@libsdl.org>
--
--  This software is provided 'as-is', without any express or implied
--  warranty. In no event will the authors be held liable for any damages
--  arising from the use of this software.
--
--  Permission is granted to anyone to use this software for any purpose,
--  including commercial applications, and to alter it and redistribute it
--  freely, subject to the following restrictions:
--
--     1. The origin of this software must not be misrepresented; you must not
--     claim that you wrote the original software. If you use this software
--     in a product, an acknowledgment in the product documentation would be
--     appreciated but is not required.
--
--     2. Altered source versions must be plainly marked as such, and must not be
--     misrepresented as being the original software.
--
--     3. This notice may not be removed or altered from any source
--     distribution.
--------------------------------------------------------------------------------------------------------------------
--  SDL.Mixer.Music
--------------------------------------------------------------------------------------------------------------------

with SDL.RWops;

package SDL.Mixer.Music is

   type Music_Type_Type is (None, CMD, WAV, Mod_C, MID, OGG, MP3,
                            MP3_Mad_Unused, Flac, MUS_MODPLUG_Unused, Opus);
   --  These are types of music files (not libraries used to load them)

   type Music_Type is private;

   function Number_Of_Decoders return Natural;
   function Decoder_Name (Index : in Positive) return String;
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

   procedure Load_MUS (Filename : in     String;
                       Music    :    out Music_Type);

   procedure Load_MUS_RW (Source      : in out SDL.RWops.RWops;
                          Free_Source : in     Boolean;
                          Music       :    out Music_Type);
   --  Load a music file from an SDL_RWop object (Ogg and MikMod specific
   --  currently)
   --  Matt Campbell (matt@campbellhome.dhs.org) April 2000

   procedure Load_MUS_Type_RW (Source      : in out SDL.RWops.RWops;
                               Typ         : in     Music_Type_Type;
                               Free_Source : in     Boolean;
                               Music       :    out Music_Type);
   --  Load a music file from an SDL_RWop object assuming a specific format

   procedure Free (Music : in out Music_Type);
   --  Free an audio chunk previously loaded

   type Loop_Count is new Integer;
   Loop_Forever : constant Loop_Count := -1;

   procedure Play (Music : in Music_Type;
                   Loops : in Loop_Count);
   --  Play Music.
   --  If Loops is greater than zero, loop the sound that many times.
   --  If Loops is Loop_Forever (-1), loop inifinitely (~65000 times).
   --  Returns which channel was used to play the sound.

   procedure Fade_In (Music : in Music_Type;
                      Loops : in Loop_Count;
                      Ms    : in Integer);
   procedure Fade_In_Pos (Music    : in Music_Type;
                          Loops    : in Loop_Count;
                          Ms       : in Integer;
                          Position : in Long_Float);
   --  Fade in music or a channel over "ms" milliseconds, same semantics as the
   --  Play functions

   procedure Volume (New_Volume : in     Volume_Type;
                     Old_Volume :    out Volume_Type);
   procedure Volume (New_Volume : in Volume_Type);
   function Get_Volume return Volume_Type;
   --  Set/get the volume in the range of 0-128 of a specific channel or chunk.
   --  If the specified channel is -1, set volume for all channels.
   --  Returns the original volume.
   --  If the specified volume is -1, just return the current volume.

   procedure Pause;
   procedure Resume;
   procedure Rewind;
   --  Pause/Resume the music stream

   procedure Set_Position (Position : in Long_Float);
   --  Set the current position in the music stream.
   --  This returns 0 if successful, or -1 if it failed or isn't implemented.
   --  This function is only implemented for MOD music formats (set pattern
   --  order number) and for OGG, FLAC, MP3_MAD, MP3_MPG and MODPLUG music
   --  (set position in seconds), at the moment.

   procedure Set_CMD (Command : in String);
   --  Stop music and set external music playback command

   procedure Halt;
   --  Halt playing of a particular Channel

   procedure Fade_Out (Ms : in Integer);
   --  Halt a channel, fading it out progressively till it's silent

   function Get_Type (Music : in Music_Type) return Music_Type_Type;
   --  Find out the music format of a mixer music, or the currently playing
   --  music, if 'music' is NULL.

   function Is_Playing return Boolean;
   function Is_Paused return Boolean;
   --  Check the status of a specific channel.
   --  If the specified channel is -1, check all channels.

   function Fading return Fading_Type;
   --  Query the fading status of a channel

private

   type Music_Record is null record;
   type Music_Type is access all Music_Record;

end SDL.Mixer.Music;
