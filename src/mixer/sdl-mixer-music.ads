--------------------------------------------------------------------------------------------------------------------
--  Copyright (c) 2020 Jesper Quorning
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
   pragma Preelaborate;

   type Music_Type_Type is (None, CMD, WAV, Mod_C, MID, OGG, MP3,
                            MP3_Mad_Unused, Flac, MUS_MODPLUG_Unused, Opus);
   type Music_Type is private;

   function Number_Of_Decoders return Natural;
   function Decoder_Name (Index : in Positive) return String;

   procedure Load_MUS (Filename : in     String;
                       Music    :    out Music_Type);

   procedure Load_MUS_RW (Source      : in out SDL.RWops.RWops;
                          Free_Source : in     Boolean;
                          Music       :    out Music_Type);

   procedure Load_MUS_Type_RW (Source      : in out SDL.RWops.RWops;
                               Typ         : in     Music_Type_Type;
                               Free_Source : in     Boolean;
                               Music       :    out Music_Type);

   procedure Free (Music : in out Music_Type);

   type Loop_Count is new Integer;
   Loop_Forever : constant Loop_Count := -1;

   procedure Play (Music : in Music_Type; Loops : in Loop_Count);
   procedure Fade_In (Music : in Music_Type; Loops : in Loop_Count; Ms : in Integer);
   procedure Fade_In_Pos (Music    : in Music_Type;
                          Loops    : in Loop_Count;
                          Ms       : in Integer;
                          Position : in Long_Float);
--   procedure Hook;
   procedure Volume (New_Volume : in Volume_Type; Old_Volume : out Volume_Type);
   procedure Volume (New_Volume : in Volume_Type);
   function Get_Volume return Volume_Type;
   procedure Pause;
   procedure Resume;
   procedure Rewind;
   procedure Set_Position (Position : in Long_Float);
   procedure Set_CMD (Command : in String);
   procedure Halt;
   procedure Fade_Out (Ms : in Integer);
--   procedure Hook_Finished;
   function Get_Type (Music : in Music_Type) return Music_Type_Type;
   function Is_Playing return Boolean;
   function Is_Paused return Boolean;
   function Fading return Fading_Type;
--   function Get_Hook_Data return Unsigned_32;

private

   type Music_Record is null record;
   type Music_Type is access all Music_Record;

end SDL.Mixer.Music;
