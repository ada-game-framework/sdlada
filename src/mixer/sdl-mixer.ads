--------------------------------------------------------------------------------------------------------------------
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
--  SDL.Mixer
--------------------------------------------------------------------------------------------------------------------

with SDL.Audio;

with Interfaces;
with System;

package SDL.Mixer is
   pragma Preelaborate;

   Mixer_Error : exception;

   type Init_Flag is new Interfaces.Unsigned_32;
   Init_Flac : constant Init_Flag := 16#0000_0001#;
   Init_MOD  : constant Init_Flag := 16#0000_0002#;
   Init_MP3  : constant Init_Flag := 16#0000_0008#;
   Init_OGG  : constant Init_Flag := 16#0000_0010#;
   Init_MID  : constant Init_Flag := 16#0000_0020#;
   Init_Opus : constant Init_Flag := 16#0000_0040#;

   procedure Initialise (Flags : in Init_Flag);
   procedure Quit;

   subtype Sample_Rate  is SDL.Audio.Sample_Rate;
   subtype Audio_Format is SDL.Audio.Audio_Format;
   type Channel_Count   is range 1 .. 8;
   type Volume_Type     is range 0 .. 128
     with Size => 8;

   Default_Frequency : constant Sample_Rate   := 22_050;
   Default_Format    : constant Audio_Format  := Audio_S16_SYS;
   Default_Channels  : constant Channel_Count := 2;
   Max_Volume        : constant Volumen_Type  := Volumen_Type'Last;

   type Chunk_Type is private;

   type Fading_Type is (No_Fading, Fading_Out, Fading_In);

   procedure Open (Frequency  : in Sample_Rate;
                   Format     : in Audio_Format;
                   Channels   : in Channel_Count;
                   Chunk_Size : in Integer);

   procedure Open (Frequency       : in Sample_Rate;
                   Format          : in Audio_Format;
                   Channels        : in Channel_Count;
                   Chunk_Size      : in Integer;
                   Device_Name     : in String;
                   Allowed_Changes : in Integer);

   procedure Close;

   procedure Query_Spec (Frequency : out Sample_Rate;
                         Format    : out Audio_Format;
                         Channels  : out Channel_Count);

private

   type Chunk_Record is
      record
         Allocated : Boolean;
         Abuf      : System.Address;
         Alen      : Interfaces.Unsigned_32;
         Volume    : Volume_Type;
      end record;

   type Chunk_Type is access all Chunk_Record;

end SDL.Mixer;
