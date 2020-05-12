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

with Interfaces.C.Strings;

package body SDL.Mixer is


   procedure Initialise (Flags : in Init_Flag)
   is
      function Mix_Init (Flags : in C.int) return C.int
        with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_Init";
      Flags_C : constant C.int := C.int (Flags);
      Result  : constant C.int := Mix_Init (Flags_C);
   begin
      if Result = 0 then
         raise Mixer_Error;
      end if;
   end Initialise;


   procedure Quit
   is
      procedure Mix_Quit
        with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_Quit";
   begin
      Mix_Quit;
   end Quit;


   procedure Open (Frequency  : in Sample_Rate;
                   Format     : in Audio_Format;
                   Channels   : in Channel_Count;
                   Chunk_Size : in Integer)
   is
      function Mix_Open_Audio (Frequency  : in C.int;
                               Format     : in Audio_Format; -- Interfaces.Unsigned_16;
                               Channels   : in C.int;
                               Chunk_Size : in C.int)
                              return C.int
        with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_OpenAudio";
      Result_C : constant C.int := Mix_Open_Audio (C.int (Frequency),
                                                   Format,
                                                   C.int (Channels),
                                                   C.int (Chunk_Size));
   begin
      if Result_C /= 0 then
         raise Mixer_Error;
      end if;
   end Open;


   procedure Open (Frequency       : in Sample_Rate;
                   Format          : in Audio_Format;
                   Channels        : in Channel_Count;
                   Chunk_Size      : in Integer;
                   Device_Name     : in String;
                   Allowed_Changes : in Integer)
   is
      use Interfaces.C.Strings;
      function Mix_Open_Audio_Device (Frequency       : in C.int;
                                      Format          : in Audio_Format; -- Interfaces.Unsigned_16;
                                      Channels        : in C.int;
                                      Chunk_Size      : in C.int;
                                      Device_Name     : in chars_ptr;
                                      Allowed_Changes : in C.int)
                              return C.int
        with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_OpenAudioDevice";
      Result_C : constant C.int := Mix_Open_Audio_Device (C.int (Frequency),
                                                          Format,
                                                          C.int (Channels),
                                                          C.int (Chunk_Size),
                                                          New_String (Device_Name),
                                                          C.int (Allowed_Changes));
   begin
      if Result_C /= 0 then
         raise Mixer_Error;
      end if;
   end Open;


   procedure Close
   is
      procedure Mix_Close_Audio
        with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_CloseAudio";
   begin
      Mix_Close_Audio;
   end Close;


   procedure Query_Spec (Frequency : out Sample_Rate;
                         Format    : out Audio_Format;
                         Channels  : out Channel_Count)
   is
      use Interfaces;
      procedure Mix_Query_Spec (Frequency : out C.int;
                                Format    : out Audio_Format; -- Unsigned_16;
                                Channels  : out C.int)
        with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_QuerySpec";
      Frequency_C : C.int;
      Format_C    : Audio_Format; -- Unsigned_16;
      Channels_C  : C.int;
   begin
      Mix_Query_Spec (Frequency_C, Format_C, Channels_C);
      Frequency := Sample_Rate (Frequency_C);
      Format    := Format_C;
      Channels  := Channel_Count (Channels_C);
   end Query_Spec;


end SDL.Mixer;
