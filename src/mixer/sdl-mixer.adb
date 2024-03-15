--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
package body SDL.Mixer is
   procedure Initialise (Flags : in Init_Flag) is
      function Mix_Init (Flags : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_Init";
   begin
      if Mix_Init (C.int (Flags)) = 0 then
         raise Mixer_Error;
      end if;
   end Initialise;


   procedure Open (Frequency  : in Sample_Rate;
                   Format     : in Audio_Format;
                   Channels   : in Channel_Count;
                   Chunk_Size : in Integer) is
      function Mix_Open_Audio (Frequency  : in C.int;
                               Format     : in Audio_Format;
                               Channels   : in C.int;
                               Chunk_Size : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_OpenAudio";

      Result_C : constant C.int := Mix_Open_Audio (C.int (Frequency),
                                                   Format,
                                                   C.int (Channels),
                                                   C.int (Chunk_Size));
   begin
      if Result_C /= Success then
         raise Mixer_Error;
      end if;
   end Open;


   procedure Open (Frequency       : in Sample_Rate;
                   Format          : in Audio_Format;
                   Channels        : in Channel_Count;
                   Chunk_Size      : in Integer;
                   Device_Name     : in String;
                   Allowed_Changes : in Integer) is
      function Mix_Open_Audio_Device (Frequency       : in C.int;
                                      Format          : in Audio_Format;
                                      Channels        : in C.int;
                                      Chunk_Size      : in C.int;
                                      Device_Name     : in C.char_array;
                                      Allowed_Changes : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_OpenAudioDevice";

      Result_C : constant C.int := Mix_Open_Audio_Device (C.int (Frequency),
                                                          Format,
                                                          C.int (Channels),
                                                          C.int (Chunk_Size),
                                                          C.To_C (Device_Name),
                                                          C.int (Allowed_Changes));
   begin
      if Result_C /= Success then
         raise Mixer_Error;
      end if;
   end Open;


   procedure Query_Spec (Frequency : out Sample_Rate;
                         Format    : out Audio_Format;
                         Channels  : out Channel_Count) is
      procedure Mix_Query_Spec (Frequency : out C.int;
                                Format    : out Audio_Format;
                                Channels  : out C.int) with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_QuerySpec";

      Frequency_C : C.int;
      Channels_C  : C.int;
   begin
      Mix_Query_Spec (Frequency_C, Format, Channels_C);

      Frequency := Sample_Rate (Frequency_C);
      Channels  := Channel_Count (Channels_C);
   end Query_Spec;
end SDL.Mixer;
