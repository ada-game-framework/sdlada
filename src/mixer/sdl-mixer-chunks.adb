--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
with SDL.Error;
with Interfaces.C.Strings;

package body SDL.Mixer.Chunks is
   procedure Load (Source      : in out SDL.RWops.RWops;
                   Free_Source : in     Boolean;
                   Chunk       :    out Chunk_Type) is
      function Mix_Load_WAV_RW (Source   : in SDL.RWops.RWops;
                                Free_Src : in C.int) return Chunk_Type with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_LoadWAV_RW";

      Chunk_C : constant Chunk_Type := Mix_Load_WAV_RW (Source, Boolean'Pos (Free_Source));
   begin
      if Chunk_C = null then
         raise Mixer_Error with SDL.Error.Get;
      end if;

      Chunk := Chunk_C;
   end Load;


   procedure Load (Filename : in String; Chunk : out Chunk_Type) is
      use SDL.RWops;

      Ops : RWops.RWops := From_File (Filename, Read_Binary);
   begin
      Load_WAV_RW (Ops, True, Chunk);
   end Load;


   procedure Quick_Load_WAV (Mem   : in     System.Address;
                             Chunk :    out Chunk_Type) is
      function Mix_Quick_Load_WAV (Mem : in System.Address) return Chunk_Type with
        Import => True,
        Convention => C,
        External_Name => "Mix_QuickLoad_WAV";

      Result : constant Chunk_Type := Mix_Quick_Load_WAV (Mem);
   begin
      if Result = null then
         raise Mixer_Error with SDL.Error.Get;
      end if;

      Chunk := Result;
   end Quick_Load_WAV;


   procedure Quick_Load_RAW (Mem   : in     System.Address;
                             Len   : in     Byte_Count;
                             Chunk :    out Chunk_Type) is
      function Mix_Quick_Load_RAW (Mem : in System.Address;
                                   Len : in Interfaces.Unsigned_32) return Chunk_Type with
        Import => True,
        Convention => C,
        External_Name => "Mix_QuickLoad_RAW";

      Result : constant Chunk_Type := Mix_Quick_Load_RAW (Mem, Interfaces.Unsigned_32 (Len));
   begin
      if Result = null then
         raise Mixer_Error with SDL.Error.Get;
      end if;

      Chunk := Result;
   end Quick_Load_RAW;


   procedure Free (Chunk : in out Chunk_Type) is
      procedure Mix_Free_Chunk (Chunk : in Chunk_Type) with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_FreeChunk";
   begin
      Mix_Free_Chunk (Chunk);
   end Free;


   function Number_Of_Decoders return Natural is
      function Mix_Get_Num_Chunk_Decoders return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_GetNumChunkDecoders";
   begin
      return Natural (Mix_Get_Num_Chunk_Decoders);
   end Number_Of_Decoders;


   function Decoder_Name (Index : in Positive) return String is
      function Mix_Get_Chunk_Decoder_Name (Index : in C.int) return C.Strings.chars_ptr with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_GetChunkDecoder";
   begin
      return C.Strings.Value (Mix_Get_Chunk_Decoder_Name (C.int (Index - 1)));
   end Decoder_Name;


   function Has_Decoder (Name : in String) return Boolean is
      function Mix_Has_Chunk_Decoder (Name : in C.char_array) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_HasChunkDecoder";
   begin
      return Mix_Has_Chunk_Decoder (C.To_C (Name)) /= Success;
   end Has_Decoder;
end SDL.Mixer.Chunks;
