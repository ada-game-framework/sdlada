--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
with SDL.Error;
with Interfaces.C.Strings;

package body SDL.Mixer.Chunks is
   -----------------
   -- Load_WAV_RW --
   -----------------

   procedure Load_WAV_RW (Source      : in out SDL.RWops.RWops;
                          Free_Source : in     Boolean;
                          Chunk       :    out Chunk_Type) is
      function Mix_Load_WAV_RW (Source   : in SDL.RWops.RWops;
                                Free_Src : in C.int) return Chunk_Type with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_LoadWAV_RW";

      Free_C  : constant C.int      := Boolean'Pos (Free_Source);
      Chunk_C : constant Chunk_Type := Mix_Load_WAV_RW (Source, Free_C);
   begin
      if Chunk_C = null then
         raise Mixer_Error with SDL.Error.Get;
      end if;
      Chunk := Chunk_C;
   end Load_WAV_RW;

   --------------
   -- Load_WAV --
   --------------

   procedure Load_WAV (Filename : in     String;
                       Chunk    :    out Chunk_Type) is
      use SDL.RWops;

      Ops : RWops.RWops := From_File (Filename, Read_Binary);
   begin
      Load_WAV_RW (Ops, True, Chunk);
   end Load_WAV;

   --------------------
   -- Quick_Load_WAV --
   --------------------

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

   --------------------
   -- Quick_Load_RAW --
   --------------------

   procedure Quick_Load_RAW (Mem   : in     System.Address;
                             Len   : in     Byte_Count;
                             Chunk :    out Chunk_Type) is
      use Interfaces;

      function Mix_Quick_Load_RAW (Mem : in System.Address;
                                   Len : in Unsigned_32) return Chunk_Type with
        Import => True,
        Convention => C,
        External_Name => "Mix_QuickLoad_RAW";

      Result : constant Chunk_Type := Mix_Quick_Load_RAW (Mem, Unsigned_32 (Len));
   begin
      if Result = null then
         raise Mixer_Error with SDL.Error.Get;
      end if;

      Chunk := Result;
   end Quick_Load_RAW;

   ----------
   -- Free --
   ----------

   procedure Free (Chunk : in out Chunk_Type) is
      procedure Mix_Free_Chunk (Chunk : in Chunk_Type) with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_FreeChunk";
   begin
      Mix_Free_Chunk (Chunk);
   end Free;

   ------------------------
   -- Number_Of_Decoders --
   ------------------------

   function Number_Of_Decoders return Natural is
      function Mix_Get_Num_Chunk_Decoders return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_GetNumChunkDecoders";
   begin
      return Natural (Mix_Get_Num_Chunk_Decoders);
   end Number_Of_Decoders;

   ------------------
   -- Decoder_Name --
   ------------------

   function Decoder_Name (Index : in Positive) return String is
      use Interfaces.C.Strings;

      function Mix_Get_Chunk_Decoder_Name (Index : in C.int)
                                          return chars_ptr with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_GetChunkDecoder";

      Index_C : constant C.int := C.int (Index - 1);
   begin
      return Value (Mix_Get_Chunk_Decoder_Name (Index_C));
   end Decoder_Name;

   -----------------
   -- Has_Decoder --
   -----------------

   function Has_Decoder (Name : in String) return Boolean is
      function Mix_Has_Chunk_Decoder (Name : in C.char_array) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_HasChunkDecoder";

      Result : constant C.int := Mix_Has_Chunk_Decoder (C.To_C (Name));
   begin
      return Result /= 0;
   end Has_Decoder;
end SDL.Mixer.Chunks;
