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

with SDL.Error;

with Interfaces.C.Strings;

package body SDL.Mixer.Chunks is


   procedure Load_WAV_RW (Source      : in out SDL.RWops.RWops;
                          Free_Source : in     Boolean;
                          Chunk       :    out Chunk_Type)
   is
      function Mix_Load_WAV_RW (Source   : in SDL.RWops.RWops;
                                Free_Src : in C.int) return Chunk_Type
        with
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


   procedure Load_WAV (Filename : in     String;
                       Chunk    :    out Chunk_Type)
   is
      use SDL.RWops;
      Ops : RWops.RWops := From_File (Filename, Read_Binary);
   begin
      Load_WAV_RW (Ops, True, Chunk);
   end Load_WAV;


   procedure Quick_Load_WAV (Mem   : in     System.Address;
                             Chunk :    out Chunk_Type)
   is
      function Mix_Quick_Load_WAV (Mem : in System.Address) return Chunk_Type
        with
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
                             Chunk :    out Chunk_Type)
   is
      use Interfaces;
      function Mix_Quick_Load_RAW (Mem : in System.Address;
                                   Len : in Unsigned_32) return Chunk_Type
        with
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


   procedure Free (Chunk : in out Chunk_Type)
   is
      procedure Mix_Free_Chunk (Chunk : in Chunk_Type)
        with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_FreeChunk";
   begin
      Mix_Free_Chunk (Chunk);
   end Free;


   function Number_Of_Decoders return Natural
   is
      function Mix_Get_Num_Chunk_Decoders return C.int
        with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_GetNumChunkDecoders";
   begin
      return Natural (Mix_Get_Num_Chunk_Decoders);
   end Number_Of_Decoders;


   function Decoder_Name (Index : in Positive) return String
   is
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


   function Has_Decoder (Name : in String) return Boolean
   is
      use Interfaces.C.Strings;
      function Mix_Has_Chunk_Decoder (Name : in chars_ptr) return C.int
        with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_HasChunkDecoder";
      Result : constant C.int := Mix_Has_Chunk_Decoder (New_String (Name));
   begin
      return Result /= 0;
   end Has_Decoder;


end SDL.Mixer.Chunks;
