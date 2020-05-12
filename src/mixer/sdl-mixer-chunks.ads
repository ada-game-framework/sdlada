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
--  SDL.Mixer.Chunks
--------------------------------------------------------------------------------------------------------------------

with SDL.RWops;

package SDL.Mixer.Chunks is
   pragma Preelaborate;

   procedure Load_WAV_RW (Source      : in out SDL.RWops.RWops;
                          Free_Source : in     Boolean;
                          Chunk       :    out Chunk_Type);

   procedure Load_WAV (Filename : in     String;
                       Chunk    :    out Chunk_Type);

   procedure Quick_Load_WAV (Mem   : in     System.Address;
                             Chunk :    out Chunk_Type);

   type Byte_Count is new Interfaces.Unsigned_32;
   procedure Quick_Load_RAW (Mem   : in     System.Address;
                             Len   : in     Byte_Count;
                             Chunk :    out Chunk_Type);

   procedure Free (Chunk : in out Chunk_Type);

   function Number_Of_Decoders return Natural;
   function Decoder_Name (Index : in Positive) return String;
   function Has_Decoder (Name : in String) return Boolean;

end SDL.Mixer.Chunks;
