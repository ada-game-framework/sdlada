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
--  SDL.Mixer.Chunks
--------------------------------------------------------------------------------------------------------------------

with SDL.RWops;

package SDL.Mixer.Chunks is

   procedure Load_WAV_RW (Source      : in out SDL.RWops.RWops;
                          Free_Source : in     Boolean;
                          Chunk       :    out Chunk_Type);
   procedure Load_WAV (Filename : in     String;
                       Chunk    :    out Chunk_Type);
   --  Load a wave file or a music (.mod .s3m .it .xm) File

   procedure Quick_Load_WAV (Mem   : in     System.Address;
                             Chunk :    out Chunk_Type);
   --  Load a wave file of the mixer format from a memory buffer

   type Byte_Count is new Interfaces.Unsigned_32;
   procedure Quick_Load_RAW (Mem   : in     System.Address;
                             Len   : in     Byte_Count;
                             Chunk :    out Chunk_Type);
   --  Load raw audio data of the mixer format from a memory buffer

   procedure Free (Chunk : in out Chunk_Type);
   --  Free an audio chunk previously loaded

   function Number_Of_Decoders return Natural;
   function Decoder_Name (Index : in Positive) return String;
   function Has_Decoder (Name : in String) return Boolean;
   --  Get a list of chunk decoders that this build of SDL2_mixer provides.
   --  This list can change between builds AND runs of the program, if external
   --  libraries that add functionality become available.
   --  You must successfully call SDL.Mixer.Open calling these functions.
   --
   --  Usage:
   --  for Index in 1 .. Number_Of_Decoders loop
   --     Ada.Text_IO.Put_Line ("Supported chunk decoder: "
   --                           & Decoder_Name (Index));
   --  end loop;
   --
   --  Appearing in this list doesn't promise your specific audio file will
   --  decode...but it's handy to know if you have, say, a functioning Timidity
   --  install.

end SDL.Mixer.Chunks;
