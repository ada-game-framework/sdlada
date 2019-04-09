--------------------------------------------------------------------------------------------------------------------
--  Copyright (c) 2013-2020, Luke A. Guest
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
--  SDL.RWops.Streams
--
--  Ada stream interface to SDL's Read/Write operations.
--------------------------------------------------------------------------------------------------------------------
with Ada.Streams;

package SDL.RWops.Streams is
   pragma Preelaborate;

   type RWops_Stream is new Ada.Streams.Root_Stream_Type with private;

   function Open (Op : in RWops) return RWops_Stream;
   procedure Open (Op : in RWops; Stream : out RWops_Stream);

   procedure Close (Stream : in RWops_Stream);

   overriding
   procedure Read (Stream : in out RWops_Stream;
                   Item   : out Ada.Streams.Stream_Element_Array;
                   Last   : out Ada.Streams.Stream_Element_Offset);

   overriding
   procedure Write (Stream : in out RWops_Stream; Item : Ada.Streams.Stream_Element_Array);
private
   type RWops_Stream is new Ada.Streams.Root_Stream_Type with
      record
         Context : RWops;
      end record;
end SDL.RWops.Streams;
