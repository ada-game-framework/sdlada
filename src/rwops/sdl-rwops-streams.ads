--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
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
