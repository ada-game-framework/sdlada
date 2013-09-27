--                              -*- Mode: Ada -*-
--  Filename        : sdl-video-palettes.ads
--  Description     : Palettes, colours and various conversions.
--  Author          : Luke A. Guest
--  Created On      : Tue Sep 24 20:17:57 2013
with Ada.Finalization;
with Ada.Iterator_Interfaces;
with Interfaces.C.Pointers;

package SDL.Video.Palettes is
   package C renames Interfaces.C;

   type Colour_Component is range 0 .. 255 with
     Size       => 8,
     Convention => C;

   type Colour is
      record
         Red   : Colour_Component;
         Green : Colour_Component;
         Blue  : Colour_Component;
         alpha : Colour_Component;
      end record with
        Convention => C,
        Size       => Colour_Component'Size * 4;

   for Colour use
      record
         Red   at 0 range  0 ..  7;
         Green at 0 range  8 .. 15;
         Blue  at 0 range 16 .. 23;
         Alpha at 0 range 24 .. 31;
      end record;

   Null_Colour : constant Colour := Colour'(others => Colour_Component'First);

   type Colour_Array is array (C.size_t range <>) of aliased Colour with
     Convention => C;

   package Colour_Array_Pointer is new Interfaces.C.Pointers
     (Index              => C.size_t,
      Element            => Colour,
      Element_Array      => Colour_Array,
      Default_Terminator => Null_Colour);

   --  The Palette is not a real container, but a wrapper to a C array pointer, we want to wrap this with a
   --  new "container" type and an iterator.

   --  TODO: Get this wrapper going at a later date.
   --  type Palette_Cursor is private;

   --  type Palette_Array is tagged limited private
   --    with
   --      Constant_Indexing => Element_Value,
   --      Default_Iterator  => Iterate,
   --      Iterator_Element  => Colour_Array_Pointer.Pointer;

   --  function Element_Value (Container : in Palette_Array; Pos : in Palette_Cursor) return Colour;

   --  function Has_Element (Pos : in Palette_Cursor) return Boolean with
   --    Inline => True;
--  private
   --  Internal use only.
   type Internal_Palette is
      record
         Total   : C.int;
         Colours : Colour_array_Pointer.Pointer;
         Version : Interfaces.Unsigned_32;
         Count   : C.int;
      end record with
        Convention => C;

   type Internal_Palette_Access is access Internal_Palette with
     Convention => C;

   --  type Palette_Iterators;

   --  type Palette_Array is new Ada.Finalization.Limited_Controlled with
   --     record
   --        Internal : Internal_Palette_Access;
   --     end record;

   --  function Iterate (Container : not null access Palette_Array) return Palette_Iterators'Class;

   --  type Palette_Access is access all Palette_Array;

   --  type Palette_Cursor is
   --     record
   --        --  Container : Palette_Access               := null;
   --        Index     : Positive                     := Positive'First;
   --        Total     : Positive                     := Positive'First;
   --        Current   : Colour_Array_Pointer.Pointer := null;
   --     end record;

   --  package Palette_Iterator_Interfaces is new Ada.Iterator_Interfaces
   --    (Cursor      => Palette_Cursor,
   --     Has_Element => Has_Element);

   --  type Palette_Iterators is new Palette_Iterator_Interfaces.Forward_Iterator with
   --     record
   --        Container : Palette_Access := null;
   --     end record;

   --  function First (Object : in Palette_Iterators) return Palette_Cursor;
   --  function Next (Object : in Palette_Iterators; Position : in Palette_Cursor) return Palette_Cursor;
end SDL.Video.Palettes;
