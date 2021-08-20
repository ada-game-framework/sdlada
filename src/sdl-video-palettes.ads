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
--  SDL.Video.Palettes
--
--  Palettes, colours and various conversions.
--------------------------------------------------------------------------------------------------------------------

with Ada.Iterator_Interfaces;
with Interfaces.C.Pointers;

package SDL.Video.Palettes is
   pragma Preelaborate;

   package C renames Interfaces.C;

   type Colour_Component is range 0 .. 255 with
     Size       => 8,
     Convention => C;

   type Colour is
      record
         Red   : Colour_Component := Colour_Component'First;
         Green : Colour_Component := Colour_Component'First;
         Blue  : Colour_Component := Colour_Component'First;
         Alpha : Colour_Component := Colour_Component'First;
      end record with
     Convention => C_Pass_by_Copy,
     Size       => Colour_Component'Size * 4;

   Null_Colour : constant Colour := (others => <>);

   type RGB_Colour is
      record
         Red   : Colour_Component := Colour_Component'First;
         Green : Colour_Component := Colour_Component'First;
         Blue  : Colour_Component := Colour_Component'First;
      end record with
     Convention => C_Pass_by_Copy,
     Size       => Colour_Component'Size * 4;

   Null_RGB_Colour : constant RGB_Colour := (others => <>);

   --  Cursor type for our iterator.
   type Cursor is private;

   No_Element : constant Cursor;

   function Element (Position : in Cursor) return Colour;

   function Has_Element (Position : in Cursor) return Boolean with
     Inline;

   --  Create the iterator interface package.
   package Palette_Iterator_Interfaces is new
     Ada.Iterator_Interfaces (Cursor, Has_Element);

   type Palette is tagged limited private with
     Default_Iterator  => Iterate,
     Iterator_Element  => Colour,
     Constant_Indexing => Constant_Reference;

   type Palette_Access is access Palette;

   function Constant_Reference
     (Container : aliased Palette;
      Position  : Cursor) return Colour;

   function Iterate (Container : Palette)
                     return Palette_Iterator_Interfaces.Forward_Iterator'Class;

   function Create (Total_Colours : in Positive) return Palette;

   procedure Free (Container : in out Palette);

   Empty_Palette : constant Palette;
private

   type Colour_Array is array (C.size_t range <>) of aliased Colour with
     Convention => C;

   package Colour_Array_Pointer is new Interfaces.C.Pointers
     (Index              => C.size_t,
      Element            => Colour,
      Element_Array      => Colour_Array,
      Default_Terminator => (others => Colour_Component'First));

   type Internal_Palette is
      record
         Total     : C.int;
         Colours   : Colour_Array_Pointer.Pointer;
         Version   : Interfaces.Unsigned_32;
         Ref_Count : C.int;
      end record with
     Convention => C;

   type Internal_Palette_Access is access Internal_Palette with
     Convention => C;

   type Palette is tagged limited
      record
         Data : Internal_Palette_Access;
      end record;

   type Palette_Constant_Access is access constant Palette'Class;

   type Cursor is
      record
         Container : Palette_Constant_Access;
         Index     : Natural;
         Current   : Colour_Array_Pointer.Pointer;
      end record;

   No_Element : constant Cursor := Cursor'(Container => null,
                                           Index     => Natural'First,
                                           Current   => null);

   Empty_Palette : constant Palette := Palette'(Data => null);
end SDL.Video.Palettes;
