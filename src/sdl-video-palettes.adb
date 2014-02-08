--------------------------------------------------------------------------------------------------------------------
--  Copyright (c) 2014 Luke A. Guest
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
with Ada.Finalization; use Ada.Finalization;

package body SDL.Video.Palettes is
   --  function Element_Value (Container : in Palette_array; Pos : in Palette_Cursor) return Colour is
   --  begin
   --     return Pos.Current.all;
   --  end Element_Value;

   --  function Has_Element (Pos : in Palette_Cursor) return Boolean is
   --  begin
   --     --  if Pos.Index < Positive (Pos.Container.Internal.Total) then
   --     if Pos.Index < Pos.Total then
   --        return True;
   --     end if;

   --     return False;
   --  end Has_Element;

   --  function Iterate (Container : not null access Palette_Array) return Palette_Iterators'Class is
   --  begin
   --     return It : constant Palette_Iterators := Palette_Iterators'(Container => Palette_Access (Container)) do
   --        null;
   --     end return;
   --  end Iterate;

   --  function First (Object : in Palette_Iterators) return Palette_Cursor is
   --  begin
   --     return Palette_Cursor'
   --       (--  Container => Object.Internal,
   --        Index     => Positive'First,
   --        Total     => Positive (Object.Container.Internal.Total),
   --        Current   => Object.Container.Internal.Colours);
   --  end First;

   --  function Next (Object : in Palette_Iterators; Position : in Palette_Cursor) return Palette_Cursor is
   --     Curr : Colour_Array_Pointer.Pointer := Position.Current;
   --  begin
   --     Colour_Array_Pointer.Increment (Curr);

   --     return Palette_Cursor'
   --       (--  Container => Object.Internal,
   --        Index     => Position.Index + 1,
   --        Total     => Position.Total,
   --        Current   => Curr);
   --  end Next;

   type Iterator (Container : access constant Palette'Class) is new Limited_Controlled and
        Palette_Iterator_Interfaces.Forward_Iterator with
   record
      Index : Natural;
   end record;

   overriding
   function First (Object : Iterator) return Cursor;

   overriding
   function Next (Object : Iterator; Position : Cursor) return Cursor;

   function Element (Position : in Cursor) return Colour is
   begin
      --  return Position.Container.Data.Colours (Position.Index);
      return Colour_Array_Pointer.Value (Position.Current) (0);
   end Element;

   function Has_Element (Position : Cursor) return Boolean is
   begin
      if Position.Index <= Natural (Position.Container.Data.Total) then
         return True;
      end if;

      return False;
   end Has_Element;

   function Constant_Reference
     (Container : aliased Palette;
      Position  : Cursor) return Colour is
   begin
      --Put_Line ("Constant_Reference" & Natural'Image (Position.Index));

      --  return Position.Container.Data.Colours (Position.Index);
      return Colour_Array_Pointer.Value (Position.Current) (0);
   end Constant_Reference;

   function Iterate (Container : Palette) return
     Palette_Iterator_Interfaces.Forward_Iterator'Class is
   begin
--      Put_Line ("Iterate");

      return It : constant Iterator :=
        (Limited_Controlled with
           Container => Container'Access, Index => Natural'First + 1)
      do
        --Put_Line ("  index = " & Natural'Image(It.Index));
        null;
      end return;
   end Iterate;

   function Create (Total_Colours : in Positive) return Palette is
      function SDL_Alloc_Palette (Ncolors : in C.int) return Internal_Palette_Access with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_AllocPalette";
   begin
      return P : constant Palette :=
        (Data => SDL_Alloc_Palette (C.int (Total_Colours)))
      do
        null;
      end return;
   end Create;

   procedure Free (Container : in out Palette) is
      procedure SDL_Free_Palette (Self : in Internal_Palette_Access) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_FreePalette";
   begin
      SDL_Free_Palette (Container.Data);

      Container.Data := null;
   end Free;

   function First (Object : Iterator) return Cursor is
   begin
      --Put_Line ("First -> Index = " & Natural'Image (Object.Index));

      return Cursor'(Container => Object.Container,
                     Index     => Object.Index,
                     Current   => Object.Container.Data.Colours);
   end First;

   function Next (Object : Iterator; Position : Cursor) return Cursor is
      Next_Ptr : Colour_Array_Pointer.Pointer := Position.Current;
   begin
      Colour_Array_Pointer.Increment (Next_Ptr);

      --  Put_Line ("Next");

      --  if Object.Container /= Position.Container then
      --     raise Program_Error with "Wrong containers";
      --  end if;

      return Cursor'(Container => Object.Container,
                     Index     => Position.Index + 1,
                     Current   => Next_Ptr);
   end Next;
end SDL.Video.Palettes;
