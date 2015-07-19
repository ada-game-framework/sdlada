--------------------------------------------------------------------------------------------------------------------
--  Copyright (c) 2014-2015 Luke A. Guest
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
--  SDL.Video.Pixels
--
--  Access to pixel data.
--------------------------------------------------------------------------------------------------------------------
with Ada.Finalization;
with Interfaces;
with Interfaces.C;
with Interfaces.C.Pointers;
with SDL.Video.Windows;
with SDL.Video.Pixel_Formats;
with SDL.Video.Palettes;

package SDL.Video.Pixels is
   package C renames Interfaces.C;

   --  Define pixel data access. Each pixel can be of any pixel format type.
   --  A bitmap returned, say from Textures.Lock is an array of pixels.
   Pixels_Error : exception;

   --  These give access to the pitch data returned by locking a texture.
   type Pitch is new C.int with
     Size       => 32,
     Convention => C;

   type Pitch_Ptr is access all Pitch with
     Convention => C;

   type Pitch_Array is array (Positive range <>) of aliased Pitch;

   package Pitch_Access is new Interfaces.C.Pointers (Index              => Positive,
                                                      Element            => Pitch,
                                                      Element_Array      => Pitch_Array,
                                                      Default_Terminator => 0);

   --  ARGB8888 pixels.
   --  These give access to a texture's/surface's (TODO??) pixel data in the above format.
   type ARGB_8888 is
      record
         Alpha : SDL.Video.Palettes.Colour_Component;
         Red   : SDL.Video.Palettes.Colour_Component;
         Green : SDL.Video.Palettes.Colour_Component;
         Blue  : SDL.Video.Palettes.Colour_Component;
      end record with
     Size       => 32,
     Convention => C;

   for ARGB_8888 use
      record
         Blue  at 0 range  0 ..  7;
         Green at 0 range  8 .. 15;
         Red   at 0 range 16 .. 23;
         Alpha at 0 range 24 .. 31;
      end record;

   type ARGB_8888_Array is array (Positive range <>) of aliased ARGB_8888;

   package ARGB_8888_Access is new Interfaces.C.Pointers
     (Index              => Positive,
      Element            => ARGB_8888,
      Element_Array      => ARGB_8888_Array,
      Default_Terminator => ARGB_8888'(others => SDL.Video.Palettes.Colour_Component'First));

   generic
      type Index is (<>);
      type Element is private;
      type Element_Array_1D is array (Index range <>) of aliased Element;
      type Element_Array_2D is array (Index range <>, Index range <>) of aliased Element;

      Default_Terminator : Element;
   package Texture_Data is
      package Texture_Data_1D is new Interfaces.C.Pointers (Index              => Index,
                                                            Element            => Element,
                                                            Element_Array      => Element_Array_1D,
                                                            Default_Terminator => Default_Terminator);

      subtype Pointer is Texture_Data_1D.Pointer;
   end Texture_Data;
end SDL.Video.Pixels;
