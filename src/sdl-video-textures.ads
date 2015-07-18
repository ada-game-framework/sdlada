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
--  SDL.Video.Textures
--
--  Texture abstraction.
--------------------------------------------------------------------------------------------------------------------
with Ada.Finalization;
with System;
private with SDL.C_Pointers;
with SDL.Video.Palettes;
with SDL.Video.Pixel_Formats;
with SDL.Video.Pixels;
--  with SDL.Video.Rectangles;
limited with SDL.Video.Renderers;
with SDL.Video.Surfaces;
with SDL.Video.Windows;

package SDL.Video.Textures is
   Texture_Error : exception;

   --  Was SDL_TextureAccess.
   type Kinds is (Static, Streaming, Target) with
     Convention => C;

   type Blend_Modes is (None, Alpha_Blend, Additive, Colour_Modulate) with
     Convention => C;

   for Blend_Modes use
     (None            => 16#0000_0000#,
      Alpha_Blend     => 16#0000_0001#,
      Additive        => 16#0000_0002#,
      Colour_Modulate => 16#0000_0004#);

   type Texture is new Ada.Finalization.Limited_Controlled with private;

   Null_Texture : constant Texture;

   procedure Destroy (Self : in out Texture);

   --  Get the alpha value to be multiplied (modulated) into render copy operations.
   function Get_Alpha (Self : in Texture) return SDL.Video.Palettes.Colour_Component;
   procedure Set_Alpha (Self : in out Texture; Alpha : in SDL.Video.Palettes.Colour_Component);

   function Get_Blend_Mode (Self : in Texture) return Blend_Modes;
   procedure Set_Blend_Mode (Self : in out Texture; Mode : in Blend_Modes);

   function Get_Modulate_Colour (Self : in Texture) return SDL.Video.Palettes.RGB_Colour;
   procedure Set_Modulate_Colour (Self : in out Texture; Colour : in SDL.Video.Palettes.RGB_Colour);

   --  TODO: Fix this.
   --  Lock returns access to pixel data as write-only.
   --  function Lock (Self : in out Texture; Pixel_Data : out SDL.Video.Pixels.Pixel) return Boolean with
   --  function Lock (Self : in out Texture; Area : in SDL.Video.Rectangles.Rectangle;
   --    Pixel_Data : out SDL.Video.Pixels.Pixel) return Boolean with
   --    Pre  => Self.Locked = False,
   --    Post => Result = True and then Self.Locked = True;
   --
   --  Lock should return an object representing the bitmap data. We should be able to access it like an array,
   --  e.g. (x, y) and also using an iterator, which traverses, top -> bottom, left -> right, 1 pixel at a time. We
   --  need to be able to do subimage copies using Ada's slices, e.g. bm1 (x .. x2, y .. y2) := bm2 (x .. x2, y .. y2)
   --
   --  For YV12 format:
   --

   --  package ARGB_8888_Array is new SDL.Video.Pixels.Texture_Data (Width => , Height => , Element => );
   --     procedure Lock_Texture (Self   : in out Texture;
   --                             Pixels : out SDL.Video.Pixels.Pixel_ARGB_8888_Array_Access);
   procedure Lock (Self    : in out Texture;
                   Pixels  : out SDL.Video.Pixels.ARGB_8888_Access.Pointer;
                   Pitches : out SDL.Video.Pixels.Pitch_Access.Pointer);
   procedure Unlock (Self : in out Texture);

   --  SDL_QueryTexture
   --  SDL_UpdateTexture
   --  SDL_UpdateYUVTexture
private
   type Texture is new Ada.Finalization.Limited_Controlled with
      record
         --  TODO: Change System.Address to a C convention access type as Address is a different size compared to
         --        a C pointer.
         Internal     : SDL.C_Pointers.Texture_Pointer             := null;
         Owns         : Boolean                                    := True;
         Locked       : Boolean                                    := False;
         Size         : SDL.Video.Windows.Sizes                    := (Positive'First, Positive'First);
         Pixel_Format : SDL.Video.Pixel_Formats.Pixel_Format_Names := SDL.Video.Pixel_Formats.Pixel_Format_Unknown;
      end record;

   overriding
   procedure Finalize (Self : in out Texture);

   function Get_Internal_Texture (Self : in Texture) return SDL.C_Pointers.Texture_Pointer with
     Export     => True,
     Convention => Ada;

   Null_Texture : constant Texture := (Ada.Finalization.Limited_Controlled with
                                       Internal     => null,
                                       Owns         => True,
                                       Size         => (Positive'First, Positive'First),
                                       Pixel_Format => Pixel_Formats.Pixel_Format_Unknown,
                                       Locked       => False);
end SDL.Video.Textures;
