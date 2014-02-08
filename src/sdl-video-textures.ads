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
--  SDL.Video.Textures
--
--  Texture abstraction.
--------------------------------------------------------------------------------------------------------------------
with Ada.Finalization;
with System;
with SDL.Video.Palettes;
with SDL.Video.Pixel_Formats;
with SDL.Video.Rectangles;
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

   --  Was SDL_TextureModulate - Possible doc bug.

   type Texture is new Ada.Finalization.Limited_Controlled with private;

   Null_Texture : constant Texture;

   procedure Create
     (Self     : in out Texture;
      Renderer : in SDL.Video.Renderers.Renderer;
      Format   : in SDL.Video.Pixel_Formats.Pixel_Format;
      Kind     : in Kinds;
      Size     : in SDL.Video.Windows.Sizes);

   procedure Create
     (Self     : in out Texture;
      Renderer : in SDL.Video.Renderers.Renderer;
      Surface  : in SDL.Video.Surfaces.Surface);

   procedure Destroy (Self : in out Texture);

   --  Get the alpha value to be multiplied (modulated) into render copy operations.
   function Get_Alpha (Self : in Texture) return SDL.Video.Palettes.Colour_Component;
   procedure Set_Alpha (Self : in out Texture; Alpha : in SDL.Video.Palettes.Colour_Component);

   function Get_Blend_Mode (Self : in Texture) return Blend_Modes;
   procedure Set_Blend_Mode (Self : in out Texture; Mode : in Blend_Modes);

   function Get_Modulate_Colour (Self : in Texture) return SDL.Video.Palettes.RGB_Colour;
   procedure Set_Modulate_Colour (Self : in out Texture; Colour : in SDL.Video.Palettes.RGB_Colour);

   --  Lock returns access to pixel data as write-only.
   --  function Lock (Self : in out Texture; Pixel_Data : out SDL.Video.Pixels.Pixel) return Boolean with
   --  function Lock (Self : in out Texture; Area : in SDL.Video.Rectangles.Rectangle; Pixel_Data : out SDL.Video.Pixels.Pixel) return Boolean with
   --    Pre  => Self.Locked = False,
   --    Post => Result = True and then Self.Locked = True;
--  SDL_LockTexture
--  SDL_UnlockTexture
--  SDL_QueryTexture
--  SDL_UpdateTexture
--  SDL_UpdateYUVTexture
private
   type Texture is new Ada.Finalization.Limited_Controlled with
      record
         Internal : System.Address := System.Null_Address;
         Locked   : Boolean        := False;
      end record;

   overriding
   procedure Finalize (Self : in out Texture);

   function Get_Address (Self : in Texture) return System.Address with
     Export     => True,
     Convention => Ada;

   Null_Texture : constant Texture := (Ada.Finalization.Limited_Controlled with
                                         Internal => System.Null_Address,
                                         Locked   => False);
end SDL.Video.Textures;
