--                              -*- Mode: Ada -*-
--  Filename        : sdl-video-textures.ads
--  Description     : Texture abstraction.
--  Author          : Luke A. Guest
--  Created On      : Sat Oct 12 21:34:12 2013
with Ada.Finalization;
with System;
with SDL.Video.Palettes;
with SDL.Video.Pixel_Formats;
with SDL.Video.Rectangles;
with SDL.Video.Renderers;
with SDL.Video.Surfaces;
with SDL.Video.Windows;

package SDL.Video.Textures is
   Texture_Error : exception;

   --  Was SDL_TextureAccess.
   type Kinds is (Static, Streaming, Target) with
     Convention => C;

   type Blend_Modes is range 0 .. 4 with
     Convention => C;

   Blend_None     : constant Blend_Modes := 16#0000_0000#;
   Blend_Alpha    : constant Blend_Modes := 16#0000_0001#;
   Blend_Additive : constant Blend_Modes := 16#0000_0002#;
   Blend_Modulate : constant Blend_Modes := 16#0000_0004#;

   --  Was SDL_TextureModulate

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

   function Get_Colour (Self : in Texture) return SDL.Video.Palettes.RGB_Colour;
   procedure Set_Colour (Self : in out Texture; Colour : in SDL.Video.Palettes.RGB_Colour);

   --  Lock returns access to pixel data as write-only.
   --  function Lock (Self : in out Texture; Pixel_Data : out SDL.Video.Pixels.Pixel) return Boolean with
   --  function Lock (Self : in out Texture; Area : in SDL.Video.Rectangles.Rectangle; Pixel_Data : out SDL.Video.Pixels.Pixel) return Boolean with
   --    Pre  => Self.Locked = False,
   --    Post => Result = True and then Self.Locked = True;
--  SDL_LockTexture
--  SDL_UnlockTexture
--  SDL_QueryTexture
--  SDL_UpdateTexture
private
   type Texture is new Ada.Finalization.Limited_Controlled with
      record
         Internal : System.Address := System.Null_Address;
         Locked   : Boolean        := False;
      end record;

   overriding
   procedure Finalize (Self : in out Texture);

   Null_Texture : constant Texture := (Ada.Finalization.Limited_Controlled with
                                         Internal => System.Null_Address,
                                         Locked   => False);
end SDL.Video.Textures;
