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
with Interfaces.C;
with SDL.Error;

package body SDL.Video.Textures is
   package C renames Interfaces.C;

   use type C.int;
   use type System.Address;

   --  We need to get the actual adresses of these objects in this package.
   function Get_Address (Self : in SDL.Video.Renderers.Renderer) return System.Address with
     Import     => True,
     Convention => Ada;

   function Get_Address (Self : in SDL.Video.Surfaces.Surface) return System.Address with
     Import     => True,
     Convention => Ada;

   procedure Create
     (Self     : in out Texture;
      Renderer : in SDL.Video.Renderers.Renderer;
      Format   : in SDL.Video.Pixel_Formats.Pixel_Format;
      Kind     : in Kinds;
      Size     : in SDL.Video.Windows.Sizes) is

      function SDL_Create_Texture
        (R      : in System.Address;
         Format : in SDL.Video.Pixel_Formats.Pixel_Format;
         Kind   : in Kinds;
         W, H   : in C.int) return System.Address with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_CreateTexture";
   begin
      Self.Internal := SDL_Create_Texture (Get_Address (Renderer), Format, Kind, C.int (Size.Width), C.int (Size.Height));

      if Self.Internal = System.Null_Address then
         raise Texture_Error with SDL.Error.Get;
      end if;
   end Create;

   procedure Create
     (Self     : in out Texture;
      Renderer : in SDL.Video.Renderers.Renderer;
      Surface  : in SDL.Video.Surfaces.Surface) is

      function SDL_Create_Texture_Form_Surface (R, S : in System.Address) return System.Address with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_CreateTextureFromSurface";
   begin
      Self.Internal := SDL_Create_Texture_Form_Surface (Get_Address (Renderer), Get_Address (Surface));

      if Self.Internal = System.Null_Address then
         raise Texture_Error with SDL.Error.Get;
      end if;
   end Create;

   procedure Destroy (Self : in out Texture) is
      procedure SDL_Destroy_Texture (T : in System.Address) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_DestroyTexture";
   begin
      SDL_Destroy_Texture (Self.Internal);

      Self.Internal := System.Null_Address;
   end Destroy;

   function Get_Alpha (Self : in Texture) return SDL.Video.Palettes.Colour_Component is
      function SDL_Get_Texture_Alpha_Mod (T     : in System.Address;
                                          Alpha : out SDL.Video.Palettes.Colour_Component) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetTextureAlphaMod";

      Data   : SDL.Video.Palettes.Colour_Component;
      Result : C.int := SDL_Get_Texture_Alpha_Mod (Self.Internal, Data);
   begin
      if Result /= Success then
         raise Texture_Error with SDL.Error.Get;
      end if;

      return Data;
   end Get_Alpha;

   procedure Set_Alpha (Self : in out Texture; Alpha : in SDL.Video.Palettes.Colour_Component) is
      function SDL_Set_Texture_Alpha_Mod (T     : in System.Address;
                                          Alpha : in SDL.Video.Palettes.Colour_Component) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_SetTextureAlphaMod";

      Result : C.int := SDL_Set_Texture_Alpha_Mod (Self.Internal, Alpha);
   begin
      if Result /= Success then
         raise Texture_Error with SDL.Error.Get;
      end if;
   end Set_Alpha;

   function Get_Blend_Mode (Self : in Texture) return Blend_Modes is
      function SDL_Get_Texture_Blend_Mode (T     : in System.Address;
                                           Blend : out Blend_Modes) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetTextureBlendMode";

      Data   : Blend_Modes;
      Result : C.int := SDL_Get_Texture_Blend_Mode (Self.Internal, Data);
   begin
      if Result /= Success then
         raise Texture_Error with SDL.Error.Get;
      end if;

      return Data;
   end Get_Blend_Mode;

   procedure Set_Blend_Mode (Self : in out Texture; Mode : in Blend_Modes) is
      function SDL_Set_Texture_Blend_Mode (T    : in System.Address;
                                           Mode : in Blend_Modes) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_SetTextureBlendMode";

      Result : C.int := SDL_Set_Texture_Blend_Mode (Self.Internal, Mode);
   begin
      if Result /= Success then
         raise Texture_Error with SDL.Error.Get;
      end if;
   end Set_Blend_Mode;

   function Get_Modulate_Colour (Self : in Texture) return SDL.Video.Palettes.RGB_Colour is
      function SDL_Get_Texture_Color_Mod (T       : in System.Address;
                                          R, G, B : out SDL.Video.Palettes.Colour_Component) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetTextureColorMod";

      Data   : SDL.Video.Palettes.RGB_Colour;
      Result : C.int := SDL_Get_Texture_Color_Mod (Self.Internal, Data.Red, Data.Green, Data.Blue);
   begin
      if Result /= Success then
         raise Texture_Error with SDL.Error.Get;
      end if;

      return Data;
   end Get_Modulate_Colour;

   procedure Set_Modulate_Colour (Self : in out Texture; Colour : in SDL.Video.Palettes.RGB_Colour) is
      function SDL_Set_Texture_Color_Mod (T       : in System.Address;
                                          R, G, B : in SDL.Video.Palettes.Colour_Component) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_SetTextureColorMod";

      Result : C.int := SDL_Set_Texture_Color_Mod (Self.Internal, Colour.Red, Colour.Green, Colour.Blue);
   begin
      if Result /= Success then
         raise Texture_Error with SDL.Error.Get;
      end if;
   end Set_Modulate_Colour;

   procedure Finalize (Self : in out Texture) is
   begin
      if Self.Internal /= System.Null_Address then
         Destroy (Self);
      end if;
   end Finalize;

   function Get_Address (Self : in Texture) return System.Address is
   begin
      return Self.Internal;
   end Get_Address;
end SDL.Video.Textures;
