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
with Ada.Unchecked_Conversion;
with Interfaces.C;
private with SDL.C_Pointers;
with SDL.Error;

package body SDL.Video.Textures.Makers is
   package C renames Interfaces.C;

   use type C.int;
   use type SDL.C_Pointers.Texture_Pointer;

   function Get_Internal_Surface (Self : in SDL.Video.Surfaces.Surface) return SDL.C_Pointers.Surface_Pointer with
     Import     => True,
     Convention => Ada;

   function Get_Internal_Renderer (Self : in SDL.Video.Renderers.Renderer) return SDL.C_Pointers.Renderer_Pointer with
     Import     => True,
     Convention => Ada;

   procedure Create
     (Tex      : in out Texture;
      Renderer : in SDL.Video.Renderers.Renderer;
      Format   : in SDL.Video.Pixel_Formats.Pixel_Format_Names;
      Kind     : in Kinds;
      Size     : in SDL.Video.Windows.Sizes) is

      --  Convert the Pixel_Format_Name to an Unsigned_32 because the compiler is changing the value somewhere along
      --  the lines from the start of this procedure to calling SDL_Create_Texture.
      function To_Unsigned32 is new Ada.Unchecked_Conversion (Source => SDL.Video.Pixel_Formats.Pixel_Format_Names,
                                                              Target => Interfaces.Unsigned_32);

      function SDL_Create_Texture
        (R      : in SDL.C_Pointers.Renderer_Pointer;
         Format : in Interfaces.Unsigned_32;
         Kind   : in Kinds;
         W, H   : in C.int) return SDL.C_Pointers.Texture_Pointer with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_CreateTexture";

   begin
      Tex.Internal := SDL_Create_Texture (Get_Internal_Renderer (Renderer),
                                          To_Unsigned32 (Format),
                                          Kind,
                                          C.int (Size.Width),
                                          C.int (Size.Height));

      if Tex.Internal = null then
         raise Texture_Error with SDL.Error.Get;
      end if;

      Tex.Size         := Size;
      Tex.Pixel_Format := Format;
   end Create;

   procedure Create
     (Tex      : in out Texture;
      Renderer : in SDL.Video.Renderers.Renderer;
      Surface  : in SDL.Video.Surfaces.Surface) is

      function SDL_Create_Texture_Form_Surface (R : in SDL.C_Pointers.Renderer_Pointer;
                                                S : in SDL.C_Pointers.Surface_Pointer)
                                                return SDL.C_Pointers.Texture_Pointer with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_CreateTextureFromSurface";
   begin
      Tex.Internal := SDL_Create_Texture_Form_Surface (Get_Internal_Renderer (Renderer),
                                                       Get_Internal_Surface (Surface));

      if Tex.Internal = null then
         raise Texture_Error with SDL.Error.Get;
      end if;
   end Create;
end SDL.Video.Textures.Makers;
