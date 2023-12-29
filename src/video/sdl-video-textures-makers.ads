--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
--  SDL.Video.Surfaces.Textures
--
--  Functions to create Texture objects.
--------------------------------------------------------------------------------------------------------------------
with SDL.Video.Surfaces;
with SDL.Video.Renderers;

package SDL.Video.Textures.Makers is
   pragma Preelaborate;

   procedure Create
     (Tex      : in out Texture;
      Renderer : in SDL.Video.Renderers.Renderer;
      Format   : in SDL.Video.Pixel_Formats.Pixel_Format_Names;
      Kind     : in Kinds;
      Size     : in SDL.Positive_Sizes);

   procedure Create
     (Tex      : in out Texture;
      Renderer : in SDL.Video.Renderers.Renderer;
      Surface  : in SDL.Video.Surfaces.Surface);
end SDL.Video.Textures.Makers;
