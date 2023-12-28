--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
--  SDL.Video.Renderers.Makers
--
--  Constructor subprograms for Renderers.
--------------------------------------------------------------------------------------------------------------------
with SDL.Video.Surfaces;

package SDL.Video.Renderers.Makers is
   pragma Preelaborate;

   procedure Create
     (Rend   : in out Renderer;
      Window : in out SDL.Video.Windows.Window;
      Driver : in Positive;
      Flags  : in Renderer_Flags := Default_Renderer_Flags);

   --  Specifically create a renderer using the first available driver.
   procedure Create
     (Rend   : in out Renderer;
      Window : in out SDL.Video.Windows.Window;
      Flags  : in Renderer_Flags := Default_Renderer_Flags);

   --  Create a software renderer using a surface.
   procedure Create
     (Rend    : in out Renderer;
      Surface : in SDL.Video.Surfaces.Surface);

   --  SDL_CreateWindowAndRenderer
end SDL.Video.Renderers.Makers;
