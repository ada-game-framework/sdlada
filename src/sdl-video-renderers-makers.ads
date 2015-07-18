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
--  SDL.Video.Renderers.Makers
--
--  Constructor subprograms for Renderers.
--------------------------------------------------------------------------------------------------------------------
package SDL.Video.Renderers.Makers is
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
