--------------------------------------------------------------------------------------------------------------------
--  Copyright (c) 2013-2020, Luke A. Guest
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
--  SDL.Images.IO
--
--  Various loading routines.
--------------------------------------------------------------------------------------------------------------------
with SDL.Video.Renderers;
with SDL.Video.Surfaces;
with SDL.Video.Textures;
with SDL.RWops;

package SDL.Images.IO is
   --  TODO: I don't like the idea of leaving the freeing of the Source to the programmer, this is error prone and will
   --        cause leaks! I would prefer the RWops object be controlled.
   procedure Create (Surface : in out Video.Surfaces.Surface; File_Name : in String);

   procedure Create (Surface     : in out Video.Surfaces.Surface;
                     Source      : in RWops.RWops;
                     Format      : in Formats;
                     Free_Source : in Boolean := True);

   procedure Create (Texture   : in out Video.Textures.Texture;
                     Renderer  : in Video.Renderers.Renderer;
                     File_Name : in String);

   procedure Create (Texture     : in out Video.Textures.Texture;
                     Renderer    : in Video.Renderers.Renderer;
                     Source      : in RWops.RWops;
                     Free_Source : in Boolean := True);

   procedure Write_PNG (Surface : in out Video.Surfaces.Surface; File_Name : in String);
end SDL.Images.IO;
