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
--  SDL.Video.Surfaces.Makers
--
--  Functions to create surface objects.
--------------------------------------------------------------------------------------------------------------------
package SDL.Video.Surfaces.Makers is
   pragma Preelaborate;

   procedure Create (Self       : in out Surface;
                     Size       : in SDL.Sizes;
                     BPP        : in Pixel_Depths;
                     Red_Mask   : in Colour_Masks;
                     Blue_Mask  : in Colour_Masks;
                     Green_Mask : in Colour_Masks;
                     Alpha_Mask : in Colour_Masks);

   --  TODO: This is likely a temporary place for this. It's likely I will add a Streams package.
   --     procedure Create (Self : in out Surface; File_Name : in String);
private
   function Get_Internal_Surface (Self : in Surface) return Internal_Surface_Pointer with
     Export     => True,
     Convention => Ada;

   --  Create a surface from an internal pointer, this pointer will be owned by something else, so we don't delete it.
   function Make_Surface_From_Pointer (S : in Internal_Surface_Pointer; Owns : in Boolean := False) return Surface with
     Export     => True,
     Convention => Ada;

   --  TODO: SDL_ConvertSurface
   --  TODO: SDL_ConvertSurfaceFormat
   --  TODO: SDL_CreateRGBSurfaceFrom
end SDL.Video.Surfaces.Makers;
