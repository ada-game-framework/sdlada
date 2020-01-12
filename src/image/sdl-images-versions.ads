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
--  SDL.Images.Versions
--
--  Library version information.
--------------------------------------------------------------------------------------------------------------------
with SDL.Versions;

package SDL.Images.Versions is
   --  These allow the user to determine which version of SDLAda_Image they compiled with.
   Compiled_Major : constant SDL.Versions.Version_Level with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_Ada_Image_Major_Version";

   Compiled_Minor : constant SDL.Versions.Version_Level with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_Ada_Image_Minor_Version";

   Compiled_Patch : constant SDL.Versions.Version_Level with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_Ada_Image_Patch_Version";

   Compiled : constant SDL.Versions.Version := (Major => Compiled_Major,
                                                Minor => Compiled_Minor,
                                                Patch => Compiled_Patch);

   procedure Linked_With (Info : in out SDL.Versions.Version);
end SDL.Images.Versions;
