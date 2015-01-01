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
--  SDL.Versions
--
--  Library version information.
--------------------------------------------------------------------------------------------------------------------
package SDL.Versions is
   type Version_Level is mod 2 ** 8 with
     Size       => 8,
     Convention => C;

   --  TODO: Check this against the library, as they use an int.
   type Revision_Level is mod 2 ** 32;

   type Version is
      record
         Major : Version_Level;
         Minor : Version_Level;
         Patch : Version_Level;
      end record with
        Convention => C;

   --  These allow the user to determine which version of SDLAda they compiled with.
   Compiled_Major : constant Version_Level with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_Ada_Major_Version";

   Compiled_Minor : constant Version_Level with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_Ada_Minor_Version";

   Compiled_Patch : constant Version_Level with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_Ada_Patch_Version";

   Compiled : constant Version := (Major => Compiled_Major, Minor => Compiled_Minor, Patch => Compiled_Patch);

   function Revision return String with
     Inline => True;

   function Revision return Revision_Level;

   procedure Linked_With (Info : in out Version);
end SDL.Versions;
