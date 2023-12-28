--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
--  SDL.TTFs.Versions
--
--  Library version information.
--------------------------------------------------------------------------------------------------------------------
with SDL.Versions;

package SDL.TTFs.Versions is
   pragma Elaborate_Body;

   --  These allow the user to determine which version of SDLAda_TTF they compiled with.
   Compiled_Major : constant SDL.Versions.Version_Level with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_Ada_TTF_Major_Version";

   Compiled_Minor : constant SDL.Versions.Version_Level with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_Ada_TTF_Minor_Version";

   Compiled_Patch : constant SDL.Versions.Version_Level with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_Ada_TTF_Patch_Version";

   Compiled : constant SDL.Versions.Version := (Major => Compiled_Major,
                                                Minor => Compiled_Minor,
                                                Patch => Compiled_Patch);

   procedure Linked_With (Info : in out SDL.Versions.Version);
end SDL.TTFs.Versions;
