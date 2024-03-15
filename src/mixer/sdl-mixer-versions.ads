--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
--  SDL.Mixer.Versions
--
--  Library version information.
--------------------------------------------------------------------------------------------------------------------
with SDL.Versions;

package SDL.Mixer.Versions is
   --  These allow the user to determine which version of SDLAda_Mixer they compiled with.
   Compiled_Major : constant SDL.Versions.Version_Level with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_Ada_Mixer_Major_Version";

   Compiled_Minor : constant SDL.Versions.Version_Level with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_Ada_Mixer_Minor_Version";

   Compiled_Patch : constant SDL.Versions.Version_Level with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_Ada_Mixer_Patch_Version";

   Compiled : constant SDL.Versions.Version := (Major => Compiled_Major,
                                                Minor => Compiled_Minor,
                                                Patch => Compiled_Patch);

   procedure Linked_With (Info : in out SDL.Versions.Version) with
     Inline;
end SDL.Mixer.Versions;
