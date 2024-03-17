--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
--  SDL.Versions
--
--  Library version information.
--
--  **IMPORTANT** DO NOT REMOVE THESE FUNCTIONS, they are very important. They allow the running application to see
--                what version of the libraries are installed on the system. This allows the running application
--                to modify it's behaviour if certain functionality from a certain version is missing.
--                i.e. Steam controls it's libs and whilst they are usually up to date, there is no guarantee that
--                the version you built against has the same functionality as that installed on the user's machine.
--------------------------------------------------------------------------------------------------------------------
package SDL.Versions is
   pragma Preelaborate;

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

   function Compiled return Version is
     (Major => Compiled_Major,
      Minor => Compiled_Minor,
      Patch => Compiled_Patch)
   with Inline => True;

   function Revision return String with
     Inline => True;

   function Revision return Revision_Level;

   procedure Linked_With (Info : in out Version);
end SDL.Versions;
