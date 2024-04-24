--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
with Interfaces.C;
with Interfaces.C.Strings;

package body SDL.Versions is
   package C renames Interfaces.C;


   function Revision return String is
      function SDL_Get_Revision return C.Strings.chars_ptr with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetRevision";
   begin
      return C.Strings.Value (SDL_Get_Revision);
   end Revision;


   function Revision return Revision_Level is
      function SDL_Get_Revision_Number return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetRevisionNumber";
   begin
      return Revision_Level (SDL_Get_Revision_Number);
   end Revision;


   procedure Linked_With (Info : in out Version) is
      procedure SDL_Get_Version (V : in out Version) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetVersion";
   begin
      SDL_Get_Version (Info);
   end Linked_With;
end SDL.Versions;
