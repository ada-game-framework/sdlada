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

      C_Str : constant C.Strings.chars_ptr := SDL_Get_Revision;
   begin
      return C.Strings.Value (C_Str);
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
      procedure SDL_Get_Version (V : access Version) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetVersion";

      Data : aliased Version;
   begin
      SDL_Get_Version (Data'Access);

      Info := Data;
   end Linked_With;
end SDL.Versions;
