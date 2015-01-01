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
with Interfaces.C;
with Interfaces.C.Strings;
with System;

package body SDL.Versions is
   package C renames Interfaces.C;

   function Revision return String is
      function SDL_Get_Revision return C.Strings.chars_ptr with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetRevision";

      C_Str : C.Strings.chars_ptr := SDL_Get_Revision;
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
