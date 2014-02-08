--------------------------------------------------------------------------------------------------------------------
--  Copyright (c) 2014 Luke A. Guest
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

package body SDL.Error is
   package C renames Interfaces.C;

   procedure Set (S : in String) is
      procedure SDL_Set_Error (C_Str : in C.char_array) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_SetError";
   begin
      SDL_Set_Error (C.To_C (S));
   end Set;

   function Get return String is
      function SDL_Get_Error return C.Strings.chars_ptr with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetError";

      C_Str : C.Strings.chars_ptr := SDL_Get_Error;
   begin
      return C.Strings.Value (C_Str);
   end Get;
end SDL.Error;
