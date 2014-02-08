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

package body SDL.Platform is
   package C renames Interfaces.C;

   use type C.Strings.chars_ptr;

   function Get return Platforms is
      function SDL_Get_Platform return C.Strings.chars_Ptr with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetPlatform";

      C_Str : constant C.Strings.chars_Ptr := SDL_Get_Platform;
   begin
      if C.Strings.Value (C_Str) = "Windows" then
         return Windows;
      elsif C.Strings.Value (C_Str) = "Mac OS X" then
         return Mac_OS_X;
      elsif C.Strings.Value (C_Str) = "Linux" then
         return Linux;
      elsif C.Strings.Value (C_Str) = "iOS" then
         return iOS;
      elsif C.Strings.Value (C_Str) = "Android" then
         return Android;
      else
         raise Platform_Error with "Unknown SDL platform";
      end if;
   end Get;
end SDL.Platform;
