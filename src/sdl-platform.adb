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
