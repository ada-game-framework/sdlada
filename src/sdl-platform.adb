with Interfaces.C;
with Interfaces.C.Strings;

package body SDL.Platform is
   function SDL_GetPlatform return C.Strings.chars_Ptr with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_GetPlatform";

   function Get return Platforms is
      C_Str : constant C.Strings.chars_Ptr := SDL_GetPlatform;
   begin
      if C_Str = "Windows" then
         return Windows;
      elsif C_Str = "Mac OS X" then
         return Mac_OS_X;
      elsif C_Str = "Linux" then
         return Linux;
      elsif C_Str = "iOS" then
         return iOS;
      elsif C_Str = "Android" then
         return Android;
      else
         raise Platform_Error with "Unknown SDL platform";
      end if;
   end Get;
end SDL.Platform;
