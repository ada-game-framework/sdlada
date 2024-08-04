--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
package body SDL.Video.Windows.Manager is
   function Get_WM_Info (Win : in Window; Info : out WM_Info) return Boolean is
      function SDL_Get_Window_WM_Info (W : in SDL.C_Pointers.Windows_Pointer; Info : out WM_Info) return SDL_Bool with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetWindowWMInfo";
   begin
      return To_Boolean (SDL_Get_Window_WM_Info (Win.Internal, Info));
   end Get_WM_Info;
end SDL.Video.Windows.Manager;
