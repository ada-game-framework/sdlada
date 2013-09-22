--                              -*- Mode: Ada -*-
--  Filename        : sdl-error.ads
--  Description     : Error handling.
--  Author          : Luke A. Guest
--  Created On      : Sun Sep 22 01:54:54 2013
package SDL.Error is
   procedure Clear with
      Import        => True,
      Convention    => C,
      External_Name => "SDL_ClearError";

   procedure Set (S : in String);

   function Get return String;
end SDL.Error;
