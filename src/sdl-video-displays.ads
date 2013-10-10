--                              -*- Mode: Ada -*-
--  Filename        : sdl-video-displays.ads
--  Description     : Display modes.
--  Author          : Luke A. Guest
--  Created On      : Thu Oct 10 21:49:32 2013
with Interfaces.C;
with SDL.Video.Pixel_Formats;
with System;

package SDL.Video.Displays is
   package C renames Interfaces.C;

   type Modes is
      record
         Format       : SDL.Video.Pixel_Formats.Pixel_Format_Names;
         Width        : C.int;
         Height       : C.int;
         Refresh_Rate : C.int;
         Driver_Data  : System.Address;
      end record;

   --  SDL_GetClosestDisplayMode
   --  SDL_GetCurrentDisplayMode
   --  SDL_GetDesktopDisplayMode
   --  SDL_GetDisplayMode
   --  SDL_GetNumDisplayModes
   --  SDL_GetDisplayBounds
end SDL.Video.Displays;
