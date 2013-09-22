--                              -*- Mode: Ada -*-
--  Filename        : sdl-platform.ads
--  Description     : Determine which platform we are running on.
--  Author          : Luke A. Guest
--  Created On      : Sun Sep 22 09:46:33 2013
package SDL.Platform is
   Platform_Error : exception;

   type Platforms is (Windows, Mac_OS_X, Linux, iOS, Android);

   function Get return Platforms;
end SDL.Platform;
