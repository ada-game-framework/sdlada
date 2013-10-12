--                              -*- Mode: Ada -*-
--  Filename        : sdl-video-rectangles.ads
--  Description     : Functions relating to rectangles.
--  Author          : Luke A. Guest
--  Created On      : Fri Oct 11 11:41:28 2013
with Interfaces.C;

package SDL.Video.Rectangles is
   package C renames Interfaces.C;

   type Rectangle is
      record
         X      : C.int;
         Y      : C.int;
         Width  : C.int;
         height : C.int;
      end record with
        Convention => C;

   type Rectangle_Arrays is array (C.size_t range <>) of aliased Rectangle with
     Convention => C;
end SDL.Video.Rectangles;
