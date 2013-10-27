--                              -*- Mode: Ada -*-
--  Filename        : sdl-video-pixels.ads
--  Description     : Access to pixel data.
--  Author          : Luke A. Guest
--  Created On      : Fri Oct 18 17:56:05 2013
with Ada.Finalization;
with Interfaces.C.Pointers;

package SDL.Video.Pixels is
   --  Define pixel data access. Each pixel can be of any pixel format type.
   --  A bitmap returned, say from Textures.Lock is an array of pixels.

   --  This package wraps a C pointer to pixel data with an access mechanism.
   type Kinds is (Read, Write, Any) with
     Convention => C;

   type Pixel is new Ada.Finalization.Limited_Controlled with private;
private
   type Pixel is new Ada.Finalization.Limited_Controlled with
      record
--         Data          : Interfaces.C.Pointers.Pointer;
         Access_Method : Kinds;
      end record;
end SDL.Video.Pixels;
