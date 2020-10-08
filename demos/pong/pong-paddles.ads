--  Pong-Demo for SDLAda, paddle actions.
--  Copyright (C) 2012 - 2020, Vinzent "Jellix" Saranen

with SDL.Video;

package Pong.Paddles is

   type Paddle is new Pong.Display_Object with private;

   ---------------------------------------------------------------------
   --  Create
   ---------------------------------------------------------------------
   function Create (Surface : in SDL.Video.Surfaces.Surface;
                    Initial : in SDL.Video.Rectangles.Rectangle;
                    Bounds  : in SDL.Video.Rectangles.Rectangle;
                    Speed   : in Interfaces.C.unsigned_char) return Paddle;

   ---------------------------------------------------------------------
   --  Move
   ---------------------------------------------------------------------
   overriding
   procedure Move (This    : in out Paddle;
                   Clipped :    out Boolean;
                   Delta_X : in     Interfaces.C.int := 0;
                   Delta_Y : in     Interfaces.C.int);

   ---------------------------------------------------------------------
   --  Draw
   ---------------------------------------------------------------------
   overriding
   procedure Draw (This    : in out Paddle;
                   Surface : in out SDL.Video.Surfaces.Surface);

private

   type Paddle is new Pong.Display_Object with
      record
         Speed : Interfaces.C.unsigned_char;
         Black : Interfaces.Unsigned_32;
         White : Interfaces.Unsigned_32;
      end record;

end Pong.Paddles;
