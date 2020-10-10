--  Pong-Demo for SDLAda, ball actions.
--  Copyright (C) 2012 - 2020, Vinzent "Jellix" Saranen

with SDL.Video.Palettes,
     SDL.Video.Rectangles;

package Pong.Balls is

   type Ball is new Display_Object with private;

   ---------------------------------------------------------------------
   --  Create
   --
   --  Initializes and returns the display object.
   --  Initial is the initial position of this object on the screen.
   --  Bounds is the area within which the object can move.
   --  Colour is the display colour of the ball.
   --  Speed is the speed of the ball.
   --
   --  The initial movement vector of the ball is slightly randomized,
   --  but generally in the -1/-1 direction for both X and Y (left/down).
   ---------------------------------------------------------------------
   function Create (Initial : in SDL.Video.Rectangles.Rectangle;
                    Bounds  : in SDL.Video.Rectangles.Rectangle;
                    Colour  : in SDL.Video.Palettes.Colour;
                    Speed   : in Float) return Ball;

   ---------------------------------------------------------------------
   --  Draw
   ---------------------------------------------------------------------
   overriding
   procedure Draw (This     : in out Ball;
                   Renderer : in out SDL.Video.Renderers.Renderer);

   ---------------------------------------------------------------------
   --  Move
   ---------------------------------------------------------------------
   overriding
   procedure Move (This    : in out Ball;
                   Clipped :    out Boolean);

   ---------------------------------------------------------------------
   --  Warp
   ---------------------------------------------------------------------
   procedure Warp (This        : in out Ball;
                   To_Position : in     SDL.Coordinates);

   ---------------------------------------------------------------------
   --  Collides
   ---------------------------------------------------------------------
   function Collides (This : in Ball;
                      That : in Display_Object'Class) return Boolean;

   ---------------------------------------------------------------------
   --  Change_Dir
   ---------------------------------------------------------------------
   procedure Change_Dir (This : in out Ball;
                         X    : in     Boolean;
                         Y    : in     Boolean);

private

   type Ball is new Display_Object with
      record
         Speed     : Float;
         Direction : Smooth_Coordinates; --  Actual moving vector.
      end record;

end Pong.Balls;
