--  Pong-Demo for SDLAda, paddle actions.
--  Copyright (C) 2012 - 2020, Vinzent "Jellix" Saranen

with Interfaces.C;

with SDL.Video.Palettes,
     SDL.Video.Rectangles;

package Pong.Paddles is

   use type Interfaces.C.int;

   type Paddle is new Pong.Display_Object with private;

   subtype Velocity is Float range -1.0 .. +1.0;

   ---------------------------------------------------------------------
   --  Create
   ---------------------------------------------------------------------
   function Create (Initial : in SDL.Video.Rectangles.Rectangle;
                    Bounds  : in SDL.Video.Rectangles.Rectangle;
                    Colour  : in SDL.Video.Palettes.Colour;
                    Speed   : in Float) return Paddle;

   ---------------------------------------------------------------------
   --  Set_Velocity
   ---------------------------------------------------------------------
   not overriding
   procedure Set_Velocity (This : in out Paddle;
                           Vel  : in     Velocity);

   ---------------------------------------------------------------------
   --  Move
   ---------------------------------------------------------------------
   overriding
   procedure Move (This    : in out Paddle;
                   Clipped :    out Boolean);

   ---------------------------------------------------------------------
   --  Draw
   ---------------------------------------------------------------------
   overriding
   procedure Draw (This     : in out Paddle;
                   Renderer : in out SDL.Video.Renderers.Renderer);

private

   type Paddle is new Pong.Display_Object with
      record
         Max_Speed : Float;
         Velocity  : Pong.Paddles.Velocity;
      end record;

end Pong.Paddles;
