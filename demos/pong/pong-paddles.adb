--  Pong-Demo for SDLAda, paddle actions.
--  Copyright (C) 2012 - 2020, Vinzent "Jellix" Saranen

package body Pong.Paddles is

   ---------------------------------------------------------------------
   --  Create
   ---------------------------------------------------------------------
   function Create (Initial : in SDL.Video.Rectangles.Rectangle;
                    Bounds  : in SDL.Video.Rectangles.Rectangle;
                    Colour  : in SDL.Video.Palettes.Colour;
                    Speed   : in Float) return Paddle is
   begin
      return Paddle'(Old_Pos   => Smooth_Coordinates'(X => Float (Initial.X),
                                                      Y => Float (Initial.Y)),
                     New_Pos   => Smooth_Coordinates'(X => Float (Initial.X),
                                                      Y => Float (Initial.Y)),
                     Size      => SDL.Sizes'(Width  => Initial.Width,
                                             Height => Initial.Height),
                     Bounds    =>
                       Smooth_Bounds'(Min => (X => Float (Bounds.X),
                                              Y => Float (Bounds.Y)),
                                      Max => (X => Float (Bounds.X + Bounds.Width - Initial.Width),
                                              Y => Float (Bounds.Y + Bounds.Height - Initial.Height))),
                     Colour    => Colour,
                     Max_Speed => Speed,
                     Velocity  => 0.0);
   end Create;

   ---------------------------------------------------------------------
   --  Set_Velocity
   ---------------------------------------------------------------------
   not overriding
   procedure Set_Velocity (This : in out Paddle;
                           Vel  : in     Velocity) is
   begin
      This.Velocity := Vel;
   end Set_Velocity;

   ---------------------------------------------------------------------
   --  Move
   ---------------------------------------------------------------------
   overriding
   procedure Move (This    : in out Paddle;
                   Clipped :    out Boolean) is
   begin
      This.New_Pos.Y := This.Old_Pos.Y + This.Velocity * This.Max_Speed;

      if This.New_Pos.Y > This.Bounds.Max.Y then
         Clipped := True;
         This.New_Pos.Y := This.Bounds.Max.Y;
      elsif This.New_Pos.Y < This.Bounds.Min.Y then
         Clipped := True;
         This.New_Pos.Y := This.Bounds.Min.Y;
      else
         Clipped := False;
      end if;
   end Move;

   ---------------------------------------------------------------------
   --  Draw
   ---------------------------------------------------------------------
   overriding
   procedure Draw (This     : in out Paddle;
                   Renderer : in out SDL.Video.Renderers.Renderer)
   is
      Draw_At  : constant SDL.Video.Rectangles.Rectangle :=
        SDL.Video.Rectangles.Rectangle'(X      => SDL.Dimension (This.New_Pos.X),
                                        Y      => SDL.Dimension (This.New_Pos.Y),
                                        Width  => This.Size.Width,
                                        Height => This.Size.Height);
   begin
      Renderer.Set_Draw_Colour (Colour => This.Colour);
      Renderer.Fill (Rectangle => Draw_At);

      This.Old_Pos := This.New_Pos;
   end Draw;

end Pong.Paddles;
