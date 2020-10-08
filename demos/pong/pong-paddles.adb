--  Pong-Demo for SDLAda, paddle actions.
--  Copyright (C) 2012 - 2020, Vinzent "Jellix" Saranen

with SDL.Video.Rectangles,
     SDL.Video.Surfaces;

package body Pong.Paddles is

   use type Interfaces.C.int;

   ---------------------------------------------------------------------
   --  Create
   ---------------------------------------------------------------------
   function Create (Surface : in SDL.Video.Surfaces.Surface;
                    Initial : in SDL.Video.Rectangles.Rectangle;
                    Bounds  : in SDL.Video.Rectangles.Rectangle;
                    Speed   : in Interfaces.C.unsigned_char) return Paddle is
   begin
      return Paddle'(Old_Pos => Position'(X => Initial.X,
                                          Y => Initial.Y),
                     New_Pos => Position'(X => Initial.X,
                                          Y => Initial.Y),
                     Size    => Dimensions'(W => Initial.Width,
                                            H => Initial.Height),
                     Bounds  =>
                       SDL.Video.Rectangles.Rectangle'(X      => Bounds.X,
                                                       Y      => Bounds.Y,
                                                       Width  => Bounds.Width - Initial.Width,
                                                       Height => Bounds.Height - Initial.Height),
                     Black   => 16#00_00_00#,
                     White   => 16#FF_FF_FF#,
                     Speed   => Speed);
   end Create;

   ---------------------------------------------------------------------
   --  Move
   ---------------------------------------------------------------------
   overriding
   procedure Move (This    : in out Paddle;
                   Clipped :    out Boolean;
                   Delta_X : in     Interfaces.C.int := 0;
                   Delta_Y : in     Interfaces.C.int)
   is
      pragma Unreferenced (Delta_X);
      Max_Y : constant Interfaces.C.int := This.Bounds.Y + This.Bounds.Height;
   begin
      This.New_Pos.Y := This.Old_Pos.Y + Delta_Y * Interfaces.C.int (This.Speed);

      if
        This.New_Pos.Y > Max_Y
      then
         Clipped := True;
         This.New_Pos.Y := Max_Y;
      elsif
        This.New_Pos.Y < This.Bounds.Y
      then
         Clipped := True;
         This.New_Pos.Y := This.Bounds.Y;
      else
         Clipped := False;
      end if;
   end Move;

   ---------------------------------------------------------------------
   --  Draw
   ---------------------------------------------------------------------
   overriding
   procedure Draw (This    : in out Paddle;
                   Surface : in out SDL.Video.Surfaces.Surface)
   is
      Clear_At : SDL.Video.Rectangles.Rectangle :=
        SDL.Video.Rectangles.Rectangle'(X      => This.Old_Pos.X,
                                        Y      => This.Old_Pos.Y,
                                        Width  => This.Size.W,
                                        Height => This.Size.H);
      Draw_At  : SDL.Video.Rectangles.Rectangle :=
        SDL.Video.Rectangles.Rectangle'(X      => This.New_Pos.X,
                                        Y      => This.New_Pos.Y,
                                        Width  => This.Size.W,
                                        Height => This.Size.H);
   begin
      Surface.Fill (Area   => Clear_At,
                    Colour => This.Black);
      pragma Unreferenced (Clear_At);

      Surface.Fill (Area   => Draw_At,
                    Colour => This.White);
      pragma Unreferenced (Draw_At);

      This.Old_Pos := This.New_Pos;
   end Draw;

end Pong.Paddles;
