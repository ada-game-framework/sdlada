--  Pong-Demo for SDLAda, game constants.
--  Copyright (C) 2012 - 2020, Vinzent "Jellix" Saranen

with Ada.Real_Time;

with SDL.Video.Palettes,
     SDL.Video.Rectangles;

package Game.Constants is

   use type SDL.Dimension;

   --  Frame/update rate.
   Game_Speed : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds (10);

   --  Virtual screen size.
   Screen_Width  : constant := 1600;
   Screen_Height : constant := 960;

   Background_Color : SDL.Video.Palettes.Colour :=
     SDL.Video.Palettes.Colour'(Red   => 16#00#,
                                Green => 16#00#,
                                Blue  => 16#00#,
                                Alpha => 16#FF#);

   Line_Colour : SDL.Video.Palettes.Colour :=
     SDL.Video.Palettes.Colour'(Red   => 16#80#,
                                Green => 16#80#,
                                Blue  => 16#80#,
                                Alpha => 16#FF#);

   Ball_Colour : SDL.Video.Palettes.Colour :=
     SDL.Video.Palettes.Colour'(Red   => 16#FF#,
                                Green => 16#FF#,
                                Blue  => 16#FF#,
                                Alpha => 16#FF#);

   Paddle_Colour : SDL.Video.Palettes.Colour :=
     SDL.Video.Palettes.Colour'(Red   => 16#FF#,
                                Green => 16#FF#,
                                Blue  => 16#FF#,
                                Alpha => 16#FF#);

   --  Ball and paddle sizes.
   Ball_Size     : constant := Screen_Width  / 80;
   Paddle_Width  : constant := Screen_Width  / 80;
   Paddle_Height : constant := Screen_Height / 8;

   Ball_Initial : constant SDL.Coordinates :=
     SDL.Coordinates'(X => SDL.Dimension (Screen_Width / 2  - Ball_Size / 2),
                      Y => SDL.Dimension (Screen_Height / 2 - Ball_Size / 2));

   --  Ball can use the full screen
   Ball_Bounds : constant SDL.Video.Rectangles.Rectangle :=
     SDL.Video.Rectangles.Rectangle'(X      => 0,
                                     Y      => 0,
                                     Width  => Screen_Width,
                                     Height => Screen_Height);

   Border_Left : constant := Screen_Width / 80;
   Border_Top  : constant := Screen_Height / 10;

   Paddle_Bounds : constant SDL.Video.Rectangles.Rectangle :=
     SDL.Video.Rectangles.Rectangle'(X      => Border_Left,
                                     Y      => Border_Top,
                                     Width  => Screen_Width  - 2 * Border_Left,
                                     Height => Screen_Height - 2 * Border_Top);

   --  Minimum position difference before computer moves its paddle.
   Threshold     : constant := Paddle_Height / 4;

   --  Speed constants for movement.
   Ball_Speed     : constant Float := Float (Screen_Width) / 160.0;
   Computer_Speed : constant Float := Float (Screen_Height) / 50.0;
   Player_Speed   : constant Float := Float (Screen_Height) / 50.0;

   --  Minimum score and points difference to reach winning condition.
   Min_Winning_Score : constant := 11;
   Min_Difference    : constant :=  2;

end Game.Constants;
