--  Pong-Demo for SDLAda, game constants.
--  Copyright (C) 2012 - 2020, Vinzent "Jellix" Saranen

with Ada.Real_Time;

with Interfaces.C;

with SDL;

package Game.Constants is

   --  Frame rate.
   Game_Speed : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds (10);

   --  Screen sizes.
   Screen_Width  : constant := 640;
   Screen_Height : constant := 480;

   --  Ball and paddle sizes.
   Ball_Size     : constant := 10;
   Paddle_Width  : constant := 10;
   Paddle_Height : constant := 50;

   Ball_Initial : constant SDL.Coordinates :=
     SDL.Coordinates'(X => Interfaces.C.int (Screen_Width / 2  - Ball_Size / 2),
                      Y => Interfaces.C.int (Screen_Height / 2 - Ball_Size / 2));

   --  Minimum position difference before computer moves its paddle.
   Threshold     : constant := Paddle_Height / 4;

   --  Speed constants for movement.
   Ball_Speed     : constant := 2;
   Computer_Speed : constant := 8;
   Player_Speed   : constant := 8;

   --  Minimum score and points difference to reach winning condition.
   Min_Winning_Score : constant := 11;
   Min_Difference    : constant :=  2;

end Game.Constants;
