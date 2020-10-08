--  Pong-Demo for SDLAda, main program.
--  Copyright (C) 2012 - 2020, Vinzent "Jellix" Saranen

with Ada.Exceptions,
     Ada.Real_Time,
     Ada.Text_IO;

with Interfaces.C;

with SDL.Error,
     SDL.Events.Events,
     SDL.Events.Keyboards,
     SDL.Video.Rectangles,
     SDL.Video.Surfaces,
     SDL.Video.Windows.Makers;

with Pong.Balls,
     Pong.Paddles;

with Game.Audio,
     Game.Constants;

use type Ada.Real_Time.Time;

use type Interfaces.C.int,
         SDL.Init_Flags;

procedure Pong_Demo is
   type Players is (Computer, Human);
   type Score is array (Players) of Natural;

   type Object_Ref  is access all Pong.Display_Object'Class;
   type Object_List is array (Natural range <>) of Object_Ref;

   SDL_Error   : exception;

   Ball        : aliased Pong.Balls.Ball;
   Smart_Ass   : aliased Pong.Paddles.Paddle;
   Player      : aliased Pong.Paddles.Paddle;
   Objects     : constant Object_List := (0 => Ball'Access,
                                          1 => Smart_Ass'Access,
                                          2 => Player'Access);

   Clipped     : Boolean;
   The_Score   : Score;
   Event       : SDL.Events.Events.Events;
   Game_Window : SDL.Video.Windows.Window;
   Game_Screen : SDL.Video.Surfaces.Surface;
   Next_Time   : Ada.Real_Time.Time;

   package GC renames Game.Constants;
begin
   if
     not SDL.Initialise (Flags => (SDL.Enable_Screen or SDL.Enable_Audio))
   then
      Ada.Text_IO.Put_Line (File => Ada.Text_IO.Standard_Error,
                            Item => "Could not initialize SDL!");
      raise SDL_Error with SDL.Error.Get;
   end if;

   begin
      SDL.Video.Windows.Makers.Create
        (Win    => Game_Window,
         Title  => "Ada/SDL Demo - Pong",
         X      => 0,
         Y      => 0,
         Width  => 640,
         Height => 480,
         Flags  => SDL.Video.Windows.Full_Screen);
   exception
      when others =>

         Ada.Text_IO.Put_Line (File => Ada.Text_IO.Standard_Error,
                               Item => "Could not setup video mode!");
         raise SDL_Error with SDL.Error.Get;
   end;

   Game.Audio.Initialize;
   Game_Screen := Game_Window.Get_Surface;

   Ball :=
     Pong.Balls.Create
       (Surface => Game_Screen,
        Initial => SDL.Video.Rectangles.Rectangle'(X      => GC.Ball_Initial.X,
                                                   Y      => GC.Ball_Initial.Y,
                                                   Width  => GC.Ball_Size,
                                                   Height => GC.Ball_Size),
        Bounds  =>
          SDL.Video.Rectangles.Rectangle'(X      => 5,
                                          Y      => 5,
                                          Width  => GC.Screen_Width  - 10,
                                          Height => GC.Screen_Height - 10),
        Speed   => GC.Ball_Speed);

   Smart_Ass :=
     Pong.Paddles.Create
       (Surface => Game_Screen,
        Initial =>
          SDL.Video.Rectangles.Rectangle'(X      => 10,
                                          Y      => (GC.Screen_Height / 2 -
                                                         GC.Paddle_Height / 2),
                                          Width  => GC.Paddle_Width,
                                          Height => GC.Paddle_Height),
        Bounds  =>
          SDL.Video.Rectangles.Rectangle'(X      => 10,
                                          Y      => 0,
                                          Width  => GC.Paddle_Width,
                                          Height => GC.Screen_Height),
        Speed   =>  GC.Computer_Speed);

   Player :=
     Pong.Paddles.Create
       (Surface => Game_Screen,
        Initial =>
          SDL.Video.Rectangles.Rectangle'(X      => GC.Screen_Width - 10 - GC.Paddle_Width,
                                          Y      => (GC.Screen_Height / 2 -
                                                         GC.Paddle_Height / 2),
                                          Width  => GC.Paddle_Width,
                                          Height => GC.Paddle_Height),
        Bounds  =>
          SDL.Video.Rectangles.Rectangle'(X      => GC.Screen_Width - 10 - GC.Paddle_Width,
                                          Y      => 0,
                                          Width  => GC.Paddle_Width,
                                          Height => GC.Screen_Height),
        Speed   => GC.Player_Speed);

   Game_Window.Update_Surface;

   The_Score := Score'(Computer => 0,
                       Human    => 0);

   Next_Time := Ada.Real_Time.Clock + GC.Game_Speed;

   Game_Loop :
   loop
      Game_Screen := Game_Window.Get_Surface;

      --  Update paddle and ball positions.
      Draw_Objects :
      for O in Objects'Range loop
         Pong.Draw (This    => Objects (O).all,
                    Surface => Game_Screen);
      end loop Draw_Objects;

      --  Update video display.
      Game_Window.Update_Surface;

      delay until Next_Time;
      Next_Time := Next_Time + GC.Game_Speed;

      if SDL.Events.Events.Poll (Event => Event) then
         case Event.Common.Event_Type is
            when SDL.Events.Quit     =>
               --  Reset score and get out.
               The_Score := Score'(Computer => 0,
                                   Human    => 0);
               exit Game_Loop;

            when SDL.Events.Keyboards.Key_Down =>
               case Event.Keyboard.Key_Sym.Scan_Code is
                  when SDL.Events.Keyboards.Scan_Code_Escape =>
                     --  Reset score and get out.
                     The_Score := Score'(Computer => 0,
                                         Human    => 0);
                     exit Game_Loop;

                  when SDL.Events.Keyboards.Scan_Code_Down =>
                     Pong.Paddles.Move (This    => Player,
                                        Clipped => Clipped,
                                        Delta_Y => +1);

                  when SDL.Events.Keyboards.Scan_Code_Up =>
                     Pong.Paddles.Move (This    => Player,
                                        Clipped => Clipped,
                                        Delta_Y => -1);

                  when others =>
                     null;
               end case;

            when others =>
               null;
         end case;
      end if;

      --  Let computer's paddle simply follow the ball.
      Move_Paddle :
      declare
         Ball_Center   : constant Interfaces.C.int :=
                           Pong.Y_Position (This => Ball) + GC.Ball_Size / 2;
         Paddle_Center : constant Interfaces.C.int :=
                           Pong.Y_Position (This => Smart_Ass) + GC.Paddle_Height / 2;
      begin
         if
           Ball_Center - Paddle_Center < -GC.Threshold
         then
            Pong.Paddles.Move (This    => Smart_Ass,
                               Clipped => Clipped,
                               Delta_Y => -1);
         elsif
           Ball_Center - Paddle_Center > GC.Threshold
         then
            Pong.Paddles.Move (This    => Smart_Ass,
                               Clipped => Clipped,
                               Delta_Y => +1);
         end if;
      end Move_Paddle;

      --  Check collision with paddles.
      if
        Pong.Balls.Collides (This => Ball,
                             That => Smart_Ass) or
        Pong.Balls.Collides (This => Ball,
                             That => Player)
      then
         Game.Audio.Play_Pong;

         Pong.Balls.Change_Dir (This => Ball,
                                X    => True,
                                Y    => False);
      else
         Pong.Balls.Move (This    => Ball,
                          Clipped => Clipped);

         if Clipped then
            Game.Audio.Play_Ping;
         end if;
      end if;

      --  Check winning/losing condition.
      Check_Score :
      declare
         Score_Changed : Boolean := False;
      begin
         if
           Pong.X_Position (This => Ball) < 10
         then
            The_Score (Human) := The_Score (Human) + 1;
            Score_Changed := True;
         elsif
           Pong.X_Position (This => Ball) > GC.Screen_Width - 10 - GC.Ball_Size
         then
            The_Score (Computer) := The_Score (Computer) + 1;
            Score_Changed := True;
         end if;

         if Score_Changed then
            Ada.Text_IO.Put_Line
              (Natural'Image (The_Score (Computer)) & " :" &
               Natural'Image (The_Score (Human)));

            Pong.Balls.Warp (This    => Ball,
                             Initial => GC.Ball_Initial);
         end if;
      end Check_Score;

      exit Game_Loop when
        (abs (The_Score (Computer) - The_Score (Human)) >= GC.Min_Difference) and --  points difference
        Natural'Max (The_Score (Computer), The_Score (Human)) >= GC.Min_Winning_Score;
   end loop Game_Loop;

   SDL.Finalise_Sub_System (Flags => (SDL.Enable_Screen or SDL.Enable_Audio));

   Game.Audio.Finalize;

   if
     The_Score (Computer) /= The_Score (Human)
   then
      if
        The_Score (Computer) > The_Score (Human)
      then
         Ada.Text_IO.Put_Line ("I WIN! YEAH!!!");
      else
         Ada.Text_IO.Put_Line ("You win. Pah.");
      end if;

      delay 1.0;
   end if;

   Game_Window.Finalize;
   pragma Unreferenced (Game_Window);
   SDL.Finalise;

exception

   when SDL_Error =>
      SDL.Finalise;

   when E : others =>
      Ada.Text_IO.Put_Line
        (File => Ada.Text_IO.Standard_Error,
         Item => String'("Game crashed: " & Ada.Exceptions.Exception_Information (E)));

      SDL.Finalise;

end Pong_Demo;
