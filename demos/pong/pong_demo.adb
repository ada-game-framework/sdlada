--  Pong-Demo for SDLAda, main program.
--  Copyright (C) 2012 - 2020, Vinzent "Jellix" Saranen

with Ada.Exceptions,
     Ada.Real_Time,
     Ada.Text_IO;

with SDL.Error,
     SDL.Events.Events,
     SDL.Events.Keyboards,
     SDL.Video.Rectangles,
     SDL.Video.Renderers.Makers,
     SDL.Video.Windows.Makers;

with Pong.Balls,
     Pong.Paddles;

with Game.Audio,
     Game.Constants;

use type Ada.Real_Time.Time;

use type SDL.Dimension,
         SDL.Init_Flags;

procedure Pong_Demo is

   package GC renames Game.Constants;

   --  Some useful game related constants.
   Left_Goal  : constant := GC.Screen_Width / 16;
   Right_Goal : constant := GC.Screen_Width - Left_Goal;

   type Object_Ref  is access all Pong.Display_Object'Class;
   type Object_List is array (Natural range <>) of Object_Ref;

   procedure Render_Frame (Renderer : in out SDL.Video.Renderers.Renderer;
                           Objects  : in     Object_List) is
   begin
      Renderer.Set_Draw_Colour (Colour => GC.Background_Color);
      Renderer.Clear;

      Renderer.Set_Draw_Colour (Colour => GC.Line_Colour);
      Renderer.Draw (Line => ((Left_Goal, 0),
                              (Left_Goal, GC.Screen_Height - 1)));
      Renderer.Draw (Line => ((Right_Goal, 0),
                              (Right_Goal, GC.Screen_Height - 1)));

      --  Update paddle and ball positions.
      Draw_Objects :
      for O of Objects loop
         O.all.Draw (Renderer => Renderer);
      end loop Draw_Objects;

      --  Update video display.
      Renderer.Present;
   end Render_Frame;

   type Players is (Computer, Human);
   type Score is array (Players) of Natural;

   SDL_Error     : exception;

   Ball          : aliased Pong.Balls.Ball;
   Smart_Ass     : aliased Pong.Paddles.Paddle;
   Player        : aliased Pong.Paddles.Paddle;
   Objects       : constant Object_List := (0 => Ball'Access,
                                            1 => Smart_Ass'Access,
                                            2 => Player'Access);

   Clipped       : Boolean;
   The_Score     : Score;
   Event         : SDL.Events.Events.Events;
   Game_Window   : SDL.Video.Windows.Window;
   Game_Renderer : SDL.Video.Renderers.Renderer;
   Next_Time     : Ada.Real_Time.Time;

   Stop_Ball_Until : Ada.Real_Time.Time := Ada.Real_Time.Clock;
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
        (Win      => Game_Window,
         Title    => "Ada/SDL Demo - Pong",
         Position => SDL.Coordinates'(X => 0,
                                      Y => 0),
         Size     => SDL.Sizes'(Width  => GC.Screen_Width,
                                Height => GC.Screen_Height),
         Flags    => SDL.Video.Windows.Full_Screen_Desktop);
   exception
      when others =>

         Ada.Text_IO.Put_Line (File => Ada.Text_IO.Standard_Error,
                               Item => "Could not setup video mode!");
         raise SDL_Error with SDL.Error.Get;
   end;

   SDL.Video.Renderers.Makers.Create
     (Rend   => Game_Renderer,
      Window => Game_Window,
      Flags  => SDL.Video.Renderers.Present_V_Sync);
   Game_Renderer.Set_Logical_Size
     (Size => SDL.Sizes'(Width  => GC.Screen_Width,
                         Height => GC.Screen_Height));

   Game.Audio.Initialize;

   Ball :=
     Pong.Balls.Create
       (Initial => SDL.Video.Rectangles.Rectangle'(X      => GC.Ball_Initial.X,
                                                   Y      => GC.Ball_Initial.Y,
                                                   Width  => GC.Ball_Size,
                                                   Height => GC.Ball_Size),
        Bounds  => GC.Ball_Bounds,
        Colour  => GC.Ball_Colour,
        Speed   => GC.Ball_Speed);

   Smart_Ass :=
     Pong.Paddles.Create
       (Initial =>
          SDL.Video.Rectangles.Rectangle'(X      => Left_Goal + 1,
                                          Y      => (GC.Screen_Height / 2 -
                                                         GC.Paddle_Height / 2),
                                          Width  => GC.Paddle_Width,
                                          Height => GC.Paddle_Height),
        Bounds  => GC.Paddle_Bounds,
        -- Give the human player a winning chance by restricting the movement of
        -- the computer opponent.
        Colour  => GC.Paddle_Colour,
        Speed   => GC.Computer_Speed);

   Player :=
     Pong.Paddles.Create
       (Initial =>
          SDL.Video.Rectangles.Rectangle'
            (X      => Right_Goal - GC.Paddle_Width,
             Y      => (GC.Screen_Height / 2 - GC.Paddle_Height / 2),
             Width  => GC.Paddle_Width,
             Height => GC.Paddle_Height),
        Bounds  => GC.Ball_Bounds,
        -- Give the human player a winning chance by allowing full movement of
        -- the human opponent's paddle.
        Colour  => GC.Paddle_Colour,
        Speed   => GC.Player_Speed);

   The_Score := Score'(Computer => 0,
                       Human    => 0);

   Next_Time       := Ada.Real_Time.Clock + GC.Game_Speed;
   Stop_Ball_Until := Ada.Real_Time.Clock + Ada.Real_Time.Seconds (1);

   Game_Loop :
   loop
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
                     Player.Set_Velocity (Vel => +1.0);

                  when SDL.Events.Keyboards.Scan_Code_Up =>
                     Player.Set_Velocity (Vel => -1.0);

                  when others =>
                     null;
               end case;

            when SDL.Events.Keyboards.Key_Up =>
               case Event.Keyboard.Key_Sym.Scan_Code is

                  --  No movement key(s) pressed anymore, stop movement.
                  when SDL.Events.Keyboards.Scan_Code_Down |
                       SDL.Events.Keyboards.Scan_Code_Up =>
                     Player.Set_Velocity (Vel => 0.0);

                  when others =>
                     null;
               end case;

            when others =>
               null;
         end case;
      end if;

      --  Move player paddle according to previously set velocity.
      Player.Move (Clipped => Clipped);

      --  Let computer's paddle simply follow the ball.
      Move_Paddle :
      declare
         Ball_Center   : constant SDL.Dimension :=
           Pong.Position (This => Ball).Y + GC.Ball_Size / 2;
         Paddle_Center : constant SDL.Dimension :=
           Pong.Position (This => Smart_Ass).Y + GC.Paddle_Height / 2;
      begin
         if
           Ball_Center - Paddle_Center < -GC.Threshold
         then
            Smart_Ass.Set_Velocity (Vel => -1.0);
         elsif
           Ball_Center - Paddle_Center > GC.Threshold
         then
            Smart_Ass.Set_Velocity (Vel => +1.0);
         else
            Smart_Ass.Set_Velocity (Vel => 0.0);
         end if;

         --  Do movement according to velocity.
         Smart_Ass.Move (Clipped => Clipped);
      end Move_Paddle;

      --  Don't move ball until it's time.
      if Ada.Real_Time.Clock > Stop_Ball_Until then
         --  Check collision with paddles.
         if
           Ball.Collides (That => Smart_Ass) or
           Ball.Collides (That => Player)
         then
            Game.Audio.Play_Pong;

            Ball.Change_Dir (X => True,
                             Y => False);

            --  TODO: Warp the ball in the X coordinate to move it out of the way
            --        of the paddle. Otherwise, the paddle may "push" the ball
            --        multiple times.
            declare
               Old_Ball_Position : constant SDL.Coordinates := Ball.Position;
            begin
               if Old_Ball_Position.X < Left_Goal + GC.Paddle_Width then
                  Ball.Warp (Initial => (X => Left_Goal + GC.Paddle_Width,
                                         Y => Old_Ball_Position.Y));
               elsif Old_Ball_Position.X + GC.Ball_Size > Right_Goal - GC.Paddle_Width then
                  Ball.Warp (Initial => (X => Right_Goal - GC.Ball_Size - GC.Paddle_Width,
                                         Y => Old_Ball_Position.Y));
               end if;
            end;

            Ball.Move (Clipped => Clipped);
         else
            Ball.Move (Clipped => Clipped);

            if Clipped then
               Game.Audio.Play_Ping;
            end if;
         end if;
      end if;

      --  Check winning/losing condition.
      Check_Score :
      declare
         Score_Changed : Boolean := False;
      begin
         if
           Ball.Position.X < Left_Goal - GC.Ball_Size
         then
            The_Score (Human) := The_Score (Human) + 1;
            Score_Changed := True;
         elsif
           Ball.Position.X > Right_Goal
         then
            The_Score (Computer) := The_Score (Computer) + 1;
            Score_Changed := True;
         end if;

         if Score_Changed then
            Ada.Text_IO.Put_Line
              (Natural'Image (The_Score (Computer)) & " :" &
                 Natural'Image (The_Score (Human)));

            Ball.Warp (Initial => GC.Ball_Initial);

            --  After placing the ball back into the field, delay ball movement
            --  for a second.
            Stop_Ball_Until := Ada.Real_Time.Clock + Ada.Real_Time.Seconds (1);
         end if;
      end Check_Score;

      exit Game_Loop when
        (abs (The_Score (Computer) - The_Score (Human)) >= GC.Min_Difference) and --  points difference
        Natural'Max (The_Score (Computer), The_Score (Human)) >= GC.Min_Winning_Score;

      --  Render the new frame.
      Render_Frame (Renderer => Game_Renderer,
                    Objects  => Objects);

      delay until Next_Time;
      Next_Time := Next_Time + GC.Game_Speed;

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
