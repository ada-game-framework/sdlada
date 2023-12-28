with Ada.Real_Time;
with Interfaces.C;
with SDL;
with SDL.Events.Events;
with SDL.Events.Keyboards;
with SDL.Images.IO;
with SDL.Log;
with SDL.Video.Rectangles;
with SDL.Video.Surfaces;
with SDL.Video.Windows.Makers;

procedure Load_Surface is
   use type Interfaces.C.int;

   W           : SDL.Video.Windows.Window;
   Window_Size : constant SDL.Positive_Sizes := (Width => 800, Height => 640);
begin
   SDL.Log.Set (Category => SDL.Log.Application, Priority => SDL.Log.Debug);

   if SDL.Initialise (Flags => SDL.Enable_Screen) = True
      and then SDL.Images.Initialise
   then
      SDL.Video.Windows.Makers.Create (Win      => W,
                                       Title    => "Surface (Esc to exit)",
                                       Position => SDL.Natural_Coordinates'(X => 100, Y => 100),
                                       Size     => Window_Size,
                                       Flags    => SDL.Video.Windows.Resizable);

      --  Main loop.
      declare
         Event              : SDL.Events.Events.Events;
         Window_Surface     : SDL.Video.Surfaces.Surface;
         Image_Surface      : SDL.Video.Surfaces.Surface;
         Image_Area         : SDL.Video.Rectangles.Rectangle :=
           (X => 0, Y => 0, Width => 400, Height => 300);
         Image_Dest_Area    : SDL.Video.Rectangles.Rectangle :=
           (X      => Window_Size.Width / 2 - Image_Area.Width / 2,
            Y      => Window_Size.Height / 2 - Image_Area.Height / 2,
            Width  => 400,
            Height => 300);
         Scaled_Dest_Area   : SDL.Video.Rectangles.Rectangle :=
           (X      => 10,
            Y      => 10,
            Width  => Image_Area.Width / 4,
            Height => Image_Area.Height / 4);
         Scaled_Dest_Area_2 : SDL.Video.Rectangles.Rectangle :=
           (X      => 10,
            Y      => 100,
            Width  => Image_Area.Width / 4,
            Height => Image_Area.Height / 4);
         Finished           : Boolean := False;

         Loop_Start_Time_Goal : Ada.Real_Time.Time;

         Frame_Duration : constant Ada.Real_Time.Time_Span :=
           Ada.Real_Time.Microseconds (16_667);
         --  60 Hz refresh rate (set to anything you like)

         use type SDL.Events.Keyboards.Key_Codes;
         use type Ada.Real_Time.Time;
      begin
         Window_Surface := W.Get_Surface;

         SDL.Images.IO.Create (Image_Surface, "../../test/assets/sdl_logo_400_300.png");

         Window_Surface.Blit (Self_Area   => Image_Dest_Area,
                              Source      => Image_Surface,
                              Source_Area => Image_Area);

         Window_Surface.Blit_Scaled (Self_Area   => Scaled_Dest_Area,
                                     Source      => Image_Surface,
                                     Source_Area => SDL.Video.Rectangles.Null_Rectangle);

         Window_Surface.Blit_Scaled (Self_Area   => Scaled_Dest_Area_2,
                                     Source      => Image_Surface,
                                     Source_Area => SDL.Video.Rectangles.Rectangle'(X      => 0,
                                                                                    Y      => 0,
                                                                                    Width  => Image_Area.Width / 2,
                                                                                    Height => Image_Area.Height / 2));

         --  Set next frame delay target using monotonic clock time
         Loop_Start_Time_Goal := Ada.Real_Time.Clock;

         loop
            --  Limit event loop to 60 Hz using realtime "delay until"
            Loop_Start_Time_Goal := Loop_Start_Time_Goal + Frame_Duration;
            delay until Loop_Start_Time_Goal;

            W.Update_Surface;

            while SDL.Events.Events.Poll (Event) loop
               case Event.Common.Event_Type is
                  when SDL.Events.Quit =>
                     Finished := True;

                  when SDL.Events.Keyboards.Key_Down =>
                     if Event.Keyboard.Key_Sym.Key_Code = SDL.Events.Keyboards.Code_Escape then
                        Finished := True;
                     end if;

                  when others =>
                     null;
               end case;
            end loop;

            exit when Finished;
         end loop;
      end;

      SDL.Log.Put_Debug ("");

      W.Finalize;
      SDL.Images.Finalise;
      SDL.Finalise;
   end if;
end Load_Surface;
