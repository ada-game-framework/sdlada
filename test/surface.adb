with Ada.Calendar;
with Ada.Directories;
with Ada.Text_IO.Text_Streams;
with Ada.Unchecked_Conversion;
with Interfaces.C.Pointers;
with SDL;
with SDL.Error;
with SDL.Events.Events;
with SDL.Events.Keyboards;
with SDL.Log;
--  with SDL.Video.Palettes;
with SDL.Video.Pixel_Formats;
--  with SDL.Video.Pixels;
with SDL.Video.Rectangles;
--  with SDL.Video.Renderers.Makers;
--  with SDL.Video.Textures.Makers;
with SDL.Video.Surfaces;
with SDL.Video.Windows.Makers;
with SDL.Versions;
with System;
with System.Address_To_Access_Conversions;

procedure Surface is
   W : SDL.Video.Windows.Window;
begin
   SDL.Log.Set (Category => SDL.Log.Application, Priority => SDL.Log.Debug);

   if SDL.Initialise (Flags => SDL.Enable_Screen) = True then
      SDL.Video.Windows.Makers.Create (Win    => W,
                                       Title  => "Surface (Esc to exit)",
                                       X      => 100,
                                       Y      => 100,
                                       Width  => 800,
                                       Height => 640,
                                       Flags  => SDL.Video.Windows.Resizable);

      --  Main loop.
      declare
         Event          : SDL.Events.Events.Events;
         Window_Surface : SDL.Video.Surfaces.Surface;
         Area           : SDL.Video.Rectangles.Rectangle        := (X => 10, Y => 10, Width => 50, Height => 50);
         Areas          : SDL.Video.Rectangles.Rectangle_Arrays := ((X => 100, Y => 10, Width => 50, Height => 50),
                                                                    (X => 120, Y => 20, Width => 50, Height => 50),
                                                                    (X => 160, Y => 40, Width => 50, Height => 50));
         Green_Area     : SDL.Video.Rectangles.Rectangle        := (X => 15, Y => 15, Width => 10, Height => 10);
         Blue_Areas     : SDL.Video.Rectangles.Rectangle_Arrays := ((X => 150, Y => 15, Width => 10, Height => 10),
                                                                    (X => 125, Y => 25, Width => 10, Height => 10),
                                                                    (X => 165, Y => 45, Width => 10, Height => 10));
         Finished       : Boolean := False;

         use type SDL.Events.Event_Types;
         use type SDL.Events.Keyboards.Key_Codes;
         use type SDL.Events.Keyboards.Scan_Codes;
      begin
         Window_Surface := W.Get_Surface;

         Window_Surface.Fill (Area, SDL.Video.Pixel_Formats.To_Pixel
                              (Format => Window_Surface.Pixel_Format,
                               Red    => 200,
                               Green  => 100,
                               Blue   => 150));

         Window_Surface.Fill (Areas, SDL.Video.Pixel_Formats.To_Pixel
                              (Format => Window_Surface.Pixel_Format,
                               Red    => 100,
                               Green  => 100,
                               Blue   => 150));

         W.Update_Surface;  --  Shows the above two calls.

         Window_Surface.Fill (Green_Area, SDL.Video.Pixel_Formats.To_Pixel
                              (Format => Window_Surface.Pixel_Format,
                               Red    => 100,
                               Green  => 200,
                               Blue   => 100));

         W.Update_Surface_Rectangle (Rectangle => Green_Area);

         Window_Surface.Fill (Blue_Areas, SDL.Video.Pixel_Formats.To_Pixel
                              (Format => Window_Surface.Pixel_Format,
                               Red    => 150,
                               Green  => 150,
                               Blue   => 250));

         W.Update_Surface_Rectangles (Rectangles => Blue_Areas);

         loop
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
      SDL.Finalise;
   end if;
end Surface;
