with Ada.Calendar;
with Ada.Directories;
with Ada.Text_IO.Text_Streams;
with Ada.Unchecked_Conversion;
with Interfaces.C.Pointers;
with SDL;
with SDL.Error;
with SDL.Events.Events;
with SDL.Events.Keyboards;
with SDL.Images.IO;
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

procedure Load_Surface is
   use type Interfaces.C.int;

   W             : SDL.Video.Windows.Window;
   Window_Width  : constant := 800;
   Window_Height : constant := 640;
begin
   SDL.Log.Set (Category => SDL.Log.Application, Priority => SDL.Log.Debug);

   if SDL.Initialise (Flags => SDL.Enable_Screen) = True and SDL.Images.Initialise then
      SDL.Video.Windows.Makers.Create (Win    => W,
                                       Title  => "Surface (Esc to exit)",
                                       X      => 100,
                                       Y      => 100,
                                       Width  => Window_Width,
                                       Height => Window_Height,
                                       Flags  => SDL.Video.Windows.Resizable);

      --  Main loop.
      declare
         Event            : SDL.Events.Events.Events;
         Window_Surface   : SDL.Video.Surfaces.Surface;
         Image_Surface    : SDL.Video.Surfaces.Surface;
         Image_Area       : SDL.Video.Rectangles.Rectangle := (X => 0, Y => 0, Width => 400, Height => 300);
         Image_Dest_Area  : SDL.Video.Rectangles.Rectangle := (X      => Window_Width / 2 - Image_Area.Width / 2,
                                                               Y      => Window_Height / 2 - Image_Area.Height / 2,
                                                               Width  => 400,
                                                               Height => 300);
         Finished         : Boolean := False;

         use type SDL.Events.Event_Types;
         use type SDL.Events.Keyboards.Key_Codes;
         use type SDL.Events.Keyboards.Scan_Codes;
      begin
         Window_Surface := W.Get_Surface;

         SDL.Images.IO.Create (Image_Surface, "../../test/assets/sdl_logo_400_300.png");

         Window_Surface.Blit (Self_Area   => Image_Dest_Area,
                              Source      => Image_Surface,
                              Source_Area => Image_Area);

         W.Update_Surface;

         SDL.Images.IO.Write_PNG (Window_Surface, "load_surface.png");

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
      SDL.Images.Finalise;
      SDL.Finalise;
   end if;
end Load_Surface;
