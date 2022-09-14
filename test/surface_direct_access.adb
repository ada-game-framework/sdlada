with Interfaces.C.Pointers;
with System.Storage_Elements;
with System.Address_To_Access_Conversions;
with SDL.Events.Events;
with SDL.Events.Keyboards;
with SDL.Log;
with SDL.Video.Palettes;
with SDL.Video.Pixel_Formats;
with SDL.Video.Surfaces.Makers;
with SDL.Video.Windows.Makers;

procedure Surface_Direct_Access is
   W : SDL.Video.Windows.Window;
   use System.Storage_Elements;
begin
   SDL.Log.Set (Category => SDL.Log.Application, Priority => SDL.Log.Debug);

   if SDL.Initialise (Flags => SDL.Enable_Screen) = True then
      SDL.Video.Windows.Makers.Create (Win      => W,
                                       Title    => "Surface direct access (Esc to exit)",
                                       Position => SDL.Natural_Coordinates'(X => 100, Y => 100),
                                       Size     => SDL.Positive_Sizes'(800, 640),
                                       Flags    => SDL.Video.Windows.Resizable);

      --  Main loop.
      declare
         Pixel_Depth      : constant := 16;
         --  Not all Pixel_Depth values are displayed correctly.
         --  Actual data layout may differ than implied here.
         type Pixel      is mod 2**Pixel_Depth;
         type Pixel_Array is array (Integer range <>) of aliased Pixel;
         package Pixel_Pointers is new Interfaces.C.Pointers (Index              => Integer,
                                                              Element            => Pixel,
                                                              Element_Array      => Pixel_Array,
                                                              Default_Terminator => 0);
         use type Pixel_Pointers.Pointer;
         package Convert is new System.Address_To_Access_Conversions (Object => Pixel);
         S                : SDL.Video.Surfaces.Surface;
         Pixels           : System.Address;
         Pitch            : Interfaces.C.int;

         --  This procedure writes individual pixel in the surface (no blending or masking)
         procedure Write_Pixel (X, Y : Integer; Colour : Pixel) is
            Row_Addr : constant System.Address          := Pixels + Storage_Offset (Pitch) * Storage_Offset (Y);
            --  Get the starting address of a specific pixel row. Note that Pitch parameter is in
            --  storage elements, not pixels, so computation could be done before the conversion to a typed pointer
            Row_Ptr  : constant Pixel_Pointers.Pointer  := Pixel_Pointers.Pointer (Convert.To_Pointer (Row_Addr));
            --  Convert to the pointer. Two conversions required, because there's
            --  no direct conversion procedure from System.Address to Interfaces.C.Pointers.Pointer.
            Ptr      : constant Pixel_Pointers.Pointer  := Row_Ptr + Interfaces.C.ptrdiff_t (X);
         begin
            Ptr.all := Colour;
         end Write_Pixel;

         Window_Surface   : SDL.Video.Surfaces.Surface;
         Event            : SDL.Events.Events.Events;
         Finished         : Boolean := False;

         use type SDL.Events.Keyboards.Key_Codes;
      begin
         SDL.Video.Surfaces.Makers.Create (Self => S,
                                           Size => (800, 640),
                                           BPP => Pixel_Depth,
                                           Red_Mask => 0,
                                           Blue_Mask => 0,
                                           Green_Mask => 0,
                                           Alpha_Mask => 0);  --  a surface with known pixel depth

         S.Lock;

         Pixels := S.Pixels;
         Pitch := S.Pitch;

         for X in 0 .. 799 loop
            for Y in 0 .. 639 loop
               Write_Pixel (X, Y, Pixel (SDL.Video.Pixel_Formats.To_Pixel
                              (Format => S.Pixel_Format,
                               Red    => SDL.Video.Palettes.Colour_Component (Y * 255 / 639),
                               Green  => SDL.Video.Palettes.Colour_Component (X * 255 / 799),
                               Blue   => SDL.Video.Palettes.Colour_Component (255 - (X + Y) * 255 / (799 + 639)))));
            end loop;
         end loop;
         S.Unlock;

         Window_Surface := W.Get_Surface;
         Window_Surface.Blit (S);
         W.Update_Surface;

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

      W.Finalize;
      SDL.Finalise;
   end if;
end Surface_Direct_Access;
