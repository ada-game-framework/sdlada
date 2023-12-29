--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
with Ada.Real_Time;
with Ada.Unchecked_Conversion;

with Moose;
with SDL;
with SDL.Events.Events;
with SDL.Events.Keyboards;
with SDL.Log;
with SDL.Video.Pixel_Formats;
with SDL.Video.Pixels;
with SDL.Video.Renderers.Makers;
with SDL.Video.Textures.Makers;
with SDL.Video.Windows.Makers;
with System;

procedure Stream2 is
   use type SDL.Dimension;
   use type SDL.Positive_Sizes;

   W        : SDL.Video.Windows.Window;
   Renderer : SDL.Video.Renderers.Renderer;
   Texture  : SDL.Video.Textures.Texture;
   Pixels   : SDL.Video.Pixels.ARGB_8888_Access.Pointer;

   procedure Lock is new SDL.Video.Textures.Lock (Pixel_Pointer_Type => SDL.Video.Pixels.ARGB_8888_Access.Pointer);

   use type SDL.Video.Pixels.ARGB_8888_Access.Pointer;

   Left_Cursor_Down : Boolean := False;
   Right_Cursor_Down : Boolean := False;

   Loop_Start_Time_Goal : Ada.Real_Time.Time;
   Loop_Start_Time_Real : Ada.Real_Time.Time;
   Loop_Delay_Overhead_Time : Ada.Real_Time.Time_Span;
   Loop_Delay_Overhead_Average : Ada.Real_Time.Time_Span :=
     Ada.Real_Time.Time_Span_Zero;

   Frame_Duration : constant Ada.Real_Time.Time_Span :=
     Ada.Real_Time.Microseconds (6_944);
   --  144 Hz refresh rate

   Loop_Iterator : Natural := 0;
   --  This is used for animation timing as well as debug timing
begin
   SDL.Log.Set (Category => SDL.Log.Application, Priority => SDL.Log.Debug);

   Moose.Load_Moose_Data (Data => Moose.Moose_Frame_Data);

   Moose.Cache_Moose (Moose.Cache, Moose.Moose_Frame_Data, Moose.Moose_Palette);

   if SDL.Initialise = True then
      SDL.Video.Windows.Makers.Create (Win      => W,
                                       Title    => "Stream (Moose animation)",
                                       Position => SDL.Natural_Coordinates'(X => 100, Y => 100),
                                       Size     => Moose.Moose_Size * 4,
                                       Flags    => SDL.Video.Windows.Resizable);

      SDL.Video.Renderers.Makers.Create (Renderer, W);

      SDL.Video.Textures.Makers.Create (Tex      => Texture,
                                        Renderer => Renderer,
                                        Format   => SDL.Video.Pixel_Formats.Pixel_Format_ARGB_8888,
                                        Kind     => SDL.Video.Textures.Streaming,
                                        Size     => Moose.Moose_Size);

      --  Set next frame delay target using monotonic clock time
      Loop_Start_Time_Goal := Ada.Real_Time.Clock;

      SDL.Log.Put_Debug ("Frame duration: " & Ada.Real_Time.To_Duration (Frame_Duration)'Img);

      --  Main loop.
      declare
         Event    : SDL.Events.Events.Events;
         Finished : Boolean := False;

         use type SDL.Events.Keyboards.Key_Codes;
         use type SDL.Events.Keyboards.Scan_Codes;
         use type Ada.Real_Time.Time;
         use type Ada.Real_Time.Time_Span;
         use type Moose.Moose_Frames;
      begin
         loop
            Loop_Start_Time_Goal := Loop_Start_Time_Goal + Frame_Duration;
            --  Calculate goal time for the next 50 Hz frame

            delay until Loop_Start_Time_Goal;

            Loop_Start_Time_Real := Ada.Real_Time.Clock;

            Loop_Delay_Overhead_Time := Loop_Start_Time_Real -
              Loop_Start_Time_Goal;

            Loop_Delay_Overhead_Average := (Loop_Delay_Overhead_Average +
                                              Loop_Delay_Overhead_Time) / 2;

            Loop_Iterator := Loop_Iterator + 1;

            if Loop_Iterator mod 256 = 0 then
               SDL.Log.Put_Debug ("Loop_Delay_Overhead_Time: " &
                                    Ada.Real_Time.To_Duration (Loop_Delay_Overhead_Time)'Img);
               SDL.Log.Put_Debug ("Loop_Delay_Overhead_Average: " &
                                    Ada.Real_Time.To_Duration (Loop_Delay_Overhead_Average)'Img);
            end if;

            while SDL.Events.Events.Poll (Event) loop
               case Event.Common.Event_Type is
                  when SDL.Events.Quit =>
                     Finished := True;

                  when SDL.Events.Keyboards.Key_Down =>
                     if Event.Keyboard.Key_Sym.Key_Code = SDL.Events.Keyboards.Code_Escape then
                        Finished := True;
                        --  elsif Event.Keyboard.Key_Sym.Scan_Code = SDL.Events.Keyboards.Scan_Code_Left then
                        --     if Moose_Frame = Moose_Frames'First then
                        --        Moose_Frame := Moose_Frames'Last;
                        --     else
                        --        Moose_Frame := Moose_Frame - 1;
                        --     end if;
                        --  elsif Event.Keyboard.Key_Sym.Scan_Code = SDL.Events.Keyboards.Scan_Code_Right then
                        --     if Moose_Frame = Moose_Frames'Last then
                        --        Moose_Frame := Moose_Frames'First;
                        --     else
                        --        Moose_Frame := Moose_Frame + 1;
                        --     end if;
                     elsif Event.Keyboard.Key_Sym.Scan_Code = SDL.Events.Keyboards.Scan_Code_Left then
                        Left_Cursor_Down := True;
                     elsif Event.Keyboard.Key_Sym.Scan_Code = SDL.Events.Keyboards.Scan_Code_Right then
                        Right_Cursor_Down := True;
                     end if;

                  when SDL.Events.Keyboards.Key_Up =>
                     if Event.Keyboard.Key_Sym.Scan_Code = SDL.Events.Keyboards.Scan_Code_Left then
                        Left_Cursor_Down := False;
                     elsif Event.Keyboard.Key_Sym.Scan_Code = SDL.Events.Keyboards.Scan_Code_Right then
                        Right_Cursor_Down := False;
                     end if;

                  when others =>
                     null;
               end case;
            end loop;

            --  Update the animation every 4 frames (36 Hz)

            if Loop_Iterator mod 4 = 0 then
               if Left_Cursor_Down and not Right_Cursor_Down then
                  Moose.Moose_Frame := Moose.Moose_Frame - 1;
               elsif Right_Cursor_Down and not Left_Cursor_Down then
                  Moose.Moose_Frame := Moose.Moose_Frame + 1;
               end if;
            end if;

            Lock (Texture, Pixels);

            declare
               function To_Address is new Ada.Unchecked_Conversion (Source => SDL.Video.Pixels.ARGB_8888_Access.Pointer,
                                                                    Target => System.Address);

               Actual_Pixels    : Moose.Texture_2D_Array
                 (1 .. Moose.Moose_Size.Height,
                  1 .. Moose.Moose_Size.Width) with
                    Address => To_Address (Pixels);
            begin
               Actual_Pixels := Moose.Cache (Moose.Moose_Frame);
            end;

            Texture.Unlock;

            Renderer.Clear;
            Renderer.Copy (Texture);
            Renderer.Present;

            exit when Finished;
         end loop;
      end;

      W.Finalize;
      SDL.Finalise;
   end if;
end Stream2;
