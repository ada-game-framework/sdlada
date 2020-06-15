with SDL;
with SDL.Events.Events;
with SDL.Events.Keyboards;
with SDL.Inputs.Mice;
with SDL.Log;
with SDL.Video.Pixel_Formats;
with SDL.Video.Renderers.Makers;
with SDL.Video.Textures.Makers;
with SDL.Video.Windows.Makers;

procedure Mouse is
   Window_Size : constant SDL.Positive_Sizes := SDL.Positive_Sizes'(800, 640);

   W           : SDL.Video.Windows.Window;
   Renderer    : SDL.Video.Renderers.Renderer;
   Texture     : SDL.Video.Textures.Texture;
begin
   SDL.Log.Set (Category => SDL.Log.Application, Priority => SDL.Log.Debug);

   if SDL.Initialise = True then
      SDL.Video.Windows.Makers.Create (Win      => W,
                                       Title    => "Mouse",
                                       Position => SDL.Natural_Coordinates'(X => 300, Y => 300),
                                       Size     => Window_Size,
                                       Flags    => SDL.Video.Windows.Resizable);

      SDL.Video.Renderers.Makers.Create (Renderer, W);

      SDL.Video.Textures.Makers.Create (Tex      => Texture,
                                        Renderer => Renderer,
                                        Format   => SDL.Video.Pixel_Formats.Pixel_Format_ARGB_8888,
                                        Kind     => SDL.Video.Textures.Streaming,
                                        Size     => Window_Size);

      --  Main loop.
      declare
         Event       : SDL.Events.Events.Events;
         Finished    : Boolean := False;
         Mouse_Shown : Boolean := True;
         Warp_Rel    : Boolean := True;
         Warp_Screen : Boolean := False;
      begin
         loop
            while SDL.Events.Events.Poll (Event) loop
               case Event.Common.Event_Type is
                  when SDL.Events.Quit =>
                     Finished := True;

                  when SDL.Events.Keyboards.Key_Down =>
                     case Event.Keyboard.Key_Sym.Key_Code is
                        when SDL.Events.Keyboards.Code_Escape =>
                           Finished := True;

                        when SDL.Events.Keyboards.Code_M =>
                           Mouse_Shown := not Mouse_Shown;

                           SDL.Inputs.Mice.Show_Cursor (Mouse_Shown);

                           SDL.Log.Put_Debug ("Mouse Shown        : " & Boolean'Image (Mouse_Shown));

                        when SDL.Events.Keyboards.Code_R =>
                           Warp_Rel := True;

                           SDL.Log.Put_Debug ("Mouse warp relative: " & Boolean'Image (Warp_Rel));

                        when SDL.Events.Keyboards.Code_A =>
                           Warp_Rel := False;

                           SDL.Log.Put_Debug ("Mouse warp relative: " & Boolean'Image (Warp_Rel));

                        when SDL.Events.Keyboards.Code_W =>
                           SDL.Log.Put_Debug ("Warping mouse!");

                           if Warp_Screen then
                              SDL.Inputs.Mice.Warp ((0, 0));
                           else
                              SDL.Inputs.Mice.Warp (W, (0, 0));
                           end if;

                        when SDL.Events.Keyboards.Code_S =>
                           Warp_Screen := not Warp_Screen;

                           SDL.Log.Put_Debug ("Mouse warp to " & (if Warp_Screen then "screen!" else "window!"));

                        when others =>
                           null;
                     end case;

                  when others =>
                     null;
               end case;
            end loop;

            Renderer.Clear;
            Renderer.Copy (Texture);
            Renderer.Present;

            exit when Finished;
         end loop;
      end;

      W.Finalize;
      SDL.Finalise;
   end if;
end Mouse;
