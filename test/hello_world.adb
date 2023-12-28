with SDL;
with SDL.Events.Events;
with SDL.Log;
with SDL.Video.Pixel_Formats;
with SDL.Video.Renderers.Makers;
with SDL.Video.Textures.Makers;
with SDL.Video.Windows.Makers;

procedure Hello_World is
   W                : SDL.Video.Windows.Window;
   W_Size           : constant SDL.Positive_Sizes := (800, 640);
   Renderer         : SDL.Video.Renderers.Renderer;
   Texture          : SDL.Video.Textures.Texture;
   Event            : SDL.Events.Events.Events;

   use type SDL.Events.Event_Types;
begin
   SDL.Log.Set (Category => SDL.Log.Application, Priority => SDL.Log.Debug);

   if SDL.Initialise = True then
      SDL.Video.Windows.Makers.Create
        (Win      => W,
         Title    => "Hello, World",
         Position => SDL.Natural_Coordinates'(X => 100, Y => 100),
         Size     => W_Size,
         Flags    => SDL.Video.Windows.Resizable);

      SDL.Video.Renderers.Makers.Create (Renderer, W);

      --  Set the texture to the same size as the window, as the window scales, the texture
      --  will also scale, it will *not* be rebuilt to match the new window size.
      SDL.Video.Textures.Makers.Create
        (Tex      => Texture,
         Renderer => Renderer,
         Format   => SDL.Video.Pixel_Formats.Pixel_Format_ARGB_8888,
         Kind     => SDL.Video.Textures.Streaming,
         Size     => W_Size);

      Main : loop
         while SDL.Events.Events.Poll (Event) loop
            if Event.Common.Event_Type = SDL.Events.Quit then
               exit Main;
            end if;

            Renderer.Clear;
            Renderer.Copy (Texture);
            Renderer.Present;
         end loop;
      end loop Main;

      --  Clean up and exit.
      W.Finalize;
      SDL.Finalise;
   end if;
end Hello_World;
