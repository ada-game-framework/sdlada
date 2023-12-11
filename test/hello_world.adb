with Ada.Real_Time; use Ada.Real_Time;

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

   Loop_Start_Time_Goal : Ada.Real_Time.Time;

   Frame_Duration : constant Ada.Real_Time.Time_Span :=
     Ada.Real_Time.Microseconds (16_667);
   --  60 Hz refresh rate (set to anything you like)

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

      --  Set next frame delay target using monotonic clock time
      Loop_Start_Time_Goal := Ada.Real_Time.Clock;

      Main : loop
         --  Limit event loop to 60 Hz using realtime "delay until"
         Loop_Start_Time_Goal := Loop_Start_Time_Goal + Frame_Duration;
         delay until Loop_Start_Time_Goal;

         while SDL.Events.Events.Poll (Event) loop
            if Event.Common.Event_Type = SDL.Events.Quit then
               exit Main;
            end if;

            Renderer.Clear;
            Renderer.Copy (Texture);
            Renderer.Present;
         end loop;
      end loop Main;

      -- Clean up and exit.
      W.Finalize;
      SDL.Finalise;
   end if;
end Hello_World;
