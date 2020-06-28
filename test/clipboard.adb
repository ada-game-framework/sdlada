with SDL;
with SDL.Clipboard;
with SDL.Log;
with SDL.Video.Windows;
with SDL.Video.Windows.Makers;

procedure Clipboard is
   W : SDL.Video.Windows.Window;
begin
   SDL.Log.Set (Category => SDL.Log.Application, Priority => SDL.Log.Debug);

   if SDL.Initialise = True then
      SDL.Video.Windows.Makers.Create (Win      => W,
                                       Title    => "Test SDLAda 2.0 - हिन्दी समाचार",
                                       Position => SDL.Natural_Coordinates'(X => 100, Y => 100),
                                       Size     => SDL.Positive_Sizes'(800, 640));

      delay 2.0;

      if SDL.Clipboard.Is_Empty = True then
         SDL.Log.Put_Debug ("Clipboard is empty");
      end if;

      SDL.Clipboard.Set ("Hello");

      SDL.Log.Put_Debug ("Text on clipboard: " & SDL.Clipboard.Get);
   end if;

   SDL.Finalise;
end Clipboard;
