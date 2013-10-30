with SDL;
with SDL.Clipboard;
with SDL.Error;
with SDL.Log;
with SDL.Video.Windows;

procedure Clipboard is
   W : SDL.Video.Windows.Window;
begin
   SDL.Log.Set (Category => SDL.Log.Application, Priority => SDL.Log.Debug);

   if SDL.Initialise = True then
      W.Create (Title => "Test SDLAda 2.0 - हिन्दी समाचार", X => 100, Y => 100, Width => 800, Height => 640);

      delay 2.0;

      if SDL.Clipboard.Is_Empty = True then
         SDL.Log.Put_Debug ("Clipboard is empty");
      end if;

      SDL.Clipboard.Set ("Hello");

      SDL.Log.Put_Debug ("Text on clipboard: " & SDL.Clipboard.Get);
   end if;

   SDL.Finalise;
end Clipboard;
