with SDL;
with SDL.Error;
with SDL.Log;
with SDL.Video.Windows;
with SDL.Versions;
with System;

procedure Test is
   W              : SDL.Video.Windows.Window;
   Total_Drivers  : Positive := SDL.Video.Total_Drivers;
   Linked_Version : SDL.Versions.Version;
begin
   SDL.Log.Set (Category => SDL.Log.Application, Priority => SDL.Log.Debug);

   SDL.Versions.Linked_With (Info => Linked_Version);

   SDL.Log.Put_Debug ("Revision       : " & SDL.Versions.Revision);
   SDL.Log.Put_Debug ("Linked with    : " & SDL.Versions.Version_Level'Image (Linked_Version.Major) &
                        "." & SDL.Versions.Version_Level'Image (Linked_Version.Minor) &
                        "." & SDL.Versions.Version_Level'Image (Linked_Version.Patch));
   SDL.Log.Put_Debug ("Compiled with  : " & SDL.Versions.Version_Level'Image (SDL.Versions.Compiled_Major) &
                        "." & SDL.Versions.Version_Level'Image (SDL.Versions.Compiled_Minor) &
                        "." & SDL.Versions.Version_Level'Image (SDL.Versions.Compiled_Patch));
   SDL.Log.Put_Debug ("Bit Order      : " & System.Bit_Order'Image (SDL.Video.Windows.Window'Bit_Order));
   SDL.Log.Put_Debug ("Total drivers  : " & Positive'Image (Total_Drivers));

   for Index in Positive'First .. Total_Drivers loop
      SDL.Log.Put_Debug ("Driver (" & Positive'Image (Index) & ")    : " & SDL.Video.Driver_Name (Natural (Index)));
   end loop;

   if SDL.Initialise = True then
      SDL.Log.Put_Debug ("Current driver : " & SDL.Video.Current_Driver_Name);
      SDL.Log.Put_Debug ("Total displays : " & Positive'Image (SDL.Video.Total_Displays));

      SDL.Error.Clear;

      SDL.Log.Put_Debug ("Error          : " & SDL.Error.Get);

      W.Create (Title => "Test SDLAda 2.0 - हिन्दी समाचार", X => 100, Y => 100, Width => 800, Height => 640);

      SDL.Log.Put_Debug ("Window Grabbed : " & Boolean'Image (W.Is_Grabbed));

      W.Set_Grabbed;

      SDL.Log.Put_Debug ("Window Grabbed : " & Boolean'Image (W.Is_Grabbed));
      SDL.Log.Put_Debug ("Window ID      : " & SDL.Video.Windows.ID'Image (W.Get_ID));
      SDL.Log.Put_Debug ("Window Title   : " & W.Get_Title);

      --  W.Set_Mode (SDL.Video.Windows.Full_Screen);

      delay 10.0;

      W.Finalize;
      SDL.Finalise;
   end if;
end Test;
