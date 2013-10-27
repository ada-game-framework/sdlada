with SDL;
with SDL.Error;
with SDL.Log;
with SDL.Versions;

procedure Version is
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
   SDL.Finalise;
end Version;
