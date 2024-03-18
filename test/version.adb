--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
with Ada.Strings.Fixed;
with SDL;
with SDL.Log;
with SDL.Versions;

procedure Version is
   package ASF renames Ada.Strings.Fixed;

   Linked_Version : SDL.Versions.Version;
begin
   SDL.Log.Set (Category => SDL.Log.Application, Priority => SDL.Log.Debug);

   SDL.Versions.Linked_With (Info => Linked_Version);

   SDL.Log.Put_Debug ("Revision       : " & SDL.Versions.Revision);
   SDL.Log.Put_Debug ("Linked with    : " & ASF.Trim (Linked_Version.Major'Image, Ada.Strings.Left) &
                        "." & ASF.Trim (Linked_Version.Minor'Image, Ada.Strings.Left) &
                        "." & ASF.Trim (Linked_Version.Patch'Image, Ada.Strings.Left));
   SDL.Log.Put_Debug ("Compiled with  : " & ASF.Trim (SDL.Versions.Compiled_Major'Image, Ada.Strings.Left) &
                        "." & ASF.Trim (SDL.Versions.Compiled_Minor'Image, Ada.Strings.Left) &
                        "." & ASF.Trim (SDL.Versions.Compiled_Patch'Image, Ada.Strings.Left));
   SDL.Finalise;
end Version;
