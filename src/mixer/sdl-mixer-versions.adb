--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------

package body SDL.Mixer.Versions is
   procedure Linked_With (Info : in out SDL.Versions.Version) is
      function Mix_Linked_Version return access SDL.Versions.Version with
        Import        => True,
        Convention    => C,
        External_Name => "Mix_Linked_Version";
   begin
      Info := Mix_Linked_Version.all;
   end Linked_With;
end SDL.Mixer.Versions;
