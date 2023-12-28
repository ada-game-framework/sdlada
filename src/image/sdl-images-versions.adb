--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------

package body SDL.Images.Versions is

   procedure Linked_With (Info : in out SDL.Versions.Version) is
      function IMG_Linked_Version return access SDL.Versions.Version with
        Import        => True,
        Convention    => C,
        External_Name => "IMG_Linked_Version";

      Data : constant access SDL.Versions.Version := IMG_Linked_Version;
   begin
      Info := Data.all;
   end Linked_With;
end SDL.Images.Versions;
