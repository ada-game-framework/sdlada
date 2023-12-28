--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------

package body SDL.TTFs.Versions is

   procedure Linked_With (Info : in out SDL.Versions.Version) is
      function TTF_Linked_Version return access SDL.Versions.Version with
        Import        => True,
        Convention    => C,
        External_Name => "TTF_Linked_Version";

      Data : constant access SDL.Versions.Version := TTF_Linked_Version;
   begin
      Info := Data.all;
   end Linked_With;
end SDL.TTFs.Versions;
