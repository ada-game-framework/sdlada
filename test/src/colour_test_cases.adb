--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
--  Colour_Test_Cases
--------------------------------------------------------------------------------------------------------------------
with AUnit.Assertions; use AUnit.Assertions;

package body Colour_Test_Cases is
   overriding
   function Name (Test : Colour_Test_Case) return Message_String is
      pragma Unreferenced (Test);  --  TODO: Fix me!
   begin
      return Format ("Colour test");
   end Name;

   overriding
   procedure Run_Test (Test : in out Colour_Test_Case) is
      pragma Unreferenced (Test);  --  TODO: Fix me!

      use type SDL.Video.Palettes.Colour_Component;

      Colour : constant SDL.Video.Palettes.Colour := (Red => 16#FF#, Green => 16#DD#, Blue => 16#AA#, Alpha => 16#88#);
   begin
      Assert (Colour.Red = C_Test.Red, "Red values do not match");
      Assert (Colour.Green = C_Test.Green, "Green values do not match");
      Assert (Colour.Blue = C_Test.Blue, "Blue values do not match");
      Assert (Colour.Alpha = C_Test.Alpha, "Alpha values do not match");
   end Run_Test;
end Colour_Test_Cases;
