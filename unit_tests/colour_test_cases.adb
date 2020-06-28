--------------------------------------------------------------------------------------------------------------------
--  Copyright (c) 2013-2020, Luke A. Guest
--
--  This software is provided 'as-is', without any express or implied
--  warranty. In no event will the authors be held liable for any damages
--  arising from the use of this software.
--
--  Permission is granted to anyone to use this software for any purpose,
--  including commercial applications, and to alter it and redistribute it
--  freely, subject to the following restrictions:
--
--     1. The origin of this software must not be misrepresented; you must not
--     claim that you wrote the original software. If you use this software
--     in a product, an acknowledgment in the product documentation would be
--     appreciated but is not required.
--
--     2. Altered source versions must be plainly marked as such, and must not be
--     misrepresented as being the original software.
--
--     3. This notice may not be removed or altered from any source
--     distribution.
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
