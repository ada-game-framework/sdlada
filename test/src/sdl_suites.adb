--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
--  SDL_Suites
--
--  Tests to check the Ada 2012 bindings for correctness.
--------------------------------------------------------------------------------------------------------------------
with AUnit; use AUnit;
with Pixel_Format_Test_Cases;
with Colour_Test_Cases;

package body SDL_Suites is
   use Test_Suites;

   Pixel_Tests  : aliased Pixel_Format_Test_Cases.Pixel_Format_Test_Case;
   Colour_Tests : aliased Colour_Test_Cases.Colour_Test_Case;

   function Suite return Access_Test_Suite is
      Result : constant Access_Test_Suite := AUnit.Test_Suites.New_Suite;
   begin
      Result.Add_Test (Pixel_Tests'Access);
      Result.Add_Test (Colour_Tests'Access);

      return Result;
   end Suite;
end SDL_Suites;
