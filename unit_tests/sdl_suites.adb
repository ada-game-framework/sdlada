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
