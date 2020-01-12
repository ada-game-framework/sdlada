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
--
--  Tests to check whether the memory layout of the pixel formats is correct.
--------------------------------------------------------------------------------------------------------------------
with AUnit; use AUnit;
with AUnit.Simple_Test_Cases;
with SDL.Video.Palettes;

package Colour_Test_Cases is
   type Colour_Test_Case is new AUnit.Simple_Test_Cases.Test_Case with null record;

   overriding
   function Name (Test : Colour_Test_Case) return Message_String;

   overriding
   procedure Run_Test (Test : in out Colour_Test_Case);
private
   C_Test : constant SDL.Video.Palettes.Colour with
     Import        => True,
     Convention    => C,
     External_Name => "test_colour";
end Colour_Test_Cases;
