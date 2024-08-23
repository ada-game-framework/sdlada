--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
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
