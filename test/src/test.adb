--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
--  Unit_Tests
--
--  Test runner.
--------------------------------------------------------------------------------------------------------------------
with AUnit.Run;
with AUnit.Reporter.Text;
with SDL_Suites;

procedure Test is
   procedure Run is new AUnit.Run.Test_Runner (SDL_Suites.Suite);

   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Reporter.Set_Use_ANSI_Colors (True);

   Run (Reporter);
end Test;
