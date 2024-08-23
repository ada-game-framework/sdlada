--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
--  SDL_Suites
--
--  Tests to check the Ada 2012 bindings for correctness.
--------------------------------------------------------------------------------------------------------------------
with AUnit.Test_Suites;

package SDL_Suites is
   function Suite return AUnit.Test_Suites.Access_Test_Suite;
end SDL_Suites;
