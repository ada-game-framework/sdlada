--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
--  SDL.Error
--
--  Error message handling.
--------------------------------------------------------------------------------------------------------------------
package SDL.Error is
   pragma Preelaborate;

   procedure Clear with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_ClearError";

   procedure Set (S : in String);

   function Get return String;
end SDL.Error;
