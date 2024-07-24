--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
--  SDL.Timers
--
--  SDL time management routines and data types.
--------------------------------------------------------------------------------------------------------------------
with Interfaces;

package SDL.Timers is
   pragma Pure;

   type Milliseconds_Long is new Interfaces.Unsigned_64;
   type Milliseconds is new Interfaces.Unsigned_32;

   --  Return the number of milliseconds since the SDL library initialization.
   function Ticks return Milliseconds with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_GetTicks",
     Obsolescent;

   function Ticks return Milliseconds_Long with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_GetTicks64";

   --  Wait a specified number of milliseconds before returning.
   procedure Wait_Delay (MS : Milliseconds) with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_Delay";
end SDL.Timers;
