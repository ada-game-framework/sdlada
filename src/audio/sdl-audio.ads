--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
with Interfaces.C;

package SDL.Audio is
   pragma Preelaborate;

   package C renames Interfaces.C;

   Audio_Error : exception;

   --  Audio drivers.
   --  TODO: SDL3 - Removes Initialise/Finalise
   function Initialise (Name : in String := "") return Boolean;

   procedure Finalise with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_AudioQuit";

   function Total_Drivers return Positive;

   function Driver_Name (Index : in Positive) return String;

   function Current_Driver_Name return String;
end SDL.Audio;
