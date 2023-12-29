--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
--  SDL.Inputs.Joysticks.Makers
--------------------------------------------------------------------------------------------------------------------
package SDL.Inputs.Joysticks.Makers is
   pragma Preelaborate;

   function Create (Device : in Devices) return Joystick;
   procedure Create (Device : in Devices; Actual_Stick : out Joystick);
end SDL.Inputs.Joysticks.Makers;
