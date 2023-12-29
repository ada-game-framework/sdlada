--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
--  SDL.Inputs.Joysticks.Game_Controllers.Makers
--------------------------------------------------------------------------------------------------------------------
package SDL.Inputs.Joysticks.Game_Controllers.Makers is
   pragma Preelaborate;

   function Create (Device : in Devices) return Game_Controller;
   procedure Create (Device : in Devices; Actual_Controller : out Game_Controller);
end SDL.Inputs.Joysticks.Game_Controllers.Makers;
