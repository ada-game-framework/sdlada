--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
with Interfaces.C;
private with SDL.C_Pointers;

package body SDL.Inputs.Joysticks.Game_Controllers.Makers is
   package C renames Interfaces.C;

   function SDL_Game_Controller_Open (Device : in C.int) return SDL.C_Pointers.Game_Controller_Pointer with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_GameControllerOpen";

   function Create (Device : in Devices) return Game_Controller is
   begin
      return J : constant Game_Controller := (Ada.Finalization.Limited_Controlled with
                                                Internal => SDL_Game_Controller_Open (C.int (Device) - 1), Owns => True)
      do
         null;
      end return;
   end Create;

   procedure Create (Device : in Devices; Actual_Controller : out Game_Controller) is
   begin
      Actual_Controller.Internal := SDL_Game_Controller_Open (C.int (Device) - 1);
   end Create;
end SDL.Inputs.Joysticks.Game_Controllers.Makers;
