--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
--  SDL.Inputs.Joysticks.Makers
--------------------------------------------------------------------------------------------------------------------
with Interfaces.C;
private with SDL.C_Pointers;

package body SDL.Inputs.Joysticks.Makers is
   package C renames Interfaces.C;

   function SDL_Joystick_Open (Device : in C.int) return SDL.C_Pointers.Joystick_Pointer with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_JoystickOpen";

   function Create (Device : in Devices) return Joystick is
   begin
      return J : constant Joystick := (Ada.Finalization.Limited_Controlled with
                                         Internal => SDL_Joystick_Open (C.int (Device) - 1), Owns => True) do
         null;
      end return;
   end Create;

   procedure Create (Device : in Devices; Actual_Stick : out Joystick) is
   begin
      Actual_Stick.Internal := SDL_Joystick_Open (C.int (Device) - 1);
   end Create;
end SDL.Inputs.Joysticks.Makers;
