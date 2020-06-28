--------------------------------------------------------------------------------------------------------------------
--  Copyright (c) 2013-2020, Luke A. Guest
--
--  This software is provided 'as-is', without any express or implied
--  warranty. In no event will the authors be held liable for any damages
--  arising from the use of this software.
--
--  Permission is granted to anyone to use this software for any purpose,
--  including commercial applications, and to alter it and redistribute it
--  freely, subject to the following restrictions:
--
--     1. The origin of this software must not be misrepresented; you must not
--     claim that you wrote the original software. If you use this software
--     in a product, an acknowledgment in the product documentation would be
--     appreciated but is not required.
--
--     2. Altered source versions must be plainly marked as such, and must not be
--     misrepresented as being the original software.
--
--     3. This notice may not be removed or altered from any source
--     distribution.
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
