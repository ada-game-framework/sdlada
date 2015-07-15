--------------------------------------------------------------------------------------------------------------------
--  Copyright (c) 2014-2015 Luke A. Guest
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
--  SDL
--
--  Ada 2012 bindings to the SDL 2.x.y library.
--------------------------------------------------------------------------------------------------------------------
with Interfaces.C;

package SDL is
   type Init_Flags is mod 2 ** 32 with
     Convention => C;

   Null_Init_Flags        : constant Init_Flags := 16#0000_0000#;
   Enable_Timer           : constant Init_Flags := 16#0000_0001#;
   Enable_Audio           : constant Init_Flags := 16#0000_0010#;
   Enable_Screen          : constant Init_Flags := 16#0000_0020#;
   Enable_Joystick        : constant Init_Flags := 16#0000_0200#;
   Enable_Haptic          : constant Init_Flags := 16#0000_1000#;
   Enable_Game_Controller : constant Init_Flags := 16#0000_2000#;
   Enable_Events          : constant Init_Flags := 16#0000_4000#;
   Enable_No_Parachute    : constant Init_Flags := 16#0010_0000#;
   Enable_Everything      : constant Init_Flags :=
     Enable_Timer or Enable_Audio or Enable_Screen or Enable_Joystick or Enable_Haptic or
     Enable_Game_Controller or Enable_Events or Enable_No_Parachute;

   function Initialise (Flags : in Init_Flags := Enable_Everything) return Boolean;

   procedure Finalise with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_Quit";

   function Initialise_Sub_System (Flags : in Init_Flags) return Boolean;

   procedure Finalise_Sub_System
     (Flags : in Init_Flags) with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_QuitSubSystem";

   --  Get which sub-systems were initialised.
   function Was_Initialised return Init_Flags;

   --  Check whether a set of sub-systems were initialised.
   function Was_Initialised (Flags : in Init_Flags) return Boolean;
private
   Success   : constant Interfaces.C.int := 0;

   type SDL_Bool is (SDL_False, SDL_True) with
     Convention => C;

   --  The next value is used in mapping the Ada types onto the C types, it is the word size used for all data
   --  in SDL, i.e. all data is 4 byte aligned so it works with 32-bit architectures.
   Word      : constant := 4;
end SDL;
