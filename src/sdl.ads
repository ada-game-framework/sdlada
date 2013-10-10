--                              -*- Mode: Ada -*-
--  Filename        : sdl.ads
--  Description     : Ada 2012 bindings to the SDL 2.0 library.
--  Author          : Luke A. Guest
--  Created On      : Sat Sep 21 14:29:46 2013
with Interfaces.C;

package SDL is
   type Init_Flags is mod 2 ** 32 with
     Convention => C;

   Timer           : constant Init_Flags := 16#0000_0001#;
   Audio           : constant Init_Flags := 16#0000_0010#;
   Screen          : constant Init_Flags := 16#0000_0020#;
   Joystick        : constant Init_Flags := 16#0000_0200#;
   Haptic          : constant Init_Flags := 16#0000_1000#;
   Game_Controller : constant Init_Flags := 16#0000_2000#;
   Events          : constant Init_Flags := 16#0000_4000#;
   No_Parachute    : constant Init_Flags := 16#0010_0000#;
   Everything      : constant Init_Flags :=
     Timer or Audio or Screen or Joystick or Haptic or Game_Controller or
     Events or No_Parachute;

   function Initialise (Flags : in Init_Flags := Everything) return Boolean;

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

   function Was_Initialised return Init_Flags with
       Import        => True,
       Convention    => C,
       External_Name => "SDL_WasInit";
private
   Success : constant Interfaces.C.int := 0;
end SDL;
