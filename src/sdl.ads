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
--  SDL
--
--  Ada 2012 bindings to the SDL 2.x.y library.
--------------------------------------------------------------------------------------------------------------------
with Interfaces.C;

package SDL is
   pragma Preelaborate;

   package C renames Interfaces.C;

   use type C.int;

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

   --  Coordinates are for positioning things.
   subtype Coordinate is C.int;
   subtype Natural_Coordinate is Coordinate range 0 .. Coordinate'Last;
   subtype Positive_Coordinate is Coordinate range 1 .. Coordinate'Last;

   Centre_Coordinate : constant Coordinate := 0;

   type Coordinates is
      record
         X : SDL.Coordinate;
         Y : SDL.Coordinate;
      end record with
     Convention => C;

   Zero_Coordinate : constant Coordinates := (others => 0);

   subtype Natural_Coordinates is Coordinates with
     Dynamic_Predicate =>
       Natural_Coordinates.X >= Natural_Coordinate'First and Natural_Coordinates.Y >= Natural_Coordinate'First;

   subtype Positive_Coordinates is Coordinates with
     Dynamic_Predicate =>
       Positive_Coordinates.X >= Positive_Coordinate'First and Positive_Coordinates.Y >= Positive_Coordinate'First;

   --  Dimensions are for sizing things.
   subtype Dimension is C.int;
   subtype Natural_Dimension is Dimension range 0 .. Dimension'Last;
   subtype Positive_Dimension is Dimension range 1 .. Dimension'Last;

   type Sizes is
      record
         Width  : Dimension;
         Height : Dimension;
      end record with
     Convention => C;

   Zero_Size : constant Sizes := (others => Natural_Dimension'First);

   subtype Natural_Sizes is Sizes with
     Dynamic_Predicate => Natural_Sizes.Width >= 0 and Natural_Sizes.Height >= 0;

   subtype Positive_Sizes is Sizes with
     Dynamic_Predicate => Positive_Sizes.Width >= 1 and Positive_Sizes.Height >= 1;

   function "*" (Left : in Sizes; Scale : in Positive_Dimension) return Sizes is
     (Sizes'(Width => Left.Width * Scale, Height => Left.Height * Scale));

   function "/" (Left : in Sizes; Scale : in Positive_Dimension) return Sizes is
     (Sizes'(Width => Left.Width / Scale, Height => Left.Height / Scale));

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

   --  These constants are internal to the events system.
   SDL_Query   : constant C.int := -1;
   SDL_Ignore  : constant C.int :=  0;
   SDL_Disable : constant C.int :=  0;
   SDL_Enable  : constant C.int :=  1;
end SDL;
