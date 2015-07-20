--------------------------------------------------------------------------------------------------------------------
--  Copyright (c) 2014 Luke A. Guest
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
--  SDL.Events.Joysticks
--
--  Joystick specific events.
--------------------------------------------------------------------------------------------------------------------
with Interfaces;

package SDL.Events.Joysticks is
   --  Joystick events.
   Axis_Motion       : constant Event_Types := 16#0000_0600#;
   Ball_Motion       : constant Event_Types := Axis_Motion + 1;
   Hat_Motion        : constant Event_Types := Axis_Motion + 2;
   Button_Down       : constant Event_Types := Axis_Motion + 3;
   Button_Up         : constant Event_Types := Axis_Motion + 4;
   Device_Added      : constant Event_Types := Axis_Motion + 5;
   Device_Removed    : constant Event_Types := Axis_Motion + 6;

   type IDs is range -2 ** 31 .. 2 ** 31 - 1 with
     Convention => C,
     Size       => 32;

   type Axes is range 0 .. 255 with
     Convention => C,
     Size       => 8;

   type Axes_Values is range -32768 .. 32767 with
     Convention => C,
     Size       => 16;

   type Axis_Events is
      record
         Event_Type : Event_Types;  --  Will be set to Axis_Motion.
         Time_Stamp : Time_Stamps;

         Which      : IDs;
         Axis       : Axes;
         Padding_1  : Padding_8;
         Padding_2  : Padding_8;
         Padding_3  : Padding_8;
         Value      : Axes_Values;
         Padding_4  : Padding_16;
      end record with
     Convention => C;

   type Balls is range 0 .. 255 with
     Convention => C,
     Size       => 8;

   type Ball_Values is range -32768 .. 32767 with
     Convention => C,
     Size       => 16;

   type Ball_Events is
      record
         Event_Type : Event_Types;  --  Will be set to Ball_Motion.
         Time_Stamp : Time_Stamps;

         Which      : IDs;
         Ball       : Balls;
         Padding_1  : Padding_8;
         Padding_2  : Padding_8;
         Padding_3  : Padding_8;
         X_Relative : Ball_Values;
         Y_Relative : Ball_Values;
      end record with
     Convention => C;

   type Hats is range 0 .. 255 with
     Convention => C,
     Size       => 8;

   type Hat_Positions is mod 2 ** 8 with
     Convention => C,
     Size       => 8;

   Hat_Centred    : constant Hat_Positions := 0;
   Hat_Up         : constant Hat_Positions := 1;
   Hat_Right      : constant Hat_Positions := 2;
   Hat_Down       : constant Hat_Positions := 4;
   Hat_Left       : constant Hat_Positions := 8;
   Hat_Right_Up   : constant Hat_Positions := Hat_Right or Hat_Up;
   Hat_Right_Down : constant Hat_Positions := Hat_Right or Hat_Down;
   Hat_Left_Up    : constant Hat_Positions := Hat_Left or Hat_Up;
   Hat_Left_Down  : constant Hat_Positions := Hat_Left or Hat_Down;

   type Hat_Events is
      record
         Event_Type : Event_Types;  --  Will be set to Hat_Motion.
         Time_Stamp : Time_Stamps;

         Which      : IDs;
         Hat        : Hats;
         Position   : Hat_Positions;
         Padding_1  : Padding_8;
         Padding_2  : Padding_8;
      end record with
     Convention => C;

   type Buttons is range 0 .. 255 with
     Convention => C,
     Size       => 8;

   type Button_Events is
      record
         Event_Type : Event_Types;  --  Will be set to Button_Down or Button_Up.
         Time_Stamp : Time_Stamps;

         Which      : IDs;
         Button     : Buttons;
         State      : Button_State;
         Padding_1  : Padding_8;
         Padding_2  : Padding_8;
      end record with
     Convention => C;

   type Device_Events is
      record
         Event_Type : Event_Types;  --  Will be set to Device_Added or Device_Removed.
         Time_Stamp : Time_Stamps;

         Which      : IDs;
      end record with
     Convention => C;

   --  Update the joystick event data. This is called by the event loop.
   procedure Update with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_JoystickUpdate";

   function Is_Polling_Enabled return Boolean;

   procedure Enable_Polling with
     Inline => True;

   procedure Disable_Polling with
     Inline => True;
private
   for Axis_Events use
      record
         Event_Type at 0 * SDL.Word range  0 .. 31;
         Time_Stamp at 1 * SDL.Word range  0 .. 31;

         Which      at 2 * SDL.Word range  0 .. 31;
         Axis       at 3 * SDL.Word range  0 ..  7;
         Padding_1  at 3 * SDL.Word range  8 .. 15;
         Padding_2  at 3 * SDL.Word range 16 .. 23;
         Padding_3  at 3 * SDL.Word range 24 .. 31;
         Value      at 4 * SDL.Word range  0 .. 15;
         Padding_4  at 4 * SDL.Word range 16 .. 31;
      end record;

   for Ball_Events use
      record
         Event_Type at 0 * SDL.Word range  0 .. 31;
         Time_Stamp at 1 * SDL.Word range  0 .. 31;

         Which      at 2 * SDL.Word range  0 .. 31;
         Ball       at 3 * SDL.Word range  0 ..  7;
         Padding_1  at 3 * SDL.Word range  8 .. 15;
         Padding_2  at 3 * SDL.Word range 16 .. 23;
         Padding_3  at 3 * SDL.Word range 24 .. 31;
         X_Relative at 4 * SDL.Word range  0 .. 15;
         Y_Relative at 4 * SDL.Word range 16 .. 31;
      end record;

   for Hat_Events use
      record
         Event_Type at 0 * SDL.Word range  0 .. 31;
         Time_Stamp at 1 * SDL.Word range  0 .. 31;

         Which      at 2 * SDL.Word range  0 .. 31;
         Hat        at 3 * SDL.Word range  0 ..  7;
         Position   at 3 * SDL.Word range  8 .. 15;
         Padding_1  at 3 * SDL.Word range 16 .. 23;
         Padding_2  at 3 * SDL.Word range 24 .. 31;
      end record;

   for Button_Events use
      record
         Event_Type at 0 * SDL.Word range  0 .. 31;
         Time_Stamp at 1 * SDL.Word range  0 .. 31;

         Which      at 2 * SDL.Word range  0 .. 31;
         Button     at 3 * SDL.Word range  0 ..  7;
         State      at 3 * SDL.Word range  8 .. 15;
         Padding_1  at 3 * SDL.Word range 16 .. 23;
         Padding_2  at 3 * SDL.Word range 24 .. 31;
      end record;

   for Device_Events use
      record
         Event_Type at 0 * SDL.Word range  0 .. 31;
         Time_Stamp at 1 * SDL.Word range  0 .. 31;

         Which      at 2 * SDL.Word range  0 .. 31;
      end record;
end SDL.Events.Joysticks;
