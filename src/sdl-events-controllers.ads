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
--  SDL.Events.Controllers
--
--  Game controller specific events.
--------------------------------------------------------------------------------------------------------------------
with SDL.Events.Joysticks;

package SDL.Events.Controllers is
   --  Game controller events.
   Axis_Motion     : constant Event_Types := 16#0000_0650#;
   Button_Down     : constant Event_Types := Axis_Motion + 1;
   Button_Up       : constant Event_Types := Axis_Motion + 2;
   Device_Added    : constant Event_Types := Axis_Motion + 3;
   Device_Removed  : constant Event_Types := Axis_Motion + 4;
   Device_Remapped : constant Event_Types := Axis_Motion + 5;

   type Axes is (Invalid,
                 Left_X,
                 Left_Y,
                 Right_X,
                 Right_Y,
                 Left_Trigger,
                 Right_Trigger) with
     Convention => C;

   for Axes use (Invalid       => -1,
                 Left_X        =>  0,
                 Left_Y        =>  1,
                 Right_X       =>  2,
                 Right_Y       =>  3,
                 Left_Trigger  =>  4,
                 Right_Trigger =>  5);

   type Axes_Values is range -32768 .. 32767 with
     Convention => C,
     Size       => 16;

   type Axis_Events is
      record
         Event_Type : Event_Types;  --  Will be set to Axis_Motion.
         Time_Stamp : Time_Stamps;

         Which      : SDL.Events.Joysticks.IDs;
         Axis       : Axes;
         Padding_1  : Padding_8;
         Padding_2  : Padding_8;
         Padding_3  : Padding_8;
         Value      : Axes_Values;
         Padding_4  : Padding_16;
      end record with
     Convention => C;

   type Buttons is (Invalid,
                    A,
                    B,
                    X,
                    Y,
                    Back,
                    Guide,
                    Start,
                    Left_Stick,
                    Right_Stick,
                    Left_Shoulder,
                    Right_Shoulder,
                    Pad_Up,         --  Direction pad buttons.
                    Pad_Down,
                    Pad_Left,
                    Pad_Right) with
     Convention => C;

   for Buttons use (Invalid        => -1,
                    A              =>  0,
                    B              =>  1,
                    X              =>  2,
                    Y              =>  3,
                    Back           =>  4,
                    Guide          =>  5,
                    Start          =>  6,
                    Left_Stick     =>  7,
                    Right_Stick    =>  8,
                    Left_Shoulder  =>  9,
                    Right_Shoulder =>  10,
                    Pad_Up         =>  11,
                    Pad_Down       =>  12,
                    Pad_Left       =>  13,
                    Pad_Right      =>  14);

   type Button_Events is
      record
         Event_Type : Event_Types;  --  Will be set to Button_Down or Button_Up.
         Time_Stamp : Time_Stamps;

         Which      : SDL.Events.Joysticks.IDs;
         Button     : Buttons;
         State      : Button_State;
         Padding_1  : Padding_8;
         Padding_2  : Padding_8;
      end record with
     Convention => C;

   type Device_Events is
      record
         Event_Type : Event_Types;  --  Will be set to Device_Added, Device_Removed or Device_Remapped.
         Time_Stamp : Time_Stamps;

         Which      : SDL.Events.Joysticks.IDs;
      end record with
     Convention => C;
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
end SDL.Events.Controllers;
