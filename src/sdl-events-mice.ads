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
--  SDL.Events.Mice
--
--  Mouse specific events.
--------------------------------------------------------------------------------------------------------------------
with Ada.Unchecked_Conversion;
with Interfaces;

package SDL.Events.Mice is
   --  Mouse events.
   Motion               : constant Event_Types := 16#0000_0400#;
   Button_Down          : constant Event_Types := Motion + 1;
   Button_Up            : constant Event_Types := Motion + 2;
   Wheel                : constant Event_Types := Motion + 3;

   type IDs is range 0 .. 2 ** 32 - 1 with
     Convention => C,
     Size       => 32;

   Touch_Mouse_ID : constant IDs := IDs'Last; --  Equals -1 cast to Uint32 in C.

   type Buttons is (Left,
                    Middle,
                    Right,
                    X_1,
                    X_2) with
     Convention => C;

   for Buttons use (Left   => 1,
                    Middle => 2,
                    Right  => 3,
                    X_1    => 4,
                    X_2    => 5);

   type Button_Masks is mod 2 ** 32 with
     Convention => C,
     Size       => 32;

   function Convert is new Ada.Unchecked_Conversion (Source => Interfaces.Unsigned_32, Target => Button_Masks);

   function Left_Mask return Button_Masks is
     (Convert (Interfaces.Shift_Left (1, Buttons'Pos (Left)))) with
   Inline => True;

   function Middle_Mask return Button_Masks is
     (Convert (Interfaces.Shift_Left (1, Buttons'Pos (Middle)))) with
   Inline => True;

   function Right_Mask return Button_Masks is
     (Convert (Interfaces.Shift_Left (1, Buttons'Pos (Right)))) with
   Inline => True;

   function X_1_Mask return Button_Masks is
     (Convert (Interfaces.Shift_Left (1, Buttons'Pos (X_1)))) with
   Inline => True;

   function X_2_Mask return Button_Masks is
     (Convert (Interfaces.Shift_Left (1, Buttons'Pos (X_2)))) with
   Inline => True;

   type Window_Coordinates is range -2 ** 31 .. 2 ** 31 - 1 with
     Convention => C,
     Size       => 32;

   type Screen_Coordinates is range -2 ** 31 .. 2 ** 31 - 1 with
     Convention => C,
     Size       => 32;

   type Movement_Values is range -2 ** 31 .. 2 ** 31 - 1 with
     Convention => C,
     Size       => 32;

   type Motion_Events is
      record
         Event_Type : Event_Types;  --  Will be set to Motion.
         Time_Stamp : Time_Stamps;

         Window     : SDL.Video.Windows.ID;
         Which      : IDs;
         Mask       : Button_Masks;
         X          : Window_Coordinates;
         Y          : Window_Coordinates;
         X_Relative : Movement_Values;
         Y_Relative : Movement_Values;
      end record with
     Convention => C;

   type Button_Clicks is range 0 .. 255 with
     Convention => C,
     Size       => 8;

   type Button_Events is
      record
         Event_Type : Event_Types;  --  Will be set to Button_Up or Button_Down.
         Time_Stamp : Time_Stamps;

         Window     : SDL.Video.Windows.ID;
         Which      : IDs;
         Button     : Buttons;
         State      : Button_State;
         Clicks     : Button_Clicks;
         Padding_1  : Padding_8;
         X          : Window_Coordinates;
         Y          : Window_Coordinates;
      end record with
     Convention => C;

   type Wheel_Values is range -2 ** 31 .. 2 ** 31 - 1 with
     Convention => C,
     Size       => 32;

   type Wheel_Events is
      record
         Event_Type : Event_Types;  --  Will be set to Wheel.
         Time_Stamp : Time_Stamps;

         Window     : SDL.Video.Windows.ID;
         Which      : IDs;
         X          : Wheel_Values;
         Y          : Wheel_Values;
      end record with
     Convention => C;
private
   for Motion_Events use
      record
         Event_Type at 0 * SDL.Word range  0 .. 31;
         Time_Stamp at 1 * SDL.Word range  0 .. 31;

         Window     at 2 * SDL.Word range  0 .. 31;
         Which      at 3 * SDL.Word range  0 .. 31;
         Mask       at 4 * SDL.Word range  0 .. 31;
         X          at 5 * SDL.Word range  0 .. 31;
         Y          at 6 * SDL.Word range  0 .. 31;
         X_Relative at 7 * SDL.Word range  0 .. 31;
         Y_Relative at 8 * SDL.Word range  0 .. 31;
      end record;

   for Button_Events use
      record
         Event_Type at 0 * SDL.Word range  0 .. 31;
         Time_Stamp at 1 * SDL.Word range  0 .. 31;

         Window     at 2 * SDL.Word range  0 .. 31;
         Which      at 3 * SDL.Word range  0 .. 31;
         Button     at 4 * SDL.Word range  0 ..  7;
         State      at 4 * SDL.Word range  8 .. 15;
         Clicks     at 4 * SDL.Word range 16 .. 23;
         Padding_1  at 4 * SDL.Word range 24 .. 31;
         X          at 5 * SDL.Word range  0 .. 31;
         Y          at 6 * SDL.Word range  0 .. 31;
      end record;

   for Wheel_Events use
      record
         Event_Type at 0 * SDL.Word range  0 .. 31;
         Time_Stamp at 1 * SDL.Word range  0 .. 31;

         Window     at 2 * SDL.Word range  0 .. 31;
         Which      at 3 * SDL.Word range  0 .. 31;
         X          at 4 * SDL.Word range  0 .. 31;
         Y          at 5 * SDL.Word range  0 .. 31;
      end record;
end SDL.Events.Mice;
