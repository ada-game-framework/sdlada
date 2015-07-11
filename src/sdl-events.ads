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
--  SDL.Events
--
--  Mapping to the underlying event handling system.
--
--  WARNING!!!!
--    I wanted to experiment with the event system and possibly hide all this and create an abstraction in another
--    task so as to separate out the events from the main window. This could change. I really don't know yet.
--------------------------------------------------------------------------------------------------------------------
with Ada.Characters.Latin_1;
with Ada.Unchecked_Conversion;
with Interfaces;
with SDL.Video.Windows;

package SDL.Events is
   type Event_Types is mod 2 ** 32 with
     Convention => C;

   type Button_State is (Released, Pressed) with
     Convention => C;

   for Button_State use (Released => 0, Pressed => 1);

   -----------------------------------------------------------------------------------------------------------------
   --  Event types.
   -----------------------------------------------------------------------------------------------------------------

   --  Handled via 'First attribute.
   First_Event                : constant Event_Types := 16#0000_0000#;

   --  Application events.
   Quit                       : constant Event_Types := 16#0000_0100#;

   --  Mobile events.
   App_Terminating            : constant Event_Types := Quit + 1;
   App_Low_Memory             : constant Event_Types := Quit + 2;
   App_Will_Enter_Background  : constant Event_Types := Quit + 3;
   App_Did_Enter_Background   : constant Event_Types := Quit + 4;
   App_Will_Enter_Foreground  : constant Event_Types := Quit + 5;
   App_Did_Enter_Foreground   : constant Event_Types := Quit + 6;

   --  Mouse events.
   Mouse_Motion               : constant Event_Types := 16#0000_0400#;
   Mouse_Button_Down          : constant Event_Types := Mouse_Motion + 1;
   Mouse_Button_Up            : constant Event_Types := Mouse_Motion + 2;
   Mouse_Wheel                : constant Event_Types := Mouse_Motion + 3;

   --  Joystick events.
   Joystick_Axis_Motion       : constant Event_Types := 16#0000_0600#;
   Joystick_Ball_Motion       : constant Event_Types := Joystick_Axis_Motion + 1;
   Joystick_Hat_Motion        : constant Event_Types := Joystick_Axis_Motion + 2;
   Joystick_Button_Down       : constant Event_Types := Joystick_Axis_Motion + 3;
   Joystick_Button_Up         : constant Event_Types := Joystick_Axis_Motion + 4;
   Joystick_Device_Added      : constant Event_Types := Joystick_Axis_Motion + 5;
   Joystick_Device_Removed    : constant Event_Types := Joystick_Axis_Motion + 6;

   --  Game controller events.
   Controller_Axis_Motion     : constant Event_Types := 16#0000_0650#;
   Controller_Button_Down     : constant Event_Types := Controller_Axis_Motion + 1;
   Controller_Button_Up       : constant Event_Types := Controller_Axis_Motion + 2;
   Controller_Device_Added    : constant Event_Types := Controller_Axis_Motion + 3;
   Controller_Device_Removed  : constant Event_Types := Controller_Axis_Motion + 4;
   Controller_Device_Remapped : constant Event_Types := Controller_Axis_Motion + 5;

   --  Touch events.
   Finger_Down                : constant Event_Types := 16#0000_0700#;
   Finger_Up                  : constant Event_Types := Finger_Down + 1;
   Finger_Motion              : constant Event_Types := Finger_Down + 2;

   --  Gesture events.
   Dollar_Gesture             : constant Event_Types := 16#0000_0800#;
   Dollar_Record              : constant Event_Types := Dollar_Gesture + 1;
   Dollar_Multi_Gesture       : constant Event_Types := Dollar_Gesture + 2;

   --  Clipboard events.
   Clipboard_Update           : constant Event_Types := 16#0000_0900#;

   --  Drag and drop events.
   Drop_File                  : constant Event_Types := 16#0000_1000#;

   --  TODO: Audio hot plug events for 2.0.4

   --  User events.
   User                       : constant Event_Types := 16#0000_8000#;

   Last_Event                 : constant Event_Types := 16#0000_FFFF#;

   type Padding_Type is mod 2 ** 8 with
     Convention => C,
     Size       => 8;

   type Time_Stamps is mod 2 ** 32 with
     Convention => C;

   type Common_Events is
      record
         Event_Type : Event_Types;
         Time_Stamp : Time_Stamps;
      end record with
     Convention => C;

   -----------------------------------------------------------------------------------------------------------------
   --  Text editing events
   -----------------------------------------------------------------------------------------------------------------
   subtype Text_Buffers is Interfaces.C.char_array (0 .. 31);

   type Text_Editing_Events is
      record
         Event_Type : Event_Types;           --  Will be set to Text_Editing.
         Time_Stamp : Time_Stamps;

         ID         : SDL.Video.Windows.ID;
         Text       : Text_Buffers;
         Repeat     : Interfaces.Unsigned_8;
         Padding_2  : Padding_Type;
         Padding_3  : Padding_Type;
      end record with
     Convention => C;

   -----------------------------------------------------------------------------------------------------------------
   --  TODO: Text input events
   -----------------------------------------------------------------------------------------------------------------

   -----------------------------------------------------------------------------------------------------------------
   --  TODO: Mouse motion events
   -----------------------------------------------------------------------------------------------------------------

   -----------------------------------------------------------------------------------------------------------------
   --  TODO: Mouse button events
   -----------------------------------------------------------------------------------------------------------------

   -----------------------------------------------------------------------------------------------------------------
   --  TODO: Mouse wheel events
   -----------------------------------------------------------------------------------------------------------------

   -----------------------------------------------------------------------------------------------------------------
   --  TODO: Jostick axis events
   -----------------------------------------------------------------------------------------------------------------

   -----------------------------------------------------------------------------------------------------------------
   --  TODO: Joystick trackball events
   -----------------------------------------------------------------------------------------------------------------

   -----------------------------------------------------------------------------------------------------------------
   --  TODO: Joystick hat events
   -----------------------------------------------------------------------------------------------------------------

   -----------------------------------------------------------------------------------------------------------------
   --  TODO: Joystick button events
   -----------------------------------------------------------------------------------------------------------------

   -----------------------------------------------------------------------------------------------------------------
   --  TODO: Joystick device events
   -----------------------------------------------------------------------------------------------------------------



   -----------------------------------------------------------------------------------------------------------------
   --  TODO: Controller button events
   -----------------------------------------------------------------------------------------------------------------

   -----------------------------------------------------------------------------------------------------------------
   --  TODO: Controller device events
   -----------------------------------------------------------------------------------------------------------------

   -----------------------------------------------------------------------------------------------------------------
   --  TODO: Touch finger events
   -----------------------------------------------------------------------------------------------------------------

   -----------------------------------------------------------------------------------------------------------------
   --  TODO: Multi gesture events
   -----------------------------------------------------------------------------------------------------------------

   -----------------------------------------------------------------------------------------------------------------
   --  TODO: Dollar gesture events
   -----------------------------------------------------------------------------------------------------------------

   -----------------------------------------------------------------------------------------------------------------
   --  TODO: Drop events
   -----------------------------------------------------------------------------------------------------------------

   -----------------------------------------------------------------------------------------------------------------
   --  TODO: User events
   -----------------------------------------------------------------------------------------------------------------

   -----------------------------------------------------------------------------------------------------------------
   --  TODO: System window manager events
   -----------------------------------------------------------------------------------------------------------------

   -----------------------------------------------------------------------------------------------------------------
   --  TODO: Audio events - 2.0.4
   -----------------------------------------------------------------------------------------------------------------


   --     for Text_Editing_Events use
   --        record
   --           Event_Type at 0 * SDL.Word range  0  .. 31;
   --           Time_Stamp at 1 * SDL.Word range  0  .. 31;
   --
   --           ID         at 2 * SDL.Word range  0  .. 31;
   --           State      at 3 * SDL.Word range  0  ..  7;
   --           Repeat     at 3 * SDL.Word range  8  .. 15;
   --           Padding_2  at 3 * SDL.Word range  16 .. 23;
   --           Padding_3  at 3 * SDL.Word range  24 .. 31;
   --        end record;

   procedure Dummy;
end SDL.Events;
