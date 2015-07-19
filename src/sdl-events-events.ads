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
--  SDL.Events.Events
--
--  Combines all of the various event types into a single variant record to match the union in the SDL library. Not
--  the nicest of names for the package, but it works.
--------------------------------------------------------------------------------------------------------------------
with SDL.Events.Windows;
with SDL.Events.Keyboards;
with SDL.Events.Mice;
with SDL.Events.Joysticks;
with SDL.Events.Controllers;
with SDL.Events.Touches;
with SDL.Events.Files;

package SDL.Events.Events is
   type Event_Selector is (Is_Event,
                           Is_Window_Event,
                           Is_Keyboard_Event,
                           Is_Text_Editing_Event,
                           Is_Text_Input_Event,
                           Is_Mouse_Motion_Event,
                           Is_Mouse_Button_Event,
                           Is_Mouse_Wheel_Event,
                           Is_Joystick_Axis_Event,
                           Is_Joystick_Ball_Event,
                           Is_Joystick_Hat_Event,
                           Is_Joystick_Button_Event,
                           Is_Joystick_Device_Event,
                           Is_Controller_Axis_Event,
                           Is_Controller_Button_Event,
                           Is_Controller_Device_Event,
                           Is_Touch_Finger_Event,
                           Is_Touch_Multi_Gesture_Event,
                           Is_Touch_Dollar_Gesture,
                           Is_Drop_Event);

   type Events (Event_Type : Event_Selector := Is_Event) is
      record
         case Event_Type is
            when Is_Window_Event =>
               Window               : SDL.Events.Windows.Window_Events;

            when Is_Keyboard_Event =>
               Keyboard             : SDL.Events.Keyboards.Keyboard_Events;

            when Is_Text_Editing_Event =>
               Text_Editing         : SDL.Events.Keyboards.Text_Editing_Events;

            when Is_Text_Input_Event =>
               Text_Input           : SDL.Events.Keyboards.Text_Input_Events;

            when Is_Mouse_Motion_Event =>
               Mouse_Motion         : SDL.Events.Mice.Motion_Events;

            when Is_Mouse_Button_Event =>
               Mouse_Button         : SDL.Events.Mice.Button_Events;

            when Is_Mouse_Wheel_Event =>
               Mouse_Wheel          : SDL.Events.Mice.Wheel_Events;

            when Is_Joystick_Axis_Event =>
               Joystick_Axis        : SDL.Events.Joysticks.Axis_Events;

            when Is_Joystick_Ball_Event =>
               Joystick_Ball        : SDL.Events.Joysticks.Ball_Events;

            when Is_Joystick_Hat_Event =>
               Joystick_Hat         : SDL.Events.Joysticks.Hat_Events;

            when Is_Joystick_Button_Event =>
               Joystick_Button      : SDL.Events.Joysticks.Button_Events;

            when Is_Joystick_Device_Event =>
               Joystick_Device      : SDL.Events.Joysticks.Device_Events;

            when Is_Controller_Axis_Event =>
               Controller_Axis      : SDL.Events.Controllers.Axis_Events;

            when Is_Controller_Button_Event =>
               Controller_Button    : SDL.Events.Controllers.Button_Events;

            when Is_Controller_Device_Event =>
               Controller_Device    : SDL.Events.Controllers.Device_Events;

            when Is_Touch_Finger_Event =>
               Touch_Finger         : SDL.Events.Touches.Finger_Events;

            when Is_Touch_Multi_Gesture_Event =>
               Touch_Multi_Gesture  : SDL.Events.Touches.Multi_Gesture_Events;

            when Is_Touch_Dollar_Gesture =>
               Touch_Dollar_Gesture : SDL.Events.Touches.Dollar_Events;

            when Is_Drop_Event =>
               Drop                 : SDL.Events.Files.Drop_Events;

            when others =>
               Common               : Common_Events;
         end case;
      end record with
     Unchecked_Union,
     Convention => C;

   function Poll (Event : out Events) return Boolean;
end SDL.Events.Events;
