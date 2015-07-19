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
--  SDL.Events.Touches
--
--  WARNING!! A lot of the data bindings in this specification is guess-work, especially the ranges for things. There
--  also an inconsistency in the usage of Fingers_Touching within SDL itself.
--
--  See:
--    https://bugzilla.libsdl.org/show_bug.cgi?id=3060
--    http://lists.libsdl.org/pipermail/sdl-libsdl.org/2015-July/098468.html
--------------------------------------------------------------------------------------------------------------------
with Interfaces.C;

package SDL.Events.Touches is
   --  Touch events.
   Finger_Down                : constant Event_Types := 16#0000_0700#;
   Finger_Up                  : constant Event_Types := Finger_Down + 1;
   Finger_Motion              : constant Event_Types := Finger_Down + 2;

   --  Gesture events.
   Dollar_Gesture             : constant Event_Types := 16#0000_0800#;
   Dollar_Record              : constant Event_Types := Dollar_Gesture + 1;
   Dollar_Multi_Gesture       : constant Event_Types := Dollar_Gesture + 2;

   --  TODO: Find out if these really should be signed or not, the C version uses Sint64 for both.
   type Touch_IDs is range -1 .. 2 ** 63 - 1 with
     Convention => C,
     Size       => 64;

   type Finger_IDs is range 0 .. 2 ** 63 - 1 with
     Convention => C,
     Size       => 64;

   type Gesture_IDs is range 0 .. 2 ** 63 - 1 with
     Convention => C,
     Size       => 64;

   type Touch_Locations is digits 3 range 0.0 .. 1.0 with
     Convention => C,
     Size       => 32;

   type Touch_Distances is digits 3 range -1.0 .. 1.0 with
     Convention => C,
     Size       => 32;

   type Touch_Pressures is digits 3 range 0.0 .. 1.0 with
     Convention => C,
     Size       => 32;

   type Finger_Events is
      record
         Event_Type : Event_Types;           --  Will be set to Finger_Down, Finger_Up or Finger_Motion.
         Time_Stamp : Time_Stamps;

         Touch_ID   : Touch_IDs;
         Finger_ID  : Finger_IDs;
         X          : Touch_Locations;
         Y          : Touch_Locations;
         Delta_X    : Touch_Distances;
         Delta_Y    : Touch_Distances;
         Pressure   : Touch_Pressures;
      end record with
     Convention => C;

   type Finger_Rotations is digits 3 range -360.0 .. 360.0 with
     Convention => C,
     Size       => 32;

   subtype Finger_Pinches is Interfaces.C.C_float;

   type Fingers_Touching is range 0 .. 2 ** 16 -1 with
     Convention => C,
     Size       => 16;

   type Multi_Gesture_Events is
      record
         Event_Type : Event_Types;           --  Will be set to Dollar_Multi_Gesture.
         Time_Stamp : Time_Stamps;

         Touch_ID   : Touch_IDs;
         Theta      : Finger_Rotations;
         Distance   : Finger_Pinches;
         Centre_X   : Touch_Locations;
         Centre_Y   : Touch_Locations;
         Fingers    : Fingers_Touching;
         Padding    : Padding_16;
      end record with
     Convention => C;

   subtype Dollar_Errors is Interfaces.C.C_float;

   type Dollar_Events is
      record
         Event_Type : Event_Types;           --  Will be set to Dollar_Gesture or Dollar_Record.
         Time_Stamp : Time_Stamps;

         Touch_ID   : Touch_IDs;
         Gesture_ID : Gesture_IDs;
         Fingers    : Fingers_Touching;
         Error      : Dollar_Errors;
         Centre_X   : Touch_Locations;
         Centre_Y   : Touch_Locations;
      end record with
     Convention => C;
private
   for Finger_Events use
      record
         Event_Type at  0 * SDL.Word range  0  .. 31;
         Time_Stamp at  1 * SDL.Word range  0  .. 31;

         Touch_ID   at  2 * SDL.Word range  0  .. 63;
         Finger_ID  at  4 * SDL.Word range  0  .. 63;
         X          at  6 * SDL.Word range  0  .. 31;
         Y          at  7 * SDL.Word range  0  .. 31;
         Delta_X    at  8 * SDL.Word range  0  .. 31;
         Delta_Y    at  9 * SDL.Word range  0  .. 31;
         Pressure   at 10 * SDL.Word range  0  .. 31;
      end record;

   for Multi_Gesture_Events use
      record
         Event_Type at  0 * SDL.Word range  0  .. 31;
         Time_Stamp at  1 * SDL.Word range  0  .. 31;

         Touch_ID   at  2 * SDL.Word range  0  .. 63;
         Theta      at  4 * SDL.Word range  0  .. 31;
         Distance   at  5 * SDL.Word range  0  .. 31;
         Centre_X   at  6 * SDL.Word range  0  .. 31;
         Centre_Y   at  7 * SDL.Word range  0  .. 31;
         Fingers    at  8 * SDL.Word range  0  .. 15;
         Padding    at  8 * SDL.Word range 16  .. 31;
      end record;

   for Dollar_Events use
      record
         Event_Type at  0 * SDL.Word range  0  .. 31;
         Time_Stamp at  1 * SDL.Word range  0  .. 31;

         Touch_ID   at  2 * SDL.Word range  0  .. 63;
         Gesture_ID at  4 * SDL.Word range  0  .. 63;
         Fingers    at  6 * SDL.Word range  0  .. 31;  --  Inconsistent, type is 16 bits, but SDL uses 32 here.
         Error      at  7 * SDL.Word range  0  .. 31;
         Centre_X   at  8 * SDL.Word range  0  .. 31;
         Centre_Y   at  9 * SDL.Word range  0  .. 31;
      end record;
end SDL.Events.Touches;
