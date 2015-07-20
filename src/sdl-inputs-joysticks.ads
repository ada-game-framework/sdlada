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
--  SDL.Inputs.Joysticks
--------------------------------------------------------------------------------------------------------------------
with Ada.Finalization;
private with SDL.C_Pointers;
with SDL.Events.Joysticks;

package SDL.Inputs.Joysticks is
   Joystick_Error : exception;

   --  Use to determine whether there are no joysticks.
   type All_Devices is range 0 .. 2 ** 31 - 1 with
     Convention => C,
     Size       => 32;

   --  This range is the range of all connected joysticks.
   subtype Devices is All_Devices range All_Devices'First + 1 .. All_Devices'Last;

   --  SDL_Joystick_ID = instance ID.
   type Instances is range 0 .. 2 ** 31 - 1 with
     Convention => C,
     Size       => 32;

   type GUID_Element is range 0 .. 255 with
     convention => C,
     Size       => 8;

   type GUID_Array is array (1 .. 16) of aliased GUID_Element with
     Convention => C;
   --     Pack       => True;

   type GUIDs is
      record
         Data : GUID_Array;
      end record with
     Convention => C_Pass_By_Copy;

   --  Device specific information.
   function Total return All_Devices;
   function Name (Device : in Devices) return String;
   function GUID (Device : in Devices) return GUIDs;

   --  GUID utilities.
   function Image (GUID : in GUIDs) return String;
   function Value (GUID : in String) return GUIDs;

   --  Joysticks.
   type Joystick is new Ada.Finalization.Limited_Controlled with private;

   Null_Joystick : constant Joystick;

   function "=" (Left, Right : in Joystick) return Boolean with
     Inline => True;

   procedure Close (Self : in out Joystick);

   --  Information.
   function Axes (Self : in Joystick) return SDL.Events.Joysticks.Axes;
   function Balls (Self : in Joystick) return SDL.Events.Joysticks.Balls;
   function Buttons (Self : in Joystick) return SDL.Events.Joysticks.Buttons;
   function Hats (Self : in Joystick) return SDL.Events.Joysticks.Hats;
   function Name (Self : in Joystick) return String;
   function Is_Haptic (Self : in Joystick) return Boolean;
   function Is_Attached (Self : in Joystick) return Boolean;
   function GUID (Self : in Joystick) return GUIDs;
   function Instance (Self : in Joystick) return Instances;

   --  Data.
   function Axis_Value (Self : in Joystick;
                        Axis : in SDL.Events.Joysticks.Axes) return SDL.Events.Joysticks.Axes_Values;

   procedure Ball_Value (Self             : in Joystick;
                         Ball             : in SDL.Events.Joysticks.Balls;
                         Delta_X, Delta_Y : out SDL.Events.Joysticks.Ball_Values);

   function Hat_Value (Self : in Joystick;
                       Hat  : in SDL.Events.Joysticks.Hats) return SDL.Events.Joysticks.Hat_Positions;

   function Is_Button_Pressed (Self : in Joystick; Button : in SDL.Events.Joysticks.Buttons)
                               return SDL.Events.Button_State;
private
   type Joystick is new Ada.Finalization.Limited_Controlled with
      record
         Internal : SDL.C_Pointers.Joystick_Pointer := null;
         Owns     : Boolean                         := True;
      end record;

   Null_Joystick : constant Joystick := (Ada.Finalization.Limited_Controlled with Internal => null, Owns => True);
end SDL.Inputs.Joysticks;
