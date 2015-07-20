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
with Interfaces.C;
with Interfaces.C.Strings;
with SDL.Error;

package body SDL.Inputs.Joysticks is
   package C renames Interfaces.C;

   use type C.int;

   function Total return All_Devices is
      function SDL_Num_Joysticks return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_NumJoysticks";

      Result : C.int := SDL_Num_Joysticks;
   begin
      if Result < 0 then
         raise Joystick_Error with SDL.Error.Get;
      end if;

      return All_Devices (Result);
   end Total;

   function Name (Device : in Devices) return String is
      function SDL_Joystick_Name_For_Index (Device : in C.int) return C.Strings.chars_ptr with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_JoystickNameForIndex";
   begin
      return C.Strings.Value (SDL_Joystick_Name_For_Index (C.int (Device) - 1));
   end Name;

   function GUID (Device : in Devices) return GUIDs is
      function SDL_Joystick_Get_Device_GUID (Device : in C.int) return GUIDs with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_JoystickGetDeviceGUID";
   begin
      return SDL_Joystick_Get_Device_GUID (C.int (Device) - 1);
   end GUID;

   function Image (GUID : in GUIDs) return String is
      procedure SDL_Joystick_Get_GUID_String (GUID   : in GUIDs;
                                              Buffer : out C.char_array;
                                              Size   : in C.int) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_JoystickGetGUIDString";

      Data_Buffer : C.char_array (0 .. 127) := (others => C.nul);
      L           : C.int := C.int (Data_Buffer'Length);
   begin
      SDL_Joystick_Get_GUID_String (GUID, Data_Buffer, L);

      return C.To_Ada (Data_Buffer);
   end Image;

   function Value (GUID : in String) return GUIDs is
      function SDL_Joystick_Get_GUID_From_String (Buffer : in C.char_array) return GUIDs with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_JoystickGetGUIDFromString";
   begin
      return SDL_Joystick_Get_GUID_From_String (C.To_C (GUID));
   end Value;

   function "=" (Left, Right : in Joystick) return Boolean is
      use type SDL.C_Pointers.Joystick_Pointer;
   begin
      if Left.Internal = Right.Internal and then Left.Owns = Right.Owns then
         return True;
      end if;

      return False;
   end "=";

   procedure Close (Self : in out Joystick) is
      procedure SDL_Joystick_Close (Stick : in SDL.C_Pointers.Joystick_Pointer) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_JoystickClose";
   begin
      SDL_Joystick_Close (Self.Internal);

      --  Reinitialise the object so it's actually a Null_Joystick.
      Self.Internal := null;
      Self.Owns     := True;
   end Close;

   function Axes (Self : in Joystick) return SDL.Events.Joysticks.Axes is
      function SDL_Joystick_Num_Axes (Stick : in SDL.C_Pointers.Joystick_Pointer) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_JoystickNumAxes";

      Total : C.int := SDL_Joystick_Num_Axes (Self.Internal);
   begin
      if Total < 0 then
         raise Joystick_Error with SDL.Error.Get;
      end if;

      return SDL.Events.Joysticks.Axes (Total);
   end Axes;

   function Balls (Self : in Joystick) return SDL.Events.Joysticks.Balls is
      function SDL_Joystick_Num_Balls (Stick : in SDL.C_Pointers.Joystick_Pointer) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_JoystickNumBalls";

      Total : C.int := SDL_Joystick_Num_Balls (Self.Internal);
   begin
      if Total < 0 then
         raise Joystick_Error with SDL.Error.Get;
      end if;

      return SDL.Events.Joysticks.Balls (Total);
   end Balls;

   function Buttons (Self : in Joystick) return SDL.Events.Joysticks.Buttons is
      function SDL_Joystick_Num_Buttons (Stick : in SDL.C_Pointers.Joystick_Pointer) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_JoystickNumButtons";

      Total : C.int := SDL_Joystick_Num_Buttons (Self.Internal);
   begin
      if Total < 0 then
         raise Joystick_Error with SDL.Error.Get;
      end if;

      return SDL.Events.Joysticks.Buttons (Total);
   end Buttons;

   function Hats (Self : in Joystick) return SDL.Events.Joysticks.Hats is
      function SDL_Joystick_Num_Hats (Stick : in SDL.C_Pointers.Joystick_Pointer) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_JoystickNumHats";

      Total : C.int := SDL_Joystick_Num_Hats (Self.Internal);
   begin
      if Total < 0 then
         raise Joystick_Error with SDL.Error.Get;
      end if;

      return SDL.Events.Joysticks.Hats (Total);
   end Hats;

   function Name (Self : in Joystick) return String is
      function SDL_Joystick_Name (Stick : in SDL.C_Pointers.Joystick_Pointer) return C.Strings.chars_ptr with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_JoystickName";
   begin
      return C.Strings.Value (SDL_Joystick_Name (Self.Internal));
   end Name;

   function Is_Haptic (Self : in Joystick) return Boolean is
      function SDL_Joystick_Is_Haptic (Stick : in SDL.C_Pointers.Joystick_Pointer) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_JoystickIsHaptic";

      Result : C.int := SDL_Joystick_Is_Haptic (Self.Internal);
   begin
      if Result < 0 then
         raise Joystick_Error with SDL.Error.Get;
      elsif Result = 1 then
         return True;
      end if;

      return False;
   end Is_Haptic;

   function Is_Attached (Self : in Joystick) return Boolean is
      function SDL_Joystick_Is_Attached (Stick : in SDL.C_Pointers.Joystick_Pointer) return SDL_Bool with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_JoystickGetAttached";
   begin
      if SDL_Joystick_Is_Attached (Self.Internal) = SDL_True then
         return True;
      end if;

      return False;
   end Is_Attached;

   function GUID (Self : in Joystick) return GUIDs is
      function SDL_Joystick_Get_GUID (Stick : in SDL.C_Pointers.Joystick_Pointer) return GUIDs with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_JoystickGetGUID";
   begin
      return SDL_Joystick_Get_GUID (Self.Internal);
   end GUID;

   function Instance (Self : in Joystick) return Instances is
      function SDL_Joystick_Instance_ID (Stick : in SDL.C_Pointers.Joystick_Pointer) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_JoystickInstanceID";

      Result : C.int := SDL_Joystick_Instance_ID (Self.Internal);
   begin
      if Result < Success then
         raise Joystick_Error with SDL.Error.Get;
      end if;

      return Instances (Result);
   end Instance;

   function Axis_Value (Self : in Joystick;
                        Axis : in SDL.Events.Joysticks.Axes) return SDL.Events.Joysticks.Axes_Values is
      function SDL_Joystick_Get_Axis (Stick : in SDL.C_Pointers.Joystick_Pointer;
                                      Axis  : in SDL.Events.Joysticks.Axes) return SDL.Events.Joysticks.Axes_Values with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_JoystickGetAxis";
   begin
      return SDL_Joystick_Get_Axis (Self.Internal, Axis);
   end Axis_Value;

   procedure Ball_Value (Self             : in Joystick;
                         Ball             : in SDL.Events.Joysticks.Balls;
                         Delta_X, Delta_Y : out SDL.Events.Joysticks.Ball_Values) is
      function SDL_Joystick_Get_Ball (Stick            : in SDL.C_Pointers.Joystick_Pointer;
                                      Ball             : in SDL.Events.Joysticks.Balls;
                                      Delta_X, Delta_Y : out SDL.Events.Joysticks.Ball_Values) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_JoystickGetBall";

      Result : C.int := SDL_Joystick_Get_Ball (Self.Internal, Ball, Delta_X, Delta_Y);
   begin
      if Result < Success then
         raise Joystick_Error with SDL.Error.Get;
      end if;
   end Ball_Value;

   function Hat_Value (Self : in Joystick;
                       Hat  : in SDL.Events.Joysticks.Hats) return SDL.Events.Joysticks.Hat_Positions is
      function SDL_Joystick_Get_Hat (Stick : in SDL.C_Pointers.Joystick_Pointer;
                                     Ball  : in SDL.Events.Joysticks.Hats)
                                     return SDL.Events.Joysticks.Hat_Positions with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_JoystickGetHat";
   begin
      return SDL_Joystick_Get_Hat (Self.Internal, Hat);
   end Hat_Value;

   function Is_Button_Pressed (Self : in Joystick; Button : in SDL.Events.Joysticks.Buttons)
                               return SDL.Events.Button_State is
      function SDL_Joystick_Get_Button (Stick  : in SDL.C_Pointers.Joystick_Pointer;
                                        Button : in SDL.Events.Joysticks.Buttons) return SDL.Events.Button_State with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_JoystickGetButton";
   begin
      return SDL_Joystick_Get_Button (Self.Internal, Button);
   end Is_Button_Pressed;
end SDL.Inputs.Joysticks;
