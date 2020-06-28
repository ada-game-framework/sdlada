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
with Interfaces.C.Strings;
with SDL.Error;
with SDL.RWops;

package body SDL.Inputs.Joysticks.Game_Controllers is
   package C renames Interfaces.C;

   use type C.Strings.chars_ptr;

   procedure Add_Mapping (Data : in String; Updated_Existing : out Boolean) is
      function SDL_Game_Controller_Add_Mapping (Buffer : in C.char_array) return C.int with
        Convention    => C,
        Import        => True,
        External_Name => "SDL_GameControllerAddMapping";

      Result : constant C.int := SDL_Game_Controller_Add_Mapping (C.To_C (Data));
   begin
      if Result = -1 then
         raise Mapping_Error with SDL.Error.Get;
      end if;

      Updated_Existing := (Result = 0);
   end Add_Mapping;

   procedure Add_Mappings_From_File (Database_Filename : in String; Number_Added : out Natural) is
      function SDL_Game_Controller_Add_Mappings_From_RW
        (RW : SDL.RWops.RWops;
         FreeRW : C.int) return C.int with
           Convention    => C,
           Import        => True,
           External_Name => "SDL_GameControllerAddMappingsFromRW";

      RW : constant SDL.RWops.RWops := SDL.RWops.From_File (Database_Filename,
                                                            Mode => SDL.RWops.Read);

      Result : constant Integer
        := Integer (SDL_Game_Controller_Add_Mappings_From_RW (RW,
                                                              FreeRW => 1));
   begin
      if Result < 0 then
         raise Mapping_Error with SDL.Error.Get;
      end if;

      Number_Added := Result;
   end Add_Mappings_From_File;

   function Axis_Value (Self : in Game_Controller;
                        Axis : in SDL.Events.Joysticks.Game_Controllers.LR_Axes)
                       return SDL.Events.Joysticks.Game_Controllers.LR_Axes_Values is

      function SDL_Game_Controller_Get_Axis (Controller : in SDL.C_Pointers.Game_Controller_Pointer;
                                             Axis       : in SDL.Events.Joysticks.Game_Controllers.LR_Axes)
                                            return SDL.Events.Joysticks.Game_Controllers.LR_Axes_Values with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GameControllerGetAxis";
   begin
      return SDL_Game_Controller_Get_Axis (Self.Internal, Axis);
   end Axis_Value;

   function Axis_Value (Self : in Game_Controller;
                        Axis : in SDL.Events.Joysticks.Game_Controllers.Trigger_Axes)
                       return SDL.Events.Joysticks.Game_Controllers.Trigger_Axes_Values is

      function SDL_Game_Controller_Get_Axis (Controller : in SDL.C_Pointers.Game_Controller_Pointer;
                                             Axis       : in SDL.Events.Joysticks.Game_Controllers.Trigger_Axes)
                                            return SDL.Events.Joysticks.Game_Controllers.Trigger_Axes_Values with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GameControllerGetAxis";
   begin
      return SDL_Game_Controller_Get_Axis (Self.Internal, Axis);
   end Axis_Value;

   procedure Close (Self : in out Game_Controller) is
      procedure SDL_Game_Controller_Close (Controller : in SDL.C_Pointers.Game_Controller_Pointer) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GameControllerClose";
   begin
      SDL_Game_Controller_Close (Self.Internal);

      --  Reinitialise the object so it's actually a Null_Game_Controller.
      Self.Internal := null;
      Self.Owns     := True;
   end Close;


   function Get_Axis (Axis : in String) return SDL.Events.Joysticks.Game_Controllers.Axes is
      function SDL_Game_Controller_Get_Axis_From_String (Axis : in C.char_array)
                                                        return SDL.Events.Joysticks.Game_Controllers.Axes with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GameControllerGetAxisFromString";
   begin
      return SDL_Game_Controller_Get_Axis_From_String (C.To_C (Axis));
   end Get_Axis;

   function Get_Binding (Self : in Game_Controller; Axis : in SDL.Events.Joysticks.Game_Controllers.Axes)
                        return Bindings is
      function SDL_Game_Controller_Get_Bind_For_Axis
        (Controller : in SDL.C_Pointers.Game_Controller_Pointer;
         Axis       : in SDL.Events.Joysticks.Game_Controllers.Axes) return Bindings with
           Import        => True,
           Convention    => C,
           External_Name => "SDL_GameControllerGetBindForAxis";
   begin
      return SDL_Game_Controller_Get_Bind_For_Axis (Self.Internal, Axis);
   end Get_Binding;

   function Get_Binding (Self : in Game_Controller; Button : in SDL.Events.Joysticks.Game_Controllers.Buttons)
                        return Bindings is
      function SDL_Game_Controller_Get_Bind_For_Button
        (Controller : in SDL.C_Pointers.Game_Controller_Pointer;
         Button     : in SDL.Events.Joysticks.Game_Controllers.Buttons) return Bindings with
           Import        => True,
           Convention    => C,
           External_Name => "SDL_GameControllerGetBindForButton";
   begin
      return SDL_Game_Controller_Get_Bind_For_Button (Self.Internal, Button);
   end Get_Binding;

   function Get_Button (Button_Name : in String) return SDL.Events.Joysticks.Game_Controllers.Buttons is
      function SDL_Game_Controller_Get_Button_From_String
        (Buffer : in C.char_array) return SDL.Events.Joysticks.Game_Controllers.Buttons with
          Convention    => C,
          Import        => True,
          External_Name => "SDL_GameControllerGetButtonFromString";
   begin
      return SDL_Game_Controller_Get_Button_From_String (C.To_C (Button_Name));
   end Get_Button;

   function Get_Joystick (Self : in Game_Controller) return Joystick is
      function SDL_Game_Controller_Get_Joystick
        (Controller : in SDL.C_Pointers.Game_Controller_Pointer)
        return SDL.C_Pointers.Joystick_Pointer with
          Convention    => C,
          Import        => True,
          External_Name => "SDL_GameControllerGetJoystick";
   begin
      return J : constant Joystick := (Ada.Finalization.Limited_Controlled with
                                         Internal => SDL_Game_Controller_Get_Joystick (Self.Internal), Owns => False) do
         null;
      end return;
   end Get_Joystick;

   function Get_Mapping  (Self : in Game_Controller) return String is
      function SDL_Game_Controller_Mapping
        (Controller : in SDL.C_Pointers.Game_Controller_Pointer) return C.Strings.chars_ptr with
          Convention    => C,
          Import        => True,
          External_Name => "SDL_GameControllerMapping";

      Result : constant C.Strings.chars_ptr := SDL_Game_Controller_Mapping (Self.Internal);
   begin
      if Result = C.Strings.Null_Ptr then
         return "";
      end if;

      return C.Strings.Value (Result);
   end Get_Mapping;

   function Get_Mapping  (Controller : in GUIDs) return String is
      function SDL_Game_Controller_Mapping_For_GUID (Controller : in GUIDs) return C.Strings.chars_ptr with
        Convention    => C,
        Import        => True,
        External_Name => "SDL_GameControllerMappingForGUID";

      Result : constant C.Strings.chars_ptr := SDL_Game_Controller_Mapping_For_GUID (Controller);
   begin
      if Result = C.Strings.Null_Ptr then
         return "";
      end if;

      return C.Strings.Value (Result);
   end Get_Mapping;

   function Get_Name (Self : in Game_Controller) return String is
      function SDL_Game_Controller_Name
        (Controller : in SDL.C_Pointers.Game_Controller_Pointer) return C.Strings.chars_ptr with
          Convention    => C,
          Import        => True,
          External_Name => "SDL_GameControllerName";

      Result : constant C.Strings.chars_ptr := SDL_Game_Controller_Name (Self.Internal);
   begin
      if Result = C.Strings.Null_Ptr then
         return "";
      end if;

      return C.Strings.Value (Result);
   end Get_Name;

   function Get_Name (Device : in Devices) return String is
      function SDL_Game_Controller_Name_For_Index (Index : in C.int) return C.Strings.chars_ptr with
          Convention    => C,
          Import        => True,
          External_Name => "SDL_GameControllerNameForIndex";

      Result : constant C.Strings.chars_ptr := SDL_Game_Controller_Name_For_Index (C.int (Device) - 1);
   begin
      if Result = C.Strings.Null_Ptr then
         return "";
      end if;

      return C.Strings.Value (Result);
   end Get_Name;

   function Image (Axis : in SDL.Events.Joysticks.Game_Controllers.Axes) return String is
      function SDL_Game_Controller_Get_String_For_Axis
        (Axis : in SDL.Events.Joysticks.Game_Controllers.Axes) return C.Strings.chars_ptr with
          Convention    => C,
          Import        => True,
          External_Name => "SDL_GameControllerGetStringForAxis";

      Result : constant C.Strings.chars_ptr := SDL_Game_Controller_Get_String_For_Axis (Axis);
   begin
      if Result = C.Strings.Null_Ptr then
         return "";
      end if;

      return C.Strings.Value (Result);
   end Image;

   function Image (Button : in SDL.Events.Joysticks.Game_Controllers.Buttons) return String is
      function SDL_Game_Controller_Get_String_For_Button
        (Button : in SDL.Events.Joysticks.Game_Controllers.Buttons) return C.Strings.chars_ptr with
          Convention    => C,
          Import        => True,
          External_Name => "SDL_GameControllerGetStringForButton";

      Result : constant C.Strings.chars_ptr := SDL_Game_Controller_Get_String_For_Button (Button);
   begin
      if Result = C.Strings.Null_Ptr then
         return "";
      end if;

      return C.Strings.Value (Result);
   end Image;

   function Is_Attached (Self : in Game_Controller) return Boolean is
      function SDL_Game_Controller_Is_Attached (Controller : in SDL.C_Pointers.Game_Controller_Pointer)
                                               return SDL_Bool with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GameControllerGetAttached";
   begin
      return SDL_Game_Controller_Is_Attached (Self.Internal) = SDL_True;
   end Is_Attached;

   function Is_Button_Pressed (Self : in Game_Controller; Button : in SDL.Events.Joysticks.Buttons)
                              return SDL.Events.Button_State is
      function SDL_Game_Controller_Get_Button (Controller : in SDL.C_Pointers.Game_Controller_Pointer;
                                               Button     : in SDL.Events.Joysticks.Buttons)
                                                 return SDL.Events.Button_State with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GameControllerGetButton";
   begin
      return SDL_Game_Controller_Get_Button (Self.Internal, Button);
   end Is_Button_Pressed;

   function Is_Game_Controller (Device : in Devices) return Boolean is
      function SDL_Is_Game_Controller (Device : in C.int) return SDL_Bool with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_IsGameController";
   begin
      return SDL_Is_Game_Controller (C.int (Device) - 1) = SDL_True;
   end Is_Game_Controller;
end SDL.Inputs.Joysticks.Game_Controllers;
