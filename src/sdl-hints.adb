with Interfaces.C;
with Interfaces.C.Strings;

package body SDL.Hints is
   package C renames Interfaces.C;

   use type C.int;

   Frame_Buffer_Acceleration_Name        : aliased constant String := "SDL_FRAMEBUFFER_ACCELERATION";
   Render_Driver_Name                    : aliased constant String := "SDL_RENDER_DRIVER";
   Render_OpenGL_Shaders_Name            : aliased constant String := "SDL_RENDER_OPENGL_SHADERS";
   Render_Scale_Quality_Name             : aliased constant String := "SDL_RENDER_SCALE_QUALITY";
   Render_VSync_Name                     : aliased constant String := "SDL_RENDER_VSYNC";
   Video_X11_XVidMode_Name               : aliased constant String := "SDL_VIDEO_X11_XVIDMODE";
   Video_X11_Xinerama_Name               : aliased constant String := "SDL_VIDEO_X11_XINERAMA";
   Video_X11_XRandR_Name                 : aliased constant String := "SDL_VIDEO_X11_XRANDR";
   Grab_Keyboard_Name                    : aliased constant String := "SDL_GRAB_KEYBOARD";
   Video_Minimise_On_Focus_Loss_Name     : aliased constant String := "SDL_VIDEO_MINIMIZE_ON_FOCUS_LOSS";
   Idle_Timer_Disabled_Name              : aliased constant String := "SDL_IOS_IDLE_TIMER_DISABLED";
   IOS_Orientations_Name                 : aliased constant String := "SDL_IOS_ORIENTATIONS";
   XInput_Enabled_Name                   : aliased constant String := "SDL_XINPUT_ENABLED";
   Game_Controller_Config_Name           : aliased constant String := "SDL_GAMECONTROLLERCONFIG";
   Joystick_Allow_Background_Events_Name : aliased constant String := "SDL_JOYSTICK_ALLOW_BACKGROUND_EVENTS";
   Allow_Topmost_Name                    : aliased constant String := "SDL_ALLOW_TOPMOST";
   Timer_Resolution_Name                 : aliased constant String := "SDL_TIMER_RESOLUTION";

   type Hint_Names is access constant String;

   Hint_Name_Map : constant array (Hint'Range) of Hint_Names :=
     (Frame_Buffer_Acceleration_Name'Access,
      Render_Driver_Name'Access,
      Render_OpenGL_Shaders_Name'Access,
      Render_Scale_Quality_Name'Access,
      Render_VSync_Name'Access,
      Video_X11_XVidMode_Name'Access,
      Video_X11_Xinerama_Name'Access,
      Video_X11_XRandR_Name'Access,
      Grab_Keyboard_Name'Access,
      Video_Minimise_On_Focus_Loss_Name'Access,
      Idle_Timer_Disabled_Name'Access,
      IOS_Orientations_Name'Access,
      XInput_Enabled_Name'Access,
      Game_Controller_Config_Name'Access,
      Joystick_Allow_Background_Events_Name'Access,
      Allow_Topmost_Name'Access,
      Timer_Resolution_Name'Access);

   function Value (H : in Hint) return String with
     Inline => True;

   function Value (H : in Hint) return String is
   begin
      return Hint_Name_Map (H).all;
   end Value;

   function Get (Name : in Hint) return String is
      function SDL_Get_Hint (C_Str : in C.Strings.chars_ptr) return C.Strings.chars_ptr with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetHint";

      C_Hint_Str : C.Strings.chars_ptr          := C.Strings.New_String (Value (H => Name));
      C_Str      : constant C.Strings.chars_ptr := SDL_Get_Hint (C_Hint_Str);
   begin
      C.Strings.Free (C_Hint_Str);

      return C.Strings.Value (C_Str);
   end Get;

   procedure Set (Name : in Hint; Value : in String) is
      function SDL_Set_Hint (Name, Value : in C.Strings.chars_ptr) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_SetHint";

      C_Hint_Str  : C.Strings.chars_ptr := C.Strings.New_String (Hints.Value (Name));
      C_Value_Str : C.Strings.chars_ptr := C.Strings.New_String (Value);
      Result      : C.int               := SDL_Set_Hint
        (Name  => C_Hint_Str,
         Value => C_Value_Str);
   begin
      C.Strings.Free (C_Hint_Str);
      C.Strings.Free (C_Value_Str);

      if Result = 0 then
         raise Hint_Error with "Could not set hint";
      end if;
   end Set;

   procedure Set (Name : in Hint; Value : in String; Priority : in Priorities) is
      function SDL_Set_Hint (Name, Value : in C.Strings.chars_ptr; P : in Priorities) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_SetHintWithPriority";

      C_Hint_Str  : C.Strings.chars_ptr := C.Strings.New_String (Hints.Value (Name));
      C_Value_Str : C.Strings.chars_ptr := C.Strings.New_String (Value);
      Result      : C.int               := SDL_Set_Hint
        (Name  => C_Hint_Str,
         Value => C_Value_Str,
         P     => Priority);
   begin
      C.Strings.Free (C_Hint_Str);
      C.Strings.Free (C_Value_Str);

      if Result = 0 then
         raise Hint_Error with "Could not set hint";
      end if;
   end Set;
end SDL.Hints;
