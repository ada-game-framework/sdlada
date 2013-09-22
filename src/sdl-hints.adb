with Interfaces.C;
with Interfaces.C.Strings;

package body SDL.Hints is
   Frame_Buffer_Acceleration        : constant String := "SDL_FRAMEBUFFER_ACCELERATION";
   Render_Driver                    : constant String := "SDL_RENDER_DRIVER";
   Render_OpenGL_Shaders            : constant String := "SDL_RENDER_OPENGL_SHADERS";
   Render_Scale_Quality             : constant String := "SDL_RENDER_SCALE_QUALITY";
   Render_VSync                     : constant String := "SDL_RENDER_VSYNC";
   Video_X11_XVidMode               : constant String := "SDL_VIDEO_X11_XVIDMODE";
   Video_X11_Xinerama               : constant String := "SDL_VIDEO_X11_XINERAMA";
   Video_X11_XRandR                 : constant String := "SDL_VIDEO_X11_XRANDR";
   Grab_Keyboard                    : constant String := "SDL_GRAB_KEYBOARD";
   Video_Minimise_On_Focus_Loss     : constant String := "SDL_VIDEO_MINIMIZE_ON_FOCUS_LOSS";
   Idle_Timer_Disabled              : constant String := "SDL_IOS_IDLE_TIMER_DISABLED";
   IOS_Orientations                 : constant String := "SDL_IOS_ORIENTATIONS";
   XInput_Enabled                   : constant String := "SDL_XINPUT_ENABLED";
   Game_Controller_Config           : constant String := "SDL_GAMECONTROLLERCONFIG";
   Joystick_Allow_Background_Events : constant String := "SDL_JOYSTICK_ALLOW_BACKGROUND_EVENTS";
   Allow_Topmost                    : constant String := "SDL_ALLOW_TOPMOST";
   Timer_Resolution                 : constant String := "SDL_TIMER_RESOLUTION";

   type Hint_Names is access String;

   Hint_Name_Map : constant array (Hint'Range) of Hint_Names :=
     (Frame_Buffer_Acceleration'Access,
      Render_Driver'Access,
      Render_OpenGL_Shaders'Access,
      Render_Scale_Quality'Access,
      Render_VSync'Access,
      Video_X11_XVidMode'Access,
      Video_X11_Xinerama'Access,
      Video_X11_XRandR'Access,
      Grab_Keyboard'Access,
      Video_Minimise_On_Focus_Loss'Access,
      Idle_Timer_Disabled'Access,
      IOS_Orientations'Access,
      XInput_Enabled'Access,
      Game_Controller_Config'Access,
      Joystick_Allow_Background_Events'Access,
      Allow_Topmost'Access,
      Timer_Resolution'Access);

   function Value (H : in Hint) return String is
   begin
      return Hint_Name_Map (H).all;
   end Value;

   function SDL_GetHint (C_Str : in C.char_array) return C.Strings.chars_ptr with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_GetHint";

   function SDL_SetHint (Name, Value : in C.char_array) return Boolean with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_SetHint";

   function SDL_SetHint (Name, Value : in C.Char_array; P : in Priorities) return Boolean with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_SetHintWithPriority";

   function Get (H : in Hint) return String is
      C_Str : constant C.Strings.chars_Ptr := SDL_GetHint (C.To_C (Value (H)));
   begin
      return C.Strings.Value (C_Str);
   end Get;

   function Set (H : in Hint; Value : in String) return Boolean is
   begin
      return SDL_SetHint (C.To_C (Value (H)), C.To_C (Value));
   end Set;

   function Set (H : in Hint; Value : in String; Priority : in Priorities)
                return Boolean is
   begin
      return SDL_SetHint (C.To_C (Value (H)), C.To_C (Value), Priority);
   end Set;
end SDL.Hints;
