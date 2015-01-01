--------------------------------------------------------------------------------------------------------------------
--  Copyright (c) 2014-2015 Luke A. Guest
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
--  SDL.Hints
--
--  Access to library configuration variables.
--------------------------------------------------------------------------------------------------------------------
package SDL.Hints is
   --  TODO: Make this more robust using more functions and platform specific
   --  packages with error checking on returned values?
   --  Would be nice to have the compiler only allow that which is allowed on
   --  a particular platform.
   --  It would be nice to have the binding test the return values as well,
   --  raising an exception on values that are just wrong for a particular
   --  platform, i.e. direct3d on Linux or Mac? Exception raised!

   --  This is raised when something has gone horribly wrong somewhere,
   --  i.e. setting the wrong hint on a platform that does not allow it.
   Hint_Error : exception;

   type Hint is
     (Frame_Buffer_Acceleration,
      Render_Driver,
      Render_OpenGL_Shaders,
      Render_Scale_Quality,
      Render_VSync,
      Video_X11_XVidMode,
      Video_X11_Xinerama,
      Video_X11_XRandR,
      Grab_Keyboard,
      Video_Minimise_On_Focus_Loss,
      Idle_Timer_Disabled,
      IOS_Orientations,
      XInput_Enabled, -- win
      Game_Controller_Config, -- win, mac, linux
      Joystick_Allow_Background_Events,
      Allow_Topmost,
      Timer_Resolution) with  -- win7 and earlier
     Discard_Names => True;

   type Priorities is (Default, Normal, Override) with
     Convention => C;

   procedure Clear with
      Import        => True,
      Convention    => C,
      External_Name => "SDL_ClearHints";

   function Get (Name : in Hint) return String;
   procedure Set (Name : in Hint; Value : in String);
   procedure Set (Name : in Hint; Value : in String; Priority : in Priorities);
end SDL.Hints;
