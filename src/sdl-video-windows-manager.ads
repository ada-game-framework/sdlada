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
--  SDL.Video.Windows.Manager
--
--  Access to the underlying window system.
--
--  Due to the nature of free OSes like Linux, one user may be using X11, another XCB, another Wayland and another
--  using Mir. We don't want to use build specific data for all these three, that makes sense for the varying OSes,
--  such as Linux, Windows, MacOS X, etc. So, if building on Windows, the compiler should only allow access to the
--  Windows/WinRT specific stuff, Linux, then X11/Wayland/Mir, etc.
--------------------------------------------------------------------------------------------------------------------
with Interfaces.C;
with SDL.Versions;

package SDL.Video.Windows.Manager is
   pragma Preelaborate;

   type WM_Types is (WM_Unknown,
                     WM_Windows,
                     WM_X11,
                     WM_Direct_FB,
                     WM_Cocoa,
                     WM_UI_Kit,
                     WM_Wayland,
                     WM_Mir,
                     WM_Win_RT,
                     WM_Android) with
     Convention => C;

   type C_Address is access all Interfaces.Unsigned_32 with
     Convention => C;

   --  These are dummy types that (should) match what a real binding would provide so that the end user can
   --  convert these using Unchecked_Conversion to pass into any other API's.
   package Windows is
      type HWNDs is new C_Address;
      type HDCs  is new C_Address;
   end Windows;

   --  WinRT only available since version 2.0.3.
   package Win_RT is
      type Inspectable is new C_Address;
   end Win_RT;

   package X11 is
      type Display is new C_Address;
      type Window  is new Interfaces.Unsigned_32;
   end X11;

   package Direct_FB is
      type Direct_FB         is new C_Address;
      type Direct_FB_Window  is new C_Address;
      type Direct_FB_Surface is new C_Address;
   end Direct_FB;

   package Cocoa is
      type NS_Window is new C_Address;
   end Cocoa;

   package UI_Kit is
      package C renames Interfaces.C;

      type Window is new C_Address;

      Frame_Buffer         : C.unsigned;
      Colour_Buffer        : C.unsigned;
      Resolve_Frame_Buffer : C.unsigned;
   end UI_Kit;

   --  Wayland only available since version 2.0.2.
   package Wayland is
      type Display       is new C_Address;
      type Surface       is new C_Address;
      type Shell_Surface is new C_Address;
   end Wayland;

   --  Mir only available since version 2.0.2.
   package Mir is
      type Connection is new C_Address;
      type Surface    is new C_Address;
   end Mir;

   --  Android only available since version 2.0.4.
   package Android is
      type Native_Window is new C_Address;
      type EGL_Surface   is new C_Address;
   end Android;

   type Information (WM : WM_Types) is
      record
         case WM is
            when WM_Unknown =>
               null;
            when WM_Windows =>
               HWND                     : Windows.HWNDs;
               HDC                      : Windows.HDCs;
            when WM_Win_RT =>
               RT_Inspectable           : Win_RT.Inspectable;
            when WM_X11 =>
               X11_Display              : X11.Display;
               X11_Window               : X11.Window;
            when WM_Direct_FB =>
               DFB_Main_Interface       : Direct_FB.Direct_FB;
               DFB_Window               : Direct_FB.Direct_FB_Window;
               DFB_Surface              : Direct_FB.Direct_FB_Surface;
            when WM_Cocoa =>
               Cocoa_Window             : Cocoa.NS_Window;
            when WM_UI_Kit =>
               UIK_Window               : UI_Kit.Window;
               UIK_Frame_Buffer         : UI_Kit.Window;
               UIK_Colour_Buffer        : UI_Kit.Window;
               UIK_Resolve_Frame_Buffer : UI_Kit.Window;
            when WM_Wayland =>
               Wayland_Display          : Wayland.Display;
               Wayland_Surface          : Wayland.Surface;
               Wayland_Shell_Surface    : Wayland.Shell_Surface;
            when WM_Mir =>
               Mir_Connection           : Mir.Connection;
               Mir_Surface              : Mir.Surface;
            when WM_Android =>
               Android_Window           : Android.Native_Window;
               Android_Surface          : Android.EGL_Surface;
         end case;
      end record with
     Unchecked_Union;

   type WM_Info is
      record
         Version    : SDL.Versions.Version;
         Sub_System : WM_Types;
         Info       : Information (WM => WM_Unknown);
      end record with
     Convention => C;

   function Get_WM_Info (Win : in Window; Info : out WM_Info) return Boolean with
     Inline => True;
end SDL.Video.Windows.Manager;
