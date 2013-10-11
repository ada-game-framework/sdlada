--                              -*- Mode: Ada -*-
--  Filename        : sdl-video-windows.ads
--  Description     : Tagged type wrapping the SDL_Window structure.
--  Author          : Luke A. Guest
--  Created On      : Tue Sep 24 13:46:54 2013
with Ada.Finalization;
with System;

package SDL.Video.Windows is
   Window_Error : exception;

   type Window_Flags is mod 2 ** 32 with
     Convention => C;

   Full_Screen         : constant Window_Flags := 16#0000_0001#;
   OpenGL              : constant Window_Flags := 16#0000_0002#;
   Shown               : constant Window_Flags := 16#0000_0004#;
   Hidden              : constant Window_Flags := 16#0000_0008#;
   Borderless          : constant Window_Flags := 16#0000_0010#;
   Resizable           : constant Window_Flags := 16#0000_0020#;
   Minimised           : constant Window_Flags := 16#0000_0040#;
   Maximised           : constant Window_Flags := 16#0000_0080#;
   Input_Grabbed       : constant Window_Flags := 16#0000_0100#;
   Input_Focus         : constant Window_Flags := 16#0000_0200#;
   Mouse_Focus         : constant Window_Flags := 16#0000_0400#;
   Full_Screen_Desktop : constant Window_Flags := Full_Screen or 16#0000_1000#;
   Foreign             : constant Window_Flags := 16#0000_0800#; --  TODO: Not implemented yet.

   type Native_Window is private;

   --  Allow users to derive new types from this.
   type User_Data is tagged private;

   type User_Data_Access is access all User_Data'Class;

   --  TODO: Check this type!
   type Brightness is digits 3 range 0.0 .. 1.0;

   --  type Window is tagged limited Private;
   type Window is new Ada.Finalization.Limited_Controlled with private;

   procedure Create
     (Self   : in out Window;
      Title  : in String;
      X      : in Integer;
      Y      : in Integer;
      Width  : in Integer;
      Height : in Integer;
      Flags  : in Window_Flags := OpenGL);

   --  Create a window from an existing window created in some other way.
   procedure Create (Self : in out Window; Native : in Native_Window) with
     Inline => True;

   procedure Finalize (Object : in out Window);

   function Get_Brightness (Self : in Window) return Brightness with
     Inline => True;

   procedure Set_Brightness (Self : in out Window; How_Bright : in Brightness);

   function Get_Data (Self : in Window; Name : in String) return User_Data_Access;
   function Set_Data (Self : in out Window; Name : in String; Item : in User_Data_Access) return User_Data_Access ;

   function Display_Index (Self : in Window) return Positive;

   --  SDL_GetWindowDisplayMode
   --  SDL_GetWindowFlags
   --  SDL_GetWindowFromID
   --  SDL_GetWindowGammaRamp
   --  SDL_GetWindowGrab
   --  SDL_GetWindowID
   --  SDL_GetWindowMaximumSize
   --  SDL_GetWindowMinimumSize
   --  SDL_GetWindowPixelFormat
   --  SDL_GetWindowPosition
   --  SDL_GetWindowSize
   --  SDL_GetWindowSurface
   --  SDL_GetWindowTitle
   --  SDL_GetWindowWMInfo
   --  SDL_HideWindow
   --  SDL_MaximizeWindow
   --  SDL_MinimizeWindow
   --  SDL_RaiseWindow
   --  SDL_RestoreWindow

   --  SDL_SetWindowDisplayMode
   --  SDL_SetWindowFullscreen
   --  SDL_SetWindowGammaRamp
   --  SDL_SetWindowGrab
   --  SDL_SetWindowIcon
   --  SDL_SetWindowMaximumSize
   --  SDL_SetWindowMinimumSize
   --  SDL_SetWindowPosition
   --  SDL_SetWindowSize
   --  SDL_SetWindowTitle
   --  SDL_ShowWindow
   --  SDL_UpdateWindowSurface
   --  SDL_UpdateWindowSurfaceRects
private
   type Native_Window is new System.Address;

   type User_Data is new Ada.Finalization.Controlled with null record;

   type Window is new Ada.Finalization.Limited_Controlled with
      record
         Internal : System.Address := System.Null_Address;
      end record;
end SDL.Video.Windows;
