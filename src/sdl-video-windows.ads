--                              -*- Mode: Ada -*-
--  Filename        : sdl-video-windows.ads
--  Description     : Tagged type wrapping the SDL_Window structure.
--  Author          : Luke A. Guest
--  Created On      : Tue Sep 24 13:46:54 2013
with Ada.Finalization;
with System;

package SDL.Video.Windows is
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

   procedure Finalize (Object : in out Window);

   function Get_Brightness (Self : in Window) return Float with
     Inline => True;
private
   type Window is new Ada.Finalization.Limited_Controlled with
      record
         Internal : System.Address := System.Null_Address;
      end record;
end SDL.Video.Windows;
