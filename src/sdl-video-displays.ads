--                              -*- Mode: Ada -*-
--  Filename        : sdl-video-displays.ads
--  Description     : Display modes.
--  Author          : Luke A. Guest
--  Created On      : Thu Oct 10 21:49:32 2013
with Interfaces.C;
with SDL.Video.Pixel_Formats;
with SDL.Video.Rectangles;
with System;

package SDL.Video.Displays is
   package C renames Interfaces.C;

   type Mode is
      record
         Format       : SDL.Video.Pixel_Formats.Pixel_Format_Names;
         Width        : C.int;
         Height       : C.int;
         Refresh_Rate : C.int;
         Driver_Data  : System.Address;
      end record with
        Convention => C;

   function Closest_Mode (Display : in Natural; Wanted : in Mode; Target : out Mode) return Boolean;

   function Current_Mode (Display : in Natural; Target : out Mode) return Boolean;

   function Desktop_Mode (Display : in Natural; Target : out Mode) return Boolean;

   function Display_Mode (Display : in Natural; Index : in Natural; Target : out Mode) return Boolean;

   function Total_Display_Modes (Display : in Natural; Total : out Positive) return Boolean;

   function Display_Bounds (Display : in Natural; Bounds : out Rectangles.Rectangle) return Boolean;
end SDL.Video.Displays;
