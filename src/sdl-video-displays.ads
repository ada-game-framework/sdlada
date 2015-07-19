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
--  SDL.Video.Displays
--
--  Access to display information including video displays and resolutions.
--------------------------------------------------------------------------------------------------------------------
with Interfaces.C;
with SDL.Video.Pixel_Formats;
with SDL.Video.Rectangles;
with System;

package SDL.Video.Displays is
   package C renames Interfaces.C;

   type Refresh_Rates is range 0 .. 400 with
     Convention => C;

   type Mode is
      record
         Format       : SDL.Video.Pixel_Formats.Pixel_Format_Names;
         Width        : C.int;
         Height       : C.int;
         Refresh_Rate : Refresh_Rates;
         Driver_Data  : System.Address;  --  TODO: Somehow make this a real type.
      end record with
     Convention => C;

   type Access_Mode is access all Mode with
     Convention => C;

   function Closest_Mode (Display : in Natural; Wanted : in Mode; Target : out Mode) return Boolean;

   function Current_Mode (Display : in Natural; Target : out Mode) return Boolean;

   function Desktop_Mode (Display : in Natural; Target : out Mode) return Boolean;

   function Display_Mode (Display : in Natural; Index : in Natural; Target : out Mode) return Boolean;

   function Total_Display_Modes (Display : in Natural; Total : out Positive) return Boolean;

   function Display_Bounds (Display : in Natural; Bounds : out Rectangles.Rectangle) return Boolean;
end SDL.Video.Displays;
