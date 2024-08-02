--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
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
   pragma Preelaborate;

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

   --  Video displays.
   type Display_Indices is new Positive;

   type Display_Orientations is
     (Orientation_Unknown,
      Orientation_Landscape,
      Orientation_Landscape_Flipped,
      Orientation_Portrait,
      Orientation_Portrait_Flipped)
   with Convention => C;


   function Total return Display_Indices;

   function Get_Display_Name (Display : Display_Indices) return String;

   function Closest_Mode (Display : in Display_Indices; Wanted : in Mode; Target : out Mode) return Boolean;

   function Current_Mode (Display : in Display_Indices; Target : out Mode) return Boolean;

   function Desktop_Mode (Display : in Display_Indices; Target : out Mode) return Boolean;

   function Display_Mode (Display : in Display_Indices; Index : in Natural; Target : out Mode) return Boolean;

   function Total_Display_Modes (Display : in Display_Indices; Total : out Positive) return Boolean;

   function Display_Bounds (Display : in Display_Indices; Bounds : out Rectangles.Rectangle) return Boolean;

   procedure Get_Display_DPI (Display : Display_Indices; Diagonal, Horizontal, Vertical : out Float);
   procedure Get_Display_DPI (Display : Display_Indices; Horizontal, Vertical : out Float);
   function Get_Display_Horizontal_DPI (Display : Display_Indices) return Float;
   function Get_Display_Vertical_DPI (Display : Display_Indices) return Float;
end SDL.Video.Displays;
