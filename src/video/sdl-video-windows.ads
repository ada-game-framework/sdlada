--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
--  SDL.Video.Windows
--
--  Operating system window access and control.
--------------------------------------------------------------------------------------------------------------------
with Ada.Finalization;
with Ada.Strings.UTF_Encoding;
with Interfaces;
private with SDL.C_Pointers;
with SDL.Video.Displays;
with SDL.Video.Pixel_Formats;
with SDL.Video.Rectangles;
with SDL.Video.Surfaces;
with System;

package SDL.Video.Windows is
   pragma Preelaborate;

   Window_Error : exception;

   use type Interfaces.Unsigned_32;

   --  Return a special coordinate value to indicate that you don't care what
   --  the window position is. Note that you can still specify a target
   --  display.
   function Undefined_Window_Position (Display : Natural := 0) return SDL.Natural_Coordinate is
     (C.int (Interfaces.Unsigned_32 (Display) or 16#1FFF_0000#));

   --  Return a special coordinate value to indicate that the window position
   --  should be centered.
   function Centered_Window_Position (Display : Natural := 0) return SDL.Natural_Coordinate is
     (C.int (Interfaces.Unsigned_32 (Display) or 16#2FFF_0000#));

   function Centered_Window_Position (Display : Natural := 0) return SDL.Coordinates is
     (others => C.int (Interfaces.Unsigned_32 (Display) or 16#2FFF_0000#));

   type Window_Flags is mod 2 ** 32 with
     Convention => C;

   Windowed            : constant Window_Flags := 16#0000_0000#;
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
   Allow_High_DPI      : constant Window_Flags := 16#0000_2000#;
   Mouse_Capture       : constant Window_Flags := 16#0000_4000#;
   Always_On_Top       : constant Window_Flags := 16#0000_8000#;
   Skip_Taskbar        : constant Window_Flags := 16#0001_0000#;
   Utility             : constant Window_Flags := 16#0002_0000#;
   Tool_Tip            : constant Window_Flags := 16#0004_0000#;
   Pop_Up_Menu         : constant Window_Flags := 16#0008_0000#;
   Keyboard_Grabbed    : constant Window_Flags := 16#0010_0000#;
   Vulkan              : constant Window_Flags := 16#1000_0000#;
   Metal               : constant Window_Flags := 16#2000_0000#;


   --  TODO: This isn't raising any exception when I pass a different value for some reason.
   subtype Full_Screen_Flags is Window_Flags with
     Static_Predicate => Full_Screen_Flags in Windowed | Full_Screen | Full_Screen_Desktop;

   type ID is mod 2 ** 32 with
     Convention => C;

   type Native_Window is private;

   --  Allow users to derive new types from this.
   type User_Data is tagged private;

   type User_Data_Access is access all User_Data'Class;
   pragma No_Strict_Aliasing (User_Data_Access);

   --  TODO: Check this type!
   type Brightness is digits 3 range 0.0 .. 1.0;

   --  type Window is tagged limited Private;
   type Window is new Ada.Finalization.Limited_Controlled with private;

   Null_Window : constant Window;

   --  TODO: Normalise the API by adding a destroy sub program and making this one call destroy,
   --  see textures for more info.
   overriding
   procedure Finalize (Self : in out Window);

   function Get_Brightness (Self : in Window) return Brightness with
     Inline => True;

   procedure Set_Brightness (Self : in out Window; How_Bright : in Brightness);

   function Get_Data (Self : in Window; Name : in String) return User_Data_Access;
   function Set_Data (Self : in out Window; Name : in String; Item : in User_Data_Access) return User_Data_Access;

   function Display_Index (Self : in Window) return SDL.Video.Displays.Display_Indices;

   procedure Get_Display_Mode (Self : in Window; Mode : out SDL.Video.Displays.Mode);
   procedure Set_Display_Mode (Self : in out Window; Mode : in SDL.Video.Displays.Mode);

   function Get_Flags (Self : in Window) return Window_Flags;

   function From_ID (Window_ID : in ID) return Window;

   procedure Get_Gamma_Ramp (Self : in Window; Red, Green, Blue : out SDL.Video.Pixel_Formats.Gamma_Ramp);
   procedure Set_Gamma_Ramp (Self : in out Window; Red, Green, Blue : in SDL.Video.Pixel_Formats.Gamma_Ramp);

   function Is_Grabbed (Self : in Window) return Boolean with
     Inline => True;

   procedure Set_Grabbed (Self : in out Window; Grabbed : in Boolean := True) with
     Inline => True;

   function Get_ID (Self : in Window) return ID with
     Inline => True;

   function Get_Maximum_Size (Self : in Window) return SDL.Sizes;
   procedure Set_Maximum_Size (Self : in out Window; Size : in SDL.Sizes) with
     Inline => True;

   function Get_Minimum_Size (Self : in Window) return SDL.Sizes;
   procedure Set_Minimum_Size (Self : in out Window; Size : in SDL.Sizes) with
     Inline => True;

   function Pixel_Format (Self : in Window) return SDL.Video.Pixel_Formats.Pixel_Format with
     Inline => True;

   function Get_Position (Self : in Window) return SDL.Natural_Coordinates;
   procedure Set_Position (Self : in out Window; Position : SDL.Natural_Coordinates) with
     Inline => True;

   function Get_Size (Self : in Window) return SDL.Sizes;
   procedure Set_Size (Self : in out Window; Size : in SDL.Sizes) with
     Inline => True;

   function Get_Surface (Self : in Window) return SDL.Video.Surfaces.Surface;

   function Get_Title (Self : in Window) return Ada.Strings.UTF_Encoding.UTF_8_String;
   procedure Set_Title (Self : in Window; Title : in Ada.Strings.UTF_Encoding.UTF_8_String);

   --  SDL_GetWindowWMInfo

   procedure Hide (Self : in Window) with
     Inline => True;

   procedure Show (Self : in Window) with
     Inline => True;

   procedure Maximise (Self : in Window) with
     Inline => True;

   procedure Minimise (Self : in Window) with
     Inline => True;

   procedure Raise_And_Focus (Self : in Window) with
     Inline => True;

   procedure Restore (Self : in Window) with
     Inline => True;

   procedure Set_Mode (Self : in out Window; Flags : in Full_Screen_Flags);

   procedure Set_Icon (Self : in out Window; Icon : in SDL.Video.Surfaces.Surface) with
     Inline => True;

   procedure Update_Surface (Self : in Window);

   procedure Update_Surface_Rectangle (Self : in Window; Rectangle : in SDL.Video.Rectangles.Rectangle);
   procedure Update_Surface_Rectangles (Self : in Window; Rectangles : in SDL.Video.Rectangles.Rectangle_Arrays);

   --  Determine whether any windows have been created.
   function Exist return Boolean with
     Inline => True;
private
   --  TODO: Make this a proper type.
   type Native_Window is new System.Address;

   type User_Data is new Ada.Finalization.Controlled with null record;

   type Window is new Ada.Finalization.Limited_Controlled with
      record
         Internal : SDL.C_Pointers.Windows_Pointer := null;  --  System.Address := System.Null_Address;
         Owns     : Boolean                        := True;  --  Does this Window type own the Internal data?
      end record;

   function Get_Internal_Window (Self : in Window) return SDL.C_Pointers.Windows_Pointer with
     Export        => True,
     Convention    => Ada;

   Null_Window : constant Window := (Ada.Finalization.Limited_Controlled with
                                     Internal => null, --  System.Null_Address,
                                     Owns     => True);

   Total_Windows_Created : Natural := Natural'First;

   procedure Increment_Windows;
   procedure Decrement_Windows;
end SDL.Video.Windows;
