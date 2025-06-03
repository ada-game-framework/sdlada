--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
with Ada.Unchecked_Conversion;
with Interfaces.C;
with Interfaces.C.Strings;
with SDL.Error;
--  with SDL.Video.Surfaces.Makers;
--  with SDL.Log;

package body SDL.Video.Windows is
   package C renames Interfaces.C;

   --  use type Interfaces.Unsigned_32;
   use type SDL.C_Pointers.Windows_Pointer;
   use type System.Address;

   procedure Increment_Windows is
   begin
      Total_Windows_Created := Total_Windows_Created + 1;
   end Increment_Windows;

   procedure Decrement_Windows is
   begin
      Total_Windows_Created := Total_Windows_Created - 1;
   end Decrement_Windows;

   overriding
   procedure Finalize (Self : in out Window) is
      procedure SDL_Destroy (W : in SDL.C_Pointers.Windows_Pointer) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_DestroyWindow";
   begin
      --  SDL.Log.Put_Debug ("Windows.Finalize: " & (if Self.Internal = null then "null" else "not null") &
      --                       "    " & (if Self.Owns = True then "owns" else "Doesn't own"));
      --  Make sure we don't delete this twice!
      if Self.Internal /= null and then Self.Owns then
         --  SDL.Log.Put_Debug ("Windows.Finalize: Deleting");

         SDL_Destroy (Self.Internal);

         Self.Internal := null;

         Decrement_Windows;
      end if;
   end Finalize;

   function Get_Brightness (Self : in Window) return Brightness is
      function SDL_Get_Brightness (W : in SDL.C_Pointers.Windows_Pointer) return C.C_float with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetWindowBrightness";
   begin
      return Brightness (SDL_Get_Brightness (Self.Internal));
   end Get_Brightness;

   procedure Set_Brightness (Self : in out Window; How_Bright : in Brightness) is
      function SDL_Set_Brightness (W : in SDL.C_Pointers.Windows_Pointer; B : in C.C_float) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_SetWindowBrightness";

      Result : constant C.int := SDL_Set_Brightness (Self.Internal, C.C_float (How_Bright));
   begin
      if Result /= Success then
         raise Window_Error with SDL.Error.Get;
      end if;
   end Set_Brightness;

   --  TODO: Try to see if we can just see User_Data_Access as the return type from the C function.
   function To_Data_Access is new Ada.Unchecked_Conversion (Source => System.Address, Target => User_Data_Access);
   function To_Address is new Ada.Unchecked_Conversion (Source => User_Data_Access, Target => System.Address);

   --  TODO: Make this and Set_Data generic.
   function Get_Data (Self : in Window; Name : in String) return User_Data_Access is
      function SDL_Get_Window_Data (W    : in SDL.C_Pointers.Windows_Pointer;
                                    Name : in C.Strings.chars_ptr) return System.Address with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetWindowData";

      C_Name_Str : C.Strings.chars_ptr       := C.Strings.New_String (Name);
      Item       : constant User_Data_Access := To_Data_Access (SDL_Get_Window_Data (Self.Internal, C_Name_Str));
   begin
      C.Strings.Free (C_Name_Str);

      return Item;
   end Get_Data;

   function Set_Data (Self : in out Window; Name : in String; Item : in User_Data_Access) return User_Data_Access is
      function SDL_Set_Window_Data (W         : in SDL.C_Pointers.Windows_Pointer;
                                    Name      : in C.Strings.chars_ptr;
                                    User_Data : in System.Address) return System.Address with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_SetWindowData";

      C_Name_Str    : C.Strings.chars_ptr       := C.Strings.New_String (Name);
      Previous_Data : constant User_Data_Access := To_Data_Access (SDL_Set_Window_Data (Self.Internal,
                                                                                        C_Name_Str,
                                                                                        To_Address (Item)));
   begin
      C.Strings.Free (C_Name_Str);

      return Previous_Data;
   end Set_Data;

   function Display_Index (Self : in Window) return SDL.Video.Displays.Display_Indices is
      function SDL_Get_Window_Display_Index (W : in SDL.C_Pointers.Windows_Pointer) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetWindowDisplayIndex";

      Total : constant C.int := SDL_Get_Window_Display_Index (Self.Internal);
   begin
      if Total < 0 then
         raise Window_Error with SDL.Error.Get;
      end if;

      return SDL.Video.Displays.Display_Indices (Total + 1);
   end Display_Index;

   procedure Get_Display_Mode (Self : in Window; Mode : out SDL.Video.Displays.Mode) is
      function SDL_Get_Window_Display_Mode (W : in SDL.C_Pointers.Windows_Pointer;
                                            M : out SDL.Video.Displays.Mode) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetWindowDisplayMode";

      Result : constant C.int := SDL_Get_Window_Display_Mode (Self.Internal, Mode);
   begin
      if Result /= Success then
         raise Window_Error with SDL.Error.Get;
      end if;
   end Get_Display_Mode;

   procedure Set_Display_Mode (Self : in out Window; Mode : in SDL.Video.Displays.Mode) is
      function SDL_Set_Window_Display_Mode (W : in SDL.C_Pointers.Windows_Pointer;
                                            M : in SDL.Video.Displays.Mode) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_SetWindowDisplayMode";

      Result : constant C.int := SDL_Set_Window_Display_Mode (Self.Internal, Mode);
   begin
      if Result /= Success then
         raise Window_Error with SDL.Error.Get;
      end if;
   end Set_Display_Mode;

   function Get_Flags (Self : in Window) return Window_Flags is
      function SDL_Get_Window_Flags (W : in SDL.C_Pointers.Windows_Pointer) return Window_Flags with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetWindowFlags";
   begin
      return SDL_Get_Window_Flags (Self.Internal);
   end Get_Flags;

   function From_ID (Window_ID : in ID) return Window is
      function SDL_Get_Window_From_ID (W : in ID) return SDL.C_Pointers.Windows_Pointer with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetWindowFromID";
   begin
      return W : constant Window :=
        (Ada.Finalization.Limited_Controlled with Internal => SDL_Get_Window_From_ID (Window_ID), Owns => False)
      do
         null;
      end return;
   end From_ID;

   procedure Get_Gamma_Ramp (Self : in Window; Red, Green, Blue : out SDL.Video.Pixel_Formats.Gamma_Ramp) is
      function SDL_Get_Window_Gamma_Ramp (W       : in SDL.C_Pointers.Windows_Pointer;
                                          R, G, B : out SDL.Video.Pixel_Formats.Gamma_Ramp) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetWindowGammaRamp";

      Result : constant C.int := SDL_Get_Window_Gamma_Ramp (Self.Internal, Red, Green, Blue);
   begin
      if Result /= Success then
         raise Window_Error with SDL.Error.Get;
      end if;
   end Get_Gamma_Ramp;

   procedure Set_Gamma_Ramp (Self : in out Window; Red, Green, Blue : in SDL.Video.Pixel_Formats.Gamma_Ramp) is
      function SDL_Set_Window_Gamma_Ramp (W       : in SDL.C_Pointers.Windows_Pointer;
                                          R, G, B : in SDL.Video.Pixel_Formats.Gamma_Ramp) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_SetWindowGammaRamp";

      Result : constant C.int := SDL_Set_Window_Gamma_Ramp (Self.Internal, Red, Green, Blue);
   begin
      if Result /= Success then
         raise Window_Error with SDL.Error.Get;
      end if;
   end Set_Gamma_Ramp;

   function Is_Grabbed (Self : in Window) return Boolean is
      function SDL_Get_Window_Grab (W : in SDL.C_Pointers.Windows_Pointer) return SDL_Bool with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetWindowGrab";
   begin
      return To_Boolean (SDL_Get_Window_Grab (Self.Internal));
   end Is_Grabbed;

   procedure Set_Grabbed (Self : in out Window; Grabbed : in Boolean := True) is
      procedure SDL_Set_Window_Grab (W : in SDL.C_Pointers.Windows_Pointer; G : in SDL_Bool) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_SetWindowGrab";
   begin
      SDL_Set_Window_Grab (Self.Internal, (if Grabbed then SDL_True else SDL_False));
   end Set_Grabbed;

   function Get_ID (Self : in Window) return ID is
      function SDL_Get_Window_ID (W : in SDL.C_Pointers.Windows_Pointer) return ID with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetWindowID";
   begin
      return SDL_Get_Window_ID (Self.Internal);
   end Get_ID;

   function Get_Maximum_Size (Self : in Window) return SDL.Sizes is
      procedure SDL_Get_Window_Maximum_Size (Win  : in SDL.C_Pointers.Windows_Pointer;
                                             W, H : out SDL.Dimension) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetWindowMaximumSize";

      W, H : C.int := 0;
   begin
      SDL_Get_Window_Maximum_Size (Self.Internal, W, H);

      return SDL.Sizes'(Width => W, Height => H);
   end Get_Maximum_Size;

   procedure Set_Maximum_Size (Self : in out Window; Size : in SDL.Sizes) is
      procedure SDL_Get_Window_Maximum_Size (Win : in SDL.C_Pointers.Windows_Pointer; W, H : in C.int) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_SetWindowMaximumSize";
   begin
      SDL_Get_Window_Maximum_Size (Self.Internal, C.int (Size.Width), C.int (Size.Height));
   end Set_Maximum_Size;

   function Get_Minimum_Size (Self : in Window) return SDL.Sizes is
      procedure SDL_Get_Window_Minimum_Size (Win  : in SDL.C_Pointers.Windows_Pointer;
                                             W, H : out SDL.Dimension) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetWindowMinimumSize";

      W, H : C.int := 0;
   begin
      SDL_Get_Window_Minimum_Size (Self.Internal, W, H);

      return SDL.Sizes'(Width => W, Height => H);
   end Get_Minimum_Size;

   procedure Set_Minimum_Size (Self : in out Window; Size : in SDL.Sizes) is
      procedure SDL_Get_Window_Minimum_Size (Win : in SDL.C_Pointers.Windows_Pointer; W, H : in C.int) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_SetWindowMinimumSize";
   begin
      SDL_Get_Window_Minimum_Size (Self.Internal, C.int (Size.Width), C.int (Size.Height));
   end Set_Minimum_Size;

   function Pixel_Format (Self : in Window) return SDL.Video.Pixel_Formats.Pixel_Format is
      function SDL_Get_Window_Pixel_Format (W : in SDL.C_Pointers.Windows_Pointer)
                                            return SDL.Video.Pixel_Formats.Pixel_Format with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetWindowPixelFormat";
   begin
      return SDL_Get_Window_Pixel_Format (Self.Internal);
   end Pixel_Format;

   function Get_Position (Self : in Window) return SDL.Natural_Coordinates is
      procedure SDL_Get_Window_Position (W : in SDL.C_Pointers.Windows_Pointer; X, Y : out C.int) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetWindowPosition";

      Position : SDL.Natural_Coordinates := SDL.Zero_Coordinate;
   begin
      SDL_Get_Window_Position (Self.Internal, Position.X, Position.Y);

      return Position;
   end Get_Position;

   procedure Set_Position (Self : in out Window; Position : SDL.Natural_Coordinates) is
      procedure SDL_Set_Window_Position (W : in SDL.C_Pointers.Windows_Pointer; X, Y : in C.int) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_SetWindowPosition";
   begin
      SDL_Set_Window_Position (Self.Internal, Position.X, Position.Y);
   end Set_Position;

   function Get_Size (Self : in Window) return SDL.Sizes is
      procedure SDL_Get_Window_Size (Win : in SDL.C_Pointers.Windows_Pointer; W, H : out SDL.Dimension) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetWindowSize";

      W, H : C.int := 0;
   begin
      SDL_Get_Window_Size (Self.Internal, W, H);

      return SDL.Sizes'(Width => W, Height => H);
   end Get_Size;

   procedure Set_Size (Self : in out Window; Size : in SDL.Sizes) is
      procedure SDL_Get_Window_Size (Win : in SDL.C_Pointers.Windows_Pointer; W, H : in C.int) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_SetWindowSize";
   begin
      SDL_Get_Window_Size (Self.Internal, C.int (Size.Width), C.int (Size.Height));
   end Set_Size;

   function Get_Surface (Self : in Window) return SDL.Video.Surfaces.Surface is
      function SDL_Get_Window_Surface (W : in SDL.C_Pointers.Windows_Pointer)
                                       return SDL.Video.Surfaces.Internal_Surface_Pointer with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetWindowSurface";

      use type SDL.Video.Surfaces.Internal_Surface_Pointer;

      S : constant SDL.Video.Surfaces.Internal_Surface_Pointer := SDL_Get_Window_Surface (Self.Internal);

      function Make_Surface_From_Pointer (S    : in SDL.Video.Surfaces.Internal_Surface_Pointer;
                                          Owns : in Boolean := False)
                                          return SDL.Video.Surfaces.Surface with
        Convention    => Ada,
        Import        => True;
   begin
      if S = null then
         raise Window_Error with SDL.Error.Get;
      end if;

      return Make_Surface_From_Pointer (S);
   end Get_Surface;

   function Get_Title (Self : in Window) return Ada.Strings.UTF_Encoding.UTF_8_String is
      function SDL_Get_Window_Title (W : in SDL.C_Pointers.Windows_Pointer) return C.Strings.chars_ptr with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetWindowTitle";
   begin
      return C.Strings.Value (SDL_Get_Window_Title (Self.Internal));
   end Get_Title;

   procedure Set_Title (Self : in Window; Title : in Ada.Strings.UTF_Encoding.UTF_8_String) is
      procedure SDL_Set_Window_Title (W : in SDL.C_Pointers.Windows_Pointer; C_Str : in C.char_array) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_SetWindowTitle";
   begin
      SDL_Set_Window_Title (Self.Internal, C.To_C (Title));
   end Set_Title;

   procedure Hide (Self : in Window) is
      procedure SDL_Hide_Window (W : in SDL.C_Pointers.Windows_Pointer) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_HideWindow";
   begin
      SDL_Hide_Window (Self.Internal);
   end Hide;

   procedure Show (Self : in Window) is
      procedure SDL_Show_Window (W : in SDL.C_Pointers.Windows_Pointer) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_ShowWindow";
   begin
      SDL_Show_Window (Self.Internal);
   end Show;

   procedure Maximise (Self : in Window) is
      procedure SDL_Maximise_Window (W : in SDL.C_Pointers.Windows_Pointer) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_MaximizeWindow";
   begin
      SDL_Maximise_Window (Self.Internal);
   end Maximise;

   procedure Minimise (Self : in Window) is
      procedure SDL_Minimise_Window (W : in SDL.C_Pointers.Windows_Pointer) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_MinimizeWindow";
   begin
      SDL_Minimise_Window (Self.Internal);
   end Minimise;

   procedure Raise_And_Focus (Self : in Window) is
      procedure SDL_Raise_Window (W : in SDL.C_Pointers.Windows_Pointer) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RaiseWindow";
   begin
      SDL_Raise_Window (Self.Internal);
   end Raise_And_Focus;

   procedure Restore (Self : in Window) is
      procedure SDL_Restore_Window (W : in SDL.C_Pointers.Windows_Pointer) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RestoreWindow";
   begin
      SDL_Restore_Window (Self.Internal);
   end Restore;

   procedure Set_Mode (Self : in out Window; Flags : in Full_Screen_Flags) is
      function SDL_Window_Full_Screen (W : in SDL.C_Pointers.Windows_Pointer;
                                       F : in Full_Screen_Flags) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_SetWindowFullscreen";

      Result : constant C.int := SDL_Window_Full_Screen (Self.Internal, Flags);
   begin
      if Result /= Success then
         raise Window_Error with SDL.Error.Get;
      end if;
   end Set_Mode;

   procedure Set_Icon (Self : in out Window; Icon : in SDL.Video.Surfaces.Surface) is
      procedure SDL_Set_Window_Icon (W : in SDL.C_Pointers.Windows_Pointer;
                                     S : SDL.Video.Surfaces.Internal_Surface_Pointer) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_SetWindowIcon";

      function Get_Internal_Surface (Self : in SDL.Video.Surfaces.Surface)
                                     return SDL.Video.Surfaces.Internal_Surface_Pointer with
        Import     => True,
        Convention => Ada;
   begin
      SDL_Set_Window_Icon (Self.Internal, Get_Internal_Surface (Icon));
   end Set_Icon;

   procedure Update_Surface (Self : in Window) is
      function SDL_Update_Window_Surface (W : in SDL.C_Pointers.Windows_Pointer) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_UpdateWindowSurface";

      Result : constant C.int := SDL_Update_Window_Surface (Self.Internal);
   begin
      if Result /= Success then
         raise Window_Error with SDL.Error.Get;
      end if;
   end Update_Surface;

   procedure Update_Surface_Rectangle (Self : in Window; Rectangle : in SDL.Video.Rectangles.Rectangle) is
      function SDL_Update_Window_Surface_Rects (W : in SDL.C_Pointers.Windows_Pointer;
                                                R : in SDL.Video.Rectangles.Rectangle;
                                                L : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_UpdateWindowSurfaceRects";

      Result : constant C.int := SDL_Update_Window_Surface_Rects (Self.Internal, Rectangle, 1);
   begin
      if Result /= Success then
         raise Window_Error with SDL.Error.Get;
      end if;
   end Update_Surface_Rectangle;

   procedure Update_Surface_Rectangles (Self : in Window; Rectangles : SDL.Video.Rectangles.Rectangle_Arrays) is
      function SDL_Update_Window_Surface_Rects (W : in SDL.C_Pointers.Windows_Pointer;
                                                R : in SDL.Video.Rectangles.Rectangle_Arrays;
                                                L : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_UpdateWindowSurfaceRects";

      Result : constant C.int := SDL_Update_Window_Surface_Rects (Self.Internal, Rectangles, Rectangles'Length);
   begin
      if Result /= Success then
         raise Window_Error with SDL.Error.Get;
      end if;
   end Update_Surface_Rectangles;

   function Exist return Boolean is
   begin
      return Total_Windows_Created /= Natural'First;
   end Exist;

   function Get_Internal_Window (Self : in Window) return SDL.C_Pointers.Windows_Pointer is
   begin
      return Self.Internal;
   end Get_Internal_Window;
end SDL.Video.Windows;
