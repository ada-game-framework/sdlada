--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
with SDL.Error;

package body SDL.Video.Displays is

   function Total return Display_Indices is
      --  This function returns a value >= 1, use this as a new lower type bound.
      function SDL_Get_Num_Video_Displays return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetNumVideoDisplays";

      Num : constant C.int := SDL_Get_Num_Video_Displays;
   begin
      if Num <= 0 then
         raise Video_Error with SDL.Error.Get;
      end if;

      return Display_Indices (Num);
   end Total;

   function Closest_Mode (Display : in Display_Indices; Wanted : in Mode; Target : out Mode) return Boolean is
      function SDL_Get_Closest_Display_Mode (D : C.int; W : in Mode; T : out Mode) return Access_Mode with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetClosestDisplayMode";

      Result : constant Access_Mode := SDL_Get_Closest_Display_Mode (C.int (Display - 1), Wanted, Target);
   begin
      return (Result = null);
   end Closest_Mode;

   function Current_Mode (Display : in Display_Indices; Target : out Mode) return Boolean is
      function SDL_Get_Current_Display_Mode (D : C.int; M : out Mode) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetCurrentDisplayMode";

      Result : constant C.int := SDL_Get_Current_Display_Mode (C.int (Display - 1), Target);
   begin
      return (Result = Success);
   end Current_Mode;

   function Desktop_Mode (Display : in Display_Indices; Target : out Mode) return Boolean is
      function SDL_Get_Desktop_Display_Mode (D : C.int; M : out Mode) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetDesktopDisplayMode";

      Result : constant C.int := SDL_Get_Desktop_Display_Mode (C.int (Display - 1), Target);
   begin
      return (Result = Success);
   end Desktop_Mode;

   function Display_Mode (Display : in Display_Indices; Index : in Natural; Target : out Mode) return Boolean is
      function SDL_Get_Display_Mode (D : in C.int; I : in C.int; T : out Mode) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetDisplayMode";

      Result : constant C.int := SDL_Get_Display_Mode (C.int (Display - 1), C.int (Index), Target);
   begin
      return (Result = Success);
   end Display_Mode;

   function Total_Display_Modes (Display : in Display_Indices; Total : out Positive) return Boolean is
      function SDL_Get_Num_Display_Modes (I : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetNumDisplayModes";

      Result : constant C.int := SDL_Get_Num_Display_Modes (C.int (Display - 1));
   begin
      if Result >= 1 then
         Total := Positive (Result);

         return True;
      end if;

      return False;
   end Total_Display_Modes;

   function Display_Bounds (Display : in Display_Indices; Bounds : out Rectangles.Rectangle) return Boolean is
      function SDL_Get_Display_Bounds (D : in C.int; B : out Rectangles.Rectangle) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetDisplayBounds";

      Result : constant C.int := SDL_Get_Display_Bounds (C.int (Display - 1), Bounds);
   begin
      return (Result = Success);
   end Display_Bounds;
end SDL.Video.Displays;
