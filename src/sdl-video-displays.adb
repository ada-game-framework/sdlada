package body SDL.Video.Displays is
   use type System.Address;
   use type C.int;

   function Closest_Mode (Display : in Natural; Wanted : in Mode; Target : out Mode) return Boolean is
      function SDL_Get_Closest_Display_Mode (D : C.int; W : in Mode; T : out Mode) return System.Address with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetClosestDisplayMode";

      Result : System.Address := SDL_Get_Closest_Display_Mode (C.int (Display), Wanted, Target);
   begin
      return (Result = System.Null_Address);
   end Closest_Mode;

   function Current_Mode (Display : in Natural; Target : out Mode) return Boolean is
      function SDL_Get_Current_Display_Mode (D : C.int; M : out Mode) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetCurrentDisplayMode";

      Result : C.int := SDL_Get_Current_Display_Mode (C.int (Display), Target);
   begin
      return (Result = Success);
   end Current_Mode;

   function Desktop_Mode (Display : in Natural; Target : out Mode) return Boolean is
      function SDL_Get_Desktop_Display_Mode (D : C.int; M : out Mode) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetDesktopDisplayMode";

      Result : C.int := SDL_Get_Desktop_Display_Mode (C.int (Display), Target);
   begin
      return (Result = Success);
   end Desktop_Mode;

   function Display_Mode (Display : in Natural; Index : in Natural; Target : out Mode) return Boolean is
      function SDL_Get_Display_Mode (D : in C.int; I : in C.int; T : out Mode) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetDisplayMode";

      Result : C.int := SDL_Get_Display_Mode (C.int (Display), C.int (Index), Target);
   begin
      return (Result = Success);
   end Display_Mode;

   function Total_Display_Modes (Display : in Natural; Total : out Positive) return Boolean is
      function SDL_Get_Num_Display_Modes (I : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetNumDisplayModes";

      Result : C.int := SDL_Get_Num_Display_Modes (C.int (Display));
   begin
      if Result >= 1 then
         Total := Positive (Result);

         return True;
      end if;

      return False;
   end Total_Display_Modes;

   function Display_Bounds (Display : in Natural; Bounds : out Rectangles.Rectangle) return Boolean is
      function SDL_Get_Display_Bounds (D : in C.int; B : out Rectangles.Rectangle) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetDisplayBounds";

      Result : C.int := SDL_Get_Display_Bounds (C.int (Display), Bounds);
   begin
      return (Result = Success);
   end Display_Bounds;
end SDL.Video.Displays;
