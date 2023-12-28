--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
with Interfaces.C;
with Interfaces.C.Strings;
private with SDL.C_Pointers;
with SDL.Error;

package body SDL.Video.Windows.Makers is
   package C renames Interfaces.C;

   use type SDL.C_Pointers.Windows_Pointer;

   procedure Create
     (Win      : in out Window;
      Title    : in Ada.Strings.UTF_Encoding.UTF_8_String;
      Position : in SDL.Natural_Coordinates;
      Size     : in SDL.Positive_Sizes;
      Flags    : in Window_Flags := OpenGL) is

      function SDL_Create
        (Title      : C.Strings.chars_ptr;
         X, Y, W, H : in C.int;
         F          : in Window_Flags) return SDL.C_Pointers.Windows_Pointer with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_CreateWindow";

      C_Title_Str : C.Strings.chars_ptr := C.Strings.New_String (Title);
   begin
      Win.Internal := SDL_Create (C_Title_Str, Position.X, Position.Y, Size.Width, Size.Height, Flags);

      C.Strings.Free (C_Title_Str);

      if Win.Internal = null then
         raise Window_Error with SDL.Error.Get;
      end if;

      Increment_Windows;
   end Create;

   procedure Create
     (Win    : in out Window;
      Title  : in Ada.Strings.UTF_Encoding.UTF_8_String;
      X      : in SDL.Natural_Coordinate;
      Y      : in SDL.Natural_Coordinate;
      Width  : in SDL.Positive_Dimension;
      Height : in SDL.Positive_Dimension;
      Flags  : in Window_Flags := OpenGL) is
   begin
      Create (Win, Title, SDL.Natural_Coordinates'(X, Y), SDL.Positive_Sizes'(Width, Height), Flags);
   end Create;

   procedure Create (Win : in out Window; Native : in Native_Window) is
      function SDL_Create_Window_From (Native : Native_Window) return SDL.C_Pointers.Windows_Pointer with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_CreateWindowFrom";
   begin
      Win.Internal := SDL_Create_Window_From (Native);
      Win.Owns     := True;

      if Win.Internal = null then
         raise Window_Error with SDL.Error.Get;
      end if;

      Increment_Windows;
   end Create;
end SDL.Video.Windows.Makers;
