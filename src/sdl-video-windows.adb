with Interfaces.C;
with Interfaces.C.Strings;

package body SDL.Video.Windows is
   package C renames Interfaces.C;

   procedure Create
     (Self   : in out Window;
      Title  : in String;
      X      : in Integer;
      Y      : in Integer;
      Width  : in Integer;
      Height : in Integer;
      Flags  : in Window_Flags := OpenGL) is

      function SDL_Create
        (Title      : C.Strings.chars_ptr;
         X, Y, W, H : in C.int;
         F : in Window_Flags) return System.Address with

        Import        => True,
        Convention    => C,
        External_Name => "SDL_CreateWindow";

      C_Title_Str : C.Strings.chars_ptr := C.Strings.New_String (Title);
   begin
      Self.Internal := SDL_Create
        (C_Title_Str, C.int (X), C.int (Y), C.int (Width), C.int (Height), Flags);

      C.Strings.Free (C_Title_Str);
   end Create;

   procedure Finalize (Object : in out Window) is
      procedure SDL_Destroy (W : in System.Address) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_DestroyWindow";
   begin
      SDL_Destroy (Object.Internal);

      Object.Internal := System.Null_Address;
   end Finalize;

   function Get_Brightness (Self : in Window) return Float is
      function SDL_Get_Brightness (W : in System.Address) return C.C_float With
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetWindowBrightness";
   begin
      return Float (SDL_Get_Brightness (Self.Internal));
   end Get_Brightness;
end SDL.Video.Windows;
