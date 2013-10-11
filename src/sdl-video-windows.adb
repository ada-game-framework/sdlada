with Ada.Unchecked_Conversion;
with Interfaces.C;
with Interfaces.C.Strings;

package body SDL.Video.Windows is
   package C renames Interfaces.C;

   use type C.int;

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

   procedure Create (Self : in out Window; Native : in Native_Window) is
      function SDL_Create_Window_From (Native : Native_Window) return System.Address with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_CreateWindowFrom";
   begin
      Self.Internal := SDL_Create_Window_From (Native);
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

   function Get_Brightness (Self : in Window) return Brightness is
      function SDL_Get_Brightness (W : in System.Address) return C.C_float With
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetWindowBrightness";
   begin
      return Brightness (SDL_Get_Brightness (Self.Internal));
   end Get_Brightness;

   procedure Set_Brightness (Self : in out Window; How_Bright : in Brightness) is
      function SDL_Set_Brightness (W : in System.Address; B : in C.C_float) return C.int With
        Import        => True,
        Convention    => C,
        External_Name => "SDL_SetWindowBrightness";

      Result : C.int := SDL_Set_Brightness (Self.Internal, C.C_float (How_Bright));
   begin
      if Result /= Success then
         raise Window_Error with "Unable to set brightness.";
      end if;
   end Set_Brightness;

   function To_Data_Access is new Ada.Unchecked_Conversion (Source => System.Address, Target => User_Data_Access);
   function To_Address is new Ada.Unchecked_Conversion (Source => User_Data_Access, Target => System.Address);

   function Get_Data (Self : in Window; Name : in String) return User_Data_Access is
      function SDL_Get_Window_Data (W : in System.Address; Name : in C.Strings.chars_ptr) return System.Address with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetWindowData";

      C_Name_Str : C.Strings.chars_ptr := C.Strings.New_String (Name);
      Item       : User_Data_Access    := To_Data_Access (SDL_Get_Window_Data (Self.Internal, C_Name_Str));
   begin
      C.Strings.Free (C_Name_Str);

      return Item;
   end Get_Data;

   function Set_Data (Self : in out Window; Name : in String; Item : in User_Data_Access) return User_Data_Access is
      function SDL_Set_Window_Data (W : in System.Address; Name : in C.Strings.chars_ptr; User_Data : in System.address)
                                   return System.Address with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_SetWindowData";

      C_Name_Str    : C.Strings.chars_ptr := C.Strings.New_String (Name);
      Previous_Data : User_Data_Access    := To_Data_Access (SDL_Set_Window_Data (Self.Internal, C_Name_Str, To_Address (Item)));
   begin
      C.Strings.Free (C_Name_Str);

      return Previous_Data;
   end Set_Data;

   function Display_Index (Self : in Window) return Positive is
      function SDL_Get_Window_Display_Index (W : in System.Address) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetWindowDisplayIndex";

      Total : C.int := SDL_Get_Window_Display_Index (Self.Internal);
   begin
      if Total < 0 then
         raise Window_Error with "Cannot get display index";
      end if;

      return Positive (Total);
   end Display_Index;
end SDL.Video.Windows;
