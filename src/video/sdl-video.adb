--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------

with Interfaces.C.Strings;
with SDL.Error;

package body SDL.Video is

   function Is_Screen_Saver_Enabled return Boolean is
      function SDL_Is_Screen_Saver_Enabled return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_IsScreenSaverEnabled";
   begin
      return (if SDL_Is_Screen_Saver_Enabled = 1 then True else False);
   end Is_Screen_Saver_Enabled;

   function Initialise (Name : in String) return Boolean is
      function SDL_Video_Init (C_Name : in C.Strings.chars_ptr) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_VideoInit";

      C_Str  : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      Result : C.int;
   begin
      if Name /= "" then
         C_Str := C.Strings.New_String (Name);

         Result := SDL_Video_Init (C_Name => C_Str);

         C.Strings.Free (C_Str);
      else
         Result := SDL_Video_Init (C_Name => C.Strings.Null_Ptr);
      end if;

      return (Result = Success);
   end Initialise;

   function Total_Drivers return Positive is
      function SDL_Get_Num_Video_Drivers return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetNumVideoDrivers";

      Num : constant C.int := SDL_Get_Num_Video_Drivers;
   begin
      if Num < 0 then
         raise Video_Error with SDL.Error.Get;
      end if;

      return Positive (Num);
   end Total_Drivers;

   function Driver_Name (Index : in Positive) return String is
      function SDL_Get_Video_Driver (I : in C.int) return C.Strings.chars_ptr with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetVideoDriver";

      --  Index is zero based, so need to subtract 1 to correct it.
      C_Str : constant C.Strings.chars_ptr := SDL_Get_Video_Driver (C.int (Index) - 1);
   begin
      return C.Strings.Value (C_Str);
   end Driver_Name;

   function Current_Driver_Name return String is
      function SDL_Get_Current_Video_Driver return C.Strings.chars_ptr with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetCurrentVideoDriver";

      C_Str : constant C.Strings.chars_ptr := SDL_Get_Current_Video_Driver;

      use type C.Strings.chars_ptr;
   begin
      if C_Str = C.Strings.Null_Ptr then
         raise Video_Error with SDL.Error.Get;
      end if;

      return C.Strings.Value (C_Str);
   end Current_Driver_Name;
end SDL.Video;
