--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
with Ada.Unchecked_Conversion;
with Interfaces.C.Strings;
with SDL.Error;

package body SDL.Video.GL is
   package C renames Interfaces.C;

   use type SDL.C_Pointers.GL_Context_Pointer;

   pragma Warnings (Off, """Attribute_Retained_Backing"" is not referenced"); --  This attribute is deprecated
   type Attributes is
     (Attribute_Red_Size,
      Attribute_Green_Size,
      Attribute_Blue_Size,
      Attribute_Alpha_Size,
      Attribute_Buffer_Size,
      Attribute_Double_Buffer,
      Attribute_Depth_Buffer_Size,
      Attribute_Stencil_Size,
      Attribute_Accumulator_Red_Size,
      Attribute_Accumulator_Green_Size,
      Attribute_Accumulator_Blue_Size,
      Attribute_Accumulator_Alpha_Size,
      Attribute_Stereo,
      Attribute_Multisample_Buffers,
      Attribute_Multisample_Samples,
      Attribute_Accelerated,
      Attribute_Retained_Backing, --  not used (deprecated)
      Attribute_Context_Major_Version,
      Attribute_Context_Minor_Version,
      Attribute_Context_EGL,
      Attribute_Context_Flags,
      Attribute_Context_Profile,
      Attribute_Share_With_Current_Context) with
     Convention => C;
   pragma Warnings (On, """Attribute_Retained_Backing"" is not referenced");

   function To_int is new Ada.Unchecked_Conversion (Source => Profiles, Target => C.int);

   function SDL_GL_Set_Attribute (Attr : in Attributes; Value : in C.int) return C.int with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_GL_SetAttribute";

   function SDL_GL_Get_Attribute (Attr : in Attributes; Value : out C.int) return C.int with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_GL_GetAttribute";

   function Red_Size return Colour_Bit_Size is
      Data   : C.int;
      Result : constant C.int := SDL_GL_Get_Attribute (Attribute_Red_Size, Data);
   begin
      if Result /= Success then
         raise SDL_GL_Error with SDL.Error.Get;
      end if;

      return Colour_Bit_Size (Data);
   end Red_Size;

   procedure Set_Red_Size (Size : in Colour_Bit_Size) is
      Result : constant C.int := SDL_GL_Set_Attribute (Attribute_Red_Size, C.int (Size));
   begin
      if Result /= Success then
         raise SDL_GL_Error with SDL.Error.Get;
      end if;
   end Set_Red_Size;

   function Green_Size return Colour_Bit_Size is
      Data   : C.int;
      Result : constant C.int := SDL_GL_Get_Attribute (Attribute_Green_Size, Data);
   begin
      if Result /= Success then
         raise SDL_GL_Error with SDL.Error.Get;
      end if;

      return Colour_Bit_Size (Data);
   end Green_Size;

   procedure Set_Green_Size (Size : in Colour_Bit_Size) is
      Result : constant C.int := SDL_GL_Set_Attribute (Attribute_Green_Size, C.int (Size));
   begin
      if Result /= Success then
         raise SDL_GL_Error with SDL.Error.Get;
      end if;
   end Set_Green_Size;

   function Blue_Size return Colour_Bit_Size is
      Data   : C.int;
      Result : constant C.int := SDL_GL_Get_Attribute (Attribute_Blue_Size, Data);
   begin
      if Result /= Success then
         raise SDL_GL_Error with SDL.Error.Get;
      end if;

      return Colour_Bit_Size (Data);
   end Blue_Size;

   procedure Set_Blue_Size (Size : in Colour_Bit_Size) is
      Result : constant C.int := SDL_GL_Set_Attribute (Attribute_Blue_Size, C.int (Size));
   begin
      if Result /= Success then
         raise SDL_GL_Error with SDL.Error.Get;
      end if;
   end Set_Blue_Size;

   function Alpha_Size return Colour_Bit_Size is
      Data   : C.int;
      Result : constant C.int := SDL_GL_Get_Attribute (Attribute_Alpha_Size, Data);
   begin
      if Result /= Success then
         raise SDL_GL_Error with SDL.Error.Get;
      end if;

      return Colour_Bit_Size (Data);
   end Alpha_Size;

   procedure Set_Alpha_Size (Size : in Colour_Bit_Size) is
      Result : constant C.int := SDL_GL_Set_Attribute (Attribute_Alpha_Size, C.int (Size));
   begin
      if Result /= Success then
         raise SDL_GL_Error with SDL.Error.Get;
      end if;
   end Set_Alpha_Size;

   function Buffer_Size return Buffer_Sizes is
      Data   : C.int;
      Result : constant C.int := SDL_GL_Get_Attribute (Attribute_Buffer_Size, Data);
   begin
      if Result /= Success then
         raise SDL_GL_Error with SDL.Error.Get;
      end if;

      return Buffer_Sizes (Data);
   end Buffer_Size;

   procedure Set_Buffer_Size (Size : in Buffer_Sizes) is
      Result : constant C.int := SDL_GL_Set_Attribute (Attribute_Buffer_Size, C.int (Size));
   begin
      if Result /= Success then
         raise SDL_GL_Error with SDL.Error.Get;
      end if;
   end Set_Buffer_Size;

   function Is_Double_Buffered return Boolean is
      Data   : C.int;
      Result : constant C.int := SDL_GL_Get_Attribute (Attribute_Double_Buffer, Data);
   begin
      if Result /= Success then
         raise SDL_GL_Error with SDL.Error.Get;
      end if;

      return (Data = 1);
   end Is_Double_Buffered;

   procedure Set_Double_Buffer (On : in Boolean) is
      Data   : constant C.int := (if On then 1 else 0);
      Result : constant C.int := SDL_GL_Set_Attribute (Attribute_Double_Buffer, Data);
   begin
      if Result /= Success then
         raise SDL_GL_Error with SDL.Error.Get;
      end if;
   end Set_Double_Buffer;

   function Depth_Buffer_Size return Depth_Buffer_Sizes is
      Data   : C.int;
      Result : constant C.int := SDL_GL_Get_Attribute (Attribute_Depth_Buffer_Size, Data);
   begin
      if Result /= Success then
         raise SDL_GL_Error with SDL.Error.Get;
      end if;

      return Depth_Buffer_Sizes (Data);
   end Depth_Buffer_Size;

   procedure Set_Depth_Buffer_Size (Size : in Depth_Buffer_Sizes) is
      Result : constant C.int := SDL_GL_Set_Attribute (Attribute_Depth_Buffer_Size, C.int (Size));
   begin
      if Result /= Success then
         raise SDL_GL_Error with SDL.Error.Get;
      end if;
   end Set_Depth_Buffer_Size;

   function Stencil_Buffer_Size return Stencil_Buffer_Sizes is
      Data   : C.int;
      Result : constant C.int := SDL_GL_Get_Attribute (Attribute_Stencil_Size, Data);
   begin
      if Result /= Success then
         raise SDL_GL_Error with SDL.Error.Get;
      end if;

      return Stencil_Buffer_Sizes (Data);
   end Stencil_Buffer_Size;

   procedure Set_Stencil_Buffer_Size (Size : in Stencil_Buffer_Sizes) is
      Result : constant C.int := SDL_GL_Set_Attribute (Attribute_Stencil_Size, C.int (Size));
   begin
      if Result /= Success then
         raise SDL_GL_Error with SDL.Error.Get;
      end if;
   end Set_Stencil_Buffer_Size;

   function Accumulator_Red_Size return Colour_Bit_Size is
      Data   : C.int;
      Result : constant C.int := SDL_GL_Get_Attribute (Attribute_Accumulator_Red_Size, Data);
   begin
      if Result /= Success then
         raise SDL_GL_Error with SDL.Error.Get;
      end if;

      return Colour_Bit_Size (Data);
   end Accumulator_Red_Size;

   procedure Set_Accumulator_Red_Size (Size : in Colour_Bit_Size) is
      Result : constant C.int := SDL_GL_Set_Attribute (Attribute_Accumulator_Red_Size, C.int (Size));
   begin
      if Result /= Success then
         raise SDL_GL_Error with SDL.Error.Get;
      end if;
   end Set_Accumulator_Red_Size;

   function Accumulator_Green_Size return Colour_Bit_Size is
      Data   : C.int;
      Result : constant C.int := SDL_GL_Get_Attribute (Attribute_Accumulator_Green_Size, Data);
   begin
      if Result /= Success then
         raise SDL_GL_Error with SDL.Error.Get;
      end if;

      return Colour_Bit_Size (Data);
   end Accumulator_Green_Size;

   procedure Set_Accumulator_Green_Size (Size : in Colour_Bit_Size) is
      Result : constant C.int := SDL_GL_Set_Attribute (Attribute_Accumulator_Green_Size, C.int (Size));
   begin
      if Result /= Success then
         raise SDL_GL_Error with SDL.Error.Get;
      end if;
   end Set_Accumulator_Green_Size;

   function Accumulator_Blue_Size return Colour_Bit_Size is
      Data   : C.int;
      Result : constant C.int := SDL_GL_Get_Attribute (Attribute_Accumulator_Blue_Size, Data);
   begin
      if Result /= Success then
         raise SDL_GL_Error with SDL.Error.Get;
      end if;

      return Colour_Bit_Size (Data);
   end Accumulator_Blue_Size;

   procedure Set_Accumulator_Blue_Size (Size : in Colour_Bit_Size) is
      Result : constant C.int := SDL_GL_Set_Attribute (Attribute_Accumulator_Blue_Size, C.int (Size));
   begin
      if Result /= Success then
         raise SDL_GL_Error with SDL.Error.Get;
      end if;
   end Set_Accumulator_Blue_Size;

   function Accumulator_Alpha_Size return Colour_Bit_Size is
      Data   : C.int;
      Result : constant C.int := SDL_GL_Get_Attribute (Attribute_Accumulator_Alpha_Size, Data);
   begin
      if Result /= Success then
         raise SDL_GL_Error with SDL.Error.Get;
      end if;

      return Colour_Bit_Size (Data);
   end Accumulator_Alpha_Size;

   procedure Set_Accumulator_Alpha_Size (Size : in Colour_Bit_Size) is
      Result : constant C.int := SDL_GL_Set_Attribute (Attribute_Accumulator_Alpha_Size, C.int (Size));
   begin
      if Result /= Success then
         raise SDL_GL_Error with SDL.Error.Get;
      end if;
   end Set_Accumulator_Alpha_Size;

   function Is_Stereo return Boolean is
      Data   : C.int;
      Result : constant C.int := SDL_GL_Get_Attribute (Attribute_Stereo, Data);
   begin
      if Result /= Success then
         raise SDL_GL_Error with SDL.Error.Get;
      end if;

      return (Data = 1);
   end Is_Stereo;

   procedure Set_Stereo (On : in Boolean) is
      Data   : constant C.int := (if On then 1 else 0);
      Result : constant C.int := SDL_GL_Set_Attribute (Attribute_Stereo, Data);
   begin
      if Result /= Success then
         raise SDL_GL_Error with SDL.Error.Get;
      end if;
   end Set_Stereo;

   function Is_Multisampled return Boolean is
      Data   : C.int;
      Result : constant C.int := SDL_GL_Get_Attribute (Attribute_Multisample_Buffers, Data);
   begin
      if Result /= Success then
         raise SDL_GL_Error with SDL.Error.Get;
      end if;

      return (Data = 1);
   end Is_Multisampled;

   procedure Set_Multisampling (On : in Boolean) is
      Data   : constant C.int := (if On then 1 else 0);
      Result : constant C.int := SDL_GL_Set_Attribute (Attribute_Multisample_Buffers, Data);
   begin
      if Result /= Success then
         raise SDL_GL_Error with SDL.Error.Get;
      end if;
   end Set_Multisampling;

   function Multisampling_Samples return Multisample_Samples is
      Data   : C.int;
      Result : constant C.int := SDL_GL_Get_Attribute (Attribute_Multisample_Samples, Data);
   begin
      if Result /= Success then
         raise SDL_GL_Error with SDL.Error.Get;
      end if;

      return Multisample_Samples (Data);
   end Multisampling_Samples;

   procedure Set_Multisampling_Samples (Samples : in Multisample_Samples) is
      Result : constant C.int := SDL_GL_Set_Attribute (Attribute_Multisample_Samples, C.int (Samples));
   begin
      if Result /= Success then
         raise SDL_GL_Error with SDL.Error.Get;
      end if;
   end Set_Multisampling_Samples;

   function Is_Accelerated return Boolean is
      Data   : C.int;
      Result : constant C.int := SDL_GL_Get_Attribute (Attribute_Accelerated, Data);
   begin
      if Result /= Success then
         raise SDL_GL_Error with SDL.Error.Get;
      end if;

      return (Result = 1);
   end Is_Accelerated;

   procedure Set_Accelerated (On : in Boolean) is
      Data   : constant C.int := (if On then 1 else 0);
      Result : constant C.int := SDL_GL_Set_Attribute (Attribute_Accelerated, Data);
   begin
      if Result /= Success then
         raise SDL_GL_Error with SDL.Error.Get;
      end if;
   end Set_Accelerated;

   function Context_Major_Version return Major_Versions is
      Data   : C.int;
      Result : constant C.int := SDL_GL_Get_Attribute (Attribute_Context_Major_Version, Data);
   begin
      if Result /= Success then
         raise SDL_GL_Error with SDL.Error.Get;
      end if;

      return Major_Versions (Data);
   end Context_Major_Version;

   procedure Set_Context_Major_Version (Version : Major_Versions) is
      Result : constant C.int := SDL_GL_Set_Attribute (Attribute_Context_Major_Version, C.int (Version));
   begin
      if Result /= Success then
         raise SDL_GL_Error with SDL.Error.Get;
      end if;
   end Set_Context_Major_Version;

   function Context_Minor_Version return Minor_Versions is
      Data   : C.int;
      Result : constant C.int := SDL_GL_Get_Attribute (Attribute_Context_Minor_Version, Data);
   begin
      if Result /= Success then
         raise SDL_GL_Error with SDL.Error.Get;
      end if;

      return Minor_Versions (Data);
   end Context_Minor_Version;

   procedure Set_Context_Minor_Version (Version : Minor_Versions) is
      Result : constant C.int := SDL_GL_Set_Attribute (Attribute_Context_Minor_Version, C.int (Version));
   begin
      if Result /= Success then
         raise SDL_GL_Error with SDL.Error.Get;
      end if;
   end Set_Context_Minor_Version;

   function Is_Context_EGL return Boolean is
      Data   : C.int;
      Result : constant C.int := SDL_GL_Get_Attribute (Attribute_Context_EGL, Data);
   begin
      if Result /= Success then
         raise SDL_GL_Error with SDL.Error.Get;
      end if;

      return (Data = 1);
   end Is_Context_EGL;

   procedure Set_Context_EGL (On : in Boolean) is
      Data   : constant C.int := (if On then 1 else 0);
      Result : constant C.int := SDL_GL_Set_Attribute (Attribute_Context_EGL, Data);
   begin
      if Result /= Success then
         raise SDL_GL_Error with SDL.Error.Get;
      end if;
   end Set_Context_EGL;

   function Context_Flags return Flags is
      Data   : C.int;
      Result : constant C.int := SDL_GL_Get_Attribute (Attribute_Context_Flags, Data);
   begin
      if Result /= Success then
         raise SDL_GL_Error with SDL.Error.Get;
      end if;

      return Flags (Data);
   end Context_Flags;

   procedure Set_Context_Flags (Context_Flags : in Flags) is
      Result : constant C.int := SDL_GL_Set_Attribute (Attribute_Context_Flags, C.int (Context_Flags));
   begin
      if Result /= Success then
         raise SDL_GL_Error with SDL.Error.Get;
      end if;
   end Set_Context_Flags;

   function Context_Profile return Profiles is
      Data   : C.int;
      Result : constant C.int := SDL_GL_Get_Attribute (Attribute_Context_Profile, Data);
   begin
      if Result /= Success then
         raise SDL_GL_Error with SDL.Error.Get;
      end if;

      return Profiles'Val (Data);
   end Context_Profile;

   procedure Set_Context_Profile (Profile : in Profiles) is
      Result : constant C.int := SDL_GL_Set_Attribute (Attribute_Context_Profile, To_int (Profile));
   begin
      if Result /= Success then
         raise SDL_GL_Error with SDL.Error.Get;
      end if;
   end Set_Context_Profile;

   procedure Set_Core_Context_Profile (Major : in Major_Versions; Minor : Minor_Versions) is
      Result : C.int;
   begin
      Result := SDL_GL_Set_Attribute (Attribute_Context_Profile, To_int (Core));

      if Result /= Success then
         raise SDL_GL_Error with SDL.Error.Get;
      end if;

      Result := SDL_GL_Set_Attribute (Attribute_Context_Major_Version, C.int (Major));

      if Result /= Success then
         raise SDL_GL_Error with SDL.Error.Get;
      end if;

      Result := SDL_GL_Set_Attribute (Attribute_Context_Minor_Version, C.int (Minor));

      if Result /= Success then
         raise SDL_GL_Error with SDL.Error.Get;
      end if;
   end Set_Core_Context_Profile;

   function Is_Sharing_With_Current_Context return Boolean is
      Data   : C.int;
      Result : constant C.int := SDL_GL_Get_Attribute (Attribute_Share_With_Current_Context, Data);
   begin
      if Result /= Success then
         raise SDL_GL_Error with SDL.Error.Get;
      end if;

      return (Data = 1);
   end Is_Sharing_With_Current_Context;

   procedure Set_Share_With_Current_Context (On : in Boolean) is
      Data   : constant C.int := (if On then 1 else 0);
      Result : constant C.int := SDL_GL_Set_Attribute (Attribute_Share_With_Current_Context, Data);
   begin
      if Result /= Success then
         raise SDL_GL_Error with SDL.Error.Get;
      end if;
   end Set_Share_With_Current_Context;

   --  Some helper functions to get make this type work ok.
   function Get_Internal_Window (Self : in SDL.Video.Windows.Window) return SDL.C_Pointers.Windows_Pointer with
     Import     => True,
     Convention => Ada;

   function Get_Internal_Texture (Self : in SDL.Video.Textures.Texture) return SDL.C_Pointers.Texture_Pointer with
     Import     => True,
     Convention => Ada;

   --  The GL context.

   procedure Create (Self : in out Contexts; From : in SDL.Video.Windows.Window) is
      function SDL_GL_Create_Context (W : in SDL.C_Pointers.Windows_Pointer)
                                      return SDL.C_Pointers.GL_Context_Pointer with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GL_CreateContext";

      C : constant SDL.C_Pointers.GL_Context_Pointer := SDL_GL_Create_Context (Get_Internal_Window (From));
   begin
      if C = null then
         raise SDL_GL_Error with SDL.Error.Get;
      end if;

      Self.Internal := C;
      Self.Owns     := True;
   end Create;

   overriding
   procedure Finalize (Self : in out Contexts) is
      procedure SDL_GL_Delete_Context (W : in SDL.C_Pointers.GL_Context_Pointer) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GL_DeleteContext";
   begin
      --  We have to own this pointer before we go any further...
      --  and make sure we don't delete this twice if we do!
      if Self.Internal /= null and then Self.Owns then
         SDL_GL_Delete_Context (Self.Internal);

         Self.Internal := null;
      end if;
   end Finalize;

   --  TODO: Make sure we make all similar functions across the API match this pattern.
   --  Create a temporary Context.
   function Get_Current return Contexts is
      function SDL_GL_Get_Current_Context return SDL.C_Pointers.GL_Context_Pointer with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GL_GetCurrentContext";
   begin
      return C : constant Contexts := (Ada.Finalization.Limited_Controlled with
                                         Internal => SDL_GL_Get_Current_Context, Owns => False)
      do
         if C.Internal = null then
            raise SDL_GL_Error with SDL.Error.Get;
         end if;
      end return;
   end Get_Current;


   --  extern DECLSPEC void SDLCALL SDL_GL_GetDrawableSize(SDL_Window * window, int *w, int *h);
   procedure Get_Drawable_Size (Window : in SDL.Video.Windows.Window; Width, Height : out SDL.Natural_Dimension) is
      procedure SDL_GL_Get_Drawable_Size (Win : in SDL.C_Pointers.Windows_Pointer; W, H : out C.int) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GL_GetDrawableSize";
   begin
      SDL_GL_Get_Drawable_Size (Get_Internal_Window (Window), Width, Height);
   end Get_Drawable_Size;


   procedure Set_Current (Self : in Contexts; To : in SDL.Video.Windows.Window) is
      function SDL_GL_Make_Current (W       : in SDL.C_Pointers.Windows_Pointer;
                                    Context : in SDL.C_Pointers.GL_Context_Pointer) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GL_MakeCurrent";

      Result : constant C.int := SDL_GL_Make_Current (Get_Internal_Window (To), Self.Internal);
   begin
      if Result /= Success then
         raise SDL_GL_Error with SDL.Error.Get;
      end if;
   end Set_Current;

   procedure Bind_Texture (Texture : in SDL.Video.Textures.Texture) is
      function SDL_GL_Bind_Texture (T : in SDL.C_Pointers.Texture_Pointer) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GL_BindTexture";
   begin
      if SDL_GL_Bind_Texture (Get_Internal_Texture (Texture)) /= SDL.Success then
         raise SDL_GL_Error with "Cannot bind texture, unsupported operation in this context.";
      end if;
   end Bind_Texture;

   procedure Bind_Texture (Texture : in SDL.Video.Textures.Texture; Size : out SDL.Sizes) is
      function SDL_GL_Bind_Texture (T : in SDL.C_Pointers.Texture_Pointer; W, H : out C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GL_BindTexture";
   begin
      if SDL_GL_Bind_Texture (Get_Internal_Texture (Texture), Size.Width, Size.Height) /= SDL.Success then
         raise SDL_GL_Error with "Cannot bind texture, unsupported operation in this context.";
      end if;
   end Bind_Texture;

   procedure Unbind_Texture (Texture : in SDL.Video.Textures.Texture) is
      function SDL_GL_Unbind_Texture (T : in SDL.C_Pointers.Texture_Pointer) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GL_UnbindTexture";
   begin
      if SDL_GL_Unbind_Texture (Get_Internal_Texture (Texture)) /= SDL.Success then
         raise SDL_GL_Error with "Cannot unbind texture, unsupported operation in this context.";
      end if;
   end Unbind_Texture;


   function Get_Sub_Program (Name : in String) return Access_To_Sub_Program is
      function SDL_GL_Get_Proc_Address (P : in C.char_array) return Access_To_Sub_Program with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GL_GetProcAddress";
   begin
      return SDL_GL_Get_Proc_Address (C.To_C (Name));
   end Get_Sub_Program;


   function Get_Subprogram return Access_To_Sub_Program is
      function SDL_GL_Get_Proc_Address (P : in C.char_array) return Access_To_Sub_Program with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GL_GetProcAddress";
   begin
      return SDL_GL_Get_Proc_Address (C.To_C (Subprogram_Name));
   end Get_Subprogram;


   function Supports (Extension : in String) return Boolean is
      function SDL_GL_Extension_Supported (E : in C.Strings.chars_ptr) return SDL_Bool with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GL_ExtensionSupported";

      C_Name_Str : C.Strings.chars_ptr := C.Strings.New_String (Extension);
      Result     : constant Boolean    := To_Boolean (SDL_GL_Extension_Supported (C_Name_Str));
   begin
      C.Strings.Free (C_Name_Str);

      return Result;
   end Supports;

   function Get_Swap_Interval return Swap_Intervals is
      function SDL_GL_Get_Swap_Interval return Swap_Intervals with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GL_GetSwapInterval";
   begin
      return SDL_GL_Get_Swap_Interval;
   end Get_Swap_Interval;

   procedure Set_Swap_Interval (Interval : in Allowed_Swap_Intervals) is
      function SDL_GL_Set_Swap_Interval (Interval : in Swap_Intervals) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GL_SetSwapInterval";

      Result : constant C.int := SDL_GL_Set_Swap_Interval (Interval);
   begin
      if Result /= SDL.Success then
         raise SDL_GL_Error with SDL.Error.Get;
      end if;
   end Set_Swap_Interval;

   procedure Swap (Window : in out SDL.Video.Windows.Window) is
      procedure SDL_GL_Swap_Window (W : in SDL.C_Pointers.Windows_Pointer) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GL_SwapWindow";
   begin
      SDL_GL_Swap_Window (Get_Internal_Window (Window));
   end Swap;

   function SDL_GL_Load_Library (P : in C.char_array) return C.int with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_GL_LoadLibrary";

   function SDL_GL_Load_Library (P : in C.Strings.chars_ptr) return C.int with
     Import        => True,
     Convention    => C,
     External_Name => "SDL_GL_LoadLibrary";

   --  Load the default OpenGL library.
   procedure Load_Library is
      Result : constant C.int := SDL_GL_Load_Library (C.Strings.Null_Ptr);
   begin
      if Result /= SDL.Success then
         raise SDL_GL_Error with "Unable to load the default OpenGL library";
      end if;
   end Load_Library;

   procedure Load_Library (Path : in String) is
   begin
      if SDL_GL_Load_Library (C.To_C (Path)) /= SDL.Success then
         raise SDL_GL_Error with "Unable to load OpenGL library """ & Path & '"';
      end if;
   end Load_Library;

   procedure Unload_Library is
      procedure SDL_GL_Unload_Library with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GL_UnloadLibrary";
   begin
      SDL_GL_Unload_Library;
   end Unload_Library;
end SDL.Video.GL;
