--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
with Interfaces.C.Strings;
--  with Interfaces.C.Pointers;
with System;

with SDL.C_Pointers;
with SDL.Error;

package body SDL.Video.Vulkan is
   package C renames Interfaces.C;

   function Get_Internal_Window (Self : in SDL.Video.Windows.Window) return SDL.C_Pointers.Windows_Pointer with
     Import     => True,
     Convention => Ada;


   procedure Create_Surface (Window   : in SDL.Video.Windows.Window;
                             Instance : in Instance_Address_Type;
                             Surface  : out Surface_Type) is
      function SDL_Vulkan_Create_Surface
        (Window   : in SDL.C_Pointers.Windows_Pointer;
         Instance : in Instance_Address_Type;
         Surface  : out Surface_Type) return SDL_Bool with
           Import        => True,
           Convention    => C,
           External_Name => "SDL_Vulkan_CreateSurface";
   begin
      if SDL_Vulkan_Create_Surface (Get_Internal_Window (Window), Instance, Surface) = SDL_False then
         raise SDL_Vulkan_Error with SDL.Error.Get;
      end if;
   end Create_Surface;


   procedure Get_Drawable_Size (Window : in SDL.Video.Windows.Window; Width, Height : out SDL.Natural_Dimension) is
      procedure SDL_Vulkan_Get_Drawable_Size (Window : in SDL.C_Pointers.Windows_Pointer; W, H : out C.int) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_Vulkan_GetDrawableSize";
   begin
      SDL_Vulkan_Get_Drawable_Size (Get_Internal_Window (Window), Width, Height);
   end Get_Drawable_Size;


   function Get_Instance_Extensions (Window : in SDL.Video.Windows.Window) return Extension_Name_Arrays is
      function SDL_Vulkan_Get_Instance_Extensions
        (Window : SDL.C_Pointers.Windows_Pointer;
         Count  : access C.unsigned;
         Names  : System.Address) return SDL_Bool
      with Import => True,
           Convention => C,
           External_Name => "SDL_Vulkan_GetInstanceExtensions";

      function SDL_Vulkan_Get_Instance_Extensions
        (Window : SDL.C_Pointers.Windows_Pointer;
         Count  : access C.unsigned;
         Names  : C.Strings.chars_ptr_array) return SDL_Bool
      with Import => True,
           Convention => C,
           External_Name => "SDL_Vulkan_GetInstanceExtensions";


      Count : aliased C.unsigned;

      Result : constant Boolean := To_Boolean (SDL_Vulkan_Get_Instance_Extensions
        (Get_Internal_Window (Window),
         Count => Count'Access,
         Names => System.Null_Address));
   begin
      if Result then
         declare
            use type C.unsigned;

            Total_Extensions : constant Natural := Natural (Count) - 1;
            Extensions       : C.Strings.chars_ptr_array (0 .. C.size_t (Total_Extensions));
            Result           : constant Boolean := To_Boolean (SDL_Vulkan_Get_Instance_Extensions
              (Get_Internal_Window (Window),
               Count => Count'Access,  --  Dummy, no longer care about this.
               Names => Extensions));
            Names            : Vulkan.Extension_Name_Arrays (1 .. Positive (Count));

            use Ada.Strings.Unbounded;
         begin
            if Result then
               for Index in Names'Range loop
                  declare
                     use type C.Strings.chars_ptr;
                     use type C.size_t;

                     Real_Index : constant C.size_t            := C.size_t (Index) - 1;
                     C_String   : constant C.Strings.chars_ptr := Extensions (Real_Index);
                  begin
                     if C_String /= C.Strings.Null_Ptr then
                        Names (Index) := To_Unbounded_String (C.Strings.Value (C_String));
                     end if;
                  end;
               end loop;
            end if;

            return Names;
         end;
      end if;

      return Null_Extension_Name_Array;
   end Get_Instance_Extensions;


   function Get_Instance_Procedure_Address return Instance_Address_Type is
      function SDL_Vulkan_Get_Vk_Get_Instance_Proc_Addr return Instance_Address_Type with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_Vulkan_GetVkGetInstanceProcAddr";

      Result : constant Instance_Address_Type := SDL_Vulkan_Get_Vk_Get_Instance_Proc_Addr;
   begin
      if Result = Instance_Null then
         raise SDL_Vulkan_Error with SDL.Error.Get;
      end if;

      return Result;
   end Get_Instance_Procedure_Address;


   --  Load the default Vulkan library.
   procedure Load_Library is
      function SDL_Vulkan_Load_Library (Path : C.Strings.chars_ptr) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_Vulkan_LoadLibrary";
   begin
      if SDL_Vulkan_Load_Library (C.Strings.Null_Ptr) /= SDL.Success then
         raise SDL_Vulkan_Error with "Unable to load the default Vulkan library";
      end if;
   end Load_Library;


   procedure Load_Library (Path : in String) is
      function SDL_Vulkan_Load_Library (Path : C.char_array) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_Vulkan_LoadLibrary";
   begin
      if SDL_Vulkan_Load_Library (C.To_C (Path)) /= SDL.Success then
         raise SDL_Vulkan_Error with "Unable to load Vulkan library """ & Path & '"';
      end if;
   end Load_Library;


   procedure Unload_Library is
      procedure SDL_Vulkan_Unload_Library with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_Vulkan_UnloadLibrary";
   begin
      SDL_Vulkan_Unload_Library;
   end Unload_Library;
end SDL.Video.Vulkan;
