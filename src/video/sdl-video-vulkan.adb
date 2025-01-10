--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
with Interfaces.C.Strings;

with SDL.Error;

package body SDL.Video.Vulkan is
   function Get_Instance_Procedure_Address return Instance_Address_Type is
      function SDL_Vulkan_GetVkGetInstanceProcAddr return Instance_Address_Type with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_Vulkan_GetVkGetInstanceProcAddr";

      Result : constant Instance_Address_Type := SDL_Vulkan_GetVkGetInstanceProcAddr;
   begin
      if Result = Instance_Null then
         raise SDL_Vulkan_Error with SDL.Error.Get;
      end if;

      return Result;
   end Get_Instance_Procedure_Address;


   --  Load the default Vulkan library.
   procedure Load_Library is
      function SDL_Vulkan_LoadLibrary (path : C.Strings.chars_ptr) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_Vulkan_LoadLibrary";

      Result : constant C.int := SDL_Vulkan_LoadLibrary (C.Strings.Null_Ptr);
   begin
      if Result /= SDL.Success then
         raise SDL_Vulkan_Error with "Unable to load the default Vulkan library";
      end if;
   end Load_Library;


   procedure Load_Library (Path : in String) is
      function SDL_Vulkan_LoadLibrary (path : C.char_array) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_Vulkan_LoadLibrary";
   begin
      if SDL_Vulkan_LoadLibrary (C.To_C (Path)) /= SDL.Success then
         raise SDL_Vulkan_Error with "Unable to load Vulkan library """ & Path & '"';
      end if;
   end Load_Library;


   procedure Unload_Library is
      procedure SDL_Vulkan_UnloadLibrary with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_Vulkan_UnloadLibrary";
   begin
      SDL_Vulkan_UnloadLibrary;
   end Unload_Library;
end SDL.Video.Vulkan;
