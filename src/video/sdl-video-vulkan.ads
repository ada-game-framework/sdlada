--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
--  SDL.Video.Vulkan
--
--  Vulkan functionality.
--------------------------------------------------------------------------------------------------------------------
with Interfaces.C;
with System;

package SDL.Video.Vulkan is
   pragma Preelaborate;

   package C renames Interfaces.C;

   SDL_Vulkan_Error : exception;

   --  Warning: This *may* change.
   --  You will need to convert this to the required type using whatever the bindings you are using provides with
   --  System.Address_To_Access_Conversions package.
   function Get_Instance_Procedure_Address return System.Address;

   procedure Load_Library;
   procedure Load_Library (Path : in String);
   procedure Unload_Library with
     Inline => True;
end SDL.Video.Vulkan;
