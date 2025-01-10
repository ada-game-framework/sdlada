--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
--  SDL.Video.Vulkan
--
--  Vulkan functionality.
--------------------------------------------------------------------------------------------------------------------
with Interfaces.C;

generic
   --  These need to be the base C types from your Vulkan bindings.
   type Instance_Address_Type is private;

   Instance_Null : Instance_Address_Type;
package SDL.Video.Vulkan is
   pragma Preelaborate;

   package C renames Interfaces.C;

   SDL_Vulkan_Error : exception;

   function Get_Instance_Procedure_Address return Instance_Address_Type;

   procedure Load_Library;
   procedure Load_Library (Path : in String);
   procedure Unload_Library with
     Inline => True;
end SDL.Video.Vulkan;
