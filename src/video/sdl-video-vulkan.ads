--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
--  SDL.Video.Vulkan
--
--  Vulkan functionality.
--------------------------------------------------------------------------------------------------------------------
with Ada.Strings.Unbounded;
--  with Interfaces.C;

with SDL.Video.Windows;

generic
   --  These need to be the base C types from your Vulkan bindings.
   type Instance_Address_Type is private;

   Instance_Null : Instance_Address_Type;
package SDL.Video.Vulkan is
   pragma Preelaborate;

   --  package C renames Interfaces.C;

   SDL_Vulkan_Error : exception;

   type Extension_Name_Arrays is array (Positive range <>) of Ada.Strings.Unbounded.Unbounded_String;

   Null_Extension_Name_Array : constant Extension_Name_Arrays (1 .. 1) :=
     (others => Ada.Strings.Unbounded.Null_Unbounded_String);

   procedure Get_Drawable_Size (Window : in SDL.Video.Windows.Window; Width, Height : out SDL.Natural_Dimension);

   function Get_Instance_Extensions (Window : in SDL.Video.Windows.Window) return Extension_Name_Arrays;

   function Get_Instance_Procedure_Address return Instance_Address_Type;

   procedure Load_Library;
   procedure Load_Library (Path : in String);
   procedure Unload_Library with
     Inline => True;
end SDL.Video.Vulkan;
