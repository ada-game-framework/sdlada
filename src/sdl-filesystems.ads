--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
--  SDL.RWops
--
--  Read/Write operations, i.e. file related machinery.
--------------------------------------------------------------------------------------------------------------------
with Ada.Strings.UTF_Encoding;

package SDL.Filesystems is
   package UTF_Strings renames Ada.Strings.UTF_Encoding;

   Filesystems_Error : exception;

   function Base_Path return UTF_Strings.UTF_String;

   function Preferences_Path (Organisation : in UTF_Strings.UTF_String;
                              Application  : in UTF_Strings.UTF_String) return UTF_Strings.UTF_String;
end SDL.Filesystems;
