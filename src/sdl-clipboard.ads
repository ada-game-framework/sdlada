--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
--  SDL.Clipboard
--
--  Functions to access the clipboard.
--------------------------------------------------------------------------------------------------------------------
with Ada.Strings.UTF_Encoding;

package SDL.Clipboard is
   pragma Preelaborate;

   Clipboard_Error : exception;

   function Get return Ada.Strings.UTF_Encoding.UTF_8_String;
   function Is_Empty return Boolean;
   procedure Set (Text : in Ada.Strings.UTF_Encoding.UTF_8_String);
end SDL.Clipboard;
