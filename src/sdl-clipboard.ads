--                              -*- Mode: Ada -*-
--  Filename        : sdl-clipboard.ads
--  Description     : Functions to access the clipboard.
--  Author          : Luke A. Guest
--  Created On      : Wed Oct 30 14:41:45 2013
with Ada.Strings.UTF_Encoding;

package SDL.Clipboard is
   Clipboard_Error : exception;

   function Get return Ada.Strings.UTF_Encoding.UTF_8_String;
   function Is_Empty return Boolean;
   procedure Set (Text : in Ada.Strings.UTF_Encoding.UTF_8_String);
end SDL.Clipboard;
