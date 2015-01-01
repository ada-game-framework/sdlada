--------------------------------------------------------------------------------------------------------------------
--  Copyright (c) 2014-2015 Luke A. Guest
--
--  This software is provided 'as-is', without any express or implied
--  warranty. In no event will the authors be held liable for any damages
--  arising from the use of this software.
--
--  Permission is granted to anyone to use this software for any purpose,
--  including commercial applications, and to alter it and redistribute it
--  freely, subject to the following restrictions:
--
--     1. The origin of this software must not be misrepresented; you must not
--     claim that you wrote the original software. If you use this software
--     in a product, an acknowledgment in the product documentation would be
--     appreciated but is not required.
--
--     2. Altered source versions must be plainly marked as such, and must not be
--     misrepresented as being the original software.
--
--     3. This notice may not be removed or altered from any source
--     distribution.
--------------------------------------------------------------------------------------------------------------------
--  SDL.Clipboard
--
--  Functions to access the clipboard.
--------------------------------------------------------------------------------------------------------------------
with Ada.Strings.UTF_Encoding;

package SDL.Clipboard is
   Clipboard_Error : exception;

   function Get return Ada.Strings.UTF_Encoding.UTF_8_String;
   function Is_Empty return Boolean;
   procedure Set (Text : in Ada.Strings.UTF_Encoding.UTF_8_String);
end SDL.Clipboard;
