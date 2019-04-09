--------------------------------------------------------------------------------------------------------------------
--  Copyright (c) 2013-2020, Luke A. Guest
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
--  SDL.Video.Windows.Makers
--
--  Constructor subprograms which don't dispatch for Window types.
--------------------------------------------------------------------------------------------------------------------
package SDL.Video.Windows.Makers is
   pragma Preelaborate;

   --  Create a window with the specified title, position, dimensions and
   --  flags. Put the result in Win.
   --
   --  Raise a SDL.Video.Windows.Window_Error if window creation failed.
   procedure Create
     (Win      : in out Window;
      Title    : in Ada.Strings.UTF_Encoding.UTF_8_String;
      Position : in SDL.Natural_Coordinates;
      Size     : in SDL.Positive_Sizes;
      Flags    : in Window_Flags := OpenGL);

   --  A small wrapper around the above procedure.
   procedure Create
     (Win    : in out Window;
      Title  : in Ada.Strings.UTF_Encoding.UTF_8_String;
      X      : in SDL.Natural_Coordinate;
      Y      : in SDL.Natural_Coordinate;
      Width  : in SDL.Positive_Dimension;
      Height : in SDL.Positive_Dimension;
      Flags  : in Window_Flags := OpenGL) with
     Inline => True;

   --  Create a window from an existing window created in some other way.
   procedure Create (Win : in out Window; Native : in Native_Window) with
     Inline => True;

end SDL.Video.Windows.Makers;
