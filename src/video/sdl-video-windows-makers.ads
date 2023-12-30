--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
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
   --  See: https://github.com/Lucretia/sdlada/issues/16#issuecomment-364142941
   procedure Create (Win : in out Window; Native : in Native_Window) with
     Inline => True;

end SDL.Video.Windows.Makers;
