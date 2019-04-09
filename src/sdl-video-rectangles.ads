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
--  SDL.Video.Rectangles
--
--  Rectangle bounding areas.
--------------------------------------------------------------------------------------------------------------------
with Interfaces.C;

package SDL.Video.Rectangles is
   pragma Preelaborate;

   package C renames Interfaces.C;

   Rectangle_Error : exception;

   type Size_Arrays is array (C.size_t range <>) of aliased SDL.Sizes with
     Convention => C;

   subtype Point is SDL.Coordinates;

   type Point_Arrays is array (C.size_t range <>) of aliased Point with
     Convention => C;

   type Line_Segment is
      record
         Start  : SDL.Coordinates;
         Finish : SDL.Coordinates;
      end record with
     Convention => C;

   type Line_Arrays is array (C.size_t range <>) of aliased Line_Segment with
     Convention => C;

   --  TODO: Replace with Point and Sizes?
   type Rectangle is
      record
         X      : SDL.Coordinate;
         Y      : SDL.Coordinate;
         Width  : SDL.Natural_Dimension;
         Height : SDL.Natural_Dimension;
      end record with
     Convention => C;

   Null_Rectangle : constant Rectangle := (others => 0);

   type Rectangle_Arrays is array (C.size_t range <>) of aliased Rectangle with
     Convention => C;

   type Rectangle_Access is access all Rectangle with
     Convention => C;

   function Enclose (Points : in Point_Arrays; Clip : in Rectangle; Enclosed : out Rectangle) return Boolean;
   procedure Enclose (Points : in Point_Arrays; Enclosed : out Rectangle);

   function Has_Intersected (A, B : in Rectangle) return Boolean;

   function Intersects (A, B : in Rectangle; Intersection : out Rectangle) return Boolean;

   function Clip_To (Clip_Area : in Rectangle; Line : in out Line_Segment) return Boolean;

   function Union (A, B : in Rectangle) return Rectangle;
end SDL.Video.Rectangles;
