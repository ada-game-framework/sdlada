--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
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

   type Float_Point is
      record
         X : Float;
         Y : Float;
      end record with
     Convention => C_Pass_By_Copy;

   type Float_Point_Arrays is array (C.size_t range <>) of aliased Float_Point with
     Convention => C;

   type Line_Segment is
      record
         Start  : SDL.Coordinates;
         Finish : SDL.Coordinates;
      end record with
     Convention => C_Pass_By_Copy;

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
     Convention => C_Pass_By_Copy;

   Null_Rectangle : constant Rectangle := (others => 0);

   type Rectangle_Arrays is array (C.size_t range <>) of aliased Rectangle with
     Convention => C;

   type Rectangle_Access is access all Rectangle with
     Convention => C;

   type Float_Rectangle is
      record
         X      : Float;
         Y      : Float;
         Width  : Float;
         Height : Float;
      end record with
     Convention => C_Pass_By_Copy;

   type Float_Rectangle_Arrays is array (C.size_t range <>) of aliased Float_Rectangle with
     Convention => C;

   type Float_Rectangle_Access is access all Float_Rectangle with
     Convention => C;

   function Enclose (Points : in Point_Arrays; Clip : in Rectangle; Enclosed : out Rectangle) return Boolean;
   procedure Enclose (Points : in Point_Arrays; Enclosed : out Rectangle);

   function Has_Intersected (A, B : in Rectangle) return Boolean;

   function Intersects (A, B : in Rectangle; Intersection : out Rectangle) return Boolean;

   function Clip_To (Clip_Area : in Rectangle; Line : in out Line_Segment) return Boolean;
   function Intersects (Clip_Area : in Rectangle; Line : in out Line_Segment) return Boolean renames Clip_To;

   function Union (A, B : in Rectangle) return Rectangle;
end SDL.Video.Rectangles;
