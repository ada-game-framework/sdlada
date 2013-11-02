--                              -*- Mode: Ada -*-
--  Filename        : sdl-video-rectangles.ads
--  Description     : Functions relating to rectangles.
--  Author          : Luke A. Guest
--  Created On      : Fri Oct 11 11:41:28 2013
with Interfaces.C;

package SDL.Video.Rectangles is
   package C renames Interfaces.C;

   Rectangle_Error : exception;

   type Size is
      record
         Width  : C.int;
         Height : C.int ;
      end record with
        Convention => C;

   type Size_Arrays is array (C.size_t range <>) of aliased Size with
     Convention => C;

   type Point is
      record
         X : C.int;
         Y : C.int ;
      end record with
        Convention => C;

   type Point_Arrays is array (C.size_t range <>) of aliased Point with
     Convention => C;

   type Line_Segment is
      record
         Start  : Point;
         Finish : Point;
      end record with
        Convention => C;

   type Line_Arrays is array (C.size_t range <>) of aliased Line_Segment With
     Convention => C;

   type Rectangle is
      record
         X      : C.int;
         Y      : C.int;
         Width  : C.int;
         height : C.int;
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
