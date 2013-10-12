with Ada.Unchecked_Conversion;
with SDL.Error;
with System;

package body SDL.Video.Rectangles is
   use type C.int;

   function To_Address is new Ada.Unchecked_Conversion (Source => Rectangle_Access, Target => System.Address);

   function Enclose (Points : in Point_Arrays; Clip : in Rectangle; Enclosed : out Rectangle) return Boolean is
      function SDL_Enclose_Points (P : in Point_Arrays; L : in C.int; Clip : in Rectangle; R : out Rectangle) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_EnclosePoints";

      Result : C.int := SDL_Enclose_Points (Points, C.int (Points'Length), Clip, Enclosed);
   begin
      return (Result = SDL_True);
   end Enclose;

   procedure Enclose (Points : in Point_Arrays; Enclosed : out Rectangle) is
      function SDL_Enclose_Points (P : in Point_Arrays; L : in C.int; Clip : in System.Address; R : out Rectangle) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_EnclosePoints";

      Result : C.int := SDL_Enclose_Points (Points, C.int (Points'Length), System.Null_Address, Enclosed);
   begin
      if Result /= SDL_True then
         raise Rectangle_Error with SDL.Error.Get;
      end if;
   end Enclose;

   function Has_Intersected (A, B : in Rectangle) return Boolean is
      function SDL_Has_Intersection (A, B : in Rectangle) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_HasIntersection";

      Result : C.int := SDL_Has_Intersection (A, B);
   begin
      return (Result = SDL_True);
   end Has_Intersected;

   function Intersects (A, B : in Rectangle; Intersection : out Rectangle) return Boolean is
      function SDL_Intersect_Rect (A, B : in Rectangle; R : out Rectangle) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_IntersectRect";

      Result : C.int := SDL_Intersect_Rect (A, B, R => Intersection);
   begin
      return (Result = SDL_True);
   end Intersects;

   function Clip_To (Clip_Area : in Rectangle; Line : in out Line_Segment) return Boolean is
      function SDL_Intersect_Rect_And_Line (R : in Rectangle; X1, Y1, X2, Y2 : in out C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_IntersectRectAndLine";

      Result : C.int := SDL_Intersect_Rect_And_Line (Clip_Area, Line.Start.X, Line.Start.Y, Line.Finish.X, Line.Finish.Y);
   begin
      return (Result = SDL_True);
   end Clip_To;

   function Union (A, B : in Rectangle) return Rectangle is
      procedure SDL_Union_Rect (A, B : in Rectangle; R : out Rectangle) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_IntersectRectAndLine";

      Result : Rectangle;
   begin
      SDL_Union_Rect (A, B, Result);

      return Result;
   end Union;
end SDL.Video.Rectangles;
