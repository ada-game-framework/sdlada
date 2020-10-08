--  Pong-Demo for SDLAda, root package for graphics objects.
--  Copyright (C) 2012 - 2020, Vinzent "Jellix" Saranen

with Interfaces.C;
with SDL.Video.Rectangles,
     SDL.Video.Surfaces;

package Pong is

   type Position is
      record
         X : Interfaces.C.int;
         Y : Interfaces.C.int;
      end record;

   type Dimensions is
      record
         W : Interfaces.C.int;
         H : Interfaces.C.int;
      end record;

   type Display_Object is abstract tagged private;

   ---------------------------------------------------------------------
   --  Draw
   ---------------------------------------------------------------------
   procedure Draw (This    : in out Display_Object;
                   Surface : in out SDL.Video.Surfaces.Surface) is abstract;

   ---------------------------------------------------------------------
   --  Move
   ---------------------------------------------------------------------
   procedure Move (This    : in out Display_Object;
                   Clipped :    out Boolean;
                   Delta_X : in     Interfaces.C.int;
                   Delta_Y : in     Interfaces.C.int) is abstract;

   ---------------------------------------------------------------------
   --  X_Position
   ---------------------------------------------------------------------
   function X_Position (This : in Display_Object'Class) return Interfaces.C.int;
   pragma Inline (X_Position);

   ---------------------------------------------------------------------
   --  Y_Position
   ---------------------------------------------------------------------
   function Y_Position (This : in Display_Object'Class) return Interfaces.C.int;
   pragma Inline (Y_Position);

private

   type Display_Object is abstract tagged
      record
         Old_Pos : Position;
         New_Pos : Position;
         Size    : Dimensions;
         Bounds  : SDL.Video.Rectangles.Rectangle;
      end record;

end Pong;
