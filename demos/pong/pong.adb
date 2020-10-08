--  Pong-Demo for SDLAda, root package for graphics objects.
--  Copyright (C) 2012 - 2020, Vinzent "Jellix" Saranen

package body Pong is

   ---------------------------------------------------------------------
   --  X_Position
   ---------------------------------------------------------------------
   function X_Position (This : in Display_Object'Class) return Interfaces.C.int is
   begin
      return This.New_Pos.X;
   end X_Position;

   ---------------------------------------------------------------------
   --  Y_Position
   ---------------------------------------------------------------------
   function Y_Position (This : in Display_Object'Class) return Interfaces.C.int is
   begin
      return This.New_Pos.Y;
   end Y_Position;

end Pong;
