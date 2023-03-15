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
with Interfaces.C;
with SDL.Error;

package body SDL.Video.Renderers is
   package C renames Interfaces.C;

   use type SDL.C_Pointers.Renderer_Pointer;

   type Internal_Flip is mod 2 ** 32 with
     Convention => C;

   --  type Internal_Flip_Array is array (Renderer_Flip) of Internal_Flip;

   Internal_Flip_None       : constant Internal_Flip := 16#0000_0000#;
   Internal_Flip_Horizontal : constant Internal_Flip := 16#0000_0001#;
   Internal_Flip_Vertical   : constant Internal_Flip := 16#0000_0002#;

   Internal_Flips : constant array (Renderer_Flip) of Internal_Flip :=
     (Internal_Flip_None,
      Internal_Flip_Horizontal,
      Internal_Flip_Vertical,
      Internal_Flip_Horizontal or Internal_Flip_Vertical);

   function Get_Internal_Window (Self : in SDL.Video.Windows.Window) return SDL.C_Pointers.Windows_Pointer with
     Convention => Ada,
     Import     => True;

   function Get_Internal_Texture (Self : in SDL.Video.Textures.Texture) return SDL.C_Pointers.Texture_Pointer with
     Import     => True,
     Convention => Ada;

   function Total_Drivers return Positive is
      function SDL_Get_Num_Render_Drivers return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetNumRenderDrivers";

      Result : constant C.int := SDL_Get_Num_Render_Drivers;
   begin
      if Result < C.int (Positive'First) then
         raise Renderer_Error with SDL.Error.Get;
      end if;

      return Positive (Result);
   end Total_Drivers;

   overriding
   procedure Finalize (Self : in out Renderer) is
      procedure SDL_Destroy_Renderer (R : in SDL.C_Pointers.Renderer_Pointer) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_DestroyRenderer";
   begin
      if Self.Internal /= null and then Self.Owns then
         SDL_Destroy_Renderer (Self.Internal);

         Self.Internal := null;
      end if;
   end Finalize;

   function Get_Blend_Mode (Self : in Renderer) return Blend_Modes is
      function SDL_Get_Render_Draw_Blend_Mode (R : in SDL.C_Pointers.Renderer_Pointer;
                                               M : out Blend_Modes) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetRenderDrawBlendMode";

      Mode   : Blend_Modes;
      Result : constant C.int := SDL_Get_Render_Draw_Blend_Mode (Self.Internal, Mode);
   begin
      if Result /= Success then
         raise Renderer_Error with SDL.Error.Get;
      end if;

      return Mode;
   end Get_Blend_Mode;

   procedure Set_Blend_Mode (Self : in out Renderer; Mode : in Blend_Modes) is
      function SDL_Set_Render_Draw_Blend_Mode (R : in SDL.C_Pointers.Renderer_Pointer;
                                               M : in Blend_Modes) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_SetRenderDrawBlendMode";

      Result : constant C.int := SDL_Set_Render_Draw_Blend_Mode (Self.Internal, Mode);
   begin
      if Result /= Success then
         raise Renderer_Error with SDL.Error.Get;
      end if;
   end Set_Blend_Mode;

   function Get_Draw_Colour (Self : in Renderer) return SDL.Video.Palettes.Colour is
      function SDL_Get_Render_Draw_Color
        (R                       : in SDL.C_Pointers.Renderer_Pointer;
         Red, Green, Blue, Alpha : out SDL.Video.Palettes.Colour_Component) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetRenderDrawColor";

      Colour : SDL.Video.Palettes.Colour;
      Result : constant C.int := SDL_Get_Render_Draw_Color (Self.Internal,
                                                            Colour.Red,
                                                            Colour.Green,
                                                            Colour.Blue,
                                                            Colour.Alpha);
   begin
      if Result /= Success then
         raise Renderer_Error with SDL.Error.Get;
      end if;

      return Colour;
   end Get_Draw_Colour;

   procedure Set_Draw_Colour (Self : in out Renderer; Colour : in SDL.Video.Palettes.Colour) is
      function SDL_Set_Render_Draw_Color
        (R                       : in SDL.C_Pointers.Renderer_Pointer;
         Red, Green, Blue, Alpha : in SDL.Video.Palettes.Colour_Component) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_SetRenderDrawColor";

      Result : constant C.int := SDL_Set_Render_Draw_Color (Self.Internal,
                                                            Colour.Red,
                                                            Colour.Green,
                                                            Colour.Blue,
                                                            Colour.Alpha);
   begin
      if Result /= Success then
         raise Renderer_Error with SDL.Error.Get;
      end if;
   end Set_Draw_Colour;

   procedure Clear (Self : in out Renderer) is
      function SDL_Render_Clear (R : in SDL.C_Pointers.Renderer_Pointer) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderClear";

      Result : constant C.int := SDL_Render_Clear (Self.Internal);
   begin
      if Result /= Success then
         raise Renderer_Error with SDL.Error.Get;
      end if;
   end Clear;

   procedure Copy
     (Self      : in out Renderer;
      Copy_From : in SDL.Video.Textures.Texture) is

      function SDL_Render_Copy
        (R         : in SDL.C_Pointers.Renderer_Pointer;
         T         : in SDL.C_Pointers.Texture_Pointer;
         Src, Dest : in SDL.Video.Rectangles.Rectangle_Access) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderCopy";

      Result : constant C.int := SDL_Render_Copy (Self.Internal,
                                                  Get_Internal_Texture (Copy_From),
                                                  null,
                                                  null);
   begin
      if Result /= Success then
         raise Renderer_Error with SDL.Error.Get;
      end if;
   end Copy;

   --  TODO: Check to make sure this works, if it does, apply the same logic to CopyEx, see below.
   procedure Copy
     (Self      : in out Renderer;
      Copy_From : in SDL.Video.Textures.Texture;
      From      : in SDL.Video.Rectangles.Rectangle;
      To        : in SDL.Video.Rectangles.Rectangle) is

      function SDL_Render_Copy
        (R         : in SDL.C_Pointers.Renderer_Pointer;
         T         : in SDL.C_Pointers.Texture_Pointer;
         Src, Dest : in SDL.Video.Rectangles.Rectangle) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderCopy";

      Result : constant C.int := SDL_Render_Copy (Self.Internal,
                                                  Get_Internal_Texture (Copy_From),
                                                  From,
                                                  To);
   begin
      if Result /= Success then
         raise Renderer_Error with SDL.Error.Get;
      end if;
   end Copy;

   procedure Copy
     (Self      : in out Renderer;
      Copy_From : in SDL.Video.Textures.Texture;
      To        : in SDL.Video.Rectangles.Rectangle) is

      function SDL_Render_Copy
        (R    : in SDL.C_Pointers.Renderer_Pointer;
         T    : in SDL.C_Pointers.Texture_Pointer;
         Src  : in SDL.Video.Rectangles.Rectangle_Access;
         Dest : in SDL.Video.Rectangles.Rectangle) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderCopy";

      Result : constant C.int := SDL_Render_Copy (Self.Internal,
                                                  Get_Internal_Texture (Copy_From),
                                                  null,
                                                  To);
   begin
      if Result /= Success then
         raise Renderer_Error with SDL.Error.Get;
      end if;
   end Copy;

   --  TODO: See above, rearrange the params so that the rectangles are the last elements and make
   --  them default to null_rectangle.
   procedure Copy
     (Self      : in out Renderer;
      Copy_From : in SDL.Video.Textures.Texture;
      From      : in SDL.Video.Rectangles.Rectangle;
      To        : in SDL.Video.Rectangles.Rectangle;
      Angle     : in Long_Float;
      Centre    : in SDL.Video.Rectangles.Point;
      Flip      : in Renderer_Flip) is

      function SDL_Render_Copy_Ex
        (R         : in SDL.C_Pointers.Renderer_Pointer;
         T         : in SDL.C_Pointers.Texture_Pointer;
         Src, Dest : in SDL.Video.Rectangles.Rectangle;
         A         : in C.double;
         Centre    : in SDL.Video.Rectangles.Point;
         F         : in Internal_Flip) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderCopyEx";

      Result : constant C.int := SDL_Render_Copy_Ex (Self.Internal,
                                                     Get_Internal_Texture (Copy_From),
                                                     From,
                                                     To,
                                                     C.double (Angle),
                                                     Centre,
                                                     Internal_Flips (Flip));
   begin
      if Result /= Success then
         raise Renderer_Error with SDL.Error.Get;
      end if;
   end Copy;

   procedure Draw (Self : in out Renderer; Line : in SDL.Video.Rectangles.Line_Segment) is
      function SDL_Render_Draw_Line (R              : in SDL.C_Pointers.Renderer_Pointer;
                                     X1, Y1, X2, Y2 : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderDrawLine";

      Result : constant C.int := SDL_Render_Draw_Line (Self.Internal,
                                                       Line.Start.X,
                                                       Line.Start.Y,
                                                       Line.Finish.X,
                                                       Line.Finish.Y);
   begin
      if Result /= Success then
         raise Renderer_Error with SDL.Error.Get;
      end if;
   end Draw;

   --  TODO: Check this works!
   procedure Draw (Self : in out Renderer; Lines : in SDL.Video.Rectangles.Line_Arrays) is
      --  As the records and arrays are defined as C types, an array of lines is also an array of points.
      function SDL_Render_Draw_Lines (R     : in SDL.C_Pointers.Renderer_Pointer;
                                      P     : in SDL.Video.Rectangles.Line_Arrays;
                                      Count : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderDrawLines";

      Result : constant C.int := SDL_Render_Draw_Lines (Self.Internal, Lines, C.int (Lines'Length * 2));
   begin
      if Result /= Success then
         raise Renderer_Error with SDL.Error.Get;
      end if;
   end Draw;

   procedure Draw (Self : in out Renderer; Point : in SDL.Video.Rectangles.Point) is
      function SDL_Render_Draw_Point (R : in SDL.C_Pointers.Renderer_Pointer; X, Y : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderDrawPoint";

      Result : constant C.int := SDL_Render_Draw_Point (Self.Internal, Point.X, Point.Y);
   begin
      if Result /= Success then
         raise Renderer_Error with SDL.Error.Get;
      end if;
   end Draw;

   procedure Draw (Self : in out Renderer; Points : in SDL.Video.Rectangles.Point_Arrays) is
      function SDL_Render_Draw_Points (R     : in SDL.C_Pointers.Renderer_Pointer;
                                       P     : in SDL.Video.Rectangles.Point_Arrays;
                                       Count : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderDrawPoints";

      Result : constant C.int := SDL_Render_Draw_Points (Self.Internal, Points, C.int (Points'Length));
   begin
      if Result /= Success then
         raise Renderer_Error with SDL.Error.Get;
      end if;
   end Draw;

   procedure Draw (Self : in out Renderer; Rectangle : in SDL.Video.Rectangles.Rectangle) is
      function SDL_Render_Draw_Rect (R    : in SDL.C_Pointers.Renderer_Pointer;
                                     Rect : in SDL.Video.Rectangles.Rectangle) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderDrawRect";

      Result : constant C.int := SDL_Render_Draw_Rect (Self.Internal, Rectangle);
   begin
      if Result /= Success then
         raise Renderer_Error with SDL.Error.Get;
      end if;
   end Draw;

   procedure Draw (Self : in out Renderer; Rectangles : in SDL.Video.Rectangles.Rectangle_Arrays) is
      function SDL_Render_Draw_Rects (R     : in SDL.C_Pointers.Renderer_Pointer;
                                      Rect  : in SDL.Video.Rectangles.Rectangle_Arrays;
                                      Count : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderDrawRects";

      Result : constant C.int := SDL_Render_Draw_Rects (Self.Internal, Rectangles, C.int (Rectangles'Length));
   begin
      if Result /= Success then
         raise Renderer_Error with SDL.Error.Get;
      end if;
   end Draw;

   procedure Fill (Self : in out Renderer; Rectangle : in SDL.Video.Rectangles.Rectangle) is
      function SDL_Render_Fill_Rect (R    : in SDL.C_Pointers.Renderer_Pointer;
                                     Rect : in SDL.Video.Rectangles.Rectangle) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderFillRect";

      Result : constant C.int := SDL_Render_Fill_Rect (Self.Internal, Rectangle);
   begin
      if Result /= Success then
         raise Renderer_Error with SDL.Error.Get;
      end if;
   end Fill;

   procedure Fill (Self : in out Renderer; Rectangles : in SDL.Video.Rectangles.Rectangle_Arrays) is
      function SDL_Render_Fill_Rects (R     : in SDL.C_Pointers.Renderer_Pointer;
                                      Rect  : in SDL.Video.Rectangles.Rectangle_Arrays;
                                      Count : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderFillRects";

      Result : constant C.int := SDL_Render_Fill_Rects (Self.Internal, Rectangles, C.int (Rectangles'Length));
   begin
      if Result /= Success then
         raise Renderer_Error with SDL.Error.Get;
      end if;
   end Fill;

   procedure Get_Clip (Self : in Renderer; Rectangle : out SDL.Video.Rectangles.Rectangle) is
      procedure SDL_Render_Get_Clip_Rect (R    : in SDL.C_Pointers.Renderer_Pointer;
                                          Rect : out SDL.Video.Rectangles.Rectangle) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderGetClipRect";
   begin
      SDL_Render_Get_Clip_Rect (Self.Internal, Rectangle);
   end Get_Clip;

   procedure Set_Clip (Self : in out Renderer; Rectangle : in SDL.Video.Rectangles.Rectangle) is
      function SDL_Render_Set_Clip_Rect (R    : in SDL.C_Pointers.Renderer_Pointer;
                                         Rect : in SDL.Video.Rectangles.Rectangle) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderSetClipRect";

      Result : constant C.int := SDL_Render_Set_Clip_Rect (Self.Internal, Rectangle);
   begin
      if Result /= Success then
         raise Renderer_Error with SDL.Error.Get;
      end if;
   end Set_Clip;

   procedure Get_Logical_Size (Self : in Renderer; Size : out SDL.Sizes) is
      procedure SDL_Render_Get_Logical_Size (R : in  SDL.C_Pointers.Renderer_Pointer;
                                             W : out SDL.Dimension;
                                             H : out SDL.Dimension) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderGetLogicalSize";
   begin
      SDL_Render_Get_Logical_Size (Self.Internal, Size.Width, Size.Height);
   end Get_Logical_Size;

   procedure Set_Logical_Size (Self : in out Renderer; Size : in SDL.Sizes) is
      function SDL_Render_Set_Logical_Size (R : in SDL.C_Pointers.Renderer_Pointer;
                                            W : in SDL.Dimension;
                                            H : in SDL.Dimension) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderSetLogicalSize";

      Result : constant C.int := SDL_Render_Set_Logical_Size (Self.Internal, Size.Width, Size.Height);
   begin
      if Result /= Success then
         raise Renderer_Error with SDL.Error.Get;
      end if;
   end Set_Logical_Size;

   procedure Get_Scale (Self : in Renderer; X, Y : out Float) is
      procedure SDL_Render_Get_Scale (R : in SDL.C_Pointers.Renderer_Pointer; X, Y : out C.C_float) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderGetScale";
   begin
      SDL_Render_Get_Scale (Self.Internal, C.C_float (X), C.C_float (Y));
   end Get_Scale;

   procedure Set_Scale (Self : in out Renderer; X, Y : in Float) is
      function SDL_Render_Set_Scale (R : in SDL.C_Pointers.Renderer_Pointer; X, Y : in C.C_float) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderSetScale";

      Result : constant C.int := SDL_Render_Set_Scale (Self.Internal, C.C_float (X), C.C_float (Y));
   begin
      if Result /= Success then
         raise Renderer_Error with SDL.Error.Get;
      end if;
   end Set_Scale;

   procedure Get_Viewport (Self : in Renderer; Rectangle : out SDL.Video.Rectangles.Rectangle) is
      procedure SDL_Render_Get_Viewport (R    : in SDL.C_Pointers.Renderer_Pointer;
                                         Rect : out SDL.Video.Rectangles.Rectangle) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderGetViewport";
   begin
      SDL_Render_Get_Viewport (Self.Internal, Rectangle);
   end Get_Viewport;

   procedure Set_Viewport (Self : in out Renderer; Rectangle : in SDL.Video.Rectangles.Rectangle) is
      function SDL_Render_Set_Viewport (R    : in SDL.C_Pointers.Renderer_Pointer;
                                        Rect : in SDL.Video.Rectangles.Rectangle) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderSetViewport";

      Result : constant C.int := SDL_Render_Set_Viewport (Self.Internal, Rectangle);
   begin
      if Result /= Success then
         raise Renderer_Error with SDL.Error.Get;
      end if;
   end Set_Viewport;

   procedure Present (Self : in Renderer) is
      procedure SDL_Render_Present (R : in SDL.C_Pointers.Renderer_Pointer) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderPresent";
   begin
      SDL_Render_Present (Self.Internal);
   end Present;

   function Supports_Targets (Self : in Renderer) return Boolean is
      function SDL_Render_Target_Supported (R : in SDL.C_Pointers.Renderer_Pointer) return SDL_Bool with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderTargetSupported";
   begin
      return (if SDL_Render_Target_Supported (Self.Internal) = SDL_True then True else False);
   end Supports_Targets;

   procedure Set_Target (Self : in out Renderer; Target : in SDL.Video.Textures.Texture) is
      function SDL_Set_Render_Target (R : in SDL.C_Pointers.Renderer_Pointer;
                                      T : in SDL.C_Pointers.Texture_Pointer) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_SetRenderTarget";

      Result : constant C.int := SDL_Set_Render_Target (Self.Internal, Get_Internal_Texture (Target));
   begin
      if Result /= Success then
         raise Renderer_Error with SDL.Error.Get;
      end if;
   end Set_Target;

   function Get_Renderer (Window : in SDL.Video.Windows.Window) return Renderer is
      function SDL_Get_Renderer (W : in SDL.C_Pointers.Windows_Pointer) return SDL.C_Pointers.Renderer_Pointer with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetRenderer";
   begin
      return Result : constant Renderer :=
        (Ada.Finalization.Limited_Controlled with
           Internal => SDL_Get_Renderer (Get_Internal_Window (Window)), Owns => False) do
         null;
      end return;
   end Get_Renderer;

   function Get_Internal_Renderer (Self : in Renderer) return SDL.C_Pointers.Renderer_Pointer is
   begin
      return Self.Internal;
   end Get_Internal_Renderer;
end SDL.Video.Renderers;
