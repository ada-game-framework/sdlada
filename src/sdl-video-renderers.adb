--------------------------------------------------------------------------------------------------------------------
--  Copyright (c) 2014 Luke A. Guest
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
--  with Ada.Unchecked_Conversion;
with Interfaces.C;
with SDL.Error;

package body SDL.Video.Renderers is
   package C renames Interfaces.C;

   use type C.int;

   type Internal_Flip is mod 2 ** 32 with
     Convention => C;

   --type Internal_Flip_Array is array (Renderer_Flip) of Internal_Flip;

   Internal_Flip_None       : constant Internal_Flip := 16#0000_0000#;
   Internal_Flip_Horizontal : constant Internal_Flip := 16#0000_0001#;
   Internal_Flip_Vertical   : constant Internal_Flip := 16#0000_0002#;

   Internal_Flips : constant array (Renderer_Flip) of Internal_Flip :=
     (Internal_Flip_None,
      Internal_Flip_Horizontal,
      Internal_Flip_Vertical,
      Internal_Flip_Horizontal or Internal_Flip_Vertical);

   function Get_Address (Self : in SDL.Video.Surfaces.Surface) return System.Address with
     Import     => True,
     Convention => Ada;

   function Get_Address (Self : in SDL.Video.Windows.Window) return System.Address with
     Import     => True,
     Convention => Ada;

   function Get_Address (Self : in SDL.Video.Textures.Texture) return System.Address with
     Import     => True,
     Convention => Ada;

   function Get_Address (Self : in Renderer) return System.Address is
   begin
      return Self.Internal;
   end Get_Address;

   function Total_Drivers return Positive is
      function SDL_Get_Num_Render_Drivers return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetNumRenderDrivers";

      Result : C.int := SDL_Get_Num_Render_Drivers;
   begin
      if Result < C.int (Positive'First) then
         raise Renderer_Error with SDL.Error.Get;
      end if;

      return Positive (Result);
   end Total_Drivers;

   procedure Create
     (Self   : in out Renderer;
      Window : in out SDL.Video.Windows.Window;
      Driver : in Positive;
      Flags  : in Renderer_Flags) is

      function SDL_Create_Renderer (W : in System.Address; Index : in C.int; Flags : in Renderer_Flags)
                                   return System.Address with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_CreateRenderer";
   begin
      Self.Internal := SDL_Create_Renderer (Get_Address (Window), C.int (Driver), Flags);
   end Create;

   procedure Create_Software
     (Self    : in out Renderer;
      Surface : in SDL.Video.Surfaces.Surface) is

      function SDL_Create_Software_Renderer (S : in System.Address) return System.Address with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_CreateSoftwareRenderer";
   begin
      Self.Internal := SDL_Create_Software_Renderer (Get_Address (Surface));
   end Create_Software;

   procedure Finalize (Self : in out Renderer) is
      procedure SDL_Destroy_Renderer (R : in System.Address) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_DestroyRenderer";
   begin
      SDL_Destroy_Renderer (Self.Internal);

       Self.Internal := System.Null_Address;
   end Finalize;

   function Get_Blend_Mode (Self : in Renderer) return SDL.Video.Textures.Blend_Modes is
      function SDL_Get_Render_Draw_Blend_Mode (R : in System.Address; M : out SDL.Video.Textures.Blend_Modes) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetRenderDrawBlendMode";

      Mode   : SDL.Video.Textures.Blend_Modes;
      Result : C.int := SDL_Get_Render_Draw_Blend_Mode (Self.Internal, Mode);
   begin
      if Result /= Success then
         raise Renderer_Error with SDL.Error.Get;
      end if;

      return Mode;
   end Get_Blend_Mode;

   procedure Set_Blend_Mode (Self : in out Renderer; Mode : in SDL.Video.Textures.Blend_Modes) is
      function SDL_Set_Render_Draw_Blend_Mode (R : in System.Address; M : in SDL.Video.Textures.Blend_Modes) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_SetRenderDrawBlendMode";

      Result : C.int := SDL_Set_Render_Draw_Blend_Mode (Self.Internal, Mode);
   begin
      if Result /= Success then
         raise Renderer_Error with SDL.Error.Get;
      end if;
   end Set_Blend_Mode;

   function Get_Draw_Colour (Self : in Renderer) return SDL.Video.Palettes.Colour is
      function SDL_Get_Render_Draw_Color
        (R                       : in System.Address;
         Red, Green, Blue, Alpha : out SDL.Video.Palettes.Colour_Component) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetRenderDrawColor";

      Colour : SDL.Video.Palettes.Colour;
      Result : C.int := SDL_Get_Render_Draw_Color (Self.Internal, Colour.Red, Colour.Green, Colour.Blue, Colour.Alpha);
   begin
      if Result /= Success then
         raise Renderer_Error with SDL.Error.Get;
      end if;

      return Colour;
   end Get_Draw_Colour;

   procedure Set_Draw_Colour (Self : in out Renderer; Colour : in SDL.Video.Palettes.Colour) is
      function SDL_Set_Render_Draw_Color
        (R                       : in System.Address;
         Red, Green, Blue, Alpha : in SDL.Video.Palettes.Colour_Component) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_SetRenderDrawColor";

      Result : C.int := SDL_Set_Render_Draw_Color (Self.Internal, Colour.Red, Colour.Green, Colour.Blue, Colour.Alpha);
   begin
      if Result /= Success then
         raise Renderer_Error with SDL.Error.Get;
      end if;
   end Set_Draw_Colour;

   procedure Clear (Self : in out Renderer) is
      function SDL_Render_Clear (R : in System.Address) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderClear";

      Result : C.int := SDL_Render_Clear (Self.Internal);
   begin
      if Result /= Success then
         raise Renderer_Error with SDL.Error.Get;
      end if;
   end Clear;

   --  TODO: Check to make sure this works, if it does, apply the same logic to CopyEx, see below.
   procedure Copy
     (Self      : in out Renderer;
      Copy_From : in SDL.Video.Textures.Texture;
      From      : in SDL.Video.Rectangles.Rectangle;
      To        : in SDL.Video.Rectangles.Rectangle) is

      function SDL_Render_Copy
        --  (R, T : in System.Address; Src, Dest : in SDL.Video.Rectangles.Rectangle) return C.int with
        (R, T, Src, Dest : in System.Address) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderCopy";

      Result : C.int := SDL_Render_Copy (Self.Internal, Get_Address (Copy_From), From'Address, To'Address);
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
        (R, T      : in System.Address;
         Src, Dest : in SDL.Video.Rectangles.Rectangle;
         A         : in C.Double;
         Centre    : in SDL.Video.Rectangles.Point;
         F         : in Internal_Flip) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderCopyEx";

      Result : C.int := SDL_Render_Copy_Ex (Self.Internal,
                                            Get_Address (Copy_From),
                                            From,
                                            To,
                                            C.Double (Angle),
                                            Centre,
                                            Internal_Flips (Flip));
   begin
      if Result /= Success then
         raise Renderer_Error with SDL.Error.Get;
      end if;
   end Copy;

   procedure Draw (Self : in out Renderer; Line : in SDL.Video.Rectangles.Line_Segment) is
      function SDL_Render_Draw_Line (R : in System.Address; X1, Y1, X2, Y2 : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderDrawLine";

      Result : C.int := SDL_Render_Draw_Line (Self.Internal, Line.Start.X, Line.Start.Y, Line.Finish.X, Line.Finish.Y);
   begin
      if Result /= Success then
         raise Renderer_Error with SDL.Error.Get;
      end if;
   end Draw;

   --  TODO: Check this works!
   procedure Draw (Self : in out Renderer; Lines : in SDL.Video.Rectangles.Line_Arrays) is
      --  As the records and arrays are defined as C types, an array of lines is also an array of points.
      function SDL_Render_Draw_Lines (R : in System.Address; P : in SDL.Video.Rectangles.Line_Arrays; Count : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderDrawLines";

      Result : C.int := SDL_Render_Draw_Lines (Self.Internal, Lines, C.int (Lines'Length * 2));
   begin
      if Result /= Success then
         raise Renderer_Error with SDL.Error.Get;
      end if;
   end Draw;

   procedure Draw (Self : in out Renderer; Point : in SDL.Video.Rectangles.Point) is
      function SDL_Render_Draw_Point (R : in System.Address; X, Y : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderDrawPoint";

      Result : C.int := SDL_Render_Draw_Point (Self.Internal, Point.X, Point.Y);
   begin
      if Result /= Success then
         raise Renderer_Error with SDL.Error.Get;
      end if;
   end Draw;

   procedure Draw (Self : in out Renderer; Points : in SDL.Video.Rectangles.Point_Arrays) is
      function SDL_Render_Draw_Points (R : in System.Address; P : in SDL.Video.Rectangles.Point_Arrays; Count : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderDrawPoints";

      Result : C.int := SDL_Render_Draw_Points (Self.Internal, Points, C.int (Points'Length));
   begin
      if Result /= Success then
         raise Renderer_Error with SDL.Error.Get;
      end if;
   end Draw;

   procedure Draw (Self : in out Renderer; Rectangle : in SDL.Video.Rectangles.Rectangle) is
      function SDL_Render_Draw_Rect (R : in System.Address; Rect : in SDL.Video.Rectangles.Rectangle) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderDrawRect";

      Result : C.int := SDL_Render_Draw_Rect (Self.Internal, Rectangle);
   begin
      if Result /= Success then
         raise Renderer_Error with SDL.Error.Get;
      end if;
   end Draw;

   procedure Draw (Self : in out Renderer; Rectangles : in SDL.Video.Rectangles.Rectangle_Arrays) is
      function SDL_Render_Draw_Rects (R : in System.Address; Rect : in SDL.Video.Rectangles.Rectangle_Arrays; Count : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderDrawRects";

      Result : C.int := SDL_Render_Draw_Rects (Self.Internal, Rectangles, C.int (Rectangles'Length));
   begin
      if Result /= Success then
         raise Renderer_Error with SDL.Error.Get;
      end if;
   end Draw;

   procedure Fill (Self : in out Renderer; Rectangle : in SDL.Video.Rectangles.Rectangle) is
      function SDL_Render_Fill_Rect (R : in System.Address; Rect : in SDL.Video.Rectangles.Rectangle) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderFillRect";

      Result : C.int := SDL_Render_Fill_Rect (Self.Internal, Rectangle);
   begin
      if Result /= Success then
         raise Renderer_Error with SDL.Error.Get;
      end if;
   end Fill;

   procedure Fill (Self : in out Renderer; Rectangles : in SDL.Video.Rectangles.Rectangle_Arrays) is
      function SDL_Render_Fill_Rects (R : in System.Address; Rect : in SDL.Video.Rectangles.Rectangle_Arrays; Count : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderFillRects";

      Result : C.int := SDL_Render_Fill_Rects (Self.Internal, Rectangles, C.int (Rectangles'Length));
   begin
      if Result /= Success then
         raise Renderer_Error with SDL.Error.Get;
      end if;
   end Fill;

   procedure Get_Clip (Self : in Renderer; Rectangle : out SDL.Video.Rectangles.Rectangle) is
      procedure SDL_Render_Get_Clip_Rect (R : in System.Address; Rect : out SDL.Video.Rectangles.Rectangle) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderGetClipRect";
   begin
      SDL_Render_Get_Clip_Rect (Self.Internal, Rectangle);
   end Get_Clip;

   procedure Set_Clip (Self : in out Renderer; Rectangle : in SDL.Video.Rectangles.Rectangle) is
      function SDL_Render_Set_Clip_Rect (R : in System.Address; Rect : in SDL.Video.Rectangles.Rectangle) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderSetClipRect";

      Result : C.int := SDL_Render_Set_Clip_Rect (Self.Internal, Rectangle);
   begin
      if Result /= Success then
         raise Renderer_Error with SDL.Error.Get;
      end if;
   end Set_Clip;

   procedure Get_Logical_Size (Self : in Renderer; Size : out SDL.Video.Rectangles.Size) is
      procedure SDL_Render_Get_Logical_Size (R : in System.Address; S : out SDL.Video.Rectangles.Size) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderGetLogicalSize";
   begin
      SDL_Render_Get_Logical_Size (Self.Internal, Size);
   end Get_Logical_Size;

   procedure Set_Logical_Size (Self : in out Renderer; Size : in SDL.Video.Rectangles.Size) is
      function SDL_Render_Set_Logical_Size (R : in System.Address; S : in SDL.Video.Rectangles.Size) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderGetLogicalSize";

      Result : C.int := SDL_Render_Set_Logical_Size (Self.Internal, Size);
   begin
      if Result /= Success then
         raise Renderer_Error with SDL.Error.Get;
      end if;
   end Set_Logical_Size;

   procedure Get_Scale (Self : in Renderer; X, Y : out Float) is
      procedure SDL_Render_Get_Scale (R : in System.Address; X, Y : out C.C_Float) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderGetScale";
   begin
      SDL_Render_Get_Scale (Self.Internal, C.C_Float (X), C.C_Float (Y));
   end Get_Scale;

   procedure Set_Scale (Self : in out Renderer; X, Y : in Float) is
      function SDL_Render_Set_Logical_Size (R : in System.Address; X, Y : in C.C_Float) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderGetLogicalSize";

      Result : C.int := SDL_Render_Set_Logical_Size (Self.Internal, C.C_Float (X), C.C_Float (Y));
   begin
      if Result /= Success then
         raise Renderer_Error with SDL.Error.Get;
      end if;
   end Set_Scale;

   procedure Get_Viewport (Self : in Renderer; Rectangle : out SDL.Video.Rectangles.Rectangle) is
      procedure SDL_Render_Get_Viewport (R : in System.Address; Rect : out SDL.Video.Rectangles.Rectangle) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderGetViewport";
   begin
      SDL_Render_Get_Viewport (Self.Internal, Rectangle);
   end Get_Viewport;

   procedure Set_Viewport (Self : in out Renderer; Rectangle : in SDL.Video.Rectangles.Rectangle) is
      function SDL_Render_Set_Viewport (R : in System.Address; Rect : in SDL.Video.Rectangles.Rectangle) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderSetViewport";

      Result : C.int := SDL_Render_Set_Viewport (Self.Internal, Rectangle);
   begin
      if Result /= Success then
         raise Renderer_Error with SDL.Error.Get;
      end if;
   end Set_Viewport;

   procedure Present (Self : in Renderer) is
      procedure SDL_Render_Present (R : in System.Address) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderPresent";
   begin
      SDL_Render_Present (Self.Internal);
   end Present;

   function Supports_Targets (Self : in Renderer) return Boolean is
      function SDL_Render_Target_Supported (R : in System.Address) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderTargetSupported";
   begin
      return (if SDL_Render_Target_Supported (Self.Internal) = SDL_True then True else False);
   end Supports_Targets;

   procedure Set_Target (Self : in out Renderer; Target : in SDL.Video.Textures.Texture) is
      function SDL_Set_Render_Target (R, T : in System.Address) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_SetRenderTarget";

      Result : C.int := SDL_Set_Render_Target (Self.Internal, Get_Address (Target));
   begin
      if Result /= Success then
         raise Renderer_Error with SDL.Error.Get;
      end if;
   end Set_Target;

   function Get_Renderer (Window : in SDL.Video.Windows.Window) return Renderer is
      function SDL_Get_Renderer (W : in System.Address) return System.Address with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetRenderer";
   begin
      return Result : constant Renderer := (Ada.Finalization.Limited_Controlled with
                                              Internal => SDL_Get_Renderer (Get_Address (Window))) do
        null;
      end return;
   end Get_Renderer;
end SDL.Video.Renderers;
