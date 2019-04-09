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
--  SDL.Video.Renderers
--
--  Renderer.
--------------------------------------------------------------------------------------------------------------------
with Ada.Finalization;
private with SDL.C_Pointers;
with SDL.Video.Palettes;
with SDL.Video.Rectangles;
with SDL.Video.Textures;
with SDL.Video.Windows;

package SDL.Video.Renderers is
   pragma Preelaborate;

   --  TODO: Finish this.

   Renderer_Error : exception;

   type Renderer_Flags is mod 2 ** 32 with
     Convention => C;

   Default_Renderer_Flags : constant Renderer_Flags := 16#0000_0000#;
   Software               : constant Renderer_Flags := 16#0000_0001#;
   Accelerated            : constant Renderer_Flags := 16#0000_0002#;
   Present_V_Sync         : constant Renderer_Flags := 16#0000_0004#;
   Target_Texture         : constant Renderer_Flags := 16#0000_0008#;

   type Renderer_Flip is (None, Horizontal, Vertical, Both);

   --  SDL_RendererInfo

   function Total_Drivers return Positive with
     Inline => True;

   --  SDL_GetRenderDriverInfo

   type Renderer is new Ada.Finalization.Limited_Controlled with private;

   Null_Renderer : constant Renderer;

   overriding
   procedure Finalize (Self : in out Renderer);

   function Get_Blend_Mode (Self : in Renderer) return Blend_Modes;
   procedure Set_Blend_Mode (Self : in out Renderer; Mode : in Blend_Modes);

   function Get_Draw_Colour (Self : in Renderer) return SDL.Video.Palettes.Colour;
   procedure Set_Draw_Colour (Self : in out Renderer; Colour : in SDL.Video.Palettes.Colour);

   --  SDL_GetRendererInfo

   procedure Clear (Self : in out Renderer);

   procedure Copy
     (Self      : in out Renderer;
      Copy_From : in SDL.Video.Textures.Texture);

   procedure Copy
     (Self      : in out Renderer;
      Copy_From : in SDL.Video.Textures.Texture;
      From      : in SDL.Video.Rectangles.Rectangle;
      To        : in SDL.Video.Rectangles.Rectangle);

   procedure Copy
     (Self      : in out Renderer;
      Copy_From : in SDL.Video.Textures.Texture;
      To        : in SDL.Video.Rectangles.Rectangle);

   procedure Copy
     (Self      : in out Renderer;
      Copy_From : in SDL.Video.Textures.Texture;
      From      : in SDL.Video.Rectangles.Rectangle;
      To        : in SDL.Video.Rectangles.Rectangle;
      Angle     : in Long_Float;
      Centre    : in SDL.Video.Rectangles.Point;
      Flip      : in Renderer_Flip);

   procedure Draw (Self : in out Renderer; Line : in SDL.Video.Rectangles.Line_Segment);
   procedure Draw (Self : in out Renderer; Lines : in SDL.Video.Rectangles.Line_Arrays);
   procedure Draw (Self : in out Renderer; Point : in SDL.Video.Rectangles.Point);
   procedure Draw (Self : in out Renderer; Points : in SDL.Video.Rectangles.Point_Arrays);
   procedure Draw (Self : in out Renderer; Rectangle : in SDL.Video.Rectangles.Rectangle);
   procedure Draw (Self : in out Renderer; Rectangles : in SDL.Video.Rectangles.Rectangle_Arrays);

   procedure Fill (Self : in out Renderer; Rectangle : in SDL.Video.Rectangles.Rectangle);
   procedure Fill (Self : in out Renderer; Rectangles : in SDL.Video.Rectangles.Rectangle_Arrays);

   procedure Get_Clip (Self : in Renderer; Rectangle : out SDL.Video.Rectangles.Rectangle);
   procedure Set_Clip (Self : in out Renderer; Rectangle : in SDL.Video.Rectangles.Rectangle);

   procedure Get_Logical_Size (Self : in Renderer; Size : out SDL.Sizes);
   procedure Set_Logical_Size (Self : in out Renderer; Size : in SDL.Sizes);

   procedure Get_Scale (Self : in Renderer; X, Y : out Float);
   procedure Set_Scale (Self : in out Renderer; X, Y : in Float);

   procedure Get_Viewport (Self : in Renderer; Rectangle : out SDL.Video.Rectangles.Rectangle);
   procedure Set_Viewport (Self : in out Renderer; Rectangle : in SDL.Video.Rectangles.Rectangle);

   procedure Present (Self : in Renderer);

   --  SDL_RenderReadPixels

   function Supports_Targets (Self : in Renderer) return Boolean;

   procedure Set_Target (Self : in out Renderer; Target : in SDL.Video.Textures.Texture);

   function Get_Renderer (Window : in SDL.Video.Windows.Window) return Renderer;
private
   type Renderer is new Ada.Finalization.Limited_Controlled with
      record
         Internal : SDL.C_Pointers.Renderer_Pointer := null;
         Owns     : Boolean                         := True;  --  Does this Window type own the Internal data?
      end record;

   function Get_Internal_Renderer (Self : in Renderer) return SDL.C_Pointers.Renderer_Pointer with
     Export        => True,
     Convention    => Ada;

   Null_Renderer : constant Renderer := (Ada.Finalization.Limited_Controlled with
                                         Internal => null,
                                         Owns     => True);
end SDL.Video.Renderers;
