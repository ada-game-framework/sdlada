--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
--  SDL.Video.Textures
--
--  Texture abstraction.
--------------------------------------------------------------------------------------------------------------------
with Ada.Finalization;
private with SDL.C_Pointers;
with SDL.Video.Palettes;
with SDL.Video.Pixel_Formats;
with SDL.Video.Pixels;
with SDL.Video.Rectangles;

package SDL.Video.Textures is
   pragma Preelaborate;

   Texture_Error : exception;

   --  Was SDL_TextureAccess.
   type Kinds is (Static, Streaming, Target) with
     Convention => C;

   type Scale_Modes is (Nearest, Linear, Best) with
     Convention => C;

   type Texture is new Ada.Finalization.Limited_Controlled with private;

   function Null_Texture return Texture;

   procedure Destroy (Self : in out Texture);

   --  Get the alpha value to be multiplied (modulated) into render copy operations.
   function Get_Alpha (Self : in Texture) return SDL.Video.Palettes.Colour_Component;
   procedure Set_Alpha (Self : in out Texture; Alpha : in SDL.Video.Palettes.Colour_Component);

   function Get_Blend_Mode (Self : in Texture) return Blend_Modes;
   procedure Set_Blend_Mode (Self : in out Texture; Mode : in Blend_Modes);

   function Get_Modulate_Colour (Self : in Texture) return SDL.Video.Palettes.RGB_Colour;
   procedure Set_Modulate_Colour (Self : in out Texture; Colour : in SDL.Video.Palettes.RGB_Colour);

   function Get_Scale_Mode (Self : in Texture) return Scale_Modes;
   procedure Set_Scale_Mode (Self : in out Texture; Scale_Mode : in Scale_Modes);

   --  TODO: SDL_GetTextureUserData

   --  TODO: Fix this.
   --  Lock returns access to pixel data as write-only.
   --  function Lock (Self : in out Texture; Pixel_Data : out SDL.Video.Pixels.Pixel) return Boolean with
   --  function Lock (Self : in out Texture; Area : in SDL.Video.Rectangles.Rectangle;
   --    Pixel_Data : out SDL.Video.Pixels.Pixel) return Boolean with
   --    Pre  => Self.Locked = False,
   --    Post => Result = True and then Self.Locked = True;
   --
   --  Lock should return an object representing the bitmap data. We should be able to access it like an array,
   --  e.g. (x, y) and also using an iterator, which traverses, top -> bottom, left -> right, 1 pixel at a time. We
   --  need to be able to do subimage copies using Ada's slices, e.g. bm1 (x .. x2, y .. y2) := bm2 (x .. x2, y .. y2)
   --
   --  For YV12 format:
   --

   --  package ARGB_8888_Array is new SDL.Video.Pixels.Texture_Data (Width => , Height => , Element => );
   --     procedure Lock_Texture (Self   : in out Texture;
   --                             Pixels : out SDL.Video.Pixels.Pixel_ARGB_8888_Array_Access);

   --  Lock
   --
   --  Lock the entire texture data.
   --
   --  There will be multiple pixel formats, there should only be one Lock sub-program to handle them all.
   generic
      type Pixel_Pointer_Type is private;
   procedure Lock (Self   : in out Texture;
                   Pixels : out Pixel_Pointer_Type);

   --  Lock
   --
   --  Lock a particular area of the texture data.
   generic
      type Pixel_Pointer_Type is private;
   procedure Lock_Area (Self   : in out Texture;
                        Area   : in SDL.Video.Rectangles.Rectangle;
                        Pixels : out Pixel_Pointer_Type;
                        Pitch  : out SDL.Video.Pixels.Pitches);

   procedure Unlock (Self : in out Texture);

   procedure Query (Self              : in Texture;
                    Pixel_Format_Name : out SDL.Video.Pixel_Formats.Pixel_Format_Names;
                    Kind              : out Kinds;
                    Size              : out SDL.Sizes);

   function Get_Pixel_Format (Self : in Texture) return SDL.Video.Pixel_Formats.Pixel_Format_Names;
   function Get_Kind (Self : in Texture) return Kinds;
   function Get_Size (Self : in Texture) return SDL.Sizes;

   --  SDL_UpdateTexture
   --  SDL_UpdateYUVTexture
private
   type Texture is new Ada.Finalization.Limited_Controlled with
      record
         Internal     : SDL.C_Pointers.Texture_Pointer             := null;
         Owns         : Boolean                                    := True;
         Locked       : Boolean                                    := False;
         Size         : SDL.Sizes                                  := SDL.Zero_Size;
         Pixel_Format : SDL.Video.Pixel_Formats.Pixel_Format_Names := SDL.Video.Pixel_Formats.Pixel_Format_Unknown;
      end record;

   overriding
   procedure Finalize (Self : in out Texture);

   function Get_Internal_Texture (Self : in Texture) return SDL.C_Pointers.Texture_Pointer with
     Export     => True,
     Convention => Ada;

   function Null_Texture return Texture is (Texture'(Ada.Finalization.Limited_Controlled with
                                       Internal     => null,
                                       Owns         => True,
                                       Size         => SDL.Zero_Size,
                                       Pixel_Format => Pixel_Formats.Pixel_Format_Unknown,
                                       Locked       => False));
end SDL.Video.Textures;
