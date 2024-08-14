--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
--  SDL.Video.Surfaces.Makers
--
--  Functions to create surface objects.
--------------------------------------------------------------------------------------------------------------------
with System.Storage_Elements;
with Ada.Strings.UTF_Encoding;

package SDL.Video.Surfaces.Makers is
   pragma Preelaborate;

   package UTF_Strings renames Ada.Strings.UTF_Encoding;

   procedure Create (Self       : in out Surface;
                     Size       : in SDL.Sizes;
                     BPP        : in Pixel_Depths;
                     Red_Mask   : in Colour_Masks;
                     Blue_Mask  : in Colour_Masks;
                     Green_Mask : in Colour_Masks;
                     Alpha_Mask : in Colour_Masks);

   generic
       type Element is private;
       type Element_Pointer is access all Element;
   procedure Create_From (Self       : in out Surface;
                          Pixels     : in Element_Pointer;
                          Size       : in SDL.Sizes;
                          BPP        : in Pixel_Depths := Element'Size;
                          Pitch      : in System.Storage_Elements.Storage_Offset;
                          Red_Mask   : in Colour_Masks;
                          Green_Mask : in Colour_Masks;
                          Blue_Mask  : in Colour_Masks;
                          Alpha_Mask : in Colour_Masks);

   generic
      type Element is private;
      type Index is (<>);
      type Element_Array is array (Index range <>, Index range <>) of Element;
   procedure Create_From_Array (Self       : in out Surface;
                                Pixels     : access Element_Array;
                                Red_Mask   : in Colour_Masks;
                                Green_Mask : in Colour_Masks;
                                Blue_Mask  : in Colour_Masks;
                                Alpha_Mask : in Colour_Masks);
   --  Note: I'm unsure what happen when packed (1- or -4bit) arrays are used here.
   --        So, at least check that they have whole number of bytes per row
   --        (E. g. even width in 4-bit)
   --  Note: There may be issue with 24-bit pixels (does SDL imply 4-byte alignment in this case?)

   --  TODO: This is likely a temporary place for this. It's likely I will add a Streams package.
   --     procedure Create (Self : in out Surface; File_Name : in String);

   procedure Create (Self      : in out Surface;
                     File_Name : in UTF_Strings.UTF_String);
private
   function Get_Internal_Surface (Self : in Surface) return Internal_Surface_Pointer with
     Export     => True,
     Convention => Ada;

   --  Create a surface from an internal pointer, this pointer will be owned by something else, so we don't delete it.
   function Make_Surface_From_Pointer (S : in Internal_Surface_Pointer; Owns : in Boolean := False) return Surface with
     Export     => True,
     Convention => Ada;

   --  TODO: SDL_ConvertSurface
   --  TODO: SDL_ConvertSurfaceFormat
   --  TODO: SDL_CreateRGBSurfaceFrom
end SDL.Video.Surfaces.Makers;
