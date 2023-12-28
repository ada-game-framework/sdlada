--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
--  SDL.Images.IO
--
--  Various loading routines.
--------------------------------------------------------------------------------------------------------------------
with SDL.Video.Renderers;
with SDL.Video.Surfaces;
with SDL.Video.Textures;
with SDL.RWops;

package SDL.Images.IO is
   pragma Preelaborate;

   --  TODO: I don't like the idea of leaving the freeing of the Source to the programmer, this is error prone and will
   --        cause leaks! I would prefer the RWops object be controlled.
   procedure Create (Surface : in out Video.Surfaces.Surface; File_Name : in String);

   procedure Create (Surface     : in out Video.Surfaces.Surface;
                     Source      : in RWops.RWops;
                     Format      : in Formats;
                     Free_Source : in Boolean := True);

   procedure Create (Texture   : in out Video.Textures.Texture;
                     Renderer  : in Video.Renderers.Renderer;
                     File_Name : in String);

   procedure Create (Texture     : in out Video.Textures.Texture;
                     Renderer    : in Video.Renderers.Renderer;
                     Source      : in RWops.RWops;
                     Free_Source : in Boolean := True);

   procedure Write_PNG (Surface : in out Video.Surfaces.Surface; File_Name : in String);
end SDL.Images.IO;
