--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
with SDL.Error;
with SDL.C_Pointers;

package body SDL.Images.IO is
   function Make_Surface_From_Pointer (S    : in Video.Surfaces.Internal_Surface_Pointer;
                                       Owns : in Boolean := False) return Video.Surfaces.Surface with
     Import     => True,
     Convention => Ada;

   use type Video.Surfaces.Internal_Surface_Pointer;

   procedure Create (Surface : in out Video.Surfaces.Surface; File_Name : in String) is
      function IMG_Load (Name : in C.char_array) return Video.Surfaces.Internal_Surface_Pointer with
        Import        => True,
        Convention    => C,
        External_Name => "IMG_Load";

      Result : constant Video.Surfaces.Internal_Surface_Pointer := IMG_Load (Name => C.To_C (File_Name));
   begin
      if Result = null then
         raise Image_Error with Error.Get;
      end if;

      Surface := Make_Surface_From_Pointer (S => Result, Owns => True);
   end Create;

   procedure Create (Surface     : in out Video.Surfaces.Surface;
                     Source      : in RWops.RWops;
                     Format      : in Formats;
                     Free_Source : in Boolean := True) is
      function IMG_Load_Typed_RW (Ops   : in RWops.RWops;
                                  Free  : in C.int;
                                  Which : in C.char_array) return Video.Surfaces.Internal_Surface_Pointer with
        Import        => True,
        Convention    => C,
        External_Name => "IMG_LoadTyped_RW";

      Result : constant Video.Surfaces.Internal_Surface_Pointer := IMG_Load_Typed_RW
        (Ops   => Source,
         Free  => (if Free_Source then 1 else 0),
         Which => Format_String (Format));
   begin
      if Result = null then
         raise Image_Error with Error.Get;
      end if;

      Surface := Make_Surface_From_Pointer (S => Result, Owns => True);
   end Create;

--     function Get_Internal_Texture (Self : in SDL.Video.Textures.Texture) return SDL.C_Pointers.Texture_Pointer with
--       Import     => True,
--       Convention => Ada;

   function Get_Internal_Renderer (Self : in SDL.Video.Renderers.Renderer) return SDL.C_Pointers.Renderer_Pointer with
     Import        => True,
     Convention    => Ada;

   procedure Create (Texture   : in out Video.Textures.Texture;
                     Renderer  : in Video.Renderers.Renderer;
                     File_Name : in String) is
      pragma Unreferenced (Texture);  --  TODO: Fix me!

      function IMG_Load (R    : in SDL.C_Pointers.Renderer_Pointer;
                         Name : in C.char_array) return C_Pointers.Texture_Pointer with
        Import        => True,
        Convention    => C,
        External_Name => "IMG_Load";

      use type C_Pointers.Texture_Pointer;

      Result : constant C_Pointers.Texture_Pointer := IMG_Load (R    => Get_Internal_Renderer (Renderer),
                                                                Name => C.To_C (File_Name));
   begin
      if Result = null then
         raise Image_Error with Error.Get;
      end if;

      --        Surface := Make_Texture_From_Pointer (S => Result, Owns => True);
   end Create;

   procedure Create (Texture     : in out Video.Textures.Texture;
                     Renderer    : in Video.Renderers.Renderer;
                     Source      : in RWops.RWops;
                     Free_Source : in Boolean := True) is
   begin
      null;
   end Create;

   procedure Write_PNG (Surface : in out Video.Surfaces.Surface; File_Name : in String) is
      function Get_Internal_Surface (Self : in Video.Surfaces.Surface)
                                     return Video.Surfaces.Internal_Surface_Pointer with
        Import     => True,
        Convention => Ada;

      function IMG_SavePNG (S : in Video.Surfaces.Internal_Surface_Pointer; Name : in C.char_array) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "IMG_SavePNG";

      Result : constant C.int := IMG_SavePNG (Get_Internal_Surface (Surface), C.To_C (File_Name));
   begin
      if Result < Success then
         raise Image_Error with Error.Get;
      end if;
   end Write_PNG;
end SDL.Images.IO;
