--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
with SDL.Error;

package body SDL.RWops.Streams is
   use type Interfaces.C.unsigned_long;

   function Open (Op : in RWops) return RWops_Stream is
   begin
      return (Ada.Streams.Root_Stream_Type with Context => Op);
   end Open;

   procedure Open (Op : in RWops; Stream : out RWops_Stream) is
   begin
      Stream.Context := Op;
   end Open;

   procedure Close (Stream : in RWops_Stream) is
   begin
      Close (Stream.Context);
   end Close;

   overriding
   procedure Read (Stream : in out RWops_Stream;
                   Item   : out Ada.Streams.Stream_Element_Array;
                   Last   : out Ada.Streams.Stream_Element_Offset)
   is
      Objects_Read : Interfaces.C.unsigned_long := 0;
   begin
      --  Re-implemented c-macro:
      --  #define SDL_RWread(ctx, ptr, size, n)   (ctx)->read(ctx, ptr, size, n)

      --  Read   : access function
      --    (context : RWops_Pointer;
      --     ptr     : System.Address;
      --     size    : Interfaces.C.unsigned_long;
      --     maxnum  : Interfaces.C.unsigned_long) return Interfaces.C.unsigned_long;

      Objects_Read := Stream.Context.Read
        (Context  => RWops_Pointer (Stream.Context),
         Ptr      => Item'Address,
         Size     => Item'Length,
         Max_Num  => 1);

      if Objects_Read = 0 then
         raise RWops_Error with SDL.Error.Get;
      end if;

      Last := Item'Length;
   end Read;

   overriding
   procedure Write (Stream : in out RWops_Stream; Item : Ada.Streams.Stream_Element_Array)
   is
      Objects_Written : Interfaces.C.unsigned_long := 0;
   begin
      --  Re-implemented c-macro:
      --  #define SDL_RWwrite(ctx, ptr, size, n)  (ctx)->write(ctx, ptr, size, n)

      --  Write  : access function
      --    (Context : RWops_Pointer;
      --     Ptr     : System.Address;
      --     Size    : Interfaces.C.unsigned_long;
      --     Num     : Interfaces.C.unsigned_long) return Interfaces.C.unsigned_long;

      Objects_Written := Stream.Context.Write
        (Context => RWops_Pointer (Stream.Context),
         Ptr     => Item'Address,
         Size    => Item'Length,
         Num     => 1);

      if Objects_Written = 0 then
         raise RWops_Error with SDL.Error.Get;
      end if;
   end Write;
end SDL.RWops.Streams;
