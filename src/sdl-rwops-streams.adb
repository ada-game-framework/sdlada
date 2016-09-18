with SDL.Error;

package body SDL.RWops.Streams is
   use type Interfaces.C.unsigned_long;

   procedure Close (Stream : RWops_Stream) is
   begin
      RW_Close (Stream.Context);
   end Close;

   function Open (Op : RWops) return RWops_Stream is
   begin
      return (Root_Stream_Type with Context => Op);
   end Open;

   procedure Open
     (Op     : RWops;
      Stream : out RWops_Stream) is
   begin
      Stream.Context := Op;
   end Open;

   overriding
   procedure Read
     (Stream : in out RWops_Stream;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
      Objects_Read : Interfaces.C.unsigned_long := 0;
   begin

      -- Re-implemented c-macro:
      -- #define SDL_RWread(ctx, ptr, size, n)   (ctx)->read(ctx, ptr, size, n)

      -- Read   : access function
      --   (context : RWops_Pointer;
      --    ptr     : System.Address;
      --    size    : Interfaces.C.unsigned_long;
      --    maxnum  : Interfaces.C.unsigned_long) return Interfaces.C.unsigned_long;

      Objects_Read := Stream.Context.Read
        (context => RWops_Pointer (Stream.Context),
         ptr     => Item'Address,
         size    => Item'Length,
         maxnum  => 1);

      if Objects_Read = 0 then
         raise RWops_Error with SDL.Error.Get;
      end if;

      Last := Item'Length;

   end Read;

   overriding
   procedure Write
     (Stream : in out RWops_Stream;
      Item   : Stream_Element_Array)
   is
      Objects_Written : Interfaces.C.unsigned_long := 0;
   begin
      -- Re-implemented c-macro:
      -- #define SDL_RWwrite(ctx, ptr, size, n)  (ctx)->write(ctx, ptr, size, n)

      -- Write  : access function
      --   (context : RWops_Pointer;
      --    ptr     : System.Address;
      --    size    : Interfaces.C.unsigned_long;
      --    num     : Interfaces.C.unsigned_long) return Interfaces.C.unsigned_long;

      Objects_Written := Stream.Context.Write
        (context => RWops_Pointer (Stream.Context),
         ptr     => Item'Address,
         size    => Item'Length,
         num     => 1);

      if Objects_Written = 0 then
         raise RWops_Error with SDL.Error.Get;
      end if;

   end Write;

end SDL.RWops.Streams;
