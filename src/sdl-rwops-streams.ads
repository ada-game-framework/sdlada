with Ada.Streams;
use Ada.Streams;

package SDL.RWops.Streams is
   type RWops_Stream is new Root_Stream_Type with private;

   overriding
   procedure Read
     (Stream : in out RWops_Stream;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset);

   overriding
   procedure Write
     (Stream : in out RWops_Stream;
      Item   : Stream_Element_Array);

   procedure Open
     (Op     : RWops;
      Stream : out RWops_Stream);

   function Open (Op : RWops) return RWops_Stream;

   procedure Close (Stream : RWops_Stream);

private
   type RWops_Stream is new Root_Stream_Type with
      record
         Context : RWops;
      end record;

end SDL.RWops.Streams;
