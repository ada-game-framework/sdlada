--------------------------------------------------------------------------------------------------------------------
--  This source code is subject to the Zlib license, see the LICENCE file in the root of this directory.
--------------------------------------------------------------------------------------------------------------------
--  SDL.Inputs.Joysticks.Game_Controllers
--------------------------------------------------------------------------------------------------------------------
with SDL.Events.Joysticks.Game_Controllers;

package SDL.Inputs.Joysticks.Game_Controllers is
   pragma Preelaborate;

   type Bind_Types is (None, Button, Axis, Hat) with
     Convention => C;

   type Hat_Bindings is
      record
         Hat  : SDL.Events.Joysticks.Hats;
         Mask : SDL.Events.Joysticks.Hat_Positions;
      end record with
        Convention => C;

   type Binding_Values (Which : Bind_Types := None) is
      record
         case Which is
            when None =>
               null;

            when Button =>
               Button : SDL.Events.Joysticks.Game_Controllers.Buttons;

            when Axis =>
               Axis   : SDL.Events.Joysticks.Game_Controllers.Axes;

            when Hat =>
               Hat    : Hat_Bindings;
         end case;
      end record with
        Unchecked_Union;

   type Bindings is
      record
         Which : Bind_Types;
         Value : Binding_Values (None);
      end record with
        Convention => C;

   Mapping_Error : exception;

   type Game_Controller is new Ada.Finalization.Limited_Controlled with private;

   Null_Game_Controller : constant Game_Controller;

   subtype Uint16 is Interfaces.Unsigned_16;
   subtype Uint32 is Interfaces.Unsigned_32;

   procedure Add_Mapping (Data : in String; Updated_Existing : out Boolean);
   procedure Add_Mappings_From_File (Database_Filename : in String; Number_Added : out Natural);

   function Axis_Value (Self : in Game_Controller;
                        Axis : in SDL.Events.Joysticks.Game_Controllers.LR_Axes)
                       return SDL.Events.Joysticks.Game_Controllers.LR_Axes_Values;

   function Axis_Value (Self : in Game_Controller;
                        Axis : in SDL.Events.Joysticks.Game_Controllers.Trigger_Axes)
                       return SDL.Events.Joysticks.Game_Controllers.Trigger_Axes_Values;

   procedure Close (Self : in out Game_Controller);

   function Get_Axis (Axis : in String) return SDL.Events.Joysticks.Game_Controllers.Axes;

   function Get_Binding (Self : in Game_Controller; Axis : in SDL.Events.Joysticks.Game_Controllers.Axes)
                        return Bindings;

   function Get_Binding (Self : in Game_Controller; Button : in SDL.Events.Joysticks.Game_Controllers.Buttons)
                        return Bindings;

   function Get_Button (Button_Name : in String) return SDL.Events.Joysticks.Game_Controllers.Buttons;

   function Get_Joystick (Self : in Game_Controller) return Joystick;

   function Get_Mapping (Self : in Game_Controller) return String;
   function Get_Mapping (Controller : in GUIDs) return String;

   function Get_Name (Self : in Game_Controller) return String;
   function Get_Name (Device : in Devices) return String;

   function Image (Axis : in SDL.Events.Joysticks.Game_Controllers.Axes) return String;
   function Image (Button : in SDL.Events.Joysticks.Game_Controllers.Buttons) return String;

   function Is_Attached (Self : in Game_Controller) return Boolean with
     Inline => True;

   function Is_Button_Pressed (Self : in Game_Controller; Button : in SDL.Events.Joysticks.Buttons)
                               return SDL.Events.Button_State with
     Inline => True;

   function Is_Game_Controller (Device : in Devices) return Boolean with
     Inline => True;

   function Has_Rumble (Self : in Game_Controller) return Boolean with
     Inline => True;

   function Rumble (Self           : in Game_Controller;
                    Low_Frequency  : in Uint16;
                    High_Frequency : in Uint16;
                    Duration       : in Uint32) return Integer with
     Inline => True;
private
   type Game_Controller is new Ada.Finalization.Limited_Controlled with
      record
         Internal : SDL.C_Pointers.Game_Controller_Pointer := null;
         Owns     : Boolean                                := True;
      end record;

   Null_Game_Controller : constant Game_Controller := (Ada.Finalization.Limited_Controlled with
                                                       Internal => null,
                                                       Owns => True);
end SDL.Inputs.Joysticks.Game_Controllers;
