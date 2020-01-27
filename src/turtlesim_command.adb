with Ada.Calendar;     use Ada.Calendar;
with Ada.Command_Line; use Ada.Command_Line;

with RCL.Logging;
with RCL.Nodes;
with RCL.Publishers;
with RCL.Utils;

with ROSIDL.Dynamic;
with ROSIDL.Types;
with ROSIDL.Typesupport;

procedure Turtlesim_Command is

   use RCL;
   use all type ROSIDL.Types.Float64;

   Support : constant ROSIDL.Typesupport.Message_Support :=
               ROSIDL.Typesupport.Get_Message_Support
                 ("geometry_msgs", "Twist");
   MsgDraw : ROSIDL.Dynamic.Message := ROSIDL.Dynamic.Init (Support);
   MsgLin  : ROSIDL.Dynamic.Message := ROSIDL.Dynamic.Init (Support);
   MsgRot  : ROSIDL.Dynamic.Message := ROSIDL.Dynamic.Init (Support);

   Node : Nodes.Node := Nodes.Init   (Utils.Command_Name);

   Default : Boolean := False;

begin
   Logging.Info ("Turtlesim commander starting...");

   MsgLin ("linear.x").As_Float64  := 3.0;
   MsgRot ("angular.z").As_Float64 := 2.0;

   if Argument_Count = 0 then
      Logging.Info ("First argument must be the receiver topic");
      return;
   elsif Argument_Count = 1 then
      Logging.Info ("No command given, using default pattern...");
      Default := True;
   elsif Argument_Count = 2 then
      MsgLin ("linear.x").As_Float64  :=
        ROSIDL.Types.Float64'Value (Argument (2));
   elsif Argument_Count = 3 then
      MsgLin ("angular.z").As_Float64  :=
        ROSIDL.Types.Float64'Value (Argument (3));
   else
      Logging.Error
        ("Too many arguments: expected <topic> [lin vel X] [ang vel Z]");
   end if;

   declare
      Pub    : Publishers.Publisher := Node.Publish (Support, Argument (1));
      Next   : Time    := Clock;
      Rotate : Boolean := False;
      Count  : Natural := 0;

      procedure Draw is
         type Twist is record
            Vlin, Vang : ROSIDL.Types.Float64;
            Period     : Duration;
         end record;
         Walk : constant Twist := (1.0, 0.0, 1.0);
         Twists : constant array (Positive range <>) of Twist :=
                    ((0.0, 0.0, 1.0),
                     (0.0, 1.0, 1.0), Walk,
                     (0.0, -2.0, 1.0), Walk,
                     (0.0, 1.0, 1.0),
                     (0.0, 1.57, 0.8),
                     (0.0, -1.57, 1.0), (0.7, -1.57, 2.0),
                     (0.0, 3.1416, 1.0), Walk,
                     (0.0, 1.0, 1.0), Walk,
                     (0.0, -2.0, 1.0), Walk,
                     (0.0, 1.0, 1.0), Walk);
      begin
         for T of Twists loop
            MsgDraw ("linear.x").As_Float64 := T.Vlin;
            MsgDraw ("angular.z").As_Float64 := T.Vang;
            Pub.Publish (MsgDraw);
            delay T.Period;
         end loop;
      end Draw;
   begin
      if Default then
         Draw;
      end if;

      loop
         if Next < Clock then
            Count := Count + 1;
            MsgRot ("angular.x").As_Float64 := ROSIDL.Types.Float64 (Count);

            if Rotate then
               Pub.Publish (MsgRot);
            else
               Pub.Publish (MsgLin);
            end if;

            Rotate := not Rotate;

            Next := Clock + 1.0;
            Logging.Info ("Sending..." & Count'Img);
         end if;
      end loop;
   end;
end Turtlesim_Command;
