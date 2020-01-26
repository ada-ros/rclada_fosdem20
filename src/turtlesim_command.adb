with Ada.Calendar;     use Ada.Calendar;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;      use Ada.Text_IO;

with RCL.Logging;
with RCL.Nodes;
with RCL.Publishers;
with RCL.Utils;

with ROSIDL.Dynamic;
with ROSIDL.Typesupport;

procedure Turtlesim_Command is

   use RCL;

   Support : constant ROSIDL.Typesupport.Message_Support :=
               ROSIDL.Typesupport.Get_Message_Support
                 ("geometry_msgs", "Twist");
   MsgLin  : ROSIDL.Dynamic.Message := ROSIDL.Dynamic.Init (Support);
   MsgRot  : ROSIDL.Dynamic.Message := ROSIDL.Dynamic.Init (Support);

   Node : Nodes.Node := Nodes.Init   (Utils.Command_Name);

begin
   Logging.Info ("Turtlesim commander starting...");

   if Argument_Count = 0 then
      Logging.Info ("First argument must be the receiver topic");
      return;
   elsif Argument_Count = 1 then
      Put_Line ("No command given, using default pattern...");
      MsgLin ("linear.x").As_Float64  := 3.0;
      MsgRot ("angular.z").As_Float64 := 2.0;
   else
      null;
   end if;

   declare

      Pub    : Publishers.Publisher := Node.Publish (Support, Argument (1));
      Next   : Time    := Clock;
      Rotate : Boolean := False;
   begin
      loop
         if Next < Clock then
            if Rotate then
               Pub.Publish (MsgRot);
            else
               Pub.Publish (MsgLin);
            end if;

            Rotate := not Rotate;

            Next := Clock + 1.0;
            Logging.Info ("Sending...");
         end if;
      end loop;
   end;
end Turtlesim_Command;
