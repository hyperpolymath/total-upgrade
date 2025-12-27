--  SPDX-License-Identifier: AGPL-3.0-or-later
--  SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell <jonathan@hyperpolymath.io>

--  DNFinition - Universal Package Manager TUI with Reversibility
--  Main entry point

with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Exceptions;

with Platform_Detection;
with Reversibility_Types;
with Snapshot_Manager;
with TUI.Main_Window;

procedure DNFinition is
   use Ada.Text_IO;
   use Platform_Detection;
   use Reversibility_Types;

   Platform : Platform_Info;
   Version  : constant String := "0.1.0";

   procedure Print_Banner is
   begin
      Put_Line ("╔═══════════════════════════════════════════════════════════╗");
      Put_Line ("║  DNFinition " & Version & " - Universal Package Manager         ║");
      Put_Line ("║  with Reversibility and Formal Verification               ║");
      Put_Line ("╚═══════════════════════════════════════════════════════════╝");
      New_Line;
   end Print_Banner;

   procedure Print_Platform_Info is
   begin
      Put_Line ("Platform Detection:");
      Put_Line ("  OS Family:      " & OS_Family'Image (Platform.OS));

      if Platform.OS = Linux then
         Put_Line ("  Distribution:   " &
                   Distribution_Name (Platform.Distribution));
         Put_Line ("  Immutable:      " & Boolean'Image (Platform.Is_Immutable));
         Put_Line ("  Container:      " & Boolean'Image (Platform.Is_Container));
         Put_Line ("  WSL:            " & Boolean'Image (Platform.Is_WSL));
      end if;

      Put_Line ("  Package Manager: " & PM_Name (Platform.Primary_PM));
      Put_Line ("  Snapshot Support: " &
                Snapshot_Backend'Image (Platform.Snapshot_Support));

      if Platform.Has_Flatpak then
         Put_Line ("  + Flatpak available");
      end if;
      if Platform.Has_Snap then
         Put_Line ("  + Snap available");
      end if;
      if Platform.Has_Nix then
         Put_Line ("  + Nix available");
      end if;
      if Platform.Has_Guix then
         Put_Line ("  + Guix available");
      end if;

      New_Line;
   end Print_Platform_Info;

   procedure Print_Usage is
   begin
      Put_Line ("Usage: dnfinition [COMMAND] [OPTIONS]");
      New_Line;
      Put_Line ("Commands:");
      Put_Line ("  (none)          Start interactive TUI");
      Put_Line ("  search QUERY    Search for packages");
      Put_Line ("  install PKG...  Install packages");
      Put_Line ("  remove PKG...   Remove packages");
      Put_Line ("  upgrade         Upgrade all packages");
      Put_Line ("  list            List installed packages");
      Put_Line ("  snapshots       List available snapshots");
      Put_Line ("  rollback [ID]   Rollback to snapshot");
      Put_Line ("  info            Show platform information");
      Put_Line ("  --version       Show version");
      Put_Line ("  --help          Show this help");
      New_Line;
      Put_Line ("All operations create automatic snapshots for rollback.");
      Put_Line ("Use 'dnfinition rollback' to undo the last operation.");
   end Print_Usage;

   procedure Handle_Command_Line is
      use Ada.Command_Line;
   begin
      if Argument_Count = 0 then
         --  Start TUI mode
         Print_Banner;
         Print_Platform_Info;
         Put_Line ("Starting interactive mode...");
         Put_Line ("(TUI not yet implemented - coming soon!)");
         --  TODO: TUI.Main_Window.Run (Platform);
         return;
      end if;

      declare
         Cmd : constant String := Argument (1);
      begin
         if Cmd = "--version" or else Cmd = "-v" then
            Put_Line ("DNFinition version " & Version);

         elsif Cmd = "--help" or else Cmd = "-h" then
            Print_Banner;
            Print_Usage;

         elsif Cmd = "info" then
            Print_Banner;
            Print_Platform_Info;

         elsif Cmd = "search" then
            if Argument_Count < 2 then
               Put_Line ("Error: search requires a query");
               Set_Exit_Status (Failure);
            else
               Put_Line ("Searching for: " & Argument (2));
               Put_Line ("(Search not yet implemented)");
            end if;

         elsif Cmd = "install" then
            if Argument_Count < 2 then
               Put_Line ("Error: install requires package name(s)");
               Set_Exit_Status (Failure);
            else
               Put_Line ("Would install: ");
               for I in 2 .. Argument_Count loop
                  Put_Line ("  - " & Argument (I));
               end loop;
               Put_Line ("(Install not yet implemented)");
            end if;

         elsif Cmd = "remove" then
            if Argument_Count < 2 then
               Put_Line ("Error: remove requires package name(s)");
               Set_Exit_Status (Failure);
            else
               Put_Line ("Would remove: ");
               for I in 2 .. Argument_Count loop
                  Put_Line ("  - " & Argument (I));
               end loop;
               Put_Line ("(Remove not yet implemented)");
            end if;

         elsif Cmd = "upgrade" then
            Put_Line ("Would upgrade all packages");
            Put_Line ("(Upgrade not yet implemented)");

         elsif Cmd = "list" then
            Put_Line ("Listing installed packages...");
            Put_Line ("(List not yet implemented)");

         elsif Cmd = "snapshots" then
            Put_Line ("Listing snapshots...");
            Put_Line ("(Snapshots not yet implemented)");

         elsif Cmd = "rollback" then
            if Argument_Count >= 2 then
               Put_Line ("Would rollback to snapshot: " & Argument (2));
            else
               Put_Line ("Would rollback to last snapshot");
            end if;
            Put_Line ("(Rollback not yet implemented)");

         else
            Put_Line ("Unknown command: " & Cmd);
            Put_Line ("Use --help for usage information");
            Set_Exit_Status (Failure);
         end if;
      end;
   end Handle_Command_Line;

begin
   --  Detect platform first
   Platform := Detect_Platform;

   --  Process command line
   Handle_Command_Line;

exception
   when E : others =>
      Put_Line ("Fatal error: " & Ada.Exceptions.Exception_Message (E));
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
end DNFinition;
