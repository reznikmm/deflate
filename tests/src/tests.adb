--  SPDX-FileCopyrightText: 2023 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0
----------------------------------------------------------------

with Ada.Streams.Stream_IO.C_Streams;
with Ada.Streams.Stream_IO;
with Interfaces.C_Streams;

with GNAT.CRC32;

with Deflate.Compressors;

procedure Tests is
   Input      : Ada.Streams.Stream_IO.File_Type;
   Output     : Ada.Streams.Stream_IO.File_Type;
   Compressor : Deflate.Compressors.Compressor;
   CRC        : GNAT.CRC32.CRC32;
begin
   GNAT.CRC32.Initialize (CRC);

   Ada.Streams.Stream_IO.C_Streams.Open
     (File => Input,
      Mode => Ada.Streams.Stream_IO.In_File,
      C_Stream => Interfaces.C_Streams.stdin);

   Ada.Streams.Stream_IO.C_Streams.Open
     (File => Output,
      Mode => Ada.Streams.Stream_IO.Out_File,
      C_Stream => Interfaces.C_Streams.stdout);

   Ada.Streams.Stream_IO.Write
     (Output,
      (16#1f#, 16#8b#,  --  ID1, ID2
       16#08#,  --  Compression Method = Deflate
       16#00#,  --  Flags
       0, 0, 0, 0,  --  Modification time
       0,  --  Extra flags, 2=max compression, 4=fast compression
       3));   --  OS, 3 = Unix

   declare
      use type Ada.Streams.Stream_Element_Count;
      Data   : Ada.Streams.Stream_Element_Array (1 .. 256);
      Last   : Ada.Streams.Stream_Element_Count;
      Total  : Ada.Streams.Stream_Element_Count := 0;
      Stream : Ada.Streams.Root_Stream_Type'Class renames
        Ada.Streams.Stream_IO.Stream (Output).all;
   begin
      loop
         Ada.Streams.Stream_IO.Read (Input, Data, Last);
         Total := Total + Last;
         GNAT.CRC32.Update (CRC, Data (1 .. Last));

         for Byte of Data (1 .. Last) loop
            Deflate.Compressors.Push_Byte (Compressor, Stream, Byte);
         end loop;

         exit when Last = 0;
      end loop;

      Deflate.Compressors.Complete (Compressor, Stream);

      Interfaces.Unsigned_32'Write (Stream'Access, GNAT.CRC32.Get_Value (CRC));
      --  CRC32 TDB

      Natural'Write (Stream'Access, Natural (Total)); --  SIZE
   end;
end Tests;
