--  SPDX-FileCopyrightText: 2013-2023 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0
----------------------------------------------------------------

with Ada.Streams;

with Deflate.Bit_Streams;

package Deflate.Compressors with SPARK_Mode is

   type Compressor is limited private
     with Preelaborable_Initialization;

   procedure Push_Byte
     (Self   : in out Compressor;
      Output : in out Ada.Streams.Root_Stream_Type'Class;
      Data   : Ada.Streams.Stream_Element);

   procedure Complete
     (Self   : in out Compressor;
      Output : in out Ada.Streams.Root_Stream_Type'Class);

private
   type Stage is (Empty, Completed_Search, Incomplete_Search);
   --  Compressor stage
   --  * @value Empty            - Initialization starts
   --
   --  * @value Completed_Search - Previous search of optimal pack sequence
   --    completed, start new search
   --
   --  * @value Incomplete_Search - Search of optimal pack sequence in progress

   type Cycle_Buffer is array (Cycle_Index) of Ada.Streams.Stream_Element;

   Max_Look_Ahead_Count : constant := 64;
   --  Max count of look ahead searches to keep

   Max_Look_Ahead       : constant := 1 + Max_Length * Max_Look_Ahead_Count;
   --  We could skip a byte then make Look_Ahead searches of Max_Length each,
   --  so Max bytes of look ahead searches is here.

   type Look_Ahead_Count is range 1 .. Max_Look_Ahead_Count;
   --  Count of look ahead searches

   type Look_Ahead_Position is range 0 .. Max_Look_Ahead;
   --  Bytes of look ahead searches

   type Look_Ahead_Kinds is (Use_Current, Skip_Current);
   --  We search for best of:
   --  * Skip_Current - skip one byte and search for back references then
   --  * Use_Current - search for back references from current byte

   subtype Length_Step is Cycle_Index range 0 .. Max_Length;
   --  Length of back reference

   type Back_Reference is record
      Length : Length_Step;  --  Length of back reference
      Where  : Cycle_Index;  --  Where back reference found
   end record with Size => 32;

   type Back_Reference_Array is
     array (Look_Ahead_Count, Look_Ahead_Kinds) of Back_Reference;

   type Look_Ahead_Context is record
      Skipped : Cycle_Index;
      A       : Look_Ahead_Position := 0;
      B       : Look_Ahead_Position := 1;
      M       : Look_Ahead_Count;  --  loop parameter in LA'Range (1)
      K       : Look_Ahead_Kinds;  --  loop parameter in LA'Range (2)
      LA      : Back_Reference_Array;  --  Lookahead data
   end record;
   --  Lookahead data

   type Buffer is tagged record
      Data     : Cycle_Buffer;
      Index    : Cycle_Index := Cycle_Index'Last;  --  Current byte in Data
      Last     : Cycle_Index := Cycle_Index'Last;  --  Last read element
      Filled   : Boolean := False;      --  If buffer has been filled once
   end record;

   type Compressor is limited record
      Last_Stage : Stage := Empty;
      Output     : Bit_Streams.Output_Bit_Stream;
      Context    : Look_Ahead_Context;
      Buffer     : Deflate.Compressors.Buffer;
   end record;

end Deflate.Compressors;
