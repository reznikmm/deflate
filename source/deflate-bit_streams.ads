--  SPDX-FileCopyrightText: 2013-2023 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0
----------------------------------------------------------------

private with Interfaces;

with Ada.Streams;

package Deflate.Bit_Streams with SPARK_Mode, Pure is

   Max_Bits : constant := 16;
   --  Max size of reading piece of data

   type Bit_Count is range 0 .. Max_Bits;
   subtype Positive_Bit_Count is Bit_Count range 1 .. Max_Bits;

   type Bits is mod 2 ** Max_Bits;

   type Output_Bit_Stream is limited private
     with Preelaborable_Initialization;

   procedure Write
     (Self   : in out Output_Bit_Stream;
      Length : Bit_Count;  --  Positive_Bit_Count???
      Value  : Bits;
      Output : in out Ada.Streams.Root_Stream_Type'Class);

   function Has_Bits
     (Self   : Output_Bit_Stream;
      Length : Positive_Bit_Count) return Boolean;

private

   subtype Word is Interfaces.Unsigned_32;

   type Output_Bit_Stream is limited record
      Head : Word := 0;
      Left : Bit_Count := 0;
   end record;

   function Has_Bits
     (Self   : Output_Bit_Stream;
      Length : Positive_Bit_Count) return Boolean is (Self.Left >= Length);

end Deflate.Bit_Streams;
