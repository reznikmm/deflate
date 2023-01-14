--  SPDX-FileCopyrightText: 2013-2023 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0
----------------------------------------------------------------

with Deflate.Bit_Streams;
with Deflate.Huffman_Tables;

private package Deflate.Tables with SPARK_Mode is

   --  package Literal_Tables is new Huffman_Tables
   --    (Encoded_Element => Literal,
   --     Max_Length      => 15);

   Fixed_Literal_Table : constant Huffman_Tables.Huffman_Table
     (Max_Length => 15, Max_Element => 287);

   --  package Length_Tables is new Huffman_Tables
   --    (Encoded_Element => Length_Code,
   --     Max_Length      => 7);

   --  package Distance_Tables is new Huffman_Tables
   --    (Encoded_Element => Distance_Code,
   --     Max_Length      => 15);

   Fixed_Distance_Table : constant Huffman_Tables.Huffman_Table
     (Max_Length => 15, Max_Element => 29);
   --  Predefined (fixed) Huffman tables for deflate method.

   type Extra_Bits is record
      Extra_Bits  : Bit_Streams.Bit_Count;
      Base_Length : Cycle_Index;
   end record;
   --  This record stores number of extra bits to read for some length code
   --  and corresponding length.

   Length_Codes : constant array (Length_Range) of Extra_Bits :=
     ((0, 3), (0, 4), (0, 5), (0, 6), (0, 7), (0, 8), (0, 9), (0, 10),
      (1, 11), (1, 13), (1, 15), (1, 17),
      (2, 19), (2, 23), (2, 27), (2, 31),
      (3, 35), (3, 43), (3, 51), (3, 59),
      (4, 67), (4, 83), (4, 99), (4, 115),
      (5, 131), (5, 163), (5, 195), (5, 227),
      (0, 258));
   --  This table stores number of extra bits to read for each length code
   --  and corresponding length.

   Distance_Codes : constant array (Distance_Code) of Extra_Bits :=
     ((0, 1),  (0, 2),  (0, 3),  (0, 4),  (1, 5),  (1, 7),  (2, 9),   (2, 13),
      (3, 17), (3, 25), (4, 33), (4, 49), (5, 65), (5, 97), (6, 129), (6, 193),
      (7, 257), (7, 385), (8, 513), (8, 769), (9, 1025), (9, 1537),
      (10, 2049), (10, 3073), (11, 4097), (11, 6145), (12, 8193), (12, 12289),
      (13, 16385), (13, 24577));
   --  This table stores number of extra bits to read for each length code
   --  and corresponding length.

private

   Literal_Length : constant Deflate.Huffman_Tables.Length_Map :=
     (000 .. 143 => 8,
      144 .. 255 => 9,
      256 .. 279 => 7,
      280 .. 287 => 8);

   Fixed_Literal_Table : constant Deflate.Huffman_Tables.Huffman_Table
     (Max_Length => 15, Max_Element => 287) :=
        Deflate.Huffman_Tables.Create (15, 287, Literal_Length);

   Fixed_Distance_Table : constant Huffman_Tables.Huffman_Table
     (Max_Length => 15, Max_Element => 29) :=
        Deflate.Huffman_Tables.Create (15, 29, (0 .. 29 => 5));

end Deflate.Tables;
