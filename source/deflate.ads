--  SPDX-FileCopyrightText: 2013-2023 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0
----------------------------------------------------------------

package Deflate with SPARK_Mode, Pure is

private
   --  The compressed data consists of a series of elements of two types:
   --  literal bytes (of strings that have not been detected as duplicated
   --  within the previous 32K input bytes), and pointers to duplicated
   --  strings, where a pointer is represented as a pair <length, backward
   --  distance>. The representation used in the "deflate" format limits
   --  distances to 32K bytes and lengths to 258 bytes

   Max_Distance : constant := 2 ** 15; -- 32k

   Max_Length     : constant := 258;

   type Cycle_Index is mod Max_Distance;
   --  Index for a cycle buffer

   type Block_Kind is
     (Stored_Block,            --  Block stored without any compression
      Fixed_Huffman_Block,     --  Block with predefined codes
      Dynamic_Huffman_Block,   --  Block with codes saved in stream
      Reserved);               --  Not used value
   --  Kind of block inside of deflate stream

   type Literal is range 0 .. 287;
   --  Literal is used to encode byte values, end of block and length

   subtype Literal_Range is Literal range   0 .. 255;
   subtype End_Of_Block  is Literal range 256 .. 256;
   subtype Length_Range  is Literal range 257 .. 285;

   type Length_Code is range 0 .. 18;
   --  Length code is used to encode length values

   subtype Direct_Length_Code     is Length_Code range  0 .. 15;
   subtype Copy_Length_Code       is Length_Code range 16 .. 16;
   subtype Zero_Length_Code       is Length_Code range 17 .. 17;
   subtype Long_Zero_Length_Code  is Length_Code range 18 .. 18;

   type Distance_Code is range 0 .. 29;
   --  Distance code is used to encode distance values

end Deflate;
