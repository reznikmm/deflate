--  SPDX-FileCopyrightText: 2013-2023 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0
----------------------------------------------------------------

with Deflate.Bit_Streams;
with Ada.Streams;

package Deflate.Huffman_Tables with SPARK_Mode is
   pragma Pure;

   type Encoded_Element is range 0 .. 287;

   subtype Length is Bit_Streams.Bit_Count range 0 .. 15;
   subtype Positive_Length is Length range 1 .. Length'Last;

   type Length_Map is array (Encoded_Element range <>) of Positive_Length;
   --  Map each encoded element to its length

   type Huffman_Table
     (Max_Length  : Positive_Length;
      Max_Element : Encoded_Element) is private
     with Preelaborable_Initialization;
--       Default_Initial_Condition => not Is_Initialized (Huffman_Table);

   function Is_Initialized (Self : Huffman_Table) return Boolean;

   function Create
     (Max_Length  : Positive_Length;
      Max_Element : Encoded_Element;
      Map         : Length_Map
     ) return Huffman_Table
       with Pre => Map'First = 0 and Map'Last = Max_Element,
            Post => Bit_Streams."=" (Create'Result.Max_Length, Max_Length)
              and Create'Result.Max_Element = Max_Element;

   procedure Initialize
     (Self : in out Huffman_Table;
      Map  : Length_Map;
      Ok   : out Boolean)
     with Pre => Map'First = 0 and Map'Last = Self.Max_Element,
       Post => (if Ok then Is_Initialized (Self));
   --  Create Huffman table for given length Map

   --  function Read
   --    (Self  : Huffman_Table;
   --     Input : in out Matreshka.Filters.Bit_Streams.Bit_Stream;
   --     Value : in out Encoded_Element)
   --     return Boolean;
   --  Read encoded Value from Input stream if it has enought bits

   procedure Write
     (Self   : Huffman_Table;
      Value  : Encoded_Element;
      Output : in out Ada.Streams.Root_Stream_Type'Class;
      Bits   : in out Bit_Streams.Output_Bit_Stream)
     with Pre => Is_Initialized (Self) and Value <= Self.Max_Element;
   --  Encode value and write it to Output keeping part of byte in Bits.

private

   use type Bit_Streams.Bit_Count, Bit_Streams.Bits;

   type Value_Table is array (Bit_Streams.Bits range <>) of Encoded_Element;
   --  This table maps any value of length Max_Length to Encoded_Element.
   --  Prefix of value is equal code of Encoded_Element.

   type Bits_Array is array (Encoded_Element) of Bit_Streams.Bits;

   type Huffman_Table
     (Max_Length  : Positive_Length;
      Max_Element : Encoded_Element)
   is tagged record
      Mask    : Bit_Streams.Bits := 0;
      Bits    : Bits_Array := (others => 0);
      Length  : Length_Map (0 .. Max_Element);
   end record;

   function Is_Initialized (Self : Huffman_Table) return Boolean is
      (Self.Mask > 0);
end Deflate.Huffman_Tables;
