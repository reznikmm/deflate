--  SPDX-FileCopyrightText: 2013-2023 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0
----------------------------------------------------------------

with Interfaces;

package body Deflate.Huffman_Tables with SPARK_Mode is

   function Reverse_Bits
     (Value  : Bit_Streams.Bits;
      Length : Positive_Length) return Bit_Streams.Bits;
   pragma Inline (Reverse_Bits);

   ------------
   -- Create --
   ------------

   function Create
     (Max_Length  : Positive_Length;
      Max_Element : Encoded_Element;
      Map         : Length_Map) return Huffman_Table
   is
      Ignore : Boolean;
   begin
      return Result : Huffman_Table (Max_Length, Max_Element) do
         Result.Length := Map;
         Initialize (Result, Map, Ignore);
      end return;
   end Create;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : in out Huffman_Table;
      Map  : Length_Map;
      Ok   : out Boolean)
   is
      type Codes is array (Length) of Natural;

      Min_Len    : Length := Map (0);
      Max_Len    : Length := Map (0);
      Next_Code  : Bit_Streams.Bits;
      Len        : Bit_Streams.Bit_Count;
      Counts     : Codes := (others => 0);
      Min_Codes  : Codes := (others => 0);
      Next_Min   : Natural range 0 .. Natural'Last / 2 -  Map'Length := 0;
   begin
      Ok := True;

      --  Find min and max element of Map
      for J in Map'Range loop
         Max_Len := Length'Max (Max_Len, Map (J));
         Min_Len := Length'Min (Min_Len, Map (J));

         pragma Loop_Invariant
           (for all K in Map'First .. J => Map (K) <= Max_Len);

      end loop;

      pragma Assert (for all Len of Map => Len <= Max_Len);

      Self.Mask := 2 ** Natural (Max_Len) - 1;

      pragma Assert (for all C of Counts => C = 0);

      for J in Map'Range loop
         pragma Loop_Invariant
           ((for all V of Counts => V <= Natural (J - Map'First)));

         Counts (Map (J)) := Counts (Map (J)) + 1;
      end loop;

      Min_Codes (Min_Len) := Next_Min;

      --  Calculate Min_Codes. TODO Prove Ok = True?
      for J in Min_Len .. Max_Len - 1 loop
         if Next_Min + 2 * Map'Length < Natural'Last / 4 then
            Next_Min := (Next_Min + Counts (J)) * 2;
            Min_Codes (J + 1) := Next_Min;
         else
            Ok := False;
         end if;
      end loop;

      --  Set Self.Values, Self.Bits
      for K in Map'Range loop
         Len := Map (K);
         if Len /= 0 then
            if Min_Codes (Len) < Natural (Bit_Streams.Bits'Last) then
               Next_Code :=
                 Reverse_Bits (Bit_Streams.Bits (Min_Codes (Len)), Len);
               Min_Codes (Len) := Min_Codes (Len) + 1;
               Self.Bits (K) := Next_Code;
            else
               Ok := False;
            end if;
         end if;
      end loop;

      if Ok then
         Self.Length := Map;
      else
         Ok := False;
         Self.Mask := 0;
      end if;
   end Initialize;

   ------------------
   -- Reverse_Bits --
   ------------------

   function Reverse_Bits
     (Value  : Bit_Streams.Bits;
      Length : Positive_Length) return Bit_Streams.Bits
   is
      use Interfaces;

      subtype Shiftable is Unsigned_32
        range 0 .. Shift_Left (1, Natural (Max_Length)) - 1;

      Result  : Shiftable := 0;
      Current : Shiftable := Shiftable (Value);
   begin
      for J in 1 .. Length loop
         Result  := Shift_Left  (Result, 1) or (Current and 1);
         Current := Shift_Right (Current, 1);
      end loop;

      return Bit_Streams.Bits (Result);
   end Reverse_Bits;

   -----------
   -- Write --
   -----------

   procedure Write
     (Self   : Huffman_Table;
      Value  : Encoded_Element;
      Output : in out Ada.Streams.Root_Stream_Type'Class;
      Bits   : in out Bit_Streams.Output_Bit_Stream) is
   begin
      Bit_Streams.Write
        (Bits,
         Length => Self.Length (Value),
         Value  => Self.Bits (Value),
         Output => Output);
   end Write;

end Deflate.Huffman_Tables;
