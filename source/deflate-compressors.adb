--  SPDX-FileCopyrightText: 2013-2023 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0
----------------------------------------------------------------

with Deflate.Huffman_Tables;
with Deflate.Tables;

package body Deflate.Compressors with SPARK_Mode is

   procedure Internal_Push_Byte
     (Self     : in out Compressor;
      Output   : in out Ada.Streams.Root_Stream_Type'Class;
      Data     : Ada.Streams.Stream_Element;
      Complete : Boolean);

   procedure Output_Block_Header
     (Bits   : in out Bit_Streams.Output_Bit_Stream;
      Output : in out Ada.Streams.Root_Stream_Type'Class);
   --  Write deflate block header with last block marker and fixed huffman

   procedure Send_Literal
     (Bits   : in out Bit_Streams.Output_Bit_Stream;
      Output : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : Ada.Streams.Stream_Element);
   --  Output literal

   procedure Send_Back_Reference
     (Bits   : in out Bit_Streams.Output_Bit_Stream;
      Output : in out Ada.Streams.Root_Stream_Type'Class;
      A      : Back_Reference);
   --  Output back reference

   --------------
   -- Complete --
   --------------

   procedure Complete
     (Self   : in out Compressor;
      Output : in out Ada.Streams.Root_Stream_Type'Class) is
   begin
      case Self.Last_Stage is
         when Empty =>
            Output_Block_Header (Self.Output, Output);
         when Completed_Search =>
            null;
         when Incomplete_Search =>
            Internal_Push_Byte
              (Self,
               Output,
               Ada.Streams.Stream_Element'First,
               Complete => True);
      end case;

      Huffman_Tables.Write
        (Tables.Fixed_Literal_Table,
         Value  => Huffman_Tables.Encoded_Element (Deflate.End_Of_Block'Last),
         Output => Output,
         Bits   => Self.Output);

      if Bit_Streams.Has_Bits (Self.Output, 1) then
         Bit_Streams.Write
           (Self.Output, Length => 7, Value => 0, Output => Output);
      end if;
   end Complete;

   ------------------------
   -- Internal_Push_Byte --
   ------------------------

   procedure Internal_Push_Byte
     (Self     : in out Compressor;
      Output   : in out Ada.Streams.Root_Stream_Type'Class;
      Data     : Ada.Streams.Stream_Element;
      Complete : Boolean)
   is
      procedure Advance_Last_Buffer_Index (Count : Length_Step);
      --  Make next Count bytes of input buffer available for search.
      --  Precondition: Buffer has Count bytes

      procedure Find_Best_Matched
        (Min_Length : Length_Step;
         Where      : out Cycle_Index;
         Length     : out Length_Step;
         Not_Enough : in out Boolean);
      --  Search for a string in the buffer that duplicates input.
      --  Take the string not shorten than Min_Length, rightmost.
      --  Return Where and Length if found, otherwise return Length = 1.
      --  Return Not_Enough when the search failed due to no enough input data

      procedure Swap (A, B : in out Look_Ahead_Position);
      --  Exchange A and B

      End_Of_File : Boolean := Complete;

      -------------------------------
      -- Advance_Last_Buffer_Index --
      -------------------------------

      procedure Advance_Last_Buffer_Index (Count : Length_Step) is
      begin
         Self.Buffer.Index := Self.Buffer.Index + Count;
      end Advance_Last_Buffer_Index;

      -----------------------
      -- Find_Best_Matched --
      -----------------------

      procedure Find_Best_Matched
        (Min_Length : Length_Step;
         Where      : out Cycle_Index;
         Length     : out Length_Step;
         Not_Enough : in out Boolean)
      is
         pragma Unreferenced (Min_Length);
         use type Ada.Streams.Stream_Element;

         Buf : Deflate.Compressors.Buffer renames Self.Buffer;
         From : Cycle_Index;
      begin
         Length := 1;
         Where := Cycle_Index'First;

         if Buf.Filled then
            From := Buf.Last + 1;
         elsif Buf.Index = 0 then
            --  No chars before Buf.Index, so search is impossible
            Not_Enough := End_Of_File and Buf.Last = 0;
            return;
         else
            From := 0;
         end if;

         for K in reverse From .. Buf.Index - 1 loop
            for J in 0 .. Length_Step'Last - 1 loop
               if Buf.Index + J = Buf.Last + 1 then
                  if End_Of_File then
                     Not_Enough := True;
                     exit;
                  else
                     Buf.Last := Buf.Last + 1;
                     --  if Buf.Filled then delete_hash (Buf.Last)
                     Buf.Data (Buf.Last) := Data;
                     End_Of_File := True;
                  end if;
               end if;

               exit when Buf.Data (K + J) /= Buf.Data (Buf.Index + J);

               if J + 1 > Length then
                  Where := Buf.Index - K;
                  Length := J + 1;
               end if;
            end loop;
         end loop;
      end Find_Best_Matched;

      ----------
      -- Swap --
      ----------

      procedure Swap (A, B : in out Look_Ahead_Position) is
         Tmp : constant Look_Ahead_Position := A;
      begin
         A := B;
         B := Tmp;
      end Swap;

      LA : Back_Reference_Array  renames Self.Context.LA;
      A  : Look_Ahead_Position   renames Self.Context.A;  --  f[m-2]
      B  : Look_Ahead_Position   renames Self.Context.B;  --  f[m-1]
      M  : Look_Ahead_Count      renames Self.Context.M;
      K  : Look_Ahead_Kinds      renames Self.Context.K;
      C  : Length_Step;  --  f[m-1] - f[m-2]

      Where      : Cycle_Index;
      Length     : Length_Step;
      Not_Enough : Boolean := False;
   begin
      if Self.Last_Stage = Empty then
         --  Begin of pack stream, initialize Buffer and Output stream
         Self.Buffer.Index := 0;
         Self.Buffer.Last := Self.Buffer.Index;
         Self.Buffer.Filled := False;
         Self.Buffer.Data (Self.Buffer.Index) := Data;
         End_Of_File := True;
         Output_Block_Header (Self.Output, Output);
         Self.Last_Stage := Completed_Search;
      end if;

      loop
         if Self.Last_Stage = Completed_Search then
            --  Previous search of optimal pack sequence completed, start new
            A := 0;  --  How long search prev-prev search went
            B := 1;  --  How long search prev search went
            --  Remember literal for case when we will decide to skip it
            Self.Context.Skipped := Self.Buffer.Index;

            --  Over: for M in LA'Range (1) loop
            M := 1;
            --  for K in LA'Range (2) loop
            K := Use_Current;
            Self.Last_Stage := Incomplete_Search;
         end if;

         --  continue search
         Incomplete_Search_Label :
         loop
            C := Length_Step (B - A);
            Find_Best_Matched (C + 1, Where, Length, Not_Enough);

            if Not_Enough and not Complete then
               Self.Last_Stage := Incomplete_Search;
               return;
            end if;

            LA (M, K).Where := Where;
            LA (M, K).Length := Length;
            Advance_Last_Buffer_Index (C);

            if Length < Length_Step'Max (3, C + 1) then
               exit Incomplete_Search_Label;  --  exit Over;
            end if;

            A := A + Look_Ahead_Position (Length);
            Swap (A, B);

            if K = Use_Current then
               K := Skip_Current;
               --  end loop;
            else
               K := Use_Current;

               if M = Max_Look_Ahead_Count then
                  C := Length_Step (B - A);
                  Advance_Last_Buffer_Index (C);
                  exit Incomplete_Search_Label;
               end if;

               M := M + 1;
               --  end loop Over;
            end if;
         end loop Incomplete_Search_Label;

         case K is
         when Use_Current =>
            Send_Literal
              (Self.Output, Output, Self.Buffer.Data (Self.Context.Skipped));

            for J in 1 .. M - 1 loop
               Send_Back_Reference (Self.Output, Output, LA (J, Skip_Current));
            end loop;

         when Skip_Current =>
            for J in 1 .. M loop
               Send_Back_Reference (Self.Output, Output, LA (J, Use_Current));
            end loop;
         end case;
         --
         --  if not End_Of_File or Buffer_Has_Data then
         --     goto Completed_Search_Label;
         --  end if;
         --
         Self.Last_Stage := Completed_Search;

         exit when End_Of_File and Self.Buffer.Index = Self.Buffer.Last + 1;
      end loop;
   end Internal_Push_Byte;

   -------------------------
   -- Output_Block_Header --
   -------------------------

   procedure Output_Block_Header
     (Bits   : in out Bit_Streams.Output_Bit_Stream;
      Output : in out Ada.Streams.Root_Stream_Type'Class) is
   begin
      Bit_Streams.Write (Bits, Length => 1, Value => 1, Output => Output);
      --  Last block

      Bit_Streams.Write
        (Bits,
         Length => 2,
         Value  => Block_Kind'Pos (Fixed_Huffman_Block),
         Output => Output);
   end Output_Block_Header;

   ---------------
   -- Push_Byte --
   ---------------

   procedure Push_Byte
     (Self   : in out Compressor;
      Output : in out Ada.Streams.Root_Stream_Type'Class;
      Data   : Ada.Streams.Stream_Element) is
   begin
      Internal_Push_Byte (Self, Output, Data, False);
   end Push_Byte;

   -------------------------
   -- Send_Back_Reference --
   -------------------------

   procedure Send_Back_Reference
     (Bits   : in out Bit_Streams.Output_Bit_Stream;
      Output : in out Ada.Streams.Root_Stream_Type'Class;
      A      : Back_Reference)
   is
      package T renames Tables;
      Length_Code   : Literal := Length_Range'Last;
      Distance_Code : Deflate.Distance_Code := T.Distance_Codes'Last;
      Value         : Bit_Streams.Bits;
   begin
      for J in T.Length_Codes'First .. T.Length_Codes'Last - 1 loop
         if A.Length < T.Length_Codes (J + 1).Base_Length then
            Length_Code := J;
            exit;
         end if;
      end loop;

      Huffman_Tables.Write
        (T.Fixed_Literal_Table,
         Value  => Huffman_Tables.Encoded_Element (Length_Code),
         Output => Output,
         Bits   => Bits);

      Value := Bit_Streams.Bits
        (A.Length - T.Length_Codes (Length_Code).Base_Length);

      Bit_Streams.Write
        (Bits,
         Length => T.Length_Codes (Length_Code).Extra_Bits,
         Value  => Value,
         Output => Output);

      for J in T.Distance_Codes'First .. T.Distance_Codes'Last - 1 loop
         if A.Where < T.Distance_Codes (J + 1).Base_Length then
            Distance_Code := J;
            exit;
         end if;
      end loop;

      Huffman_Tables.Write
        (T.Fixed_Distance_Table,
         Value  => Huffman_Tables.Encoded_Element (Distance_Code),
         Output => Output,
         Bits   => Bits);

      Value := Bit_Streams.Bits
        (A.Where - T.Distance_Codes (Distance_Code).Base_Length);

      Bit_Streams.Write
        (Bits,
         Length => T.Distance_Codes (Distance_Code).Extra_Bits,
         Value  => Value,
         Output => Output);
   end Send_Back_Reference;

   ------------------
   -- Send_Literal --
   ------------------

   procedure Send_Literal
     (Bits   : in out Bit_Streams.Output_Bit_Stream;
      Output : in out Ada.Streams.Root_Stream_Type'Class;
      Value  : Ada.Streams.Stream_Element)
   is
      Code : constant Huffman_Tables.Encoded_Element :=
        Ada.Streams.Stream_Element'Pos (Value);
   begin
      Huffman_Tables.Write
        (Tables.Fixed_Literal_Table,
         Value  => Code,
         Output => Output,
         Bits   => Bits);
   end Send_Literal;

end Deflate.Compressors;
