--  SPDX-FileCopyrightText: 2023 Max Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0
----------------------------------------------------------------

package body Deflate.Bit_Streams with SPARK_Mode is

   -----------
   -- Write --
   -----------

   procedure Write
     (Self   : in out Output_Bit_Stream;
      Length : Bit_Count;
      Value  : Bits;
      Output : in out Ada.Streams.Root_Stream_Type'Class)
   is
      use type Word;
      Byte_Size : constant := 8;
      Left : Bit_Count'Base := Self.Left;
   begin
      Self.Head :=
        Interfaces.Shift_Left (Word (Value), Natural (Self.Left)) + Self.Head;

      Left := Left + Length;

      while Left >= Byte_Size loop
         declare
            Next : constant Ada.Streams.Stream_Element :=
              Ada.Streams.Stream_Element'Val
                (Self.Head and (2 ** Byte_Size - 1));
         begin
            Output.Write ((1 => Next));
            Self.Head := Interfaces.Shift_Right (Self.Head, Byte_Size);
            Left := Left - Byte_Size;

            --  pragma Loop_Invariant (Left >= 0);
            --  pragma Loop_Variant (Decreases => Left);
         end;
      end loop;

      Self.Left := Left;
   end Write;

end Deflate.Bit_Streams;
