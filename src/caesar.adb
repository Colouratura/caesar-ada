-- caesar.adb
--     An improved algorithm that uses bit magic to transform ASCII
--     characters instead of looping through a lookup table.
--
-- Written by: A Dikdik <cutedikdik@yandex.com>
--     Copyright (c) 2017 A Dikdik
--     All rights reserved.
--    
--     Licensed under the BSD clause 3 license.
--
-- About:
--     The goal of this program is to function as a bi-directional way of
--     ciphering and deciphering text using the classical Caesar cipher (ROT-1)
--
--     In order to do this without using a look-up table that contains pairs of
--     both upper and lower-case ASCII letters this program uses tricks in order
--     shift and wrap around ASCII values. In some ways this is more efficient
--     and requires less code while still performing the same basic job as a
--     program that uses a look-up table.
--
-- to compile: ~$ make
--
-- to encipher: ~$ ./bin/caesar --encipher 3 'some text to encipher'
-- to decipher: ~$ ./bin/caesar --decipher 3 'some text to decipher'

with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Strings.Bounded;

procedure Caesar
is
   -- to be brief and safe we rename instead of use
   package IO renames Ada.Text_IO;
   package CL renames Ada.Command_Line;
   
   -- create a new type for bounded strings
   package BoundedString is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 4096);
   use BoundedString;
   
   -- create some variables to hold our argument parsing state
   TextArgument : BoundedString.Bounded_String;  -- stores the text argument
   TempArgument : BoundedString.Bounded_String;  -- stores a textual argument
   KeyArgument  : Integer;                       -- stores the key argument
   Mode         : Boolean;                       -- stores whether or not this is a cipher operation
   
   -- ShiftCharacter
   --     Takes a valid ASCII letter and either shifts it up in value by one
   --     or, in the case of 'z' and 'Z', assigns it the value of 'a' or 'A'
   --     or vice versa depending on the mode.
   --
   -- Parameter: (Character) InCharacter  - Valid ASCII character to shift
   -- Parameter: (Integer)   Key          - Places to shift
   -- Parameter: (Boolean)   Mode         - Whether or not this is a deciphering
   -- Return:    (Character)              - Valid ASCII character result
   function ShiftCharacter (InCharacter: Character; Key: Integer; Mode: Boolean) return Character
   is
      TempCharacter : Integer;
      KeyTextSum    : Integer;
      KeyTextSub    : Integer;
   begin
      TempCharacter := Character'Pos (InCharacter);
      KeyTextSum    := TempCharacter + Key;
      KeyTextSub    := KeyTextSum - (Key * 2);
      
      IO.Put_Line (Integer'Image (TempCharacter));
      
      if Mode = TRUE then
	 if KeyTextSum > 122 then
	    TempCharacter := ((KeyTextSum - 122) + 97);
	 elsif KeyTextSum > 90 and KeyTextSum < 97 then
	    TempCharacter := ((KeyTextSum - 90) + 65);
	 else
	    TempCharacter := KeyTextSum;
	 end if;
      elsif Mode = FALSE then
	 if KeyTextSub < 97 and KeyTextSub > 90 then
	    TempCharacter := (122 - (97 - KeyTextSub));
	 elsif KeyTextSub < 65 then
	    TempCharacter := (90 - (65 - KeyTextSub));
	 else
	    TempCharacter := KeyTextSub;
	 end if;
      end if;
      
      IO.Put_Line (Integer'Image (TempCharacter));
      
      return Character'Val (TempCharacter);
   end ShiftCharacter;
   
   -- IsValidCharacter
   --     Takes a character and returns whether or not it is considered to be
   --     a valid ASCII character that can be shifted.
   --
   -- Memo:
   --     If the character is invalid then it should be readded to the string
   --     as is, effectively ignoring it.
   --
   -- Parameter: (Character) InCharacter - Character to be evaluated
   -- Return:    (Boolean)               - Result of whether or not the input is valid
   function IsValidcharacter (InCharacter: Character) return Boolean
   is
      TempCharacter : Integer;
   begin
      TempCharacter := Character'Pos (InCharacter);
      
      if TempCharacter >= 65 and TempCharacter <= 90 then
	 return TRUE;
      elsif TempCharacter >= 97 and TempCharacter <= 122 then
	 return TRUE;
      end if;
      
      return FALSE;
   end IsValidCharacter;
   
   -- Cipher
   --     Takes a string and applies the cipher algorithm to it
   --
   -- Parameter (Bounded_String) Text    - Text to be ciphered
   -- Parameter (Integer)        Key     - Places to shift
   -- Parameter (Boolean)        Mode    - Whether to encipher or decipher
   -- Return:   (Bounded_String)         - Text that has been ciphered
   function Cipher (Text: BoundedString.Bounded_String; Key: Integer; Mode: Boolean)
		   return BoundedString.Bounded_String
   is
      I : Integer := 1;                        -- loop iterator
      OutText : BoundedString.Bounded_String;  -- output string
   begin
      for I in 1 .. Length (Text) loop
	 if IsValidCharacter (BoundedString.Element (Text, I)) = TRUE then
	    OutText := OutText & ShiftCharacter (BoundedString.Element (Text, I), Key, Mode);
	 else
	    OutText := OutText & BoundedString.Element (Text, I);
	 end if;
      end loop;
      
      return OutText;
   end Cipher;
begin
   
   -- check number of arguments (expects 3)
   if CL.Argument_Count /= 3 then
      IO.Put_Line ("Unexpected amount of arguments!");
      return;
   end if;
   
   -- grab the mode
   TempArgument := BoundedString.To_Bounded_String (CL.Argument (1));
   
   -- check if it is a mode flag
   -- memo: argument 1 must be a mode flag!
   if TempArgument = "--decipher" then
      Mode := FALSE;
   elsif TempArgument = "--encipher" then
      Mode := TRUE;
   else
      IO.Put_Line ("Could not find mode flag at position one.");
      return;
   end if;
   
   -- grab the text and the key
   TextArgument := BoundedString.To_Bounded_String (CL.Argument (3));
   TempArgument := BoundedString.To_Bounded_String (CL.Argument (2));
   
   KeyArgument := Integer'Value (BoundedString.To_String (TempArgument));
   
   IO.Put_Line (Integer'Image (KeyArgument));
   
   -- print our cipher-text
   IO.Put_Line (BoundedString.To_String (Cipher (TextArgument, KeyArgument, Mode)));
end Caesar;
