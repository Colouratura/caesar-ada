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
   package BString is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 4096);
   use BString;
   
   -- create some variables to hold our argument parsing state
   Text_Argument : BString.Bounded_String;  -- stores the text argument
   Temp_Argument : BString.Bounded_String;  -- stores a textual argument
   Key_Argument  : Integer;                 -- stores the key argument
   Mode          : Boolean;                 -- stores whether or not this is a cipher operation
   
   -- Shift_Character
   --     Takes a valid ASCII letter and either shifts it up in value by one
   --     or, in the case of 'z' and 'Z', assigns it the value of 'a' or 'A'
   --     or vice versa depending on the mode.
   --
   -- Parameter: (Character) In_Character  - Valid ASCII character to shift
   -- Parameter: (Integer)   Key           - Places to shift
   -- Parameter: (Boolean)   Mode          - Whether or not this is a deciphering
   -- Return:    (Character)               - Valid ASCII character result
   function Shift_Character (In_Character: Character; Key: Integer; Mode: Boolean)
			    return Character
   is
      Temp_Character  : Integer;
      Key_Text_Sum    : Integer;
      Key_Text_Sub    : Integer;
   begin
      Temp_Character  := Character'Pos (In_Character);
      Key_Text_Sum    := Temp_Character + Key;
      Key_Text_Sub    := Key_Text_Sum - (Key * 2);
      
      if Mode = TRUE then
	 if Key_Text_Sum > 122 then
	    Temp_Character := ((Key_Text_Sum - 122) + 97);
	 elsif Key_Text_Sum > 90 and Key_Text_Sum < 97 then
	    Temp_Character := ((Key_Text_Sum - 90) + 65);
	 else
	    Temp_Character := Key_Text_Sum;
	 end if;
      elsif Mode = FALSE then
	 if Key_Text_Sub < 97 and Key_Text_Sub > 90 then
	    Temp_Character := (122 - (97 - Key_Text_Sub));
	 elsif Key_Text_Sub < 65 then
	    Temp_Character := (90 - (65 - Key_Text_Sub));
	 else
	    Temp_Character := Key_Text_Sub;
	 end if;
      end if;
      
      return Character'Val (Temp_Character);
   end Shift_Character;
   
   -- Is_Valid_Character
   --     Takes a character and returns whether or not it is considered to be
   --     a valid ASCII character that can be shifted.
   --
   -- Memo:
   --     If the character is invalid then it should be readded to the string
   --     as is, effectively ignoring it.
   --
   -- Parameter: (Character) In_Character - Character to be evaluated
   -- Return:    (Boolean)                - Result of whether or not the input is valid
   function Is_Valid_Character (In_Character: Character)
			       return Boolean
   is
      Character_Code : Integer;
   begin
      Character_Code := Character'Pos (In_Character);
      
      if Character_Code >= 65 and Character_Code <= 90 then
	 return TRUE;
      elsif Character_Code >= 97 and Character_Code <= 122 then
	 return TRUE;
      end if;
      
      return FALSE;
   end Is_Valid_Character;
   
   -- Cipher
   --     Takes a string and applies the cipher algorithm to it
   --
   -- Parameter (BString) Text    - Text to be ciphered
   -- Parameter (Integer) Key     - Places to shift
   -- Parameter (Boolean) Mode    - Whether to encipher or decipher
   -- Return:   (BString)         - Text that has been ciphered
   function Cipher (Text: BString.Bounded_String; Key: Integer; Mode: Boolean)
		   return BString.Bounded_String
   is
      I       : Integer := 1;            -- loop iterator
      Out_Text : BString.Bounded_String;  -- output string
   begin
      for I in 1 .. Length (Text) loop
	 if Is_Valid_Character (BString.Element (Text, I)) = TRUE then
	    Out_Text := Out_Text & Shift_Character (BString.Element (Text, I), Key, Mode);
	 else
	    Out_Text := Out_Text & BString.Element (Text, I);
	 end if;
      end loop;
      
      return Out_Text;
   end Cipher;
begin
   
   -- check number of arguments (expects 3)
   if CL.Argument_Count /= 3 then
      IO.Put_Line ("Unexpected amount of arguments!");
      return;
   end if;
   
   -- grab the mode
   Temp_Argument := BString.To_Bounded_String (CL.Argument (1));
   
   -- check if it is a mode flag
   -- memo: argument 1 must be a mode flag!
   if Temp_Argument = "--decipher" then
      Mode := FALSE;
   elsif Temp_Argument = "--encipher" then
      Mode := TRUE;
   else
      IO.Put_Line ("Could not find mode flag at position one.");
      return;
   end if;
   
   -- grab the text and the key
   Text_Argument := BString.To_Bounded_String (CL.Argument (3));
   Temp_Argument := BString.To_Bounded_String (CL.Argument (2));
   
   Key_Argument := Integer'Value (BString.To_String (Temp_Argument));
   
   -- print our cipher-text
   IO.Put_Line (BString.To_String (Cipher (Text_Argument, Key_Argument, Mode)));
end Caesar;
