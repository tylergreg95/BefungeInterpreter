with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with StackPkg;
procedure Befunge is

   --An unconstrained 2 dimensional array that holds characters representing Befunge insturctions
   type Grid is array(Natural range <>, Natural range<>) of Character;

   --Record that contains an x and y value
   type Pair is record
      x : Integer;
      y : Integer;
   end record;

   --Enum to name the four cardnial directions we can travel within the grid
   type Direction is (UP, DOWN, LEFT, RIGHT);

   --A pointer that will keep track of our current position in the grid,
   --as well as the current direction it is moving
   type InstructionPointer is record
      dir : Direction := Right;
      pos : Pair := (1,1);
   end record;

   --Consumes the first two values from the input file and stores them.
   procedure GetRowsAndColumns(r, c : in out Integer) is
   begin
      Get(r);
      Get(c);
   end GetRowsAndColumns;

   --Read through based on the row and column values to populate the grid with instructions
   procedure PopulateGrid(arr : in out Grid; r, c : in Integer) is
   begin
      for I in 1..r loop
         for J in 1..c loop
            Get(arr(I,J));
         end loop;
      end loop;
   end;

   --Implement the StackPkg, using a size defined in the project specification.
   package myStackPkg is new StackPkg(Size => 1000, ItemType => Integer);
   use myStackPkg;


   --[Program fields]
   --TODO: should this be declared here? Better suited when the grid is instantiated?
   s: Stack;

   --Grid dimensions that will be obtained from input file
   rows : Integer := 0;
   columns : Integer := 0;

   --flag to stop program execution 
   running : Boolean := True;

begin

   --Get the values for row and column
   GetRowsAndColumns(rows, columns);

   declare
      --Instantiate the grid given the row and column inputs
      instructionGrid : Grid(1..rows, 1..columns);
      instrPointer : InstructionPointer;
      isOutOfBounds : Boolean := False;
      procedure ChangeGridPosition(insPntr : in out InstructionPointer) is
      begin
         case insPntr.dir is
            when RIGHT => insPntr.pos.x := insPntr.pos.x + 1;
            when UP => insPntr.pos.y := insPntr.pos.y - 1;
            when DOWN => insPntr.pos.y := insPntr.pos.y + 1;
            when LEFT => insPntr.pos.x := insPntr.pos.x - 1;
            when others => null;
         end case;

      end ChangeGridPosition;

      procedure SwapTopTwoStackElements is
         num1 : Integer;
         num2 : Integer;
      begin
         num1 := Top(s);
         Pop(s);
         num2 := Top(s);
         Pop(s);
         Push(num1, s);
         Push(num2, s);
      end SwapTopTwoStackElements;

      --Looks up and performs the appropriate action given the instruction character
      procedure PerformInstruction(c : Character) is
         --Constant to convert char representations of integers to their integer value
         asciiIntegerOffset : constant Integer := 48;
      begin
         case c is
            --Change the direction of the pointer
            when '>' => instrPointer.dir := RIGHT;
            when '<' => instrPointer.dir := LEFT;
            when '^' => instrPointer.dir := UP;
            when 'v' => instrPointer.dir := DOWN;

            --Terminate the program
            --is this the best way? As about OS library for exiting
            when '@' => running := False;
               --DEBUG: 
               --Put("hit the @ symbol");
               
            --Push the integer value the char represents to the stack
            when '0'..'9' => Push(Character'Pos(c) - asciiIntegerOffset, s);

            --Pop top value from stack, display it to the console
            when '.' => Put(Top(s), Width => 0);
               Put(" ");
               Pop(s);
            --Pop value from top of stack, and discard it
            when '$' => Pop(s);

            --Duplicate the top value of the stack
            when ':' => Push(Top(s), s);

            when '\' => SwapTopTwoStackElements;
            when '_' => null;
            when '|' => null;


            --Do nothing
            when ' ' => null;
            --An invalid instruction was passed
            when others => running := False;
               Put("Error: Invalid Instruction");
               Put_Line("");
         end case;
      end PerformInstruction;

      procedure CheckBounds(status : in out Boolean; xPos, yPos: in Integer) is
      begin
         if xPos > columns or xPos < 1 or yPos > rows or yPos < 1 then
            status := True;
         end if; 
      end CheckBounds;

      --DEBUG: Prints current grid coordinates, and the char at that location if showCharAtPos is TRUE
      procedure PrintCurrentLocation(showCharAtPos : Boolean) is
      begin
         Put(instrPointer.pos.x); Put(instrPointer.pos.y);
         if showCharAtPos then
            Put_Line("");
            Put(instructionGrid(instrPointer.pos.x, instrPointer.pos.y));
            Put_Line("");
                end if;
      end PrintCurrentLocation;

      --DEBUG: Prints the current state of the instruction grid
      procedure PrintGridState is
      begin
         for I in 1..rows loop
            for J in 1..columns loop
               Put(instructionGrid(I,J));
            end loop;
            Put_Line("");
         end loop;
      end PrintGridState;

   begin
      PopulateGrid(instructionGrid, rows, columns);
      
      while running loop
         --Every iteration, check if the pointer is out of bounds
         CheckBounds(isOutOfBounds, instrPointer.pos.x, instrPointer.pos.y);
         --If the pointer is out of bounds, display an error message and terminate the program
         if isOutOfBounds then
            running := False;
            Put("Error: Out of bounds");
            Put_Line("");
         --If the pointer is in bounds, perform the instruction, then move to the next position.0
         else
            PerformInstruction(instructionGrid(instrPointer.pos.y, instrPointer.pos.x));
            ChangeGridPosition (instrPointer);
         end if;
         
      end loop;

      --DEBUG: Properly exited the while loop
      Put("Exit");

   end;
end Befunge;
