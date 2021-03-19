with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with StackPkg;
procedure Befunge is

   --An unconstrained 2 dimensional that holds characters representing Befunge insturctions
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

   --Grid dimensions that will be obtained from input file.
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

      --Looks up and performs the appropriate action given the instruction character
      procedure PerformInstruction(c : Character) is
         asciiIntegerOffset : constant Integer := 48;
      begin
         case c is
            when '>' => instrPointer.dir := RIGHT;
            when '<' => instrPointer.dir := LEFT;
            when '^' => instrPointer.dir := UP;
            when 'v' => instrPointer.dir := DOWN;

            --is this the best way to terminate?
            when '@' => running := False;
               Put("hit the @ symbol");
            when '0'..'9' => Push(Character'Pos(c) - asciiIntegerOffset, s);

            when '.' => Put(Top(s), Width => 0);
               Put(" ");
               Pop(s);
            when '$' => Pop(s);

            --eventaully 'others' will mean an invalid input was caught, throw a custom exception (or a data error?)
            when others => null;
         end case;
      end PerformInstruction;

      --For Debugging: Prints current grid coordinates, and the char at that location if showCharAtPos is TRUE
      procedure PrintCurrentLocation(showCharAtPos : Boolean) is
      begin
         Put(instrPointer.pos.x); Put(instrPointer.pos.y);
         if showCharAtPos then
            Put_Line("");
            Put(instructionGrid(instrPointer.pos.x, instrPointer.pos.y));
            Put_Line("");
                end if;
      end PrintCurrentLocation;

      --For Debugging: Prints the current state of the instruction grid
      --TODO: relocate the performInstuction calls to a separate procedure for traversing the grid
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
         PerformInstruction(instructionGrid(instrPointer.pos.y, instrPointer.pos.x));
         ChangeGridPosition (instrPointer);
      end loop;

      Put("Exit");

   end;
end Befunge;
