with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with StackPkg;
procedure Befunge is

   --An unconstrained 2 dimensional that holds characters representing Befunge insturctions
   type Grid is array(Natural range <>, Natural range<>) of Character;

   type Pair is record
      xPos : Integer;
      yPos : Integer;
   end record;

   --Consumes the first two values from the input file, the dimensions of the grid, and stores them.
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
   --TODO: should s remain uninitialized? How to even initialize?
   s: Stack;

   --Grid dimensions that will be obtained from input file.
   rows : Integer := 0;
   columns : Integer := 0;

begin

   --Get the values for row and column
   GetRowsAndColumns(rows, columns);

   declare
      --Instantiate the grid given the row and column inputs
      instructionGrid : Grid(1..rows, 1..columns);
      --Current position of the instruction pointer, defaulting to top left
      currentPosition : Pair := (1,1);



      procedure PerformInstrcution(c : Character) is
      begin
         case c is
            --Integers to push to the stack
            when '0' => Push(0, s);
            when '1' => Push(1, s);
            when '2' => Push(2, s);
            when '3' => Push(3, s);
            when '4' => Push(4, s);
            when '5' => Push(5, s);
            when '6' => Push(6, s);
            when '7' => Push(7, s);
            when '8' => Push(8, s);
            when '9' => Push(9, s);

            when '.' => Put(Top(s), Width => 0);
                        Pop(s);
            when '@' => Put("How to terminate program??");


            when others => null;
         end case;
      end PerformInstrcution;

      --For Debugging: Prints current grid coordinates, and the char at that location if showCharAtPos is TRUE
      procedure PrintCurrentLocation(showCharAtPos : Boolean) is
      begin
         Put(currentPosition.xPos); Put(currentPosition.yPos);
         if showCharAtPos then
            Put_Line("");
            Put(instructionGrid(currentPosition.xPos, currentPosition.yPos));
            Put_Line("");
                end if;
      end PrintCurrentLocation;

      --For Debugging: Prints the current state of the instruction grid
      --TODO: relocate the performInstuction calls to a separate procedure
      procedure PrintGridState is
      begin
         for I in 1..rows loop
            for J in 1..columns loop
               --Put(instructionGrid(I,J));
               PerformInstrcution(instructionGrid(I,J));
            end loop;
            --Put_Line("");
         end loop;
      end PrintGridState;



   begin
      PopulateGrid(instructionGrid, rows, columns);
      PrintGridState;



   end;
end Befunge;
