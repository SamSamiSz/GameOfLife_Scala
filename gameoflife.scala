/* INSTRUCTIONS
 *
 * Complete the exercises below.  For each "EXERCISE" comment, add code
 * immediately below the comment.
 *
 * Please see README.md for instructions, including compilation and testing.
 *
 * GRADING
 *
 * 1. Submissions MUST compile using SBT with UNCHANGED configuration and tests
 *    with no compilation errors.  Submissions with compilation errors will
 *    receive 0 points.  Note that refactoring the code will cause the tests to
 *    fail.
 *
 * 2. You MUST NOT edit the SBT configuration and tests.  Altering it in your
 *    submission will result in 0 points for this assignment.
 *
 * 3. You may declare auxiliary functions if you like.
 *
 * SUBMISSION
 *
 * 1. Submit this file on D2L before the deadline.
 *
 * 2. Late submissions will not be permitted because solutions will be discussed
 *    in class.
 */

import contracts.*

object GameOfLife:
  // EXERCISE 1: complete the following definition that returns
  // a Blinker shape.
  //
  // You can look up a Java implementation of this function in  
  //    src/main/java/gameoflife.java#createBlinker
  //
  // Replace ??? with your implementation
  def createBlinker: Shape = {
    new Shape(Array(
      Array(0, 1, 0),
      Array(0, 1, 0),
      Array(0, 1, 0)
    ))
  }
    
  // EXERCISE 2: complete the following definition that returns
  // a Glider shape.
  //
  // You can look up a Java implementation of this function in  
  //    src/main/java/gameoflife.java#createGlider
  //
  // Replace ??? with your implementation
  def createGlider: Shape = {
    new Shape(Array(
      Array(0, 0, 1),
      Array(1, 0, 1),
      Array(0, 1, 1)
    ))
  }  

  // EXERCISE 3: complete the following definition, so that print returns a
  // string that renders the two-dimensional array `matrix` with a character
  // space for a zero entry and '#' for a non-zero entry. Every "line" in 
  // the matrix should be its own line in the string.
  //
  // You can look up a Java implementation of this function in  
  //    src/main/java/gameoflife.java#print
  //
  // Replace ??? with your implementation
  //
  // For example, the array
  //   Array(
  //     Array(1, 0, 0), 
  //     Array(1, 0, 1))
  // should result in the string
  //   "#  \n# #"
  def print(matrix: Array[Array[Int]]): String = {
    matrix.map(row => 
      row.map(cell => 
        if cell != 0 then '#' else ' '  // Updated to use `if` without parentheses
      ).mkString
    ).mkString("\n")
  }
  
  // EXERCISE 4: add a contract that requires the array pattern to 
  // have at least 1 line and at least 1 column. Implement the
  // `numRows` function to return the number of rows in `pattern`,
  // and the `numCols` function to return the number of columns in
  // `pattern`.
  //
  // You can look up a Java implementation of this class in  
  //    src/main/java/gameoflife.java#Shape
  //
  // Replace ??? with your implementation
  class Shape(val pattern: Array[Array[Int]]):
    require(pattern.length > 0 && pattern(0).length > 0, "Pattern must have one row and one column.")

    def numRows: Int = pattern.length
    def numCols: Int = pattern(0).length

    def getPattern: Array[Array[Int]] = pattern
  end Shape
  
  // You can look up a Java implementation of this class in  
  //    src/main/java/gameoflife.java#Game
  class Game(val rows: Int, val cols: Int):
    // EXERCISE 5: instantiate an array with `rows` number of rows
    // and `cols` number of columns. The array should have all entries
    // set to 0.
    var board: Array[Array[Int]] = Array.fill(rows, cols)(0) ensuring {
      (result: Array[Array[Int]]) => result.length == rows && result.forall(_.length == cols)
    }

    // EXERCISE 6: implement a `require` contract that checks 
    // that shape `s` fully fits onto the board when added
    // at `row`/`col`.
    def add(s: Shape, row: Int, col: Int): Unit = {
      require(
        row >= 0 && col >= 0 &&
        row + s.numRows <= rows &&
        col + s.numCols <= cols,
        "Shape must fit fully onto game field"
      )

      // EXERCISE 7: implement the `add` function that sets the
      // values of the board starting at the upper left corner
      // indicated by `row` and `col` to the values of shape `s`.
      for {
        i <- 0 until s.numRows
        j <- 0 until s.numCols
      } board(row + i)(col + j) = s.getPattern(i)(j)
    }

    // EXERCISE 8: implement the `step` function by following the
    // Game of Life rules.
    //
    // You can look up a Java implementation of this function in  
    //    src/main/java/gameoflife.java#Game.step
    def step(): Unit = {
      val next = Array.ofDim[Int](rows, cols)  // Create a new board for the next state

      for (i <- 0 until rows; j <- 0 until cols) {
        val aliveNeighbors = 
          (for {
            ni <- -1 to 1
            nj <- -1 to 1
            if (ni != 0 || nj != 0)
            if (i + ni >= 0 && i + ni < rows && j + nj >= 0 && j + nj < cols)
          } yield board(i + ni)(j + nj)).sum

        next(i)(j) = board(i)(j) match {
          case 1 if aliveNeighbors < 2 => 0  // Cell dies due to loneliness
          case 1 if aliveNeighbors > 3 => 0  // Cell dies due to overpopulation
          case 0 if aliveNeighbors == 3 => 1  // Cell becomes alive
          case _ => board(i)(j)  // Cell remains the same
        }
      }

      board = next  // Update the board to the next state
    }
  end Game

  def main(args: Array[String]): Unit = {
    val blinker = createBlinker
    val glider = createGlider

    val game = new Game(20, 80)
    game.add(blinker, 5, 40)
    game.add(glider, 1, 1)

    println(print(game.board))
    while true do
      game.step()
      println(print(game.board))
      Thread.sleep(100)
    end while
  }
end GameOfLife
