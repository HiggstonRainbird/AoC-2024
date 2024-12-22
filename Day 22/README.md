## Real World Application?

Believe it or not, there is a TAS (tool-assisted-speedrun) very similar to today's problem, for the [GameBoy Advance](https://en.wikipedia.org/wiki/Game_Boy_Advance) game [Dr. Sudoku](https://retroachievements.org/game/7767).  There are 1000 sudoku puzzles in *Dr. Sudoku*, which can of course be solved in a fraction of a second.  But the GameBoy Advance only has a D-pad for navigation, only one button can be pressed per frame (including the 'A' button needed to input a digit), and 81 squares is a lot more than the mere 11 on today's numeric keypad; finding the most efficient way to enter each solution is (to quote the TAS creators) equivalent to "solving 1000 NP-hard travelling salesman problems".

I highly recommend [the writeup](https://tasvideos.org/8015S), in which the two authors ([g0goTBC](https://tasvideos.org/Users/Profile/g0goTBC) and [Lightmopp](https://tasvideos.org/Users/Profile/Lightmopp)) go into detail on the *k*-opt algorithm they implemented, and the multiple weeks' worth of optimizations and computations it took to get the TAS to its current state:

> However, we can also safely conclude that if we didn’t reach the theoretical minimum, we cannot be more than a handful of frames away. Considering that we gained almost a thousand frames over our original strategies, and that we successfully created a 2h+ long TAS, we can be proud of how close we are to the ultimate goal. Saving additional frames would need either brand new algorithms, or a significant bump in the computing power that’s available to us.

> Are a few frames that might not exist worth it for a 2h17 TAS? Are there people around here that didn’t watch this TAS but would actually watch it if only it was a fraction of a second shorter? Am I losing sleep over it?

> Yes to all three, obviously, but that’s a story for another day.