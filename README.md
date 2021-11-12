# haskell-flappy-bird

#
## _CSE230 Project Proposal_

Caiwei Xiao, Zheng Ding, Linxiao Zhang, Weiqing Cao

**Instructor**

Ranjit Jhala

The brick library is a Haskell terminal user interface (TUI) programming toolkit. It could be used to write the pure function that describes how a user interface should look based on the current application state with a state transformation function to handle events. Using brick, we proposed to write a command-line flappy bird game. The game is a side-scroller where the player controls a bird, attempting to fly between columns of green pipes without hitting them. Each pipe you pass earns you a score. If you can find a good balance you can fly your bird through the pipes and earn a high score. The game can be controlled using a keyboard and/or mouse. This game is difficult and addicting.

We plan to have our app looks like a grid with swipe vertical lines symbolizing columns of green pipes. On the middle left of the grid, we have a dash representing the bird. When users click the mouse or press &#39;space&#39;, the dash would move up x% of the grid. If the users stop keyboard input, the dash would fall back y% of the grid each second. Touching the bottom of the grid or vertical lines would be marked as failing the game. To make our game more fun and complex, we plan to add more elements to the game. This may include additional tokens for bonus and/or additional moving enemies or pipes for loss of points. This might also include adding more complexity to make the game harder as time goes by, such as involving more pipes, tokens, variation of speed, etc, depending on how complex we want the game to be. We may also plan to add a scoreboard to record the user&#39;s score for each game.

Overall, we believe this project would be a fun game for users to play, as well as a good exercise for us to practice implementation skills using Haskell and functional programming.

Reference

[https://en.wikipedia.org/wiki/Flappy\_Bird](https://en.wikipedia.org/wiki/Flappy_Bird)

[https://github.com/jtdaugherty/brick/](https://github.com/jtdaugherty/brick/)
