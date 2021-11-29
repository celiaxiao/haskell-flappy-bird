# haskell-flappy-bird

## _CSE230 Project Proposal_

Caiwei Xiao, Zheng Ding, Linxiao Zhang, Weiqing Cao

**Instructor**

Ranjit Jhala

The brick library is a Haskell terminal user interface (TUI) programming toolkit. It could be used to write the pure function that describes how a user interface should look based on the current application state with a state transformation function to handle events. Using brick, we proposed to write a command-line flappy bird game. The game is a side-scroller where the player controls a bird, attempting to fly between columns of green pipes without hitting them. Each pipe you pass earns you a score. If you can find a good balance you can fly your bird through the pipes and earn a high score. The game can be controlled using a keyboard and/or mouse. This game is difficult and addicting.

We plan to have our app looks like a grid with swipe vertical lines symbolizing columns of green pipes. On the middle left of the grid, we have a dash representing the bird. When users click the mouse or press &#39;space&#39;, the dash would move up 10% of the grid. If the users stop keyboard input, the dash would fall back 10% of the grid each second. Touching the bottom of the grid or vertical lines would be marked as failing the game. To make our game more fun and complex, we plan to add more elements to the game. This may include additional tokens for bonus and/or additional moving enemies or pipes for loss of points. This might also include adding more complexity to make the game harder as time goes by, such as involving more pipes, tokens, variation of speed, etc, depending on how complex we want the game to be. We may also plan to add a scoreboard to record the user&#39;s score for each game.

Overall, we believe this project would be a fun game for users to play, as well as a good exercise for us to practice implementation skills using Haskell and functional programming.

Reference

[https://en.wikipedia.org/wiki/Flappy\_Bird](https://en.wikipedia.org/wiki/Flappy_Bird)

[https://github.com/jtdaugherty/brick/](https://github.com/jtdaugherty/brick/)


# CSE 230 Project Update 
We should expect to meet our goals until the deadline. The architecture and challenge is described below.

## Architecture

There are so far 3 game states of Flappy Bird: **before game starts**, **during game play**, and **after game ends**. **Before game starts**, the player is presented with a general introduction of the game flappy bird, this includes the title Flappy Bird, keyboard usage to start the game, and whether or not to enter multi-player mode, where the player is able to connect with a second player and the two players play the game at the same time. The multi-player mode will have two options: start server and join server. **During game play**, the actual game is displayed, with pillar moving in time and bird that user is able to control using keyboard or mouse. During game play, a player score is tracked, which reflects the number of pillars the bird has passed through and possibly bonus tokens the bird has eaten. If the bird hits the pillar, then the **game ends** and the corresponing view is presented, that includes the score of the current game play, as well as a leaderboard of top 5 highest scores the player has made so far. The display of different game states is defined in `View.hs`. 

During game play, there are several information we are keeping track of, so that we can use the information to determine the next state of the game in time as well as for game display. The information is defined in `Model.hs`. The information includes:
* **gameState** to reflect before game starts, during gameplay and after game ends.
* **bird** that contains `y` for location and `v_y` for acceleration due to gravity, `score` that the current player has earned so far.
* **pillar** which includes the `x` and `y` coordinates of pillars, and `v` to reflect the speed if the pillar is moving.
* **v_Game** for speed of the moving canvas including pillars and bonus.
* **network** that keeps track of `player2` containing the other player's `bird` information, as well as `port` for network connection and `is_server` to determine if it is server or client.
* **bonus** to store the `x` and `y` location of the bonus token that the bird is able to eat.

For the game play logic, we need to define the next game state given the current game state and user input for controlling the bird via keyboard or mouse. State changes include new location of bird, pillars, tokens, new score, whether the game has ended, etc. We also need to implement the logics during two play game play with network. These logics will be defined in `Control.hs`.

## Challenges
There are several challenges we have encountered. This includes not being familiar with the Brick library, which we tried to solve it by studying existing applications as examples and editing to see the output to study Brick. We were also concerned with pixels being too large with respect to the canvas size, which makes the game play experience not as enjoyable. We found it hard to change the pixel size, but instead we are able to change the game display size to make overall playable region large. We also experienced challenge in implementation logic when defining the bird behaviour, specifically, when user specify an up operation, the bird should not continuously go up, but instead go up for a bit and then immediately go down. We solved the problem by only adjusting the coordinates of the bird without changing the direction. <TODO>

## Project Goals

Currently we believe we can achieve the goals set by the original proposal before the deadline.
