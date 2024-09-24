# Pipe-Dream

This project focuses on a pipe connection puzzle game where players strategically rotate and place pipes on a grid to ensure all are connected correctly. Each piece of pipe can have openings on any of its four sides, and the goal is to create a seamless path for "goo" to flow through the grid without leaks. The game is programmed in a functional programming style using Racket, a dialect of the Scheme programming language. Key components include:

- Pipe Definitions: Defines various types of pipe pieces, such as straight, corner, and T-shaped configurations.

- Game Logic: Includes functions to place pipes on the grid, check if the goo can flow from one pipe to another, and propagate this flow throughout the grid.

- Game State Management: Manages the current state of the game, including the grid setup, flow of goo, and upcoming pipes.

- User Interaction: Handles user inputs to place pipes and rotate them, aiming to extend the flow path or prevent leaks.

- Visual Rendering: Draws the game grid and pipes, updating the visual representation as the player interacts with the game.

Players interact with the game by placing pipes on a grid, aiming to extend the continuous path for goo flow and achieve the highest possible connectivity without any breaks.
