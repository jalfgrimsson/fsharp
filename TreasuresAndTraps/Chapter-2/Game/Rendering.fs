namespace Game

open System
open Game.Domain

module Rendering =

    let offset (pos:Pos) = (pos.Left, pos.Top + 2)
    
    let initializeDisplay (size:Size) (state:GameState) =
        Console.SetWindowSize(size.Width, size.Height+2)
        let board = state.Board
        for x in 0 .. (size.Height - 1) do
            for y in 0 .. (size.Width - 1) do
                let pos = { Top = x; Left = y }
                Console.SetCursorPosition (offset (pos))
                let tileType = board.[x,y]
                match tileType with
                | None -> 
                    Console.ForegroundColor <- ConsoleColor.Black
                    Console.Write(" ")
                | Some(item) ->
                    match item with
                    | Treasure ->
                        Console.ForegroundColor <- ConsoleColor.Green
                        Console.Write("$")
                    | Trap -> 
                        Console.ForegroundColor <- ConsoleColor.Red
                        Console.Write("#")
                         
    let renderScore score = 
        Console.ForegroundColor <- ConsoleColor.White
        Console.SetCursorPosition (0,0)
        Console.Write (sprintf "Score: %i   " score)

    let updateDisplay (before:GameState) (after:GameState) =

        renderScore after.Score

        let oldPos = before.Creature.Position
        let newPos = after.Creature.Position
        // previous player position
        Console.SetCursorPosition (offset (oldPos))
        Console.ForegroundColor <- ConsoleColor.Black
        Console.Write("█")
        // current player position
        Console.SetCursorPosition (offset (newPos))
        Console.ForegroundColor <- ConsoleColor.Yellow
        Console.Write("█")