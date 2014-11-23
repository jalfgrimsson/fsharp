namespace Game

open System
open System.Threading
open Game.Domain
open Game.Brains
open Game.Rendering

module Program =

    [<EntryPoint>]
    let main argv = 
        
        let rng = Random ()

        let size = { Width = 40; Height = 40; }
        
        let board = Array2D.init size.Height size.Width (fun top left ->
            let dice = rng.NextDouble ()
            if dice < 0.25 then Some(Trap)
            elif dice < 0.50 then Some(Treasure)
            else None)
                
        let startPos = { Top = size.Height / 2; Left = size.Width / 2 }
        let critter = { Position = startPos; Direction = North; }

        let startState = { Board = board; Creature = critter; Score = 0; }

        initializeDisplay size startState

        let move = applyDecision size

        let rec loop (state:GameState) =

            let decision = Brains.randomDecide ()

            let creature = state.Creature |> move decision
            let board = updateBoard state.Board creature
            let gain = Domain.computeGain state.Board creature
            let score = state.Score + gain

            let updatedState = { Board = board; Creature = creature; Score = score }
            updateDisplay state updatedState

            Thread.Sleep 50

            loop updatedState

        loop startState

        0 // return an integer exit code