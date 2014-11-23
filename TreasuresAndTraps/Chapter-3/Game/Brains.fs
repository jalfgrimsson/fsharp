namespace Game

open System
open Game

module Brains =

    type State = int list

    type Experience = {
        State: State;
        Action: Act;
        Reward: float;
        NextState: State; }

    type Strategy = { State:State; Action:Act; }

    type Brain = Map<Strategy,float>

    let rng = Random ()
    let choices = [| Straight; Left; Right |]
    let randomDecide () = choices.[rng.Next(3)]

    let alpha = 0.2 // learning rate
    let epsilon = 0.05 // random learning

    let learn (brain:Brain) (exp:Experience) =
        let strat = { State = exp.State; Action = exp.Action }
        match brain.TryFind strat with
        | Some(value) -> 
            let value' = (1. - alpha) * value + alpha * exp.Reward
            brain.Add (strat, value')
        | None -> brain.Add (strat, alpha * exp.Reward)

    let decide (brain:Brain) (state:State) =
        let eval =
            choices
            |> Array.map (fun alt -> { State = state; Action = alt })
            |> Array.filter (fun strat -> brain.ContainsKey strat)
        match eval.Length with
        | 0 -> randomDecide ()
        | _ -> 
            choices
            |> Seq.maxBy (fun alt -> 
                let strat = { State = state; Action = alt }
                match brain.TryFind strat with
                | Some(value) -> value
                | None -> 0.)

(*
Determination of what the creature sees:
8 tiles surrounding it, rotated to be direction-insensitive.
i.e. tiles are re-aligned in the direction it is headed to.
*)

    let tileAt (board:Board) (pos:Pos) = board.[pos.Left,pos.Top]

    let offsets = 
        [   (-1,-1)
            (-1, 0)
            (-1, 1)
            ( 0,-1)
            ( 0, 1)
            ( 1,-1)
            ( 1, 0)
            ( 1, 1) ]

    let rotate dir (x,y) =
        match dir with
        | North -> (x,y)
        | South -> (-x,-y)
        | West -> (-y,x)
        | East -> (y,-x)

    let visibleState (size:Size) (board:Board) (hero:Hero) =        
        let (dir,pos) = hero.Direction, hero.Position
        offsets 
        |> List.map (rotate dir)
        |> List.map (fun (x,y) -> 
            onboard size { Top = pos.Top + x; Left = pos.Left + y }
            |> tileAt board)
