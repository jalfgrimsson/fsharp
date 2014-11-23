namespace Game

open System
open Game.Domain

module Brains =

    type State = Dir * (Cell option) list

    type Experience = {
        State: State;
        Action: Act;
        Reward: float;
        NextState: State; }

    type Strategy = { State:State; Action:Act; }

    type Brain = Map<Strategy,float>

    (* What the creature "sees" *)

    let visibleBy (size:Size) (creature:Creature) (board:Board) =
        creature.Direction,
        [   for t in -1 .. 1 do
                for l in -1 .. 1 ->
                    board.[(creature.Position.Top + t) %%% size.Height, (creature.Position.Left + l) %%% size.Width]
        ]
            
    let options = [| Straight; Left; Right; |]
    let rng = Random ()
    let randomDecide () = options.[rng.Next(options.Length)]
    
    let alpha = 0.2 // learning rate
    
    let learn (brain:Brain) (exp:Experience) =
        let strat = { State = exp.State; Action = exp.Action }
        match brain.TryFind strat with
        | Some(value) -> 
            brain.Add (strat, (1.-alpha) * value + alpha * exp.Reward)
        | None -> brain.Add (strat, (alpha * exp.Reward))

    let decide (brain:Brain) (state:State) =
        let eval =
            options
            |> Array.map (fun alt -> { State = state; Action = alt })
            |> Array.filter (fun strat -> brain.ContainsKey strat)
        match eval.Length with
        | 0 -> randomDecide ()
        | _ -> 
            options
            |> Seq.maxBy (fun alt -> 
                let strat = { State = state; Action = alt }
                match brain.TryFind strat with
                | Some(value) -> value
                | None -> 0.)