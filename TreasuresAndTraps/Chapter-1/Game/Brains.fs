namespace Game

open System
open Game.Domain

module Brains =

(* random decision making *)

    let options = [| Straight; Left; Right; |]
    let rng = Random ()
    let randomDecide () = options.[rng.Next(options.Length)]

(* modelling state, strategy and brain *)

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
                
(*
TODO: implement learn, so that the creature
updates its brain based on the latest experience it had.
- create the Strategy corresponding to the State and
Decision from the latest Experience recorded,
- update the Strategy value as 
(1 - alpha) * V(strat) + alpha * reward from experience 
*)

    let alpha = 0.2 // learning rate

    let noupdateFunction value =
        value
    let updateFunction value reward =
        (1.0 - alpha) * value + alpha * reward  

    let updateIfMatch (strategy:Strategy) (value:float) (state:State) (reward:float) =
        match strategy.State with
            | state -> updateFunction value reward
            | _ -> value

    let updateIfStateMatches (strategy:Strategy,value:float) (state:State) (reward:float) =
        match strategy.State with
            | state -> (strategy, (updateFunction value reward))
            | _ -> (strategy, (noupdateFunction value))

    let learn (brain:Brain) (exp:Experience) : Brain =
        let strategiesForState = brain |> Map.toArray |> Array.filter(fun (strategy,value) -> strategy.State = exp.State)
        match strategiesForState |> Array.isEmpty with
            | true ->
                let strategy:Strategy = { State = exp.State; Action = exp.Action }
                brain.Add (strategy, alpha * exp.Reward)
            | false ->
                //brain |> (Map.map (fun strategy value -> updateIfMatch strategy value exp.State exp.Reward))
                let strategy:Strategy = { State = exp.State; Action = exp.Action }
                brain.Add (strategy, updateFunction 1.0 exp.Reward)
        //        brain |> Map.toArray |> Array.map(fun (strategy,value) -> updateIfStateMatches (strategy,value) exp.State exp.Reward)
        //strategiesForState |> Array.map (fun (strategy,value) -> (strategy,(updateFunction value exp.Reward))

(*
TODO: implement decide, so that the creature 
uses its brain to make a decision, based on its
current state.
- if the state has never been seen before, 
make a random move,
- otherwise, among the known strategies that 
correspond to the current state, pick the strategy 
that has the highest value.
*)

    type Decision = { 
        WasRandom:bool;
        Action : Act;
    }
    
    let decide (brain:Brain) (state:State) =
        let eval =
            options
            |> Array.map (fun alt -> { State = state; Action = alt })
            |> Array.filter (fun strat -> brain.ContainsKey strat)
        match eval.Length with
        | 0 -> 
            { WasRandom = true; Action = randomDecide() }
        | _ -> 
            options
            |> Seq.maxBy (fun alt -> 
                let strat = { State = state; Action = alt }
                match brain.TryFind strat with
                | Some(value) -> value
                | None -> 0.)
            |> fun act -> { WasRandom = false; Action = act }

    // The above was the reference implementation, below is mine
    // Why do these work differently?
    // Function above iterates all action options (left, straight, right) with given state and tries to find strategies containing them in the brain
    // If not found (no previous experience), value of 0 is assigned
    // Then the best is chosen
    // Function below extracts all previous experiences from the brain totally ignoring (State, Action) combinations not found
    // And chooses the best from them
    // So if given a brain with only one negative experience (i.e. running into a trap) with value -20
    // function above will choose either of the other combinations with value 0
    // while function below will choose the only experience - in this case, the negative one, repeating the bad behaviour.

    let decide2 (brain:Brain) (state:State) =
        let strategiesForState = brain |> Map.toArray |> Array.filter(fun (strategy,value) -> strategy.State = state) 

        match strategiesForState |> Array.isEmpty with
            | true -> 
                { WasRandom = true; Action = randomDecide() }
            | false ->
                strategiesForState |> Array.maxBy(fun (strategy,value) -> value) |> fun (strategy,value) -> { WasRandom = false; Action = strategy.Action }
    
    // possible improvement - return randomDecide() if value < 0
    // it is still worse though, because it might choose the negative valued decision, thus negating the value of previous experiences
        
    let decide3 (brain:Brain) (state:State) =
        let strategiesForState = brain |> Map.toArray |> Array.filter(fun (strategy,value) -> strategy.State = state) 

        match strategiesForState |> Array.isEmpty with
            | true -> 
                { WasRandom = true; Action = randomDecide() }
            | false ->
                strategiesForState 
                    |> Array.maxBy(fun (strategy,value) -> value) 
                    |> fun (strategy,value) ->
                           if value > 0.0 then { WasRandom = false; Action = strategy.Action }
                           else { WasRandom = true; Action = randomDecide() }
