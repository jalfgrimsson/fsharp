namespace Game

open System
open Game.Domain

module Brains =

(* 
TODO: implement random decision making 
The Creature can go in either direction, 
with equal probability.
*)

    let r = Random()

    let randomDecide () =
        let randomValue = r.Next(30)
        match randomValue with
            | rV when rV < 10 -> Straight
            | rV when rV < 20 -> Left
            | _ -> Right

        //if (randomValue < 10) then Straight
        //else if (randomValue < 20) then Left
        //else Right
