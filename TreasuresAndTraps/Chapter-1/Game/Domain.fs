namespace Game

module Domain =

    type Pos = { Top:int; Left:int }

    type Dir = 
        | North
        | West
        | South
        | East

    type Act =
        | Left
        | Right
        | Straight

    type Creature = { Position:Pos; Direction:Dir; }

    type Cell =
        | Treasure
        | Trap    

    type Size = { Width:int; Height:int }
            
    type Board = Cell option [,]

    type GameState = { Board:Board; Creature:Creature; Score:int }

    let inline (%%%) (x:int) (y:int) =
        if x >= 0 then x % y
        else y + (x % y)

    let onboard (size:Size) (pos:Pos) =
        { Top = pos.Top %%% size.Height;
          Left = pos.Left %%% size.Width; }

    let moveTo (size:Size) (dir:Dir) (pos:Pos) =
        match dir with
        | North -> { pos with Top = (pos.Top - 1) }
        | South -> { pos with Top = (pos.Top + 1) }
        | West -> { pos with Left = (pos.Left - 1) }
        | East -> { pos with Left = (pos.Left + 1) }
        |> onboard size
            
    let goto (act:Act) (dir:Dir) =
        match act with
        | Straight -> dir
        | Left ->
            match dir with
            | North -> West
            | West -> South
            | South -> East
            | East -> North
        | Right ->
            match dir with
            | North -> East
            | East -> South
            | South -> West
            | West -> North

    let applyDecision (size:Size) (action:Act) (creature:Creature) =
        let direction = creature.Direction |> goto action
        { Position = creature.Position |> moveTo size direction; Direction = direction }

    let updateBoard (board:Board) (creature:Creature) =
        let pos = creature.Position
        board 
        |> Array2D.mapi (fun top lft x -> 
            if top = pos.Top && lft = pos.Left then None else x)

    let computeGain (board:Board) (creature:Creature) =
        let pos = creature.Position
        match (board.[pos.Top,pos.Left]) with
        | None -> 0
        | Some(item) ->
            match item with
            | Treasure -> 100
            | Trap -> -100