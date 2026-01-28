namespace Battleship.Core

module Ship =
    open Grid

    type Name =
        | Spy
        | PatrolBoat
        | Destroyer
        | Submarine
        | Cruiser
        | AircraftCarrier

    type Direction =
        | North
        | South
        | East
        | West

    type Ship = {Coords: Coord list; Center: Coord; Facing: Direction; Name: Name}

    let getShipLength = function
        | Spy | PatrolBoat -> 2
        | Destroyer | Submarine -> 3
        | Cruiser -> 4
        | AircraftCarrier -> 5

    let getOffset direction i =
        match direction with
        | North -> (i, 0)  
        | South -> (-i, 0) 
        | East  -> (0, -i) 
        | West  -> (0, i)  
               
    let generateCoords center direction length =
        let offsetStart = -(length / 2) + if length % 2 = 0 then 1 else 0
        List.init length (fun i ->
            let offsetIndex = offsetStart + i
            let (dx,dy) = getOffset direction offsetIndex
            let (x,y) = center
            (x + dx, y + dy)
        )

    let getPerimeterPoints (points: Coord list) : Coord list =
        let deltas =
            let all = List.collect
                        (fun dx -> List.map (fun dy -> (dx, dy)) [-1; 0; 1])
                        [-1; 0; 1]
            List.filter (fun (dx, dy) -> dx <> 0 || dy <> 0) all

        let isInside point = List.contains point points
        let isPerimeterPoint point = not (isInside point)

        let perimeterCandidates =
            List.collect
                (fun (x, y) ->
                    List.map (fun (dx, dy) -> (x + dx, y + dy)) deltas
                )
                points

        let perimeterUnfiltered = List.filter isPerimeterPoint perimeterCandidates
        List.distinct perimeterUnfiltered

    let isCoordInsideGrid (i, j) (maxRow, maxCol) =
        i >= 0 && i < maxRow && j >= 0 && j < maxCol

    (* ------- À COMPLÉTER ------- *)
    (* --- Nouvelles fonctions --- *)

    let createShip (center: Coord) (facing: Direction) (name: Name) : Ship =

        let length = getShipLength name
        let coords = generateCoords center facing length
        { Coords = coords; Center = center; Facing = facing; Name = name }


    let getPerimeter (ship: Ship) (dims: Dims) : Coord list =

        let coords = getPerimeterPoints(ship.Coords)
        List.filter (fun c -> isCoordInsideGrid c dims ) coords   
