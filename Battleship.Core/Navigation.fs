namespace Battleship.Core

module Navigation =
    open Grid
    open Ship

    type Sector = Clear | Active of Name * int * bool | Torpedo

    type Rotation =
        | Clockwise
        | Counterclockwise

    let getDegrees (direction: Direction) : int =
        match direction with
        | South -> 0
        | West -> 90
        | North -> 180
        | East -> 270

    (* ------- À COMPLÉTER ------- *)
    (* --- Anciennes fonctions --- *)

    let getActiveInfos (grid: Sector Grid) : (Name * Coord * int * bool) list =
        Grid.chooseGrid (fun r c s ->
            match s with
            | Active (n, i, v) -> Some (n, (r, c), i , v) // ici à modifié
            | _ -> None
        ) grid

    let verifyConditions (grid : Sector Grid) (ship : Ship) (newCoords : Coord list) (otherList : Coord list) : bool = 
        let maxRow,maxCol = Grid.getDims grid
        let isInsideGrid =
            List.foldBack (fun coord acc -> isCoordInsideGrid coord (maxRow,maxCol) && acc) newCoords true
        let newCoords = newCoords @ otherList
        let activeCoords = List.map (fun (_, coord, _ , _) -> coord) (getActiveInfos grid) 
        let otherShipsCoords =
            List.filter (fun c -> not (List.contains c ship.Coords)) activeCoords 
        let isNotOnOthersShip = not (List.exists (fun c -> List.contains c otherShipsCoords) newCoords)
        isNotOnOthersShip && isInsideGrid 

    let canPlace (center: Coord) (direction: Direction) (name: Name) (grid: Sector Grid) : bool =
        let maxRow,maxCol = Grid.getDims grid
        let ship = createShip center direction name
        let Coord = getPerimeter ship (maxRow,maxCol) @ ship.Coords
        let activeCoords = List.map (fun (_, coord, _, _) -> coord) (getActiveInfos grid) 

        not (List.exists (fun c -> List.contains c activeCoords) Coord) 
        && List.foldBack (fun coord acc -> isCoordInsideGrid coord (maxRow,maxCol) && acc) ship.Coords true 

    let canMove (ship: Ship) (direction: Direction) (grid: Sector Grid) : bool =
        let maxRow,maxCol = Grid.getDims grid
        let newCoords =
            List.map (fun (x, y) ->
                let (dx, dy) = getOffset direction -1
                (x + dx, y + dy)
            ) ship.Coords
        let newParam = 
            List.map (fun (x, y) ->
                let (dx, dy) = getOffset direction -1
                (x + dx, y + dy)
            ) (getPerimeter ship (maxRow,maxCol))
        verifyConditions grid ship newCoords newParam

    let move (ship: Ship) (direction: Direction) : Ship =
        let newCoords =
            List.map (fun (x, y) ->
                let (dx, dy) = getOffset direction -1
                (x + dx, y + dy)
            ) ship.Coords

        let newCenter =     
            let (dx, dy) = getOffset direction -1
            let (x, y) = ship.Center
            (x + dx, y + dy)

        { ship with Coords = newCoords; Center = newCenter }

    let canRotate (ship: Ship) (direction: Direction) (grid: Sector Grid) : bool =

        let length = getShipLength ship.Name
        let newCoords = generateCoords ship.Center direction length
        let newParam = getPerimeterPoints newCoords 
        verifyConditions grid ship newCoords newParam

    let rotate (ship: Ship) (direction: Direction) : Ship =

        let length = getShipLength ship.Name
        let newCoords = generateCoords ship.Center direction length

        { Coords = newCoords; Center = ship.Center; Facing = direction; Name = ship.Name }

    let canMoveForward (ship: Ship) (grid: Sector Grid) : bool = 

        let newCoords =            
            List.map (fun (x, y) ->
                let (dx, dy) = getOffset ship.Facing -1
                (x + dx, y + dy)
            ) ship.Coords
        verifyConditions grid ship newCoords [] 

    let moveForward (ship: Ship) : Ship =

        let newCoords =            
            List.map (fun (x, y) ->
                let (dx, dy) = getOffset ship.Facing -1
                (x + dx, y + dy)
            ) ship.Coords
        let newCenter =     
            let (dx, dy) = getOffset ship.Facing -1
            let (x, y) = ship.Center
            (x + dx, y + dy)
        { Coords = newCoords; Center = newCenter; Facing = ship.Facing; Name = ship.Name }

    let getNextDirection (current: Direction) (rotation: Rotation) : Direction =
        match current with 
        | North -> if rotation = Clockwise then East else West
        | South -> if rotation = Clockwise then West else East
        | East -> if rotation = Clockwise then South else North
        | West -> if rotation = Clockwise then North else South

    let canRotateForward (ship: Ship) (rotation: Rotation) (grid: Sector Grid) : bool =
        let newDirection = getNextDirection ship.Facing rotation
        let length = getShipLength ship.Name
        let rotatedCoords = generateCoords ship.Center newDirection length
        let newCoords =            
            List.map (fun (x, y) ->
                let (dx, dy) = getOffset newDirection -1
                (x + dx, y + dy)
            ) rotatedCoords
        verifyConditions grid ship newCoords []

    let rotateForward (ship: Ship) (rotation: Rotation) : Ship =
        let newDirection = getNextDirection ship.Facing rotation
        let length = getShipLength ship.Name
        let rotatedCoords = generateCoords ship.Center newDirection length
        let newCoords =            
            List.map (fun (x, y) ->
                let (dx, dy) = getOffset newDirection -1
                (x + dx, y + dy)
            ) rotatedCoords
        let NewCenter =     
            let (dx, dy) = getOffset newDirection -1
            let (x, y) = ship.Center
            (x + dx, y + dy)
        { Coords = newCoords; Center = NewCenter; Facing = newDirection; Name = ship.Name }

    (* ------- À COMPLÉTER ------- *)
    (* --- Nouvelles fonctions --- *)

    let canMoveDrone (drone: Coord) (direction: Direction) (grid: Sector Grid) : bool =
        let maxRow,maxCol = Grid.getDims grid
        let (dx, dy) = getOffset direction -1
        let (x, y) = drone
        isCoordInsideGrid (x + dx, y + dy) (maxRow,maxCol)

    let moveDrone (drone: Coord) (direction: Direction) : Coord =
        let (dx, dy) = getOffset direction -1
        let (x, y) = drone
        (x + dx, y + dy)