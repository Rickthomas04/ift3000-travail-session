namespace Battleship.Core

open Grid
open Stream
open Ship
open Navigation
open Battlefield


module Intelligence =

    
    let sign x = if x > 0 then 1 elif x < 0 then -1 else 0

    let getOptimalTorpedoDirections (spyRow,spyCol) (torpedoRow,torpidoCol) =
        let rowDirection,colDirection = sign(spyRow-torpedoRow), sign(spyCol-torpidoCol)
        if abs(spyRow-torpedoRow) >= abs(spyCol-torpidoCol) then
            [ rowDirection,0; 0,(if colDirection=0 then 1 else colDirection); 0,-(if colDirection=0 then 1 else colDirection); -rowDirection,0 ]
        else
            [ 0,colDirection; (if rowDirection=0 then 1 else rowDirection),0; -(if rowDirection=0 then 1 else rowDirection),0; 0,-colDirection ]

    let torpedoStep dims direction ((currentRow,currentCol) as coord, grid) =
        let deltaRow,deltaCol = direction
        let nextRow,nextCol = currentRow+deltaRow, currentCol+deltaCol
        if not (Grid.isCoordInsideGrid (nextRow,nextCol) dims) then coord, grid else
        match Grid.getSectorAt (nextRow,nextCol) grid with
        | Some (Active(Spy,idx,false)) ->
            let updatedGrid = grid |> Grid.insert (nextRow,nextCol) (Active(Spy,idx,true)) |> Grid.insert coord Clear
            (nextRow,nextCol), updatedGrid
        | Some Clear | None ->
            let updatedGrid = grid |> Grid.insert (nextRow,nextCol) Torpedo |> Grid.insert coord Clear
            (nextRow,nextCol), updatedGrid
        | _ -> coord, grid

    
    let extractFullCycle limit (stream:Coord Stream) (pivot:Coord) : Coord list =
        let rec loop remainingStream firstElementAfterPivot accumulatedCoords iterationCount =
            if iterationCount = limit then List.rev accumulatedCoords else
            match remainingStream with
            | Empty -> List.rev accumulatedCoords
            | Cons (currentCoordLazy, nextStreamLazy) ->
                let currentCoord  = currentCoordLazy.Force()
                let nextStream = nextStreamLazy.Force()
                match firstElementAfterPivot with
                | None when currentCoord <> pivot -> loop nextStream (Some currentCoord) (currentCoord :: accumulatedCoords) (iterationCount + 1)
                | Some firstElement when currentCoord = pivot ->
                    match nextStream with
                    | Cons (nextCoordLazy, _) when nextCoordLazy.Force () = firstElement -> List.rev accumulatedCoords
                    | _ -> loop nextStream firstElementAfterPivot (currentCoord :: accumulatedCoords) (iterationCount + 1)
                | _ -> loop nextStream firstElementAfterPivot (currentCoord :: accumulatedCoords) (iterationCount + 1)
        let streamAfterPivot = match stream with Cons (_, tl) -> tl.Force() | _ -> Empty
        loop streamAfterPivot None [] 0
   
    let manhattanDistance targetCoords (currentRow,currentCol) =
        targetCoords |> List.minBy (fun (targetRow,targetCo) -> abs(targetRow-currentRow)+abs(targetCo-currentCol))
                |> fun (targetRow,targetCo) -> abs(targetRow-currentRow)+abs(targetCo-currentCol)

    let getBoatNumber = function | Spy -> 0 | PatrolBoat -> 1 | Destroyer -> 2 | Submarine -> 3 | Cruiser -> 4 | AircraftCarrier -> 5
    let getBoatName = function | 0 -> Spy | 1 -> PatrolBoat | 2 -> Destroyer | 3 -> Submarine | 4 -> Cruiser | 5 -> AircraftCarrier

    let private repairFirstHit (ship:Name) grid =
        getActiveInfos grid
        |> List.tryFind (fun (n,_,_,hit) -> n = ship && hit)
        |> Option.map (fun (_,coord,idx,_) -> Grid.update coord (fun _ -> Active(ship,idx,false)) grid)
        |> Option.defaultValue grid

    let repairSpy (grid:Sector Grid) : Sector Grid = repairFirstHit Spy grid

    let repairOtherShip (grid: Sector Grid) : Sector Grid =
        
        let rec boatPriorityList (ships : Name List) : int List =
            match ships with
            | [] -> []
            | a::b -> List.rev(List.sort(getBoatNumber(a)::boatPriorityList(b)))
        let ships = (extractData grid).Ships
         
        let destroyedSectors = (Grid.chooseGrid (fun r c s ->
            match s with
            | Active (n, i, true) when getRemainingHealth n grid > 0 -> Some (n, (r, c), i) 
            | _ -> None
        ) grid)
        let rec nameList (l : (Name * (int * int) * int) List) = 
            match l with
            | [] -> []
            | (n, _, _)::r -> n::(nameList(r))
        if destroyedSectors.Length = 0 || (destroyedSectors.Length = 1 && (nameList destroyedSectors).Head = Spy) || (nameList destroyedSectors).Head = PatrolBoat then grid else
        let priorityBoatName = getBoatName((boatPriorityList (nameList destroyedSectors)).Head)
        let rec getShip (s : Ship List) : Ship = 
            match s with 
            | a::b -> if a.Name = priorityBoatName then a else getShip b
        let priorityShip = getShip (ships)
        let rec coordList l =
            match l with
            | [] -> []
            | (_, a, _)::b -> a::(coordList b)
        let ShipCoords = coordList(destroyedSectors)
        let rec compareCoords coord list = 
            match list with
            | [] -> false
            | (a,b)::c -> (a = fst(coord) && b = snd(coord)) || compareCoords coord c
        let rec getIndex (acc : int) (list : Coord List): int = 
            match list with
            | [] -> acc
            | a::_ when compareCoords a ShipCoords -> acc
            | _::b -> getIndex (acc + 1) b
        let index = getIndex 0 priorityShip.Coords
        let updateGrid (ship: Ship) (grid: Sector Grid) : Sector Grid =
            mapGrid (function
                | Active (n, i, true) when n = ship.Name && i = index -> Active (n, i, false)
                | s -> s
            ) grid
        updateGrid priorityShip grid

    let shootAtSpy (grid:Sector Grid) : Sector Grid =
        getActiveInfos grid
        |> List.filter (fun (name,_,_,hit) -> name = Spy && not hit)
        |> List.sortBy (fun (_,_,idex,_) -> idex)
        |> function
           | (_,coord,idx,_) :: _ -> Grid.update coord (fun _ -> Active(Spy,idx,true)) grid
           | [] -> grid

    let launchTorpedo (grid:Sector Grid) : Sector Grid =
        let data = extractData grid
        match data.Ships |> List.tryFind (fun s -> s.Name = Submarine),
              data.Ships |> List.tryFind (fun s -> s.Name = Spy) with
        | Some sub, Some spy ->
            let perim = getPerimeter sub data.Dims
            match perim |> List.tryFind (fun c ->
                    match Grid.getSectorAt c grid with
                    | Some (Active(_,_,false)) -> true | _ -> false) with
            | Some coord -> Grid.update coord (function Active(name,index,_) -> Active(name,index,true) | x -> x) grid
            | None ->
                perim |> List.sortBy (manhattanDistance spy.Coords)
                       |> List.tryFind (fun c -> match Grid.getSectorAt c grid with Some Clear | None -> true | _ -> false)
                       |> Option.map (fun c -> Grid.insert c Torpedo grid)
                       |> Option.defaultValue grid
        | _ -> grid

    let advanceTorpedoes (grid:Sector Grid) : Sector Grid =
        let battlefieldData   = extractData grid
        let gridDimensions   = battlefieldData.Dims
        let torpedoCoords  = getTorpedoes grid
        let spyCoords  = getActiveInfos grid |> List.filter (fun (n,_,_,h) -> n=Spy && not h) |> List.map (fun (_,c,_,_) -> c)
        if torpedoCoords = [] || spyCoords = [] then grid else
        let gridWithoutTorpedoes = torpedoCoords |> List.fold (fun g p -> Grid.insert p Clear g) grid
        let moveSingleTorpedo currentGrid torpedoPosition =
            let spy = spyCoords |> List.minBy (fun (sr,sc) -> abs(sr-fst torpedoPosition) + abs(sc-snd torpedoPosition))
            getOptimalTorpedoDirections spy torpedoPosition
            |> List.tryPick (fun dir ->
                let newTorpedoPos, updatedGrid = torpedoStep gridDimensions dir (torpedoPosition,currentGrid)
                if newTorpedoPos<>torpedoPosition then Some updatedGrid else None)
            |> Option.defaultValue currentGrid
        torpedoCoords |> List.fold moveSingleTorpedo gridWithoutTorpedoes

    let interceptNextHit (coord:Coord) (grid:Sector Grid) : Sector Grid =
        let data = extractData grid
        let cruiserOpt = data.Ships |> List.tryFind (fun s -> s.Name = Cruiser)
        match cruiserOpt with
        | None -> hit coord grid
        | Some cruiser when getRemainingHealth cruiser.Name grid = 0 -> hit coord grid
        | Some cruiser ->
            match Grid.getSectorAt coord grid with
            | Some (Active(name,_,false)) when name <> Cruiser && getRemainingHealth name grid = 1 ->
                cruiser.Coords |> List.tryFind (fun c -> match Grid.getSectorAt c grid with Some (Active(Cruiser,_,false)) -> true | _ -> false)
                |> Option.map (fun c -> hit c grid) |> Option.defaultValue (hit coord grid)
            | _ -> hit coord grid

    let getPlanePath (grid:Sector Grid) : Coord list =
        let gridRows,gridCols = getDims grid
        let gridCenter = gridRows/2, gridCols/2
        let maxDiagonalSteps  = min (gridRows - fst gridCenter - 1) (gridCols - snd gridCenter - 1)
        let rec buildDiagonalPath remainingSteps (currentRow,currentCol) acc = if remainingSteps=0 then acc else buildDiagonalPath (remainingSteps-1) (currentRow+1,currentCol+1) ((currentRow+1,currentCol+1)::acc)
        let diagonalRight   = buildDiagonalPath maxDiagonalSteps gridCenter [] |> List.rev
        let upwardPath   = [1..maxDiagonalSteps*2] |> List.map (fun i -> fst (List.last diagonalRight) - i, snd (List.last diagonalRight))
        let diagonalLeft   = [1..maxDiagonalSteps*2] |> List.map (fun i -> fst (List.last upwardPath) + i, snd (List.last upwardPath) - i)
        let upwardPath2  = [1..maxDiagonalSteps*2] |> List.map (fun i -> fst (List.last diagonalLeft) - i, snd (List.last diagonalLeft))
        let backToCenter = buildDiagonalPath maxDiagonalSteps (List.last upwardPath2) [] |> List.rev
        gridCenter :: diagonalRight @ upwardPath @ diagonalLeft @ upwardPath2 @ backToCenter

    let advancePlane (stream:Coord Stream) : Coord Stream =
        match stream with Empty -> Empty | Cons (_,tl) -> tl.Force()

    let shiftPath (grid:Sector Grid) (stream:Coord Stream) : Coord Stream =
        let data = extractData grid
        match data.Ships |> List.tryFind (fun s -> s.Name = Spy) with
        | None -> stream
        | Some spy ->
            let bateauxPertinents = data.Ships |> List.filter (fun s -> 
                s.Name = Destroyer || s.Name = Submarine || s.Name = Cruiser)
        
            let espionDansPerimetreRelevant = 
                bateauxPertinents |> List.exists (fun bateau ->
                    let perimetre = getPerimeter bateau data.Dims
                    spy.Coords |> List.exists (fun coordEspion -> 
                        perimetre |> List.contains coordEspion))
        
            if not espionDansPerimetreRelevant then 
                stream
            else
                let limiteIteration = 4 * fst data.Dims * snd data.Dims
                let positionActuelleAvion = Stream.head stream
                let cycleAvion = positionActuelleAvion :: extractFullCycle limiteIteration stream positionActuelleAvion
                let colonneMinCycle, colonneMaxCycle = cycleAvion |> List.minBy snd |> snd, cycleAvion |> List.maxBy snd |> snd
                let colonnesEspion = spy.Coords |> List.map snd
            
                let espionPartiellementExterieur = 
                    (colonnesEspion |> List.exists (fun colEspion -> colEspion < colonneMinCycle || colEspion > colonneMaxCycle)) &&
                    (colonnesEspion |> List.exists (fun colEspion -> colEspion >= colonneMinCycle && colEspion <= colonneMaxCycle))
            
                if not espionPartiellementExterieur then 
                    stream
                else
                    let directionDecalageColonne =
                        if List.min colonnesEspion < colonneMinCycle then -1
                        elif List.max colonnesEspion > colonneMaxCycle then 1 
                        else 0
                
                    let peutDecaler = 
                        directionDecalageColonne = 0 || 
                        (directionDecalageColonne = -1 && colonneMinCycle > 0) || 
                        (directionDecalageColonne = 1 && colonneMaxCycle < snd data.Dims - 1)
                              
                    if directionDecalageColonne <> 0 && peutDecaler then 
                        Stream.map (fun(rangee,col) -> rangee,col+directionDecalageColonne) stream 
                    else 
                        stream


    let revertPath (grid:Sector Grid) (stream:Coord Stream) : Coord Stream =
        let data = extractData grid
        match data.Ships |> List.tryFind (fun s -> s.Name = Spy) with
        | None -> stream
        | Some spy ->
            let limit = 4 * fst data.Dims * snd data.Dims
            let currentPlanePosition = Stream.head stream
            let planeCycle = currentPlanePosition :: extractFullCycle limit stream currentPlanePosition
            let nextCoordInCycle = match planeCycle with _::x::_ -> x | _ -> currentPlanePosition
            let prevCoordInCycle = List.last planeCycle
            let distanceToNext = manhattanDistance spy.Coords nextCoordInCycle
            let distanceToPrevious = manhattanDistance spy.Coords prevCoordInCycle
            if distanceToPrevious < distanceToNext then
                Stream.cycleList (currentPlanePosition :: List.rev (List.tail planeCycle))
            else stream
