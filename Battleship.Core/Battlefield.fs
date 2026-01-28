namespace Battleship.Core

module Battlefield =
    open Grid
    open Ship
    open Navigation

    type Data = { Dims: Dims; Ships: Ship list }

    let initClearGrid (dims: Dims) : Sector Grid =
        let (rows, cols) = dims

        let rec buildRow c =
            if c = 0 then []
            else Clear :: buildRow (c - 1)

        let rec buildGrid r =
            if r = 0 then Empty
            else Row (buildRow cols, buildGrid (r - 1))

        buildGrid rows

   

    let addShip (ship: Ship) (grid: Sector Grid) : Sector Grid =
        let changes = List.mapi (fun i coord -> (coord, Active (ship.Name, i,false))) ship.Coords   
        List.fold (fun g ((x, y), value) ->
            let rec setAt x y value grid =
                match grid with
                | Empty -> Empty
                | Row (row, rest) ->
                    if x = 0 then
                        let newRow = List.mapi (fun j v -> if j = y then value else v) row
                        Row (newRow, rest)
                    else Row (row, setAt (x - 1) y value rest)
            setAt x y value g
        ) grid changes


    let replaceShip (ship: Ship) (grid: Sector Grid) : Sector Grid =
        let life =
            foldGrid (fun _ _ sector acc ->
                match sector with
                | Active (name, index, isHit) when name = ship.Name -> (index, isHit) :: acc
                | _ -> acc
            ) [] grid
            |> List.sortBy fst
            |> List.map snd
            |> List.indexed
            |> Map.ofList

        let gridClean =
            mapGrid (function
                | Active (name, _, _) when name = ship.Name -> Clear
                | s -> s) grid

        ship.Coords
        |> List.indexed
        |> List.fold (fun g (index, coord) ->
            let isHitBefore = Map.tryFind index life |> Option.defaultValue false
            let haveTorpedo = 
                match getSectorAt coord gridClean with
                | Some Torpedo -> true
                | _ -> false

            let isHit = isHitBefore || haveTorpedo
            let gridWithoutTorpedo = if haveTorpedo then insert coord Clear g else g
            insert coord (Active (ship.Name, index, isHit)) gridWithoutTorpedo
        ) gridClean


    let getSelectedName (coord: Coord) (grid: Sector Grid) : Name option =   
        let (targetRow, targetCol) = coord
        Grid.foldGrid (fun row col sector acc ->
            match acc with
            | Some _ -> acc
            | None ->
                if (row, col) = (targetRow, targetCol) then
                    match sector with
                    | Active (name, _,_) -> Some name
                    | _ -> None
                else None
        ) None grid

    let extractData (grid: Sector Grid) : Data =
        let dims = Grid.getDims grid

        let activeSectors = getActiveInfos grid 

        let GridShips =
            let rec group lst acc =
                match lst with
                | [] -> acc
                | (n, c, i, isHit) :: rest ->
                    let rec insert = function
                        | [] -> [(n, [(c, i, isHit)])]
                        | (n', l) :: tl when n = n' ->
                            (n', (c, i, isHit) :: l) :: tl
                        | head :: tl -> head :: insert tl
                    group rest (insert acc)
            group activeSectors []

        let ships =
            GridShips
            |> List.map (fun (name, coordsnew) ->
                let sorted = List.sortBy (fun (_, idx, _) -> idx) coordsnew
                let coords = List.map (fun (c, _, _) -> c) sorted
                let center = List.item ((List.length coords + 1) / 2 - 1) coords
                let direction =
                    match coords with
                    | (x1, y1) :: (x2, y2) :: _ ->
                        if x1 = x2 then if y2 > y1 then West else East
                        else if x2 > x1 then North else South
                    | _ -> South
                { Name = name; Coords = coords; Center = center; Facing = direction }
            )

        { Dims = dims; Ships = ships }


    let loadData (data: Data) : Sector Grid =
        List.fold (fun acc ship -> addShip ship acc) (initClearGrid data.Dims) data.Ships

    (* ------- À COMPLÉTER ------- *)
    (* --- Nouvelles fonctions --- *)

    let getFog (grid: Sector Grid) (drone: Coord) : bool Grid =
        let { Dims = (maxRow, maxCol); Ships = ships } = extractData grid
        let spyShip = ships |> List.find (fun s -> s.Name = Spy)
        let (row1, col1), (row2, col2) =
            match spyShip.Coords with
            | a :: b :: _ -> a, b
            | _ -> failwith "il doit y avoir deux coordonnées"

        let centerR2  = row1 + row2
        let centerC2  = col1 + col2
        let rows = List.init maxRow id
        let cols = List.init maxCol id
        let delta = [ -1; 0; 1 ]
        let (rowDroneDepart, colDroneDepart) = drone

        let spyVisible =
            rows
            |> List.collect (fun r ->
                cols
                |> List.choose (fun c ->
                    let dist2 = abs (2*r - centerR2) + abs (2*c - centerC2)
                    if dist2 <= 5 then Some (r, c) else None))
            |> Set.ofList

        let droneVisible =
            delta
            |> List.collect (fun rowDrone ->
                delta
                |> List.choose (fun colDrone ->
                    let r = rowDroneDepart + rowDrone
                    let c = colDroneDepart + colDrone
                    if r >= 0 && r < maxRow && c >= 0 && c < maxCol
                    then Some (r, c)
                    else None))
            |> Set.ofList

        let visible = Set.union spyVisible droneVisible

        let rec build r =
            if r >= maxRow then Empty
            else
            let rowBoolean=
                List.init maxCol (fun c -> Set.contains (r, c) visible)
            Row(rowBoolean, build (r + 1))

        build 0


    let isRevealed (coord: Coord) (fog: bool Grid) : bool =      
        
        
        let (RowT, ColT) = coord
        foldGrid (fun row col isVisible acc ->
            match acc with
            | Some result -> Some result
            | None ->
                if (row, col) = (RowT, ColT) then Some isVisible else None
        ) None fog
        |> function
            | Some result -> result
            | None -> false 
        

    let getTorpedoes (grid: Sector Grid) : Coord list =
        Grid.chooseGrid (fun r c s ->
            match s with
            | Torpedo -> Some (r, c)
            | _ -> None
        ) grid

    let isHit (coord: Coord) (grid: Sector Grid) : bool =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        let (RowT, ColT) = coord
        foldGrid (fun row col sector acc ->
            match acc with
            | Some resultat -> Some resultat
            | None ->
                if (row, col) = (RowT, ColT) then
                    match sector with
                    | Active (_, _, isHit) -> Some isHit  
                    | _ -> Some false  
                else None
        ) None grid
        |> function
            | Some result -> result
            | None -> false 

    let canHit (coord: Coord) (grid: Sector Grid) : bool =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        let (RowT, ColT) = coord
        let isSpyRange = 
            let { Ships = ships } = extractData grid
            let spyShip = ships |> List.tryFind (fun s -> s.Name = Spy)
            match spyShip with
            | Some spy ->
                let (r1, c1), (r2, c2) =
                    match spy.Coords with
                    | a :: b :: _ -> a, b
                    | _ -> (0, 0), (0, 0)
                let centerR2 = r1 + r2
                let centerC2 = c1 + c2
                let dist2 = abs (2*RowT - centerR2) + abs (2*ColT - centerC2)
                dist2 <= 5
            | None -> false

        if not isSpyRange then false
        else
            foldGrid (fun row col sector acc ->
                match acc with
                | Some resultat -> Some resultat
                | None ->
                    if (row, col) = (RowT, ColT) then
                        match sector with
                        | Torpedo -> Some true  
                        | Active (name, _, isHit) when name <> Spy -> Some (not isHit) 
                        | _ -> Some false   
                    else None
            ) None grid
            |> function
                | Some resultat -> resultat
                | None -> false

    let hit (coord: Coord) (grid: Sector Grid) : Sector Grid =
        let (RowT, ColT) = coord
        let rec updateGrid row col currentGrid =
            match currentGrid with
            | Empty -> Empty
            | Row (rowDate, reste) ->
                if row = 0 then
                    let newRow = 
                        List.mapi (fun i sector ->
                            if i = col then
                                match sector with
                                | Active (name, index, _) -> Active (name, index, true) 
                                | Torpedo -> Clear  
                                | _ -> sector
                            else sector
                        ) rowDate
                    Row (newRow, reste)
                else 
                    Row (rowDate, updateGrid (row - 1) col reste)

        updateGrid RowT ColT grid

    let getRemainingHealth (name: Name) (grid: Sector Grid) : int =
        grid
        |> getActiveInfos
        |> List.filter (fun (namefind, _, _, hit) -> namefind = name && not hit)
        |> List.length