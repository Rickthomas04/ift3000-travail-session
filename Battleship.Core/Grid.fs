namespace Battleship.Core

module Grid =

    type Dims = int * int

    type Coord = int * int

    type 'a Grid = Empty | Row of 'a list * 'a Grid

    let getDims (grid: 'a Grid) : Dims =
        let rec countRows acc g =
            match g with
            | Empty -> acc
            | Row (_, rest) -> countRows (acc + 1) rest

        let rec getFirstRowLength g =
            match g with
            | Empty -> 0
            | Row (cols, _) -> List.length cols

        let rows = countRows 0 grid
        let cols = getFirstRowLength grid
        (rows, cols)

    let foldGrid (folder: int -> int -> 'a -> 'acc -> 'acc) (initial: 'acc) (grid: 'a Grid) : 'acc =

        let rec processGrid rowIndex g acc =
            match g with
            | Empty -> acc
            | Row (cols, rest) ->
                let rowAcc = processRow rowIndex 0 cols acc
                processGrid (rowIndex + 1) rest rowAcc
        and processRow currentRow colIndex cols acc =
            match cols with
            | [] -> acc
            | cell :: rest ->
                let newAcc = folder currentRow colIndex cell acc
                processRow currentRow (colIndex + 1) rest newAcc
        processGrid 0 grid initial


    let mapGrid (f: 'a -> 'b) (grid: 'a Grid) : 'b Grid =
        let rec map g =
            match g with
            | Empty -> Empty
            | Row (row, rest) -> Row (List.map f row, map rest)
        map grid

    let chooseGrid (f: int -> int -> 'a -> 'b option) (grid: 'a Grid) : 'b list =
        let folder row col cell acc =
            match f row col cell with
            | Some v -> acc @ [v]  
            | None -> acc
        foldGrid folder [] grid



    (* ------- À COMPLÉTER ------- *)
    (* --- Nouvelles fonctions abstraites --- *)

    let isCoordInsideGrid ((r,c): Coord) ((rows,cols): Dims) : bool =
        r >= 0 && c >= 0 && r < rows && c < cols

    let getSectorAt ((r,c): Coord) (grid: 'a Grid) : 'a option =
        let rec aux row g =
            match g with
            | Empty -> None
            | Row (cells, rest) ->
                if row = r then List.tryItem c cells
                else aux (row + 1) rest
        aux 0 grid

    let insert (coord:Coord) (v:'a) (grid:'a Grid) : 'a Grid =
        let r,c = coord
        let rec goto row g =
            match g with
            | Empty -> Empty
            | Row (cells, rest) ->
                let newRow = if row = r then List.mapi (fun j x -> if j = c then v else x) cells else cells
                Row (newRow, goto (row+1) rest)
        goto 0 grid

    let update (coord:Coord) (f:'a -> 'a) (grid:'a Grid) : 'a Grid =
        match getSectorAt coord grid with
        | Some old -> insert coord (f old) grid
        | None -> grid 