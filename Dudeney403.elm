import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Array exposing (Array)

type alias Grid a = {
    width: Int,
    height: Int,
    cells: Array a
}

type Cell
    = Empty
    | Occupied Int

value cell = case cell of
    Empty -> 0
    Occupied n -> n

type alias Pos = (Int,Int)

nToPos (width,height) n =
    let
        x = n % width
        y = (n-x)//width
    in
        (x,y)

posToN (width,height) (x,y) = y*width + x

initial : (Int,Int) -> (Pos -> a) -> Grid a
initial (width,height) default = 
    {
        width = width,
        height = height,
        cells = Array.initialize (width*height) ((nToPos (width,height))>>default)
    }

get : Grid a -> Pos -> Maybe a
get grid pos = Array.get (posToN (grid.width,grid.height) pos) grid.cells

map : Grid a -> (a -> b) -> Grid b
map grid fn = 
    {grid | cells = Array.map fn grid.cells}

indexedMap : Grid a -> (Pos -> a -> b) -> Grid b
indexedMap grid fn =
    {grid | cells = Array.indexedMap (\n -> \x -> fn (nToPos (grid.width,grid.height) n) x) grid.cells}

rows : Grid a -> List (List a)
rows grid = List.map (\y -> Array.toList <| Array.slice (grid.width*y) (grid.width*(y+1)) grid.cells) (List.range 0 (grid.height-1))

columns : Grid a -> List (List a)
columns grid =
    let
        rs = rows grid
    in
        List.map (\x -> List.concat (List.map (\r -> Array.toList <| Array.slice x (x+1) (Array.fromList r)) rs)) (List.range 0 (grid.height-1))

row n grid =
    Maybe.withDefault [] <| List.head <| List.take (n-1) (rows grid)

column n grid =
    Maybe.withDefault [] <| List.head <| List.take (n-1) (rows grid)

diagonal2 grid =
    let
        poses = List.map (\p -> (p,p)) (List.range 0 (grid.width-1))
    in
        sum <| filterJust <| List.map (get grid) poses

diagonal1 grid =
    let
        poses = List.map (\p -> (p,grid.height - p - 1)) (List.range 0 (grid.width-1))
    in
        sum <| filterJust <| List.map (get grid) poses

sum : List Cell -> Int
sum = List.sum << (List.map value)

adjacent : Pos -> List Pos
adjacent (x,y) = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

filterJust : List (Maybe a) -> List a
filterJust maybes = 
    let
        take maybe out = case maybe of
            Nothing -> out
            Just x -> x::out
    in
        List.foldr take [] maybes

adjacentCells : Pos -> Grid a -> List (Pos,a)
adjacentCells pos grid =
    let
        poses = adjacent pos
        cells = List.map (\pos -> get grid pos |> Maybe.map (\c -> (pos,c))) poses
    in
        filterJust cells

swap grid pa pb =
    let
        na = posToN (grid.width, grid.height) pa
        nb = posToN (grid.width, grid.height) pb
        ca = get grid pa
        cb = get grid pb
    in
        {grid|cells = Maybe.withDefault grid.cells <| Maybe.map2 (\a -> \b -> (Array.set na b grid.cells) |> Array.set nb a) ca cb}

type alias Model = {grid: Grid Cell, previous: List (Grid Cell)}
model : Model
model = {previous = [], grid = initial (4,4) ((posToN (4,4)) >> (\n -> if n==15 then Empty else (Occupied (n+1))))}

makeMove model new =
    {model | grid = new, previous = model.grid::model.previous}

main =
  beginnerProgram { model = model, view = view, update = update }


stylesheet = node "style" [] [text """

main {
  max-width: 30em;
  margin: 0 auto;
  display: grid;
  grid-template-areas: "grid" "controls" "intro";
  grid-gap: 1em;
  justify-content: center;
}

.intro {
  max-width: 30em;
  grid-area: intro;
  font-style: italic;
}

.grid {
    display: grid;
    grid-template: 2em [grid-start] repeat(4,2em) [grid-end] 2em / repeat(4,2em) [grid-end] 2em;
    grid-auto-flow: row dense;
    grid-gap: 0.2em;
    font-size: 2rem;
    text-align: center;
    grid-area: grid;
    justify-content: center;
}
.grid > * {
    line-height: 2em;
}

.sum.row {
    grid-column: grid-end;
}
.sum.column {
    grid-row: grid-end;
}

.cell.occupied {
    background: #eee;
    cursor: pointer;
}

.diagonal.d1 {
    grid-column: grid-end;
}
.diagonal.d2 {
    grid-area: grid-end / grid-end;
}

.controls {
    grid-area: controls;
    display: flex;
    flex-wrap: wrap;
}

.move-counter {
    flex-grow: 1;
}


"""]

view model = main_ []
    [
        stylesheet,
        intro,
        controls model,
        let
            diag1 = div [class "diagonal d1"] [text <| toString <| diagonal1 model.grid]
            diag2 = div [class "diagonal d2"] [text <| toString <| diagonal2 model.grid]
        in
            div [class "grid"] <| List.append [diag1,diag2] <| List.append (columnSums model.grid) <| List.concat <| List.indexedMap viewRow (rows model.grid)
    ]

controls model = section [class "controls"]
    [
        span [class "move-counter"] [text <| "Moves: "++(toString <| List.length model.previous)],
        button [onClick Undo] [text "Undo"]
    ]

intro = section [class "intro"] 
    [
        p [] [text "Not fifty miles from Cadiz stood in the middle ages a castle, all traces of which have for centuries disappeared. Among other interesting features, this castle contained a particularly unpleasant dungeon divided into sixteen cells, all communicating with one another, as shown in the illustration."],
        p [] [text "Now, the governor was a merry wight, and very fond of puzzles withal. One day he went to the dungeon and said to the prisoners, \"By my halidame!\" (or its equivalent in Spanish) \"you shall all be set free if you can solve this puzzle. You must so arrange yourselves in the sixteen cells that the numbers on your backs shall form a magic square in which every column, every row, and each of the two diagonals shall add up the same. Only remember this: that in no case may two of you ever be together in the same cell.\""],
        p [] [text "One of the prisoners, after working at the problem for two or three days, with a piece of chalk, undertook to obtain the liberty of himself and his fellow-prisoners if they would follow his directions and move through the doorway from cell to cell in the order in which he should call out their numbers."],
        p [] [text "He succeeded in his attempt, and, what is more remarkable, it would seem from the account of his method recorded in the ancient manuscript lying before me, that he did so in the fewest possible moves. The reader is asked to show what these moves were."]
    ]

    
columnSums grid = 
    let
        sums = List.map sum (columns grid)
    in
        List.map (\t -> div [class "sum column"] [text <| toString t]) sums

viewRow y r = 
    let
        total = sum r
    in
        (div [class "sum row"] [text <| toString total])::(List.indexedMap (\x -> viewCell (x,y)) r)

viewCell : Pos -> Cell -> Html Msg
viewCell (x,y) cell = 
    let
        st = style [("grid-column",toString <| x+1), ("grid-row",toString <| y+2)]
    in
        case cell of
            Empty -> div [st, class "cell"] []
            Occupied n -> div [st, class "cell occupied", onClick <| Click (x,y)] [text (toString n)]

type Msg 
    = Click Pos
    | Undo

update msg model =
  case msg of
    Undo -> case List.head model.previous of
        Nothing -> model
        Just grid -> {model|grid=grid,previous=List.drop 1 model.previous}
    Click pos -> case get model.grid pos of
        Nothing -> model
        Just Empty -> model
        Just (Occupied n) ->
            let
                adjacents = adjacentCells pos model.grid
                empty = List.filter (\(_,c) -> c==Empty) adjacents
            in
                case List.head empty of
                    Nothing -> model
                    Just (pos2,_) -> makeMove model <| swap model.grid pos pos2
