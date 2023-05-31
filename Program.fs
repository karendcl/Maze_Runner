open System
//working with the player
type Player() =
    let mutable Positionx = 1
    let mutable Positiony = 1
    [<DefaultValue>] val mutable name : string

    member this.Initialize(x : int, y: int, name:string)=
        Positionx <- x
        Positiony <- y
        this.name <- name
    
    member this.Print = printf " Nombre: %s, Posicion: %i, %i" this.name Positionx Positiony
    member this.MoveX steps = Positionx <- Positionx + steps
    member this.MoveY steps = Positiony <- Positiony + steps
    member this.Xpos = Positionx
    member this.YPos =  Positiony




//working with the maze now
type Cell = Wall | Open | Chest | Monster | Boss | Fountain 

let GetRandom x = 
    let rnd = new Random()
    rnd.Next(x)

let mutable maze = 
    [|
     [|Wall;Wall;Wall;Wall;Wall;Wall;Wall;Wall;Wall;Wall|];
     [|Wall;Open;Open;Open;Open;Open;Open;Open;Open;Wall|];
     [|Wall;Open;Open;Open;Open;Open;Open;Open;Open;Wall|];  
     [|Wall;Open;Open;Open;Open;Open;Open;Open;Open;Wall|];
     [|Wall;Open;Open;Open;Open;Open;Open;Open;Open;Wall|];
     [|Wall;Open;Open;Open;Open;Open;Open;Open;Open;Wall|];
     [|Wall;Open;Open;Open;Open;Open;Open;Open;Open;Wall|];
     [|Wall;Open;Open;Open;Open;Open;Open;Open;Open;Wall|];
     [|Wall;Open;Open;Open;Open;Open;Open;Open;Open;Wall|];
     [|Wall;Wall;Wall;Wall;Wall;Wall;Wall;Wall;Wall;Wall|]
    |]

let ValidMove xchange ychange sizeofmaze (player: Player) = 
        let newpos1 = player.Xpos + xchange
        let newpos2 = player.YPos + ychange

        not(newpos1 < 0 || newpos2<0 || newpos1 >= sizeofmaze || newpos2>=sizeofmaze || maze.[newpos1].[newpos2] = Cell.Wall) 



//generate it randomly
let SetRandomCell = 
    let v = GetRandom 10
    match v with
    | 0 -> Cell.Wall
    | 1 -> Cell.Chest
    | 2 -> Cell.Boss
    | 3 -> Cell.Fountain
    | 4 -> Cell.Monster
    | _ -> Cell.Open

// let CreateMaze ( maze : Cell array array) =
//     for row in maze do
//         for place in row do
//           place <- SetRandomCell
    

//#nowarn "40"

//getting input from user
let rec GetDirection (keyPressed: ConsoleKey)=
    match keyPressed with
    | ConsoleKey.UpArrow -> (0,-1)
    | ConsoleKey.DownArrow -> (0,1)
    | ConsoleKey.RightArrow -> (1,0)
    | ConsoleKey.LeftArrow -> (-1,0)
    | _ -> (0,0) 



let MovePlayer (x:int)  (y:int) (jugador:Player) =
    jugador.MoveX x
    jugador.MoveY y
    



        
//testing zone
let player = new Player()
player.Initialize(1,1,"Karen")
player.Print



let InitialLoop = 
    let mutable k = ConsoleKey.A
    while not (k.Equals(ConsoleKey.Escape)) do
        let mutable a = (0,0)
        k <- Console.ReadKey().Key
        a <- GetDirection k
        Console.Clear()
        player.Print

        let  b = fst a
        let c = snd a
        let  condition = ValidMove b c 10 player

        if (condition) then (MovePlayer b c player) 
        player.Print