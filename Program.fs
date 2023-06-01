open System
//working with the player
type Player() =
    let mutable Positionx = 1
    let mutable Positiony = 1
    let mutable Attack = 200
    let mutable Defense = 200
    let mutable Level = 1
    let mutable Health = 300
    let mutable Inventory = []
    [<DefaultValue>] val mutable name : string

    member this.Initialize(x : int, y: int, name:string)=
        Positionx <- x
        Positiony <- y
        this.name <- name
    
    member this.Print = printf " Nombre: %s, Posicion: %i, %i, Health %i" this.name Positionx Positiony Health
    member this.MoveX steps = Positionx <- Positionx + steps
    member this.MoveY steps = Positiony <- Positiony + steps
    member this.Xpos = Positionx
    member this.YPos =  Positiony
    member this.Atk = Attack
    member this.Def = Defense
    member this.LevelUp =
      let a = Level+1
      Level <- a
    
    member this.HealthLevel = Health
    member this.Damage a = 
        let h = Health - a
        Health <- h
    member this.RegenerateHealth a = 
        Health <- Health + a
    
        
    
    let rec Contains (lista : list<string>) (x : string) =
        if lista.IsEmpty then false elif lista.Head.Equals(x) then true else Contains lista.Tail x
    
    let rec GetIndex (ind :int) ( lista: list<string>) (obje:string) =
        let length = lista.Length
        if ind >= length then -1 elif (lista.Item ind).Equals(obje) then ind else GetIndex (ind+1) lista obje
    let rec AddToList (lst : list<string>) (s:string)=
        s::lst
    
    let Remove (listaOrig : list<string>) (s: string) =
        let ind = GetIndex 0 listaOrig s
        if ind <> -1 then
            let mutable newl = []
            for index = 0 to ind-1 do
                newl <- AddToList newl (listaOrig.Item index)
            for index = ind+1 to listaOrig.Length-1 do
                newl <- AddToList newl (listaOrig.Item index)
            newl
            else listaOrig
                    

    member this.HasObject x many = Contains Inventory x 

    member this.AddToInventory x = 
        Inventory <- AddToList Inventory x
            
    member this.RemoveFromInventory x = Remove Inventory x  

        

//working with the maze now
type Cell = Wall | Open | Chest | Monster | Boss | Fountain 

let GetRandom x = 
    let rnd = new Random()
    rnd.Next(x)

let mutable maze = 
    [|
     [|Wall;Wall;Wall;Wall;Wall;Wall;Wall;Wall;Wall;Wall|];
     [|Wall;Open;Open;Open;Open;Open;Open;Open;Open;Wall|];
     [|Wall;Monster;Open;Open;Open;Open;Open;Open;Open;Wall|];  
     [|Wall;Open;Open;Open;Wall;Open;Open;Open;Open;Wall|];
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

        not(newpos1 < 0 || newpos2<0 || newpos1 >= sizeofmaze || newpos2>=sizeofmaze || maze.[newpos2].[newpos1] = Cell.Wall) 



//generate it randomly
// let SetRandomCell = 
//     let v = GetRandom 10
//     match v with
//     | 0 -> Cell.Wall
//     | 1 -> Cell.Chest
//     | 2 -> Cell.Boss
//     | 3 -> Cell.Fountain
//     | 4 -> Cell.Monster
//     | _ -> Cell.Open

// let CreateMaze ( maze : Cell array array) =
//     for row in maze do
//         for place in row do
//           place <- SetRandomCell
    

//#nowarn "40"

//getting input from user
let MovePlayer (x:int)  (y:int) (jugador:Player) =
    jugador.MoveX x
    jugador.MoveY y

let GetDirection (keyPressed: ConsoleKey) (player : Player)=
    match keyPressed with
    | ConsoleKey.UpArrow -> if (ValidMove 0 -1 10 player) then (MovePlayer 0 -1 player)
    | ConsoleKey.DownArrow -> if (ValidMove 0 1 10 player) then (MovePlayer 0 1 player)
    | ConsoleKey.RightArrow -> if (ValidMove 1 0 10 player) then (MovePlayer 1 0 player)
    | ConsoleKey.LeftArrow -> if (ValidMove -1 0 10 player) then (MovePlayer -1 0 player)
    | _ -> MovePlayer 0 0 player 



let FightMonster (jugador:Player) = 
   let MonsterAtk = GetRandom jugador.Atk
   let d = MonsterAtk - jugador.Def + jugador.Atk
   if d>0 then jugador.Damage d else jugador.RegenerateHealth d


let InteractWithMaze (jugador:Player) (maze: Cell array array)=
    let a = jugador.Xpos
    let b = jugador.YPos
    let thing = maze.[b].[a]

    match thing with
    | Cell.Monster -> FightMonster jugador
    | _ -> MovePlayer 0 0 jugador
        
//testing zone
let player = new Player()
player.Initialize(1,1,"Karen")


let PrintMaze (maze: Cell array array)=
    for row in maze do
        Console.WriteLine()
        for item in row do
            Console.Write(" " + item.ToString())
    

let InitialLoop = 
    let mutable k = ConsoleKey.A
    while not (k.Equals(ConsoleKey.Escape)) do
        player.Print
        PrintMaze maze
        k <- Console.ReadKey().Key
        GetDirection k player
        InteractWithMaze player maze
        Console.Clear()
        