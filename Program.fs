open System
//working with the player
type Player(name :string) =
    let mutable Positionx = 1
    let mutable Positiony = 1
    let mutable Attack = 200
    let mutable Defense = 200
    let mutable Level = 1
    let mutable Health = 300
    let mutable Inventory = []
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

    member this.Initialize(x : int, y: int)=
        Positionx <- x
        Positiony <- y
      
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
        let h = Health + a
        Health <- h
    
    member this.HasObject x many = Contains Inventory x 

    member this.AddToInventory x = 
        Inventory <- AddToList Inventory x
            
    member this.RemoveFromInventory x = Remove Inventory x  

    member this.MoveX steps = Positionx <- Positionx + steps
    override this.ToString() = 
        let mutable inv = ""
        for (i:string) in Inventory do
            inv <- inv + " " + i.ToString()

        "Nombre: " + name + "\n" + 
        "Posicion: " + Positionx.ToString() + " " + Positiony.ToString() + "\n" + 
        "Health: " + Health.ToString() + "\n" + 
        "Inventario: " + "\n" + inv + "\n"

       //let str = " Nombre: %s, Posicion: %i, %i, Health: %i, Inventario: %s items: %i" this.name Positionx Positiony Health PrintInventory Inventory.Length
type CraftedObject(Name: string, Recipe : string list) = 
    member this.Name = Name
    member this.Recipe = Recipe
    override this.ToString() =  Name
    

//working with the maze now
type Cell = Wall | Open | Chest | Monster | Boss | Fountain 

let Resources = ["Coal"; "Stick"; "Wood";"Stone";"Iron";"Gold";"RedStone";"Diamond";"Netherite";"Obsidian";"Emerald";"Copper";"Sand"; "Wool"]
let mutable dimension = 15
let GetRandom x = 
    let rnd = new Random()
    rnd.Next(x)

let casilla_thing x y =
    let i = GetRandom 7

    match i with
    | 0 -> Cell.Wall
    | 1 -> Cell.Chest
    | 2 -> Cell.Fountain
    | 3 -> Cell.Monster
    | _ -> Cell.Open

let mutable maze = Array2D.init dimension dimension (casilla_thing)

let casilla_bool x y =
    let thing = maze[x,y]
    match thing with
    | Cell.Wall -> false
    | Cell.Boss -> false
    | _ -> true

let mutable MazeMask = Array2D.init dimension dimension ( casilla_bool )
   
let ValidMove xchange ychange (player: Player) = 
        let newpos1 = player.Xpos + xchange
        let newpos2 = player.YPos + ychange

        not(newpos1 < 0 || newpos2<0 || newpos1 >= dimension || newpos2>=dimension || maze[newpos2,newpos1] = Cell.Wall) 


//getting input from user
let MovePlayer (x:int)  (y:int) (jugador:Player) =
    jugador.MoveX x
    jugador.MoveY y


let GetDirection (keyPressed: ConsoleKey) (player : Player)=
    match keyPressed with
    | ConsoleKey.UpArrow -> if (ValidMove 0 -1  player) then (MovePlayer 0 -1 player)
    | ConsoleKey.DownArrow -> if (ValidMove 0 1 player) then (MovePlayer 0 1 player)
    | ConsoleKey.RightArrow -> if (ValidMove 1 0  player) then (MovePlayer 1 0 player)
    | ConsoleKey.LeftArrow -> if (ValidMove -1 0  player) then (MovePlayer -1 0 player)
    //| ConsoleKey.C -> Craft player
    | _ -> MovePlayer 0 0 player 

let FoundChest (jugador:Player)=
    let l = Resources.Length
    let ind = GetRandom l
    let drop = Resources.Item ind
    jugador.AddToInventory drop

let WonMonsterFight (jugador:Player) (damage:int) =
    jugador.RegenerateHealth damage
    let b = GetRandom 2
    match b with
    |0 -> MovePlayer 0 0 jugador
    |_ -> FoundChest jugador

let FightMonster (jugador:Player) = 
   let MonsterAtk = GetRandom jugador.Atk * 2
   let MonsterDef = GetRandom jugador.Def * 2
   let d = MonsterAtk + MonsterDef - jugador.Def - jugador.Atk
   let absd = abs d
   if d>0 then jugador.Damage absd else WonMonsterFight jugador absd


let InteractWithMaze (jugador:Player)  =
    let a = jugador.Xpos
    let b = jugador.YPos
    let thing = maze[b,a]

    let NewPosition = MazeMask[b,a]
    if NewPosition then
        MazeMask[b,a] <- false
        match thing with
        | Cell.Monster -> FightMonster jugador
        | Cell.Chest -> FoundChest jugador
        | _ -> MovePlayer 0 0 jugador
    else MovePlayer 0 0 jugador
        

let PrintMaze() =
    for i in [0..maze.GetLength(0)-1] do
        Console.WriteLine()
        for j in [0..maze.GetLength(1)-1] do
            Console.Write(" " + maze[i,j].ToString())


let PrintBoolMaze() =
    for i in [0..MazeMask.GetLength(0)-1] do
        Console.WriteLine()
        for j in [0..MazeMask.GetLength(1)-1] do
            Console.Write(" " + MazeMask[i,j].ToString())

let InitialLoop = 

    //get random drop points for player while its not a valid starting point
    //check with bfs that its a valid maze
    Console.Clear()
    let player = new Player("Karen")
    player.Initialize(1,1)


    let mutable k = ConsoleKey.A
    while not (k.Equals(ConsoleKey.Escape)) do
        Console.WriteLine(player.ToString())
        PrintMaze()
        Console.WriteLine()
        PrintBoolMaze()
        k <- Console.ReadKey().Key
        GetDirection k player
        InteractWithMaze player 
        Console.Clear()
        

[<EntryPoint>]
let main args = 
    InitialLoop
    1



