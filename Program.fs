open System
open System.IO
open System.Threading

//Some Methods For Lists
let Remove (lista: list<'a>) (item:'a) = 
    let rec RemoveOnce (listaorig : list<'a>) (listares : list<'a>) (item :'a) (found : bool)=
        //si la lista original esta vacia no hay mas que hacer
        if listaorig.Length.Equals(0) then listares
        //si ya se encontro se copia lo que resta
        else
        let a = listaorig.Head
        let b = listaorig.Tail
        if found.Equals(true) 
            then RemoveOnce b (a::listares) item true
        else
            if listaorig.Head.Equals(item) then RemoveOnce b listares item true
            else RemoveOnce b (a::listares) item false
    
    RemoveOnce lista [] item false

let rec Contains (lista : list<'a>) (x : 'a) =
        if lista.IsEmpty then false elif lista.Head.Equals(x) then true else Contains lista.Tail x

let rec Clone (listorig : list<'a>) (listres:list<'a>) = 

    if listorig.Length.Equals(0) then listres
    else 
    let a = listorig.Head
    let b = listorig.Tail
    Clone (b) (a::listres)

let rec FromArrayToList (lst : list<'a>) (ar : array<'a>) (ind  : int)=
        if ind < 0 then lst 
        else FromArrayToList (ar.[ind]::lst) (ar) (ind-1)

let rec AddToList (lst) (s)= s::lst

let rec RemoveAll (lista: list<'a>) (item : 'a) = 
    if Contains lista item then 
        RemoveAll (Remove lista item) item
    else lista

let GroupBy (lista : list<'a>) = 
    let rec GroupByOnce (listaorig : list<'a>) (item:'a) (counter:int)=
        if listaorig.Length.Equals(0) then counter
        else
            let a = listaorig.Head
            if a.Equals(item) then GroupByOnce listaorig.Tail item (counter+1)
            else GroupByOnce listaorig.Tail item counter

    let rec GroupByAll (listaorig : list<'a>) (listares : list<'a * int>) =
        if listaorig.Length.Equals(0) then listares
        else
            let a = listaorig.Head
            let c = GroupByOnce listaorig a 0
            GroupByAll (RemoveAll listaorig a) ((a,c)::listares)
    
    GroupByAll lista [] 

let IndexOf (objeto:'a) (lst :list<'a>) =     
    let rec GetIndex (ind :int) ( lista : list<'a>) (obje:'a) =
        let length = lista.Length
        if ind >= length then -1 elif (lista.Item ind).Equals(obje) then ind else GetIndex (ind+1) lista obje
    GetIndex 0 lst objeto

let Reverse (lst :list<'a>) = 
    let rec Rev acc = function
        |[] -> acc
        | head ::tail -> Rev (head::acc) tail
    
    Rev [] lst

let Enqueue (q : list<'a>) (item:'a) = 
    Reverse (AddToList (Reverse q) item)
    
let Dequeue (lst : list<'a>) = lst.Tail

type Inventory() = 
    let mutable Inv = []

    member this.AddToInventory( obje ) = 
        Inv <- AddToList Inv obje
    
    member this.RemoveFromInventory(a)=
        Inv <- Remove Inv a
    
    member this.HasInInventory(a)=
        Contains Inv a

    member this.Clone() = 
        Clone Inv []

    override this.ToString() = 
        let a = GroupBy Inv
        let rec CreateString (a : list<string * int>) (res : string) =
            if a.Length.Equals(0) then res
            else
                let b = a.Head
                let c = fst(b)
                let d = snd(b)
                CreateString a.Tail (res + c + " x" + d.ToString() + "\n\t") 
        CreateString a "\t"




//working with the player
type Player(name :string) =
    let mutable Positionx = 1
    let mutable Positiony = 1
    let mutable Attack = 200
    let mutable Defense = 200
    let mutable Level = 1
    let mutable Health = 300
    let mutable MaxHealth = 300
    let mutable Inventory = new Inventory()

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
      Attack <- Attack * 15/10
      Defense <- Defense * 15/10
      Level <- a
      MaxHealth <- MaxHealth + 100

    member this.ResetHealth() = 
        Health <- MaxHealth
    
    member this.ShowInventory() = 
        Inventory.ToString()
    member this.HealthLevel = Health
    member this.Damage a = 
        let h = Health - a
        Health <- h
    member this.RegenerateHealth a = 
        let h = Health + a
        if h > MaxHealth then Health <- MaxHealth else Health <- h
    

    member this.AddToInventory x = 
        Inventory.AddToInventory x
            
    member this.RemoveFromInventory x = Inventory.RemoveFromInventory x  

    member this.MoveX steps = Positionx <- Positionx + steps

    member this.InventoryCopy = Inventory.Clone()

    override this.ToString() = 
        let inv = Inventory.ToString()
        
        "Nombre: " + name + "\n" + 
        "Posicion: " + Positionx.ToString() + " " + Positiony.ToString() + "\n" + 
        "Health: " + Health.ToString() + "\n" + 
        "Inventario: " + "\n" + inv + "\n"


type CraftedObject(Name: string, Recipe : string list) = 
    member this.Name = Name
    member this.Recipe = Recipe
    override this.ToString() = 
        let a = GroupBy Recipe
        let rec CreateString (a : list<string * int>) (res : string) =
            if a.Length.Equals(0) then res
            else
                let b = a.Head
                let c = fst(b)
                let d = snd(b)
                CreateString a.Tail (res + c + " x" + d.ToString() + "; ") 
        Name + ".      " + CreateString a " "


let rec PrintMenuWithOptions (header : string)(a : list<'a>) (ind :int)=
    Console.Clear()
    Console.WriteLine(header)

    let mutable ind2 = ind
    if ind < 0 then ind2 <- a.Length-1 elif ind > a.Length-1 then ind2 <- 0 else ind2 <- ind

    for i in [0..a.Length-1] do 
        if i.Equals(ind2) then Console.WriteLine("---> " + (a.Item i).ToString())
        else Console.WriteLine( (a.Item i).ToString())
    
    let k = Console.ReadKey().Key
    match k with
    | ConsoleKey.DownArrow -> PrintMenuWithOptions header a (ind2+1)
    | ConsoleKey.UpArrow -> PrintMenuWithOptions header a (ind2-1)
    | ConsoleKey.Enter -> ind2
    | _ -> PrintMenuWithOptions header a ind2


//working with the maze now
type Cell = Wall | Open | Chest | Monster | Boss | Fountain 

let mutable dimension = 15

let GetRandom x = 
    let rnd = new Random()
    rnd.Next(x)


let casilla_thing x y =
    let i = GetRandom 7

    match i with
    | 0 -> Cell.Wall
    | 4 -> Cell.Wall
    | 1 -> Cell.Chest
    | 2 -> Cell.Fountain
    | 3 -> Cell.Monster
    | _ -> Cell.Open


let mutable maze = Array2D.init dimension dimension (casilla_thing)

let mutable Bossx = 0
let mutable Bossy = 0

let casilla_bool x y =
    let thing = maze[x,y]
    match thing with
    | Cell.Wall -> false
    | Cell.Boss -> false
    | _ -> true


let mutable MazeMask = Array2D.init dimension dimension ( casilla_bool )

let ResetBoolMaze() = 
    MazeMask <- Array2D.init dimension dimension (casilla_bool)

let GetInt (question : string)= 
    let rec G (question:string) (ind:int) = 
        Console.Clear()
        Console.WriteLine(question)

        if (ind<0) then G question 0 else

        Console.Write(ind)
        let k = Console.ReadKey().Key

        match k with
        | ConsoleKey.UpArrow -> G question (ind+1)
        | ConsoleKey.DownArrow -> G question (ind-1)
        | ConsoleKey.Enter -> ind
        |_ -> G question ind

    G question 0


let GenerateNewMaze()=

    dimension <- GetInt "Seleccione la dimension de su laberinto. Use las flechitas \n\n"
    let mutable a = Array2D.init dimension dimension (casilla_thing)
    Bossx <- GetRandom dimension
    Bossy <- GetRandom dimension

    let mutable k = true

    while k do
        match a[Bossy,Bossx] with
            |Cell.Wall ->  Bossx <- GetRandom dimension
                           Bossy <- GetRandom dimension
            |_ -> k<-false

    a[Bossy,Bossx] <- Cell.Boss
    maze <- a

    ResetBoolMaze()


//add a ToString method to the cell type

let ToStringC (c : Cell) = 
    match c with
    | Wall -> "▀"
    | Open -> " "
    | Chest -> "C"
    | Monster -> "M"
    | Boss -> "B"
    | Fountain -> "F"


let Resources = ["Coal"; "Stick"; "Wood";"Stone";"Iron";"Gold";"RedStone";"Diamond";"Netherite";"Obsidian";"Emerald";"Copper";"Sand"; "Wool"]

let mutable Crafts: CraftedObject list = []

let readlines (filepath :string) = 
    let mutable (res: string list) = []
    use sr = new StreamReader(filepath)
    while not sr.EndOfStream do
        res <- AddToList res (sr.ReadLine())
    res


let CreateCraftedObject( recipe :string) =
    let a = recipe.Split('=')

    let RemoveEmpty (str : string) = 
        let rec RemoveEmptySpaces (str: string) (res:string) (ind:int)=
            if ind.Equals(str.Length) then res
            else if (ind.Equals(str.Length-1) && str.[ind].Equals(' ')) then res
            else if (ind.Equals(0) && str.[ind].Equals(' ')) then RemoveEmptySpaces str res (ind+1)
            else if (str.[ind].Equals(' ') && not (ind.Equals(0)) && str.[ind-1].Equals(' ')) then RemoveEmptySpaces str res (ind+1)
            else RemoveEmptySpaces str (res + str.[ind].ToString()) (ind+1)
        RemoveEmptySpaces str "" 0
    
    let name = RemoveEmpty a.[0]

    let b = Array.map(fun x -> RemoveEmpty x) (a.[1].Split(','))

    Crafts <- AddToList Crafts (new CraftedObject(name, (FromArrayToList [] b (b.Length-1))))


let InitializeCrafts() = 
    //read txt Format: <Craft Name> = <Resource>,<Resource>,<Resource>,..., <Resource>
    let filepath = __SOURCE_DIRECTORY__ + "\Crafts.txt"
    let AllRecipes = readlines filepath
    for i in AllRecipes do CreateCraftedObject i


let EndGame (k: ConsoleKey) (player:Player)=
    if k.Equals(ConsoleKey.Escape) then 1
    else if player.HealthLevel <= 0 then 2
    else if maze[player.YPos, player.Xpos].Equals(Cell.Boss) then 3
    else 4

let ValidPosition x y = 
        not(x < 0 || y<0 || x >= dimension || y>=dimension || maze[y,x] = Cell.Wall) 

let ValidMove xchange ychange (player: Player) = 
        let newpos1 = player.Xpos + xchange
        let newpos2 = player.YPos + ychange

        ValidPosition newpos1 newpos2



let MovePlayer (x:int)  (y:int) (jugador:Player) =
    jugador.MoveX x
    jugador.MoveY y


let PrintCraftMenu (player:Player) = 
    Console.Clear()
    if not (player.InventoryCopy.IsEmpty) 
        then
        let mutable header = ""
        header <- header + "The player's Inventory"
        header <- header + player.ShowInventory()
        header <- header + "\n\n\n These are the possible crafts:"
        let indexofcraft = PrintMenuWithOptions header Crafts 0
        (indexofcraft, true)
        
    else
        Console.WriteLine("You do not have anything in your inventory. Get To Work! \n\n Press Any Key to Continue")
        let a = Console.ReadKey()
        (-1, false) 




let TryCraft (player:Player) (obje: CraftedObject) =
    let rec CheckForRecipe (inventoryLst: list<'a>) (recipeLeft :list<'a>)=
        if recipeLeft.Length.Equals(0) then true
        else
            let a = recipeLeft.Head
            if Contains inventoryLst a 
                then
                    CheckForRecipe (Remove inventoryLst a ) recipeLeft.Tail 
                else false

    if CheckForRecipe player.InventoryCopy obje.Recipe then
        for i in obje.Recipe do player.RemoveFromInventory i
        player.AddToInventory obje.Name


let Craft (player : Player) = 
    let (ind, bo) = PrintCraftMenu player
    if bo.Equals(true) then TryCraft player (Crafts.Item ind)

    

let GetDirection (keyPressed: ConsoleKey) (player : Player)=
    match keyPressed with
    | ConsoleKey.UpArrow -> if (ValidMove 0 -1  player) then (MovePlayer 0 -1 player)
    | ConsoleKey.DownArrow -> if (ValidMove 0 1 player) then (MovePlayer 0 1 player)
    | ConsoleKey.RightArrow -> if (ValidMove 1 0  player) then (MovePlayer 1 0 player)
    | ConsoleKey.LeftArrow -> if (ValidMove -1 0  player) then (MovePlayer -1 0 player)
    | ConsoleKey.C -> Craft player

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


let rec CanReachBoss() =
    let dx = [|0;0;1;-1|]
    let dy = [|1;-1;0;0|]

    let mutable mask = Array2D.init dimension dimension casilla_bool

    let BFS(x:int,y:int) = 
        let mutable q = []
        let mutable visited = []
        q <- Enqueue q (x,y)

        while not(q.IsEmpty) do
            visited <-AddToList visited q.Head

            for i in [0..3] do
                let mutable xn = fst(q.Head) + dx.[i]
                let mutable yn = snd(q.Head) + dy.[i]
                if (ValidPosition xn yn) && (mask[yn,xn]) then
                    q <- Enqueue q (xn,yn)
                    mask[yn,xn] <- false
                    Console.WriteLine(q)
            
            q <- Dequeue q 
            
        
        visited
    
    let visited = BFS (Bossx,Bossy)
    if visited.Length <=10 then 
        GenerateNewMaze()
        CanReachBoss()
    else
    let ran1 = GetRandom visited.Length
    visited.Item ran1




    




let FightBoss (jugador : Player) = 
    let MonsterAtk = GetRandom jugador.Atk * 2
    let MonsterDef = GetRandom jugador.Def * 2
    let d = MonsterAtk + MonsterDef - jugador.Def - jugador.Atk
    let absd = abs d
    if d>0 then jugador.Damage absd else WonMonsterFight jugador absd


let FoundFountain (jugador:Player) =
    jugador.ResetHealth() 

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
        | Cell.Boss -> FightBoss jugador
        | Cell.Fountain -> FoundFountain jugador
        | _ -> MovePlayer 0 0 jugador
    else MovePlayer 0 0 jugador


// let rec PrintRow (row : array<'a>) (res : string) (ind : int)=
//     if ind.Equals(row.Length) then res
//     else 
//         let a = row.[ind]
//         let b = res + a.ToString() + " "
//         PrintRow row b (ind+1)
    
// let rec PrintMatrix (matrix) (res : string) (ind : int) =
//     if ind.Equals(matrix.GetLength(0)) then res
//     else 
//         let a = matrix.[ind,0]
//         let b = PrintRow a "" 0
//         let c = res + b + "\n"
//         PrintMatrix matrix c (ind+1)


let PrintMaze(player:Player) =
    for i in [0..maze.GetLength(0)-1] do
        Console.WriteLine()
        for j in [0..maze.GetLength(1)-1] do
            if not (player.YPos.Equals(i) && player.Xpos.Equals(j)) then
                Console.Write(" " + ToStringC(maze[i,j]))
            else Console.Write(" ♫")


let PrintBoolMaze() =
    for i in [0..MazeMask.GetLength(0)-1] do
        Console.WriteLine()
        for j in [0..MazeMask.GetLength(1)-1] do
            Console.Write(" " + MazeMask[i,j].ToString())

let PlacePlayer(player:Player) = 
    let (x,y) = CanReachBoss() 
    player.Initialize(x,y)



let SetUpPlayer() = 
    let player = new Player("")
    PlacePlayer player

    Console.Write("Please write the name of your player: \n\n\t --> ")
    let name = Console.ReadLine()

    player
let rec InitialLoop() = 

    //set fountains

    Console.Clear()
    GenerateNewMaze()

    let player = SetUpPlayer()

    InitializeCrafts()
    InteractWithMaze player 

    let mutable k = ConsoleKey.A
    while  ((EndGame k player).Equals(4)) do
        Console.WriteLine(player.ToString())
        PrintMaze(player)
        Console.WriteLine()
        //PrintBoolMaze()
        k <- Console.ReadKey(false).Key
        GetDirection k player
        InteractWithMaze player 
        Console.Clear()

    Console.WriteLine(player.ToString() + "\n\n")

    match EndGame k player with
    |1 -> Console.WriteLine("You pressed Escape!")
    |2 -> Console.WriteLine("You are dead!")
    |3-> Console.WriteLine("You have won!")
    |_ -> Console.WriteLine("Bug?")

    Thread.Sleep(1000)

    let question = "Wanna Play Again?"
    let opt = ["Yes";"No"]
    let ans = PrintMenuWithOptions question opt 0
    if ans.Equals(0) then InitialLoop()

[<EntryPoint>]
let main args = 
    InitialLoop()
    1



