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
type Player() =
    let mutable Positionx = 1
    let mutable Positiony = 1
    let mutable Attack = 200
    let mutable Defense = 200
    let mutable Health = 300
    let mutable MaxHealth = 300
    let mutable Name = ""
    let mutable Inventory = new Inventory()

    member this.Initialize(x : int, y: int, name:string)=
        Positionx <- x
        Positiony <- y
        Name <- name
        
      
    member this.GetName() = Name
    member this.MoveY steps = Positiony <- Positiony + steps
    member this.Xpos = Positionx
    member this.YPos =  Positiony
    member this.Atk = Attack
    member this.Def = Defense

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
    
    member this.IncreaseAttack() = 
        Attack <- Attack + 100

    member this.IncreaseDefense() = 
        Defense <- Defense + 100
    member this.AddToInventory x = 
        Inventory.AddToInventory x
            
    member this.RemoveFromInventory x = Inventory.RemoveFromInventory x  

    member this.MoveX steps = Positionx <- Positionx + steps

    member this.InventoryCopy = Inventory.Clone()

    

    override this.ToString() = 
        let inv = Inventory.ToString()
        
        "Nombre: " + Name + "\n" + 
        "Posicion: " + Positionx.ToString() + " " + Positiony.ToString() + "\n" + 
        "Health: " + Health.ToString() + "\n" + 
        "Inventario: " + "\n" + inv + "\n"


type CraftedObject(Name: string, Recipe : string list) = 
    member this.Name = Name
    member this.Recipe = Recipe

    member this.Use(player:Player) = 
        match this.Name with 
        | "HealthPotion" -> player.RegenerateHealth 50 
                            player.RemoveFromInventory this.Name
        | "AttackPotion" -> player.IncreaseAttack()
                            player.RemoveFromInventory this.Name
        | "DefensePotion" -> player.IncreaseDefense()
                             player.RemoveFromInventory this.Name
        | _ -> player.MoveX 0

    

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
    if a.Length.Equals(0) then -1
    else
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


let Resources = ["Red_Herb";"Green_Herb";"Yellow_Herb"]
let GetRandom x = 
    let rnd = new Random()
    rnd.Next(x)
//working with the maze now
//type Cell = Wall | Open | Chest | Monster | Boss | Fountain 

//create abstract class Cell
[<AbstractClass>]
type Cell() = 
    abstract member Interact : Player -> unit
    
let mutable Crafts: CraftedObject list = []

type Wall() = 
    inherit Cell()
    override this.Interact player = 
        player.MoveX 0
    
    override this.ToString() = "▀"
    override this.Equals (obj : obj) = 
        obj.GetType().Equals(typeof<Wall>)

type Open() =
    inherit Cell()
    override this.Interact player = 
        player.MoveX 0
    override this.ToString() = " "
    override this.Equals (obj : obj) = 
        obj.GetType().Equals(typeof<Open>)

type Chest() =
    inherit Cell()
    override this.Interact player =
        let l = Resources.Length
        let ind = GetRandom l
        let drop = Resources.Item ind
        player.AddToInventory drop
    
    override this.ToString() = "C"
    override this.Equals (obj : obj) = 
        obj.GetType().Equals(typeof<Chest>)

type Monster() =
    inherit Cell()
    let Attack = GetRandom 300
    let Defense = GetRandom 300
    override this.Interact player = 
        let a = player.Atk
        let b = player.Def
        let c = Attack
        let d = Defense
        let rec Fight (a : int) (b : int) (c : int) (d : int) =
            if a <= 0 then false
            elif c <= 0 then true
            else
                let e = a - d
                let f = c - b
                Fight e b f d
        let res = Fight a b c d
        if res then 
            player.MoveX 0
            let x = new Chest()
            if GetRandom 2 = 0 then x.Interact player
        else player.Damage 50
    override this.ToString() = "M"
    override this.Equals (obj : obj) = 
        obj.GetType().Equals(typeof<Monster>)

type Boss() =
    inherit Monster()
    let Attack = 500
    let Defense = 500

    override this.ToString() = "B"
    override this.Equals (obj : obj) = 
        obj.GetType().Equals(typeof<Boss>)

type Fountain() =
    inherit Cell()
    override this.Interact player = 
        player.ResetHealth()
    override this.ToString() = "F"
    override this.Equals (obj : obj) = 
        obj.GetType().Equals(typeof<Fountain>)



let mutable dimension = 15



let casilla_thing x y =
    let i = GetRandom 20

    match i with
    //the casting is necessary because the type of the array is Cell. for the program to compile
    |20 -> new Open() :> Cell
    | 0 -> new Wall() 
    | 4 -> new Wall()
    | 5 -> new Wall()
    | 6 -> new Wall()
    | 1 -> new Chest()
    | 2 -> new Fountain()
    | 3 -> new Monster()
    | _ -> new Open()

let mutable maze = Array2D.init dimension dimension (casilla_thing)

let mutable Bossx = 0
let mutable Bossy = 0

let casilla_bool x y =
    let thing = maze[x,y]
    if thing.Equals(new Wall()) || thing.Equals(new Boss()) then false
    else true

    
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
        if a[Bossy,Bossx].Equals(new Wall()) then
            Bossx <- GetRandom dimension
            Bossy <- GetRandom dimension
            
        else k<-false

    a[Bossy,Bossx] <- new Boss()
    maze <- a

    ResetBoolMaze()



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
    else if maze[player.YPos, player.Xpos].Equals(new Boss()) then 3
    else 4

let ValidPosition x y = 
        not(x < 0 || y<0 || x >= dimension || y>=dimension || maze[y,x] = new Wall()) 

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

    
let UsePotion (player: Player)=
    let rec GetOptions (lstres : list<CraftedObject>) (lstopt: list<CraftedObject>) =
        if lstopt.IsEmpty then lstres else
        if Contains player.InventoryCopy (lstopt.Head).Name then GetOptions (AddToList lstres lstopt.Head) (lstopt.Tail)
        else GetOptions lstres lstopt.Tail
    let op = GetOptions [] Crafts
    if op.IsEmpty then Console.WriteLine("You do not have any potions in your inventory. Get To Work!")
                       Thread.Sleep(2000)
    else
    let ind = PrintMenuWithOptions "Select the potion you wish to use" op 0
    let a = op.Item ind
    a.Use(player)
    

let GetDirection (keyPressed: ConsoleKey) (player : Player)=
    match keyPressed with
    | ConsoleKey.UpArrow -> if (ValidMove 0 -1  player) then (MovePlayer 0 -1 player)
    | ConsoleKey.DownArrow -> if (ValidMove 0 1 player) then (MovePlayer 0 1 player)
    | ConsoleKey.RightArrow -> if (ValidMove 1 0  player) then (MovePlayer 1 0 player)
    | ConsoleKey.LeftArrow -> if (ValidMove -1 0  player) then (MovePlayer -1 0 player)
    | ConsoleKey.C -> Craft player
    | ConsoleKey.P -> UsePotion player

    | _ -> MovePlayer 0 0 player 




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
            
            q <- Dequeue q 
            
        
        visited
    
    let visited = BFS (Bossx,Bossy)
    if visited.Length <=10 then 
        GenerateNewMaze()
        CanReachBoss()
    else
    let ran1 = GetRandom visited.Length
    visited.Item ran1


let InteractWithMaze (jugador:Player)  =
    let a = jugador.Xpos
    let b = jugador.YPos
    let thing = maze[b,a]

    let NewPosition = MazeMask[b,a]
    if NewPosition then
        MazeMask[b,a] <- false
        thing.Interact jugador


let PrintMaze(player:Player) =
    for i in [0..maze.GetLength(0)-1] do
        Console.WriteLine()
        for j in [0..maze.GetLength(1)-1] do
            if not (player.YPos.Equals(i) && player.Xpos.Equals(j)) then
                Console.Write(" " + maze[i,j].ToString())
            else Console.Write(" ♫")


let PrintBoolMaze() =
    for i in [0..MazeMask.GetLength(0)-1] do
        Console.WriteLine()
        for j in [0..MazeMask.GetLength(1)-1] do
            Console.Write(" " + MazeMask[i,j].ToString())

let PlacePlayer(player:Player) (name) = 
    let (x,y) = CanReachBoss() 
    player.Initialize(x,y, name)



let rec SetUpPlayer(s:string) = 
    Console.Clear()
    let player = new Player()
    if s.Equals("") then
        Console.Write("Please write the name of your player: \n\n\t --> ")
        SetUpPlayer(Console.ReadLine())
    else
    PlacePlayer player s
    player
     
    

let NewPlayer(s:string)=
    let player = SetUpPlayer(s)
    InteractWithMaze player
    player 

let NewGame() = 
    Console.Clear()
    GenerateNewMaze()

let RestartGame(player:Player)=
    NewGame()
    NewPlayer (player.GetName())

    

let rec InitialLoop(player:Player) = 
    
    Console.Clear()
    let mutable k = ConsoleKey.A
    while  ((EndGame k player).Equals(4)) do
        Console.WriteLine(player.ToString())
        PrintMaze(player)
        Console.WriteLine()
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

    Thread.Sleep(2000)

    let question = "Wanna Play Again?"
    let opt = ["Yes";"No"]
    let ans = PrintMenuWithOptions question opt 0
    if ans.Equals(0) then 
        InitialLoop (RestartGame(player))

let MainMenu()=
    Console.Clear()
    let header = "Welcome to the game! \n\n\n The goal of the game is for you to find your way around the maze, reach the Boss, and win the fight against it.\n To move around the maze you can use the arrows on your keyboard.\n To craft potions you can press the C key.\n To use a potion you can press the P key.\n To exit the game you can press the Escape key\n\n Understood? \n Great! Let's get started!"
    
    for i in header do
        Console.Write(i)
        Thread.Sleep(50)
    
    Console.Clear()
    
    let question = "\n\n Please select an option"
    let opt = ["New Game";"Exit"]
    PrintMenuWithOptions question opt 0
    // if ans.Equals(0) then NewGame()
    // else ()

[<EntryPoint>]
let main args = 
    InitializeCrafts()
    if MainMenu().Equals(0) then 
        NewGame()
        let player = NewPlayer("")
        InitialLoop(player)
    1



