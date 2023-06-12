
namespace Game

open System
open System.Threading
open Game.ListMethods
open Game.Crafted
open Game.Player
open Game.Cell
open Game.Maze
open Game.Printing


module Main = 
    let EndGame (k: ConsoleKey) (player:Player)=
        if k.Equals(ConsoleKey.Escape) then 1
        else if player.HealthLevel <= 0 then 2
        else if maze[player.YPos, player.Xpos].Equals(new Boss()) && not MazeMask[player.YPos, player.Xpos] then 3
        else 4

    let MovePlayer (x:int)  (y:int) (jugador:Player) =
        jugador.MoveX x
        jugador.MoveY y

    let Craft (player : Player) = 
        let (ind, bo) = PrintCraftMenu player
        if bo.Equals(true) then TryCraft player (Crafts.Item ind)

        
    let UsePotion (player: Player)=
        let func (ob:CraftedObject) = ob.Name
        let rec GetOptions (lstres : list<CraftedObject>) (lstopt: list<CraftedObject>) =
            if lstopt.IsEmpty then lstres else
            if Contains player.InventoryCopy (lstopt.Head).Name then GetOptions (AddToList lstres lstopt.Head) (lstopt.Tail)
            else GetOptions lstres lstopt.Tail
        let op = GetOptions [] Crafts
        if op.IsEmpty then Console.Clear()
                           Console.WriteLine("You do not have any potions in your inventory. Get To Work!")
                           Thread.Sleep(2000)
        else
        let ind = PrintMenuWithOptions "Select the potion you wish to use" (FromArrayToList(Array.map(fun x -> func x)(FromListToArray(op)))) 0
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
            //Console.WriteLine(player.ToString())
            PrintMaze(player)
            //Console.WriteLine()
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



