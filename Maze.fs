namespace Game

open Game.ListMethods
open Game.Cell
open Game.Player
open System

module Maze =
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
            | ConsoleKey.Enter -> Console.Clear()
                                  ind
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

    let ValidPosition x y = 
            not(x < 0 || y<0 || x >= dimension || y>=dimension || maze[y,x] = new Wall()) 

    let ValidMove xchange ychange (player: Player) = 
            let newpos1 = player.Xpos + xchange
            let newpos2 = player.YPos + ychange

            ValidPosition newpos1 newpos2

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
        MazeMask[Bossy,Bossx] <- true
        visited.Item ran1

    
    let InteractWithMaze (jugador:Player)  =
        let a = jugador.Xpos
        let b = jugador.YPos
        let thing = maze[b,a]

        let NewPosition = MazeMask[b,a]
        if NewPosition then
            MazeMask[b,a] <- false
            thing.Interact jugador
