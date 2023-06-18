namespace Game

open Game.ListMethods
open Game.Cell
open Game.Player
open System

module Maze =
    let mutable dimension = 15

    let weightedRandom (items: ('a * int)list) =
        let totalweight = items |> List.sumBy snd
        let randval = GetRandom totalweight
        let rec loop acc = function
            |[] -> failwith "InvalidWeights"
            | (item,weight) :: xs ->
                let acc' = acc + int weight
                if acc' > randval then item
                else loop acc' xs
        loop 0 items

    let casilla_thing x y =
        let i = GetRandom 20

        if x=0 || y=0 || x=dimension-1 || y= dimension-1 then (new Wall() :> Cell)
        else
        let lst = [(0,10);(1,10);(2,3);(3,1);(4,1)]

        let value = weightedRandom lst

        match value with 
            | 0 -> new Wall()
            | 1 -> new Open()
            | 2 -> new Monster()
            | 3 -> new Chest()
            |_ -> new Fountain()

    let mutable maze = Array2D.init dimension dimension (casilla_thing)

    let mutable Bossx = 0
    let mutable Bossy = 0

    let casilla_bool x y =
        let thing = maze[x,y]
        if thing.Equals(new Wall()) || thing.Equals(new Boss()) then false
        else true

    let mutable MazeMask = Array2D.init dimension dimension ( casilla_bool )

    let ResetBoolMaze() = MazeMask <- Array2D.init dimension dimension (casilla_bool)

    let GetInt (question : string)= 
        let rec G (question:string) (ind:int) = 
            Console.Clear()
            Console.WriteLine(question)

            if (ind<15) then G question 15 else

            Console.Write(ind)
            let k = Console.ReadKey().Key

            match k with
            | ConsoleKey.UpArrow -> G question (ind+1)
            | ConsoleKey.DownArrow -> G question (ind-1)
            | ConsoleKey.Enter -> Console.Clear()
                                  ind
            |_ -> G question ind

        G question 0

    let GetDimension() = 
        dimension <- (GetInt "Seleccione la dimension de su laberinto. Use las flechitas \n\n") + 1

    let GenerateNewMaze()=

        let mutable a = Array2D.init dimension dimension (casilla_thing)

        let rec PseudoWhile k Bx By = 
            if k then
                if a[By,Bx].Equals(new Wall()) then
                    PseudoWhile k (GetRandom dimension) (GetRandom dimension)
                else PseudoWhile false Bx By
            else (Bx, By)
        

        let coord = PseudoWhile true (GetRandom dimension) (GetRandom dimension)
        Bossx <- fst(coord)
        Bossy <- snd(coord)
        a[Bossy,Bossx] <- new Boss()
        maze <- a
        ResetBoolMaze()

    let ValidPosition x y = 
            not(x < 0 || y<0 || x >= dimension || y>=dimension || maze[y,x] = new Wall()) 

    let ValidMove xchange ychange (player: Player) = 
            let newpos1 = player.Xpos + xchange
            let newpos2 = player.YPos + ychange

            ValidPosition newpos1 newpos2

    let BFSfromBoss() =
        let dx = [|0;0;1;-1|]
        let dy = [|1;-1;0;0|]

        let mutable mask = Array2D.init dimension dimension casilla_bool

        let BFS(x:int,y:int) = 
            let mutable q = []
            let mutable visited = []
            q <- Enqueue q (x,y)      

            while not(q.IsEmpty) do
                visited <-AddToList visited q.Head

                [0..3] |> List.iter (fun i ->
                    let xn = fst(q.Head) + dx.[i]
                    let yn = snd(q.Head) + dy.[i]
                    if (ValidPosition xn yn) && (mask[yn,xn]) then
                        q <- Enqueue q (xn,yn)
                        mask[yn,xn] <- false
                )
                
                
                q <- Dequeue q 
                    
            visited
        BFS(Bossx,Bossy)



    let rec CanReachBoss() =
        let visited = BFSfromBoss ()
        if visited.Length <=10 then 
            GenerateNewMaze()
            CanReachBoss()
        else
        let ran1 = (GetRandom visited.Length/2)
        let ind = ran1 + visited.Length/2 - 1
        MazeMask[Bossy,Bossx] <- true
        (Remove visited (Bossx,Bossy)).Item ind

    let BossReachAmount()= BFSfromBoss().Length
    
    let InteractWithMaze (jugador:Player)  =
        let a = jugador.Xpos
        let b = jugador.YPos
        let thing = maze[b,a]

        let NewPosition = MazeMask[b,a]
        if NewPosition then
            MazeMask[b,a] <- false
            thing.Interact jugador
