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

let player = new Player()
player.Initialize(10,10,"hello")
player.Print
player.MoveX 5
player.Print



//working with the maze now
type Cell = Wall | Open 

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
    let v = GetRandom 1
    match v with
    | 0 -> Cell.Wall
    | _ -> Cell.Open

// let CreateMaze ( maze : Cell array array) =
//     for row in maze do
//         for place in row do
//           place <- SetRandomCell
    

Console.WriteLine(maze.[1].[1])




//getting input from user
// let rec GetDirection =
//     let keyPressed = Console.ReadKey()
//     match keyPressed.Key with
//     | upp -> (0,-1)
//      | down -> (0,1)
//      | right -> (1,0)
//     | left -> (0,-1)
//      |_ -> GetDirection 