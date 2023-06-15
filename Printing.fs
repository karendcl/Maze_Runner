namespace Game

open Game.Player
open System
open Game.Maze
open Game.Crafted
open Game.Cell

module Printing =
    

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
    
    let PrintMaze(player:Player) =
        

        for i in [0..maze.GetLength(0)-1] do
            Console.WriteLine(" ")
            for j in [0..maze.GetLength(1)-1] do
                if not (player.YPos.Equals(i) && player.Xpos.Equals(j)) then
                    if (MazeMask[i,j] = false) && not (maze[i,j].Equals(new Wall())) then maze[i,j] <- new Open()
                    Console.Write(" " + maze[i,j].ToString())
                else 
                    let k = Console.ForegroundColor = ConsoleColor.Cyan 
                    Console.Write(" ♫")
                    Console.ResetColor()
            Console.Write(" ")
        Console.WriteLine()
        


    let PrintBoolMaze() =
        for i in [0..MazeMask.GetLength(0)-1] do
            Console.WriteLine()
            for j in [0..MazeMask.GetLength(1)-1] do
                Console.Write(" " + MazeMask[i,j].ToString())