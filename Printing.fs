namespace Game

open Game.Player
open System
open Game.Maze
open Game.Crafted
open Game.Cell
open Game.ListMethods

module Printing =
    

    let rec PrintMenuWithOptions (header : string)(a : list<'a>) (ind :int)=
        if a.Length.Equals(0) then -1
        else
        Console.Clear()
        Console.WriteLine(header)

        if ind < 0 then (PrintMenuWithOptions header a (a.Length-1)) elif ind > (a.Length-1) then (PrintMenuWithOptions header a 0) 
        else 

            for i in [0..a.Length-1] do 
                if i.Equals(ind) then Console.WriteLine("---> " + (a.Item i).ToString())
                else Console.WriteLine( (a.Item i).ToString())
            
            let k = Console.ReadKey().Key
            match k with
            | ConsoleKey.DownArrow -> PrintMenuWithOptions header a (ind+1)
            | ConsoleKey.UpArrow -> PrintMenuWithOptions header a (ind-1)
            | ConsoleKey.Enter -> ind
            | _ -> PrintMenuWithOptions header a ind



    let PrintCraftMenu (player:Player) = 
        Console.Clear()
        
        let Funct (a:CraftedObject) = CheckForRecipe player.InventoryCopy a.Recipe

        let options = Crafts |> List.filter(fun x -> Funct x )
        
        if not (options.IsEmpty) 
            then
            let header = "The player's Inventory: \n" + player.ShowInventory() + "\n\n\n These are the possible crafts: " 
            let indexofcraft = PrintMenuWithOptions header options 0
            let thing = options.Item indexofcraft
            let cr = IndexOf Crafts thing
            (cr, true)
            
        else
            Console.WriteLine("You do not have enough materials to craft. Get To Work! \n\n Press Any Key to Continue")
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
                    Console.Write(" ♫")
            Console.Write(" ")
        Console.WriteLine()
        


    let PrintBoolMaze() =
        for i in [0..MazeMask.GetLength(0)-1] do
            Console.WriteLine()
            for j in [0..MazeMask.GetLength(1)-1] do
                Console.Write(" " + MazeMask[i,j].ToString())