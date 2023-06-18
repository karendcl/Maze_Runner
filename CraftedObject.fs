namespace Game
open Game.Player
open Game.ListMethods
open System.IO

module Crafted =
    let Resources = ["Red_Herb";"Green_Herb";"Yellow_Herb"]


    type CraftedObject(Name: string, Recipe : string list) = 
        member this.Name = Name
        member this.Recipe = Recipe

        member this.Use(player:Player) = 
            match this.Name with 
            | "HealthPotion" -> player.RegenerateHealth 50 
                                player.RemoveFromInventory "HealthPotion"
            | "AttackPotion" -> player.IncreaseAttack()
                                player.RemoveFromInventory "AttackPotion"
            | "DefensePotion" -> player.IncreaseDefense()
                                 player.RemoveFromInventory "DefensePotion"
            | _ -> player.MoveX 0

        member this.GetName() = Name

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

    let readlines (filepath :string) = 
        use sr = new StreamReader(filepath)

        let rec CreateRes (currentlist:string list) =
            if sr.EndOfStream then currentlist else CreateRes (AddToList currentlist (sr.ReadLine()))

        CreateRes []
        


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

            new CraftedObject(name, (FromArrayToList b))

    
    let InitializeCrafts() = 
        //read txt Format: <Craft Name> = <Resource>,<Resource>,<Resource>,..., <Resource>
        let filepath = __SOURCE_DIRECTORY__ + "\Crafts.txt"
        let AllRecipes = readlines filepath
        List.map(fun x -> CreateCraftedObject x) (AllRecipes)
        
    let Crafts: CraftedObject list = InitializeCrafts()

    let rec CheckForRecipe (inventoryLst: list<'a>) (recipeLeft :list<'a>)=
            if recipeLeft.Length.Equals(0) then true
            else
                let a = recipeLeft.Head
                if Contains inventoryLst a 
                    then
                        CheckForRecipe (Remove inventoryLst a ) recipeLeft.Tail 
                    else false
    let TryCraft (player:Player) (obje: CraftedObject) =

        if CheckForRecipe player.InventoryCopy obje.Recipe then
            obje.Recipe |> List.iter (player.RemoveFromInventory)
            player.AddToInventory obje.Name
