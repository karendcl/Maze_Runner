namespace Game
open Game.Inventory
module Player =

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
