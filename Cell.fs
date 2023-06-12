namespace Game
open Game.Player
open Game.Crafted
open Game.ListMethods

module Cell=
    [<AbstractClass>]
    type Cell() = 
        abstract member Interact : Player -> unit


    type Wall() = 
        inherit Cell()
        override this.Interact player = 
            player.MoveX 0
        
        override this.ToString() = "▀"
        override this.Equals (obj : obj) = 
            obj.GetType().Equals(typeof<Wall>)

        override this.GetHashCode() =  base.GetHashCode()
        
        

    type Open() =
        inherit Cell()
        override this.Interact player = 
            player.MoveX 0
        override this.ToString() = " "
        override this.Equals (obj : obj) = 
            obj.GetType().Equals(typeof<Open>)
        
        override this.GetHashCode() =  base.GetHashCode()

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
        override this.GetHashCode() =  base.GetHashCode()

    type Monster() =
        inherit Cell()
        let Attack = GetRandom 400
        let Defense = GetRandom 400

        let Damage = 100
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
            else player.Damage Damage
        override this.ToString() = "M"
        override this.Equals (obj : obj) = 
            obj.GetType().Equals(typeof<Monster>)
        override this.GetHashCode() =  base.GetHashCode()

    type Boss() =
        inherit Monster()
        let Attack = 700
        let Defense = 700
        let Damage = 200

        override this.ToString() = "B"
        override this.Equals (obj : obj) = 
            obj.GetType().Equals(typeof<Boss>)
        override this.GetHashCode() =  base.GetHashCode()

    type Fountain() =
        inherit Cell()
        override this.Interact player = 
            player.ResetHealth()
        override this.ToString() = "F"
        override this.Equals (obj : obj) = 
            obj.GetType().Equals(typeof<Fountain>)
        override this.GetHashCode() =  base.GetHashCode()



