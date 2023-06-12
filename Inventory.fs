namespace Game

open Game.ListMethods

module Inventory =
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
