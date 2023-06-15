namespace Game

open System

module ListMethods =
    let Remove (lista: list<'a>) (item:'a) = 
        let rec RemoveOnce (listaorig : list<'a>) (listares : list<'a>) (item :'a) (found : bool)=
            //si la lista original esta vacia no hay mas que hacer
            if listaorig.Length.Equals(0) then listares
            //si ya se encontro se copia lo que resta
            else
            let a = listaorig.Head
            let b = listaorig.Tail
            if found.Equals(true) 
                then RemoveOnce b (a::listares) item true
            else
                if listaorig.Head.Equals(item) then RemoveOnce b listares item true
                else RemoveOnce b (a::listares) item false
        
        RemoveOnce lista [] item false

    let rec Contains (lista : list<'a>) (x : 'a) =
            if lista.IsEmpty then false elif lista.Head.Equals(x) then true else Contains lista.Tail x

    let rec Clone (listorig : list<'a>) (listres:list<'a>) = 

        if listorig.Length.Equals(0) then listres
        else 
        let a = listorig.Head
        let b = listorig.Tail
        Clone (b) (a::listres)

    let FromListToArray (lst : list<'a>) = 
        let rec FromListToArray (lst : list<'a>) (ar : array<'a>) (ind  : int)=
                if ind < 0 then ar 
                else FromListToArray (lst.Tail) (ar.[ind] <- lst.Head; ar) (ind-1)
        FromListToArray lst (Array.zeroCreate lst.Length) (lst.Length-1)

    let FromArrayToList (ar: array<'a>) =
        let rec FromArrayToList (lst : list<'a>) (ar : array<'a>) (ind  : int)=
                if ind < 0 then lst 
                else FromArrayToList (ar.[ind]::lst) (ar) (ind-1)
        FromArrayToList [] ar (ar.Length-1)

    let rec AddToList (lst) (s)= s::lst

    let rec RemoveAll (lista: list<'a>) (item : 'a) = 
        if Contains lista item then 
            RemoveAll (Remove lista item) item
        else lista

    let GroupBy (lista : list<'a>) = 
        let rec GroupByOnce (listaorig : list<'a>) (item:'a) (counter:int)=
            if listaorig.Length.Equals(0) then counter
            else
                let a = listaorig.Head
                if a.Equals(item) then GroupByOnce listaorig.Tail item (counter+1)
                else GroupByOnce listaorig.Tail item counter

        let rec GroupByAll (listaorig : list<'a>) (listares : list<'a * int>) =
            if listaorig.Length.Equals(0) then listares
            else
                let a = listaorig.Head
                let c = GroupByOnce listaorig a 0
                GroupByAll (RemoveAll listaorig a) ((a,c)::listares)
        
        GroupByAll lista [] 


    let Reverse (lst :list<'a>) = 
        let rec Rev acc = function
            |[] -> acc
            | head ::tail -> Rev (head::acc) tail
        
        Rev [] lst

    let Enqueue (q : list<'a>) (item:'a) = 
        let a = Reverse q
        let b = AddToList a item
        b |> Reverse
        
       // Reverse (AddToList (Reverse q) item)
        
    let Dequeue (lst : list<'a>) = lst.Tail

    let GetRandom x = 
        let rnd = new Random()
        rnd.Next(x)
