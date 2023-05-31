type Player =
    abstract Name : string
    //abstract Health : int
    //abstract Attack : int
    //abstract Defense : int
    //abstract Inventory : Item list
    abstract Position : int * int
    abstract Move : int * int -> Player