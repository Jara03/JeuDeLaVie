//Partie 1
type Grille = List[(Int, Int)]

//--------------------------------------------------------
//Partie 2

//Convertit une liste de String en Grille
def chaineToGrille(l:List[String]): Grille = {

    def aux1(i : Int, j : Int, g:Grille, l2:List[String]): Grille = l2 match {
        case t::q => aux1(i+1, j, g:::aux2(i, j, t.toList, List[(Int,Int)]()), q)
        case Nil => g
    }
    def aux2(i2 : Int, j2 : Int, c: List[Char], g:Grille):Grille = c match {
        case 'X'::q => aux2(i2, j2+1, q, g:::(i2, j2)::Nil)
        case _::q => aux2(i2, j2+1, q, g)
        case Nil => g
    }

    aux1(0,0,List[(Int,Int)](),l)
}

def main(args: Array[String]): Unit = {
    val string : List [String] = " X"::"X "::Nil
    val grille : Grille = (0,1)::(1,0)::Nil
    assert(chaineToGrille(string)==grille)
}


//Affiche la grille à l'écran
def afficherGrille(g: Grille): Unit = {

}


//--------------------------------------------------------
//Partie 3

//retourne les 8 voisines
def voisines8(l:Int, c:Int): List[(Int, Int)] = {
    (l, c-1)::(l-1, c-1)::(l-1, c)::(l-1, c+1)::(l, c+1)::(l+1, c+1)::(l+1, c)::(l+1, c-1)::Nil
}

//retourne la liste des cellules qui survivent
// à l'étape suivante selon les règles
def survivantes(g:Grille):Grille = {
    g foldLeft () match {
        case voisines8() match {
            case truc = truc
        }
    }

}

//liste des cellules mortes voisines (suceptibles de naître)
def candidates(g:Grille):Grille = {

}

//listes des cellules qui naissent à l'étape suivante
def naissances(g:Grille):Grille = {

}

//grille initialise affiche en fonction d'un nb d'étapes n et
// affiche n itérations de la simulation
def jeuDeLaVie(init:Grille, n:Int):Unit = {

}


