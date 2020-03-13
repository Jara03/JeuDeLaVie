object yow {
  def main(args: Array[String]): Unit = {
    val l = List(" XX",
      "  X",
      "XXX")
    afficherGrille(survivante(chaineToGrille(l)))

  }

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

  val string : List [String] = " X"::"X "::Nil
  val grille : Grille = (0,1)::(1,0)::Nil
  assert(chaineToGrille(string)==grille)



  //Affiche la grille à l'écran
  def afficherGrille(g: Grille): Unit = {

    def min(x:(Int, Int), y:(Int, Int)): (Int, Int) = (x,y) match {
      case ((a, b),(c, d)) if (a>=c &&b>=d) => (c,d)
      case ((a, b), (c,_)) if(a >= c) => (c, b)
      case ((a, b), (_, d)) if(b >= d) => (a, d)
      case ((a, b), (_,_)) => (a, b)
    }

    def max(x:(Int, Int), y:(Int, Int)): (Int, Int) = (x,y) match {
      case ((a, b), (c, d)) if(a <= c && b <= d) => (c, d)
      case ((a, b), (c,_)) if(a <= c) => (c, b)
      case ((a, b), (_, d)) if(b <= d) => (a, d)
      case ((a, b), (_,_)) => (a, b)
    }

    val maxCol = g.reduceLeft(max)._2
    val minCol = g.reduceLeft(min)._2
    def afficher(g:Grille, index: (Int, Int)): Unit = (g, index) match {
      case (Nil,_) => print("\n")
      case (grid, (a, b)) if(b > maxCol) => print("\n")
        afficher(grid, (a + 1, minCol))
      case (head::tail, (a, b)) if(head == (a, b)) => print("X")
        afficher(tail, (a, b + 1))
      case (grid, (a, b)) => print(" ")
        afficher(grid, (a, b + 1))
    }
    afficher(g, g.reduceLeft(min))
  }


  //--------------------------------------------------------
  //Partie 3

  //retourne les 8 voisines
  def voisines8(l:Int, c:Int): List[(Int, Int)] = {
      (l, c-1)::(l-1, c-1)::(l-1, c)::(l-1, c+1)::(l, c+1)::(l+1, c+1)::(l+1, c)::(l+1, c-1)::Nil
  }

  //retourne la liste des cellules qui survivent
  // à l'étape suivante selon les règles
  def survivante(g:Grille): Grille = {
    @scala.annotation.tailrec
    def aux1(grille: Grille, acc: Grille): Grille = grille match {
      case t::q if(aux2(voisines8(t._1, t._2)) == 2) => aux1(q, acc:::t::Nil)
      case t::q if(aux2(voisines8(t._1, t._2)) == 3) => aux1(q, acc:::t::Nil)
      case _::q => aux1(q, acc)
      case Nil => acc
    }

    def aux2(l:List[(Int, Int)]): Int = {
      l.intersect(g).length
    }

    aux1(g, List[(Int, Int)]())
  }

  def candidate(g: Grille): Grille = {
    def aux1(grille: Grille, acc: Grille): Grille = grille match {
      case t::q if(aux2(voisines8(t._1, t._2)) == 2) => aux1(q, acc:::t::Nil)
      case t::q if(aux2(voisines8(t._1, t._2)) == 3) => aux1(q, acc:::t::Nil)
      case _::q => aux1(q, acc)
      case Nil => acc
    }

    def aux2(l:List[(Int, Int)]): Int = {
      l.intersect(g).length
    }

    aux1(g, List[(Int, Int)]())
  }

  //grille initialise affiche en fonction d'un nb d'étapes n et
  // affiche n itérations de la simulation
  def jeuDeLaVie(init:Grille, n:Int):Unit = {

  }

}
