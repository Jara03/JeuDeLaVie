object yow {
  def main(args: Array[String]): Unit = {

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





  def coordsMin(grille:Grille) : (Int,Int) = {
    def aux(g:Grille, min:(Int,Int)): (Int,Int) = {
      if(g.isEmpty)
        min
      else
      if(g.head._1 < min._1)
        aux(g, (g.head._1,min._2))
      else if(g.head._2 < min._2)
        aux(g.tail, (min._1,g.head._2))
      else
        aux(g.tail, min)
    }
    if(grille.nonEmpty)
      aux(grille, grille.head)
    else
      (0,0)
  }

  def coordsMax(grille:Grille) : (Int,Int) = {
    def aux(g:Grille, max:(Int,Int)): (Int,Int) = {
      if(g.isEmpty)
        max
      else
      if(g.head._1 > max._1)
        aux(g, (g.head._1,max._2))
      else if(g.head._2 > max._2)
        aux(g.tail, (max._1,g.head._2))
      else
        aux(g.tail, max)

    }
    if(grille.nonEmpty)
      aux(grille, grille.head)
    else
      (0,0)
  }

  def afficherGrille(g: Grille): Unit = {
    val (xMax, yMax) = coordsMax(g)
    val (xMin, yMin) = coordsMin(g)
    println("")
    def afficherChar(b: Boolean) = {

      if (b) {
        print("X")
      } else {
        print(" ")
      }
      print(" | ")
    }

    def affcherLigne(n: Int): Unit = {
      if (n < yMax) {
        affcherLigne(n + 1)
      }
    }

    def afficherCoords(x: Int, y: Int): Unit = {
      afficherChar(g.contains((x, y)))
      if (y < yMax) {
        afficherCoords(x, y + 1)
      } else {
        if (x < xMax) {
          println()
          affcherLigne(yMin)
          afficherCoords(x + 1, yMin)
        }
      }
    }

    affcherLigne(yMin)
    afficherCoords(xMin, yMin)
    println("")
    affcherLigne(yMin)
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
    @scala.annotation.tailrec
    def aux1(grille: Grille, acc: Grille): Grille = grille match {
      case t::q if(aux2(voisines8(t._1, t._2)) == 2) => aux1(q, acc)
      case t::q if(aux2(voisines8(t._1, t._2)) == 3) => aux1(q, acc)
      case t::q => aux1(q, acc:::t::Nil)
      case Nil => acc
    }

    def aux2(l:List[(Int, Int)]): Int = {
      l.intersect(g).length
    }

    aux1(g, List[(Int, Int)]())
  }


  def naissances(g: Grille): Grille ={
    @scala.annotation.tailrec
    def aux1(grille: Grille, acc: Grille): Grille = grille match {
      case t::q if(aux2(candidate(g)) == 3) => aux1(q, acc:::t::Nil)
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
  def jeuDeLaVie(init: Grille, n: Int): Unit = {
      println("étape suivante : ")
      afficherGrille(init)
      if (n > 0) {
        jeuDeLaVie((survivante(init) ++ naissances(init)), n - 1)
      }
  }



  //-------------------------- Partie 4
  def voisine4(l:Int, c:Int):List[(Int,Int)] = {
    (l, c-1)::(l-1, c)::(l, c+1)::(l+1, c)::Nil
  }

  def naitJdlv(nbVoisines:Int): Boolean = {
    nbVoisines == 3
  }
  def naitFredkin(nbVoisines:Int) : Boolean = {
    nbVoisines == 3 || nbVoisines == 1
  }

  def survitJdlv(nbVoisines:Int): Boolean = {
    nbVoisines == 3 || nbVoisines == 2
  }
  def survitFredkin(nbVoisines:Int) : Boolean = {
    nbVoisines == 2 || nbVoisines == 4
  }


  def survivanteG(g:Grille, voisines:(Int,Int)=>List[(Int,Int)], regles:Int=>Boolean): Grille = {
    @scala.annotation.tailrec
    def aux1(grille: Grille, acc: Grille): Grille = grille match {
      case t::q if (regles(aux2(voisines(t._1, t._2)))) => aux1(q, acc:::t::Nil)
      case _::q => aux1(q, acc)
      case Nil => acc
    }

    def aux2(l:List[(Int, Int)]): Int = {
      l.intersect(g).length
    }

    aux1(g, List[(Int, Int)]())
  }



  def candidateG(g: Grille, voisines:(Int,Int)=>List[(Int,Int)], regles:Int=>Boolean): Grille = {
    @scala.annotation.tailrec
    def aux1(grille: Grille, acc: Grille): Grille = grille match {
      case t::q if (regles(aux2(voisines(t._1, t._2)))) => aux1(q, acc)
      case t::q => aux1(q, acc:::t::Nil)
      case Nil => acc
    }

    def aux2(l:List[(Int, Int)]): Int = {
      l.intersect(g).length
    }

    aux1(g, List[(Int, Int)]())
  }


  def naissancesG(g: Grille, voisines:(Int,Int)=>List[(Int,Int)], regleVie:Int=>Boolean, regleNait:Int=>Boolean): Grille ={
    @scala.annotation.tailrec
    def aux1(grille: Grille, acc: Grille): Grille = grille match {
      case t::q if(regleNait(aux2(candidateG(g, voisines, regleVie)))) => aux1(q, acc:::t::Nil)
      case _::q => aux1(q, acc)
      case Nil => acc
    }

    def aux2(l:List[(Int, Int)]): Int = {
      l.intersect(g).length
    }

    aux1(g, List[(Int, Int)]())
  }

  def moteur(init: Grille, n:Int, voisines:(Int,Int)=>List[(Int,Int)], regleVie:Int=>Boolean, regleNait:Int=>Boolean): Unit ={
    println("étape suivante : ")
    afficherGrille(init)
    if (n > 0) {
      moteur((survivanteG(init, voisines, regleVie) ++ naissancesG(init, voisines, regleVie, regleNait)), n - 1, voisines, regleVie, regleNait)
    }
  }

  def fredkins(): Unit ={
    val l = List("X  ",
      " XX",
      "XX ")
    moteur(chaineToGrille(l), 10, voisine4, naitFredkin, survitFredkin)
  }

  def voisineDiag(l:Int, c:Int):List[(Int,Int)] = {
    (l+1, c+1)::(l-1, c+1)::(l-1, c-1)::(l+1, c-1)::Nil
  }

  def diagonalesFredkins(): Unit ={
    val l = List("X  ",
      " XX",
      "XX ")
    moteur(chaineToGrille(l), 10, voisineDiag, naitFredkin, survitFredkin)
  }






}
