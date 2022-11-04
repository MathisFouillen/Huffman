package fr.istic.si2.huffman

import fr.istic.si2.huffman.HuffmanApp0._

object ConstructionCode {

  /**
   * @param l une liste de couples caractère/fréquence
   * @return la liste des arbres de Huffman réduits à des feuilles,
   *         un pour chaque élément de l
   */
  def initHuffman(l: List[(Char, Double)]): List[Huffman] = {
    l match{
      case Nil => Nil
      case (c,f)::l => Feuille(f,c)::initHuffman(l)
    }
  }

  /**
   * @param l une liste d'arbres de Huffman
   * @return la liste des éléments de l, classée par ordre croissant des fréquences aux racines
   */
  def triSelonFreq(l: List[Huffman]): List[Huffman] = {
    l match{
      case Nil => Nil
      case h::Nil => h::Nil
      case h::l => insertion(triSelonFreq(l), h)
    }
  }
  
  /**
   * @param l une liste d'arbres de Huffman triée
   * @param h un arbre de Huffman
   * @return la liste l avec h triée
   */
  def insertion(l: List[Huffman], h:Huffman): List[Huffman] = {
    (l,h) match{
      case (Nil,h) => List(h)
      case (Feuille(f, c)::l, Feuille(f1, c1)) => if(f1<f) h::Feuille(f, c)::l else Feuille(f, c)::insertion(l,h)
      case (Feuille(f, c)::l, Noeud(f1, zero, out)) => if(f1<f) h::Feuille(f, c)::l else Feuille(f, c)::insertion(l,h)
      case (Noeud(f, zero, one)::l, Feuille(f1,c1)) => if(f1<f) h::Noeud(f, zero, one)::l else Noeud(f, zero, one)::insertion(l,h)
      case (Noeud(f, zero, one)::l, Noeud(f1, zero1, one1)) => if(f1<f) h::Noeud(f, zero, one)::l else Noeud(f, zero, one)::insertion(l,h)
    }
  }
  // XXX pour toute cette partie, une fonction auxiliaire "fréquence" rendrait tout le code plus facile à lire et à écrire

  /**
   * @param freqs une liste de couples caractère/fréquence
   * @return l'arbre de code de Huffman correspondant à freqs
   */
  def codeHuffman(freqs: List[(Char, Double)]): Huffman = {
    fusion(initHuffman(freqs))
  }

  /**
   * @param l une liste d'arbres de Huffman, de longueur au moins 2
   * @return la liste obtenue après avoir fusionné les 2 arbres de l de fréquences minimales
   */
  def uneFusion(l: List[Huffman]): List[Huffman] = {
    val lt: List[Huffman] = triSelonFreq(l)
    
    lt match{
      case Feuille(f1,c1)::Feuille(f2, c2)::l => if(f1<f2) Noeud(f1 + f2,Feuille(f1, c1),Feuille(f2, c2))::l else
        Noeud(f1 + f2,Feuille(f2, c2),Feuille(f1, c1))::l
        
      case Noeud(f1,zero,one)::Feuille(f2, c2)::l => if(f1<f2) Noeud(f1 + f2,Noeud(f1, zero, one),Feuille(f2, c2))::l else
        Noeud(f1 + f2,Feuille(f2, c2),Noeud(f1, zero, one))::l
        
      case Feuille(f1,c1)::Noeud(f2, zero, one)::l => if(f1<f2) Noeud(f1 + f2,Feuille(f1, c1),Noeud(f2, zero, one))::l else
        Noeud(f1 + f2, Noeud(f2, zero, one), Feuille(f1, c1))::l
        
      case Noeud(f1,zero1,one1)::Noeud(f2, zero2, one2)::l => if(f1<f2) Noeud(f1 + f2,Noeud(f1, zero1, one1),Noeud(f2, zero2, one2))::l else
        Noeud(f1 + f2,Noeud(f2, zero2, one2),Noeud(f1, zero1, one1))::l
        
      case _ => sys.error("l'arbre est de longueur inférieur à 2")
    }
    // XXX : même remarque que précédemment: avec une fonction auxiliaire fréquence, il n'y aurait pas besoin de différencier chaque couple possible
  }

  /**
   * @param l une liste NON VIDE d'arbres de Huffman.
   * @return l'arbre de Huffman obtenu en fusionnant successivement,
   *         et 2 par 2, les arbres de l de fréquences minimales
   */
  def fusion(l: List[Huffman]): Huffman = {
    l match{
      case Nil => sys.error("La liste est vide")
      case h::Nil => h
      case _ => fusion(uneFusion(l))
    }
  }
 
  
  /**
   * @param c un caractère
   * @param l une liste de caractères
   * @return le nombre de fois où c apparaît dans l
   */
  def nbrApparition(c: Char, l:List[Char]): Double = {
    l match{
      case Nil => 0
      case ch::l => if(ch==c) 1 + nbrApparition(c,l) else nbrApparition(c,l)
    }
  }
  
  /**
   * @param l une liste de caractères
   * @return le nombre de caractères qui la compose sans les espaces
   */
  def len(l:List[Char]):Double = {
    l match{
      case Nil => 0
      case ' '::l => len(l)
      case c::l => 1 + len(l)
    }
  }

   /**
   * @param c un caractère
   * @param l une liste de caractères
   * @return la fréquence avec laquelle c apparaît dans l
   */
  def freq(c: Char, l:List[Char]): Double = {
    nbrApparition(c,l)/len(l)
  }
  
  /**
   * @param l une liste de caractères
   * @return la liste des caractères de l (sans doublons) sans les espaces
   */
  def alphabet(l:List[Char]):List[Char] = {
    l match{
      case Nil => Nil
      case ' '::l => alphabet(l)
      case c::l => if(nbrApparition(c,l)==0) c::alphabet(l) else alphabet(l)
    }
  }
 
  
  /**
   * @param l une liste de caractères
   * @param al une liste de caractères
   * @return la fréquence de tout les caractères de al dans l
   */
  def alphabetFreq(l:List[Char], al:List[Char]):List[(Char, Double)] = {
   al match{
     case Nil => Nil
     case c::al => (c,freq(c,l))::alphabetFreq(l,al)
   }
  }
  
  
  /**
   * @param s une chaîne de caractères
   * @return la liste des couples (caractère, fréquence d'apparition),
   *         calculée à partir de s. Chaque élément couple (c, f) est tel que
   *         c est un caractère apparaissant dans s, et f est sa fréquence
   *         d'apparition dans s.
   */
  def analyseFrequences(s: String): List[(Char, Double)] = {
    val l: List[Char] = s.toLowerCase().toList
    alphabetFreq(l, alphabet(l))
  }

}