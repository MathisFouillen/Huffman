package fr.istic.si2.huffman

object Decodage {

  /**
   * @param h un arbre de Huffman
   * @param l une liste de bits
   * @return caractère correspondant au décodage de l selon h
   */
  def decodeSymbolv0(h: Huffman, l: List[Bit]): Option[Char] = {
    (h,l) match{
      case (Feuille(freq,c),Nil) => Some(c)
      // XXX : Remarque, les variables "inutiles" (telles que freq) peuvent être remplacées par _
      // Cela permet de mieux voir au premier coup d'oeil ce qui est important
      case (Noeud(freq,zero,one), Zero::l) => decodeSymbolv0(zero,l)
      // ex: case (Noeud(_,zero,_), Zero::l) => decodeSymbolv0(zero,l) 
      case (Noeud(freq,zero,one), One::l) => decodeSymbolv0(one,l)
      case _ => None
    }
  }
      
  /**
   * @param l une liste de bit non vide
   * @return cette liste de bit privé de son dernier bit
   */
  def supprDernierBit(l: List[Bit]): List[Bit] = {
    l match {
      case Nil => sys.error("La liste est vide")
      case b::Nil => Nil
      case b::lb => b::supprDernierBit(lb)
    }
  }
  
  /**
   * @param pref une liste de Bit "préfixe" de lb
   * @param lb une liste de Bit
   * @return la sous-liste de lb située après le préfixe pref 
   */
  def suppPrefixe(pref: List[Bit], lb: List[Bit]): List[Bit] = {
    (pref, lb) match {
      case (Nil, Nil)           => Nil
      case (Nil, lb)            => lb
      case (pref, Nil)          => sys.error("Le préfixe est plus long que la séquence")
      case (p :: pref, b :: lb) => suppPrefixe(pref, lb)
    }
  }
  
    /**
   * @param h un arbre de Huffman
   * @param l une liste de bits
   * @return un tuple de taille 2
   *         - première composante : caractère correspondant au décodage selon h d'un préfixe de l 
   *         - deuxième composante : la liste des bits qui ont été décodés
   */
  def prefixe(h: Huffman, l: List[Bit]): (Option[Char],List[Bit]) = {
    l match{
      case Nil => (None,Nil)
      case _ => decodeSymbolv0(h, l) match{
        case None =>
          val lb = supprDernierBit(l)
          prefixe(h, lb)
        case Some(li) => (Some(li),l)
      }
    }
  }
    
  // XXX je suis un peu perdu là:
  // Si vous avez deux fonctions qui ont exactement la même doc, pourquoi y a t il besoin de deux fonctions?
  // La différence devrait apparaître dans les spécifications
  
  
  // XXX Après avoir continué la lecture, vous avez fait bien trop compliqué pour la partie suppression du préfixe
  // Il suffit de supprimer au fur et à mesure du décodage, pas besoin de garder en mémoire tout ce qu'on a décodé pour tout supprimer à la fin
  
  //ex: def decodeSymbol(h: Huffman, l: List[Bit]): (Option[Char], List[Bit]) = {
  //  (h, l) match {
  //    case (Feuille(_, c), _)             => (Some(c), l)
  //    case (Noeud(_, zero, _), Zero :: m) => decodeSymbol(zero, m)
  //    case (Noeud(_, _, one), One :: m)   => decodeSymbol(one, m)
  //    case _                              => (None, l) 
  //  }
  //}
  // A chaque bit décodé, on l'oublie
  
  /**
   * @param h un arbre de Huffman
   * @param l une liste de bits
   * @return un tuple de taille 2
   *         - première composante : caractère correspondant au décodage selon h d'un préfixe de l 
   *         - deuxième composante : la liste des bits restant à décoder
   */
  def decodeSymbol(h: Huffman, l: List[Bit]): (Option[Char], List[Bit]) = {
    (prefixe(h,l)) match{
      case(c,lb) => (c,suppPrefixe(lb,l))
    } 
  }

  /**
   * @param l une liste de bits
   * @param h un arbre de Huffman
   * @return la chaîne correspondant au décodage de l, selon h, si elle existe
   * @note les bits incorrects sont ignorés
   */
  def decode(l: List[Bit], h: Huffman): Option[String] = {
    // XXX Il faut gérer le cas où l est Nil à part (on ne veux pas essayer de décoder quoi que ce soit dans ce cas)
    decodeSymbol(h, l) match{
      case (None, _) => None
      case (Some(c),l) => decode(l, h) match{
        case None => Some(c.toString())
        case Some(str) => Some(c.toString() + str)
      }
    }
  }

}