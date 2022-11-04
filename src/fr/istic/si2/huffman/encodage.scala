package fr.istic.si2.huffman

object Encodage {

  /**
   * @param c un caractère
   * @param h un arbre de Huffman
   * @return l'encodage de c, selon h, s'il existe
   */
  def encodeSymbol(c: Char, h: Huffman): Option[List[Bit]] = {
    h match{
      case Feuille(freq,ch) => if(c!=ch) None else Some(Nil)
      // XXX : Remarque, les variables "inutiles" (telles que freq) peuvent être remplacées par _
      case Noeud(freq,zero,one) => 
        encodeSymbol(c,zero) match{
          case Some(lb) => Some(Zero::lb)
          case None => encodeSymbol(c,one) match{
            case None => None
            case Some(lb) => Some(One::lb)
        }
      }
    }
  }
  

  /**
   * @param l une liste de caractères
   * @param h un arbre de Huffman
   * @return la séquence de bits correspondants à
   *         l'encodage selon h des éléments de l, s'il a réussi.
   *         Les caractères pour lesquels l'encodage est impossible sont oubliés
   */
  def encodeList(l: List[Char], h: Huffman): List[Bit] = {
    l match{
      case Nil => Nil
      case c::l =>
        encodeSymbol(c,h) match{
          case None => encodeList(l, h)
          case Some(lb) => lb ++ encodeList(l, h)
        }
    }
  }
  

  /**
   * @param s une chaîne de caractères
   * @param h un arbre de Huffman
   * @return l'encodage de s, selon h, en une liste de bits.
   *         (concaténation de l'encodage de chaque caractère de s selon h)
   */
  def encode(s: String, h: Huffman): List[Bit] = {
    encodeList(s.toList, h)
  }

}