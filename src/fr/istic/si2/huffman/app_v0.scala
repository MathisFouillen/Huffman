package fr.istic.si2.huffman

import scala.io.Source
import fr.istic.si2.huffman.Encodage._
import fr.istic.si2.huffman.Decodage._

/**
 * Type algébrique simple modélisant les bits (0 ou 1)
 */
sealed trait Bit
case object Zero extends Bit
case object One extends Bit

/**
 * Type algébrique récursif modélisant les arbres de code de Huffman
 */
sealed trait Huffman
case class Feuille(freq: Double, c: Char) extends Huffman
case class Noeud(freq: Double, zero: Huffman, one: Huffman) extends Huffman

/**
 * Application principale V0
 */
object HuffmanApp0 extends App {

  /**
   * Arbre de code utilisé par l'application principale
   */
  val h: Huffman = Noeud(1.00,Feuille(0.45,'a'),Noeud(0.54,Feuille(0.18,'r'),Noeud(0.36,Noeud(0.18,Feuille(0.09,'c'),Feuille(0.09,'d')),Feuille(0.18,'b'))))
  
  /**
   * @param l une liste de bits
   * @return la chaîne de caractères où chaque bit de l est représenté par 0 ou 1, dans l'ordre
   */
  def toString(l: List[Bit]): String = {
    l match{
      case Nil => ""
      case Zero::l => "0" + toString(l)
      case One::l => "1" + toString(l)
    }
  }
  
  // Indication : on peut convertir une chaîne de caractères s 
  // en la liste de ses caractères par l’opération s.toList
  
  /**
   * @param h un arbre de Huffman
   * @return la liste des caractères présents dans h
   */
  def alphabet(h: Huffman):List[Char] = {
    h match{
      case Feuille(freq, c) => List(c)
      case Noeud(freq, zero, one) => alphabet(zero) ++ alphabet(one)
    }
  }
  
  /**
   * @param l une liste de chaine de caractère
   * @return une chaîne de caractère, un message détaillant de contenu de l
   */
  def affiche(l: List[Char]): String = {
    l match{
      case Nil => ""
      case c::l => 
        val opL: Option[List[Bit]] = encodeSymbol(c,h)
        opL match{
          case None => "Le caractère n'est pas encodable"
          case Some(lb) => val opC: Option[Char] = decodeSymbolv0(h,lb)
            opC match{
              case None => "Le symbole n'est pas décodable"
              case Some(ch) =>c + " " + lb + " " + toString(lb) + " " + ch + "\n" + affiche(l)
          }
        }
    }
  }
  
  /**
   * enumère tout l’alphabet et affiche sur chaque ligne le caractère, sa liste de bits, la chaîne de code, et la liste de bits décodée
   */
  def appV0():Unit = {
    print(affiche(alphabet(h)))
  }

  appV0()

}


