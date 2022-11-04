package fr.istic.si2.huffman

import scala.io.Source
import fr.istic.si2.huffman.Encodage._
import fr.istic.si2.huffman.Decodage._

/**
 * Application principale V1
 */
object HuffmanApp1 extends App {

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
  
  /**
   * @param s une chaîne de caractères
   * @return la chaîne de 0 et 1 représentant chaque caractère
   *         de s par son encodage sur 16 bits
   */
  def vers16Bits(s: String): String = {
    s.toList.map(c => String.format("%16s", c.toBinaryString).replace(' ', '0')).foldLeft("")((acc,e) => acc + e)
  }

  /*  Indications : vous pourrez utiliser la fonction suivante, qui lit une chaîne entrée
   * au clavier par l'utilisateur, et renvoie cette chaîne comme résultat.
   * def io.StdIn.readLine() : String
   */
  
  /**
   * demande à l'utilisateur s'il souhaite continuer
   * @return vrai ssi l'utilisateur rentre oui
  */
  def continuer(): Boolean = {
    println("Entrer oui pour continuer ou non pour arrêter la boucle")
    val str: String = io.StdIn.readLine()
    str.toLowerCase() match{
      case "oui" => true
      case "non" => false
      case _ => println("L'entrée n'est pas valide"); continuer()
    }
  }    

  /**
   * boucle d'interaction avec l'utilisateur
   * démontre le bon fonctionnement des fonctions de la V1
   */
  def appV1():Unit = {
    do{
      println("Chaîne à encoder ?")
      val str = io.StdIn.readLine()
      val s:String = vers16Bits(str)
      val enc: List[Bit] = encode(str,h)
      val dec: Option[String] = decode(enc,h)
      
      dec match{
        case None => println("Erreur ou caractère(s) non encodable(s)")
        case Some(dec) => 
          if(dec!=str) println("Erreur ou caractère(s) non encodable(s)")
          
          else{
            println("Chaîne encodée standard :")
            println("\t" + s)
            println("\ttaille (nb Bits) : " + s.length())
            
            println("Chaîne encodée de Huffman :")
            println("\t" + toString(enc))
            println("\ttaille (nb Bits) : " + str.length())
            
            println("Chaîne décodée de Huffman :")
            println("\t" + dec)
          }
      }
    }while(continuer())
  }

  appV1()
}