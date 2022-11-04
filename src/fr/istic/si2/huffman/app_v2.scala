package fr.istic.si2.huffman

import scala.io.Source
import java.io.{ File, PrintWriter }
import fr.istic.si2.huffman.Encodage._
import fr.istic.si2.huffman.Decodage._
import fr.istic.si2.huffman.ConstructionCode._

/**
 * Application principale
 */
object HuffmanApp extends App {

  
  /**
   * Une liste de couples caractère / fréquence d'apparition
   * à utilisée par l'application principale.
   */
  val lfreqs: List[(Char, Double)] = List(('a', 0.25), ('b', 0.21), ('c', 0.18), ('d',0.14), ('e', 0.09), ('f', 0.07), ('g', 0.06))
  
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
   * @param nom le nom d'un fichier
   * @return la chaîne contenue dans le fichier nommé nom
   */
  def lireFichier(nom: String): String = {
    Source.fromFile(nom).getLines.mkString
  }

  /**
   * Ecrit une chaîne de caractères dans un fichier.
   *  Le fichier est écrasé s'il était déjà existant.
   *
   * @param nom le nom du fichier dans lequel on écrit
   * @param contenu la chaîne de caractères à écrire
   */
  def ecrireFichier(nom: String, contenu: String): Unit = {
    val writer = new PrintWriter(new File(nom))
    writer.write(contenu)
    writer.close()
  }

  
  /**
   * demande à l'utilisateur s'il souhaite mainupuler des fichiers
   * @return vrai ssi l'utilisateur souhaite manipuler les fichiers
   */
  def manipulerFichiers():Boolean = {
    println("Souhaitez-vous manipuler des fichiers ? Entrez oui ou non :")
    val str: String = io.StdIn.readLine()
    str.toLowerCase() match{
      case "oui" => true
      case "non" => false
      case _ => println("L'entrée n'est pas valide"); manipulerFichiers()
    }
  }
  
  /**
   * application principale V2
   * démontre le bon fonctionnement des fonctions de la V2
   * boucle d'interaction avec l'utilisateur pour lire et écrire des fichiers texte
   */
  def appV2():Unit = {
    println("Construction de l'arbre de code de la figure 1 :")
    val h:Huffman = fusion(initHuffman(lfreqs))   
    println("\t" + h)
    
    while(manipulerFichiers()){
      println("Saisir un fichier texte :")
      val txt: String = lireFichier(io.StdIn.readLine())
      val lfreqstxt: List[(Char, Double)] = analyseFrequences(txt)
      val htxt: Huffman = fusion(initHuffman(lfreqstxt))
      
      println("Voici l'analyse des fréquences :")
      println("\t" + lfreqstxt)
      println("Arbre de code correspondant :")
      println("\t" + htxt)
      
      println("Saisir le nom du fichier dans lequel sera écrit l'encodage :")
      ecrireFichier(io.StdIn.readLine(),toString(encode(txt,htxt)))
      println("Le fichier a été enregistré")
    }
  }
  
 appV2()
 
}