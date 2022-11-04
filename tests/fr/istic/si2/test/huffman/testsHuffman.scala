package fr.istic.si2.test.huffman

import org.junit.Test
import org.junit.Assert._
import fr.istic.si2.huffman.Encodage._
import fr.istic.si2.huffman.Decodage._
import fr.istic.si2.huffman.ConstructionCode._

import fr.istic.si2.huffman._

class TestsHuffman {


  /**
   * Test qui illustre l'utilisation de l'assertion
   * assertEquals sur les Double
   */
  @Test
  def testEgaliteDouble() {
    // Exemple de problème d'imprécision sur les Double
    assertNotEquals(2.97, 2.8 + 0.17)

    // Problème résolu en utilisant une assertion spéciale à 3 paramètres Double
    assertEquals(2.97, 2.8 + 0.17, 0.01)

    // Selon la précision utilisée, deux Double sont soient égaux, soit différents
    assertEquals(0.001, 0.002, 0.001)
    assertNotEquals(0.001, 0.002, 0.0001)
  }
  

  // On vous fournit également une fonction auxiliaire permettant de
  // définir des tests pour contrôler l'"égalité" de deux arbres de Huffman.
  // Vous pouvez l'utiliser dans vos tests comme n'importe quelle autre assertion.

  /**
   * Vérifie que h1 et h2 sont égaux, en comparant les fréquences Double
   * avec la précision d. Echoue si ce n'est pas le cas.
   * @param h1 un arbre de Huffman
   * @param h2 un arbre de Huffman
   * @param d un double
   */
  def assertEqualsHuffman(h1: Huffman, h2: Huffman, d: Double): Unit = {
    (h1, h2) match {
      case (Feuille(f1, c1), Feuille(f2, c2)) => {
        assertEquals(f1, f2, d);
        assertEquals(c1, c2)
      }
      case (Noeud(f1, h11, h12), Noeud(f2, h21, h22)) =>
        assertEquals(f1, f2, d);
        assertEquals(h11, h21);
        assertEquals(h12, h22)
      case _ => fail("Les deux arbres n'ont pas la même structure")
    }
  }
 

  val h: Huffman = Noeud(1.00,Feuille(0.45,'a'),Noeud(0.54,Feuille(0.18,'r'),Noeud(0.36,Noeud(0.18,Feuille(0.09,'c'),Feuille(0.09,'d')),Feuille(0.18,'b'))))
  
  /**
   * Test d'encodage d'un caractère V0
   */
  @Test
  def testEncodeSymbol() {
    assert(encodeSymbol('t', h) == None)
    assert(encodeSymbol('c',Feuille(1,'a')) == None)
    assert(encodeSymbol('a',h) == Some(List(Zero)))
    assert(encodeSymbol('c',h) == Some(List(One,One,Zero,Zero)))  
  }
  
  /**
   * Test d'encodage d'une liste de caractères v1
   */
  @Test
  def testEncodeList() {
    assert(encodeList(Nil, h) == Nil)
    assert(encodeList(List('g','z'),h) == Nil)
    assert(encodeList(List('a'),Feuille(1,'c')) == Nil)
    assert(encodeList(List('a','c','g','b'),h) == List(Zero,One,One,Zero,Zero,One,One,One))
  }
  
  /**
   * Test d'encodage d'une chaîne de caractères v1
   */
  @Test
  def testEncode(){
    assert(encode("", h) == Nil)
    assert(encode("zgjz", h) == Nil)
    assert(encode("ab", Feuille(1,'k')) == Nil)
    assert(encode("argdczb",h) == List(Zero,One,Zero,One,One,Zero,One,One,One,Zero,Zero,One,One,One))
  }
  
  /**
   * Test de décodage d'un caractère v0
   */
  @Test
  def decodeSymbolv0Test(){
    assert(decodeSymbolv0(h, Nil) == None)
    assert(decodeSymbolv0(h,List(Zero,Zero)) == None)
    assert(decodeSymbolv0(Noeud(1,Feuille(0.8,'c'),Feuille(0.2,'d')),List(Zero,Zero)) == None)
    assert(decodeSymbolv0(h,List(One,One,Zero,One)) == Some('d'))
  }

  /**
   * Test de décodage d'un symbol v1
   */
  @Test
  def decodeSymbolTest(){
    assert(decodeSymbol(h,Nil) == (None,Nil))
    assert(decodeSymbol(h,List(Zero)) == (Some('a'),Nil))
    assert(decodeSymbol(Noeud(1,Feuille(0.8,'r'),Feuille(0.2,'g')),List(One,Zero)) == (Some('g'),List(Zero)))
    assert(decodeSymbol(h, List(One,One,Zero,Zero)) == (Some('c'),Nil))
    assert(decodeSymbol(h, List(One,One,Zero)) == (None,List(One,One,Zero)))
    assert(decodeSymbol(h,List(One,Zero,Zero,One,One,Zero)) == (Some('r'),List(Zero,One,One,Zero)))
  }
  
  /**
   * Test de décodage vers une chaîne de caractère v1
   */
  @Test
  def decodeTest(){
    assert(decode(Nil,h) == None)
    assert(decode(List(One,One),h) == None)
    assert(decode(List(One),Feuille(1,'c')) == None)
    assert(decode(List(One,One,Zero,One,Zero,One,One,Zero),h) == Some("da"))
  }
  
  val lfreqs: List[(Char, Double)] = List(('a', 0.25), ('b', 0.21), ('c', 0.18), ('d',0.14), ('e', 0.09), ('f', 0.07), ('g', 0.06))
  val lhuffman: List[Huffman] = 
    List(Feuille(0.25,'a'),Feuille(0.21,'b'),Feuille(0.18,'c'),Feuille(0.14,'d'),Feuille(0.09,'e'),Feuille(0.07,'f'),Feuille(0.06,'g'))
    

  /**
   * Test de la fonction initHuffman de la v2
   */
  @Test
  def initHuffmanTest(){
    assert(initHuffman(Nil) == Nil)
    assert(initHuffman(lfreqs) == lhuffman)
  }
  
  /**
   * Test de la fonction triSelonFreq de la v2
   */
  @Test
  def triSelonFreqTest(){
    assert(triSelonFreq(Nil) == Nil)
    assert(triSelonFreq(List(Feuille(0.4,'c'),Noeud(0.5,Feuille(0.2,'g'),Feuille(0.3,'z')),Feuille(0.1,'a'))) ==
      List(Feuille(0.1,'a'),Feuille(0.4,'c'),Noeud(0.5,Feuille(0.2,'g'),Feuille(0.3,'z'))))
      
    assert(triSelonFreq(List(Feuille(0.5,'c'),Noeud(0.2,Feuille(0.1,'g'),Feuille(0.1,'z')),Feuille(0.3,'a'))) ==
      List(Noeud(0.2,Feuille(0.1,'g'),Feuille(0.1,'z')), Feuille(0.3,'a'), Feuille(0.5,'c')))
      
    assert(triSelonFreq(lhuffman) ==
      List(Feuille(0.06,'g'),Feuille(0.07,'f'),Feuille(0.09,'e'),Feuille(0.14,'d'),Feuille(0.18,'c'),Feuille(0.21,'b'),Feuille(0.25,'a')))
  }
  
  /**
   * Test de la fonction uneFusion de la v2
   */
  @Test
  def uneFusionTest(){
    assert(uneFusion(List(Feuille(0.4,'c'),Noeud(0.5,Feuille(0.2,'g'),Feuille(0.3,'z')),Feuille(0.1,'a'))) ==
      List(Noeud(0.5,Feuille(0.1,'a'),Feuille(0.4,'c')),Noeud(0.5,Feuille(0.2,'g'),Feuille(0.3,'z'))))
      
    assert(uneFusion(List(Feuille(0.5,'c'),Noeud(0.2,Feuille(0.1,'g'),Feuille(0.1,'z')),Feuille(0.3,'a'))) ==
      List(Noeud(0.5,Noeud(0.2,Feuille(0.1,'g'),Feuille(0.1,'z')),Feuille(0.3,'a')),(Feuille(0.5,'c'))))
      
    assert(uneFusion(lhuffman) == 
      List(Noeud(0.13,Feuille(0.06,'g'),Feuille(0.07,'f')), Feuille(0.09,'e'), Feuille(0.14,'d'), Feuille(0.18,'c'), Feuille(0.21,'b'), Feuille(0.25,'a')))
  }
  
  /**
   * Test de la focntion fusion de la v2
   */
  @Test
  def fusionTest(){
    assert(fusion(List(Noeud(1,Feuille(0.4,'c'),Feuille(0.6,'a')))) == Noeud(1,Feuille(0.4,'c'),Feuille(0.6,'a')))
    
      val h1: Huffman = fusion(lhuffman)  
      
      h1 match{
        case Noeud(1,
          Noeud(0.43,Feuille(0.21,'b'),Noeud(0.22,Feuille(0.09,'e'),Noeud(0.13,Feuille(0.06,'g'),Feuille(0.07,'f')))),
          Noeud(n,Feuille(0.25,'a'),Noeud(0.32,Feuille(0.14,'d'),Feuille(0.18,'c')))) => assertEquals(n,0.57,0.01)
        case _ => fail
    }
    
    //assertEqualsHuffman(h2,fusion(lhuffman),0.1)
  }
  
  /**
   * Test de la fonction code Huffman de la v2
   */
  @Test
  def codeHuffmanTest(){
    val h1: Huffman = codeHuffman(lfreqs)
      h1 match{
        case Noeud(1,
          Noeud(0.43,Feuille(0.21,'b'),Noeud(0.22,Feuille(0.09,'e'),Noeud(0.13,Feuille(0.06,'g'),Feuille(0.07,'f')))),
          Noeud(n,Feuille(0.25,'a'),Noeud(0.32,Feuille(0.14,'d'),Feuille(0.18,'c')))) => assertEquals(n,0.57,0.01)
        case _ => fail
    }
    //assertEqualsHuffman(codeHuffman(lfreqs),h2,0.1)
  }
  
  /**
   * Test de la fonction analyseFrequences de la v2
   */
  @Test
  def analyseFrequencesTest(){
    assert(analyseFrequences("") == Nil)
    assert(analyseFrequences("test") == List(('e',0.25),('s',0.25),('t',0.5)))
    val anaFreq: List[(Char, Double)] = analyseFrequences("Ceci est un test")
    anaFreq match{
      case List(('c',fc),('i',fi),('u',fu),('n',fn),('e',fe),('s',fs),('t',ft)) => {
        assertEquals(fc, 0.1538, 0.001)
        assertEquals(fi, 0.0769, 0.001)
        assertEquals(fu, 0.0769, 0.001)
        assertEquals(fn, 0.0769, 0.001)
        assertEquals(fe, 0.2307, 0.001)
        assertEquals(fs, 0.1538, 0.001)
        assertEquals(ft, 0.2307, 0.001)
      }
      case _ => fail
    }
  }
}
