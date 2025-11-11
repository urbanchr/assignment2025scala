// Main Part 3 about Regular Expression Matching
//==============================================

object M3 {

enum Rexp {
  case ZERO
  case ONE 
  case CHAR(c: Char) 
  case ALT(r1: Rexp, r2: Rexp) // alternatives 
  case SEQ(r1: Rexp, r2: Rexp) // sequences
  case AND(r1: Rexp, r2: Rexp) // intersections
  case STAR(r: Rexp)           // star
  case REP(r: Rexp, n: Int)    // repetitions
}
import Rexp._


// some convenience for typing some regular expressions

def charlist2rexp(s: List[Char]): Rexp = s match {
  case Nil => ONE
  case c::Nil => CHAR(c)
  case c::s => SEQ(CHAR(c), charlist2rexp(s))
}

import scala.language.implicitConversions

given Conversion[String, Rexp] = (s => charlist2rexp(s.toList))

extension (r: Rexp) {
  def | (s: Rexp) = ALT(r, s)
  def % = STAR(r)
  def ~ (s: Rexp) = SEQ(r, s)
}

// some examples for the conversion and extension:

// val areg : Rexp = "a" | "b"
//  => ALT(CHAR('a'), CHAR('b'))
//
// val sreg : Rexp = "a" ~ "b"
//  => SEQ(CHAR('a'), CHAR('b')) 
//
// val star_reg : Rexp = ("a" ~ "b").%
//  => STAR(SEQ(CHAR('a'), CHAR('b'))) 

// ADD YOUR CODE BELOW
//======================

// (1)
def nullable (r: Rexp) : Boolean = ???

// (2) 
def der (c: Char, r: Rexp) : Rexp = ???

// (3)
def simp(r: Rexp) : Rexp = ???

// (4)
def ders (s: List[Char], r: Rexp) : Rexp = ???
def matcher(r: Rexp, s: String): Boolean = ???

// (5) 
def size(r: Rexp): Int = ???


// Some testing data
//===================

/*

simp(ALT(ONE | CHAR('a'), CHAR('a') | ONE))   
// => ALT(ALT(ONE,CHAR(a)),ALT(CHAR(a),ONE))
simp(((CHAR('a') | ZERO) ~ ONE) | 
     (((ONE | CHAR('b')) | CHAR('c')) ~ (CHAR('d') ~ ZERO)))   
// => CHAR(a)

matcher(("a" ~ "b") ~ "c", "ab")   // => false
matcher(("a" ~ "b") ~ "c", "abc")  // => true


// the supposedly 'evil' regular expression (a*)* b
val EVIL = SEQ(STAR(STAR(CHAR('a'))), CHAR('b'))

matcher(EVIL, "a" * 1000)          // => false
matcher(EVIL, "a" * 1000 ++ "b")   // => true


// size without simplifications
size(der('a', der('a', EVIL)))             // => 28
size(der('a', der('a', der('a', EVIL))))   // => 58

// size with simplification
size(simp(der('a', der('a', EVIL))))           // => 8
size(simp(der('a', der('a', der('a', EVIL))))) // => 8

// Python needs around 30 seconds for matching 28 a's with EVIL. 
// Java 9 and later increase this to an "astonishing" 40000 a's in
// 30 seconds.
//
// Lets see how long it really takes to match strings with 
// 5 Million a's...it should be in the range of a few
// of seconds.

def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  "%.5f".format((end - start)/(i * 1.0e9))
}

for (i <- 0 to 5000000 by 500000) {
  println(s"$i ${time_needed(2, matcher(EVIL, "a" * i))} secs.") 
}

// another "power" test case 
simp(Iterator.iterate(ONE:Rexp)(r => SEQ(r, ONE | ONE)).drop(50).next()) == ONE

// the Iterator produces the rexp
//
//      SEQ(SEQ(SEQ(..., ONE | ONE) , ONE | ONE), ONE | ONE)
//
//    where SEQ is nested 50 times.

*/


}
