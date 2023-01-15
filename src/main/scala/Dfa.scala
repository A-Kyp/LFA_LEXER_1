import scala.collection.mutable

class Dfa[A] (start:A, states:Set[A], fin:Set[A], transitions:Set[(A,Char,A)]){

  // The following methods are only the methods directly called by the test suite. You can (and should) define more.

  def map[B](f: A => B) : Dfa[B] = {
    var start_B: B = f(start)
    var fin_B: Set[B] = fin.map(x => f(x))

    var states_B: Set[B] = Set()
    states.foreach(s => states_B = states_B + f(s))

    var transitions_B: Set[(B, Char, B)] = Set()
    transitions.foreach{case (crt, symbol, next) =>
      var tr:(B, Char, B) = (f(crt), symbol, f(next))
      transitions_B = transitions_B + tr}

    return new Dfa[B](start_B, states_B, fin_B, transitions_B)
  }

  def next(state:A, c: Char): A = {
    transitions.foreach{ case (crt, symbol, next) =>
      if (state == crt && c == symbol) {
        return next
      }
    }
//    println("wo qu")
    return state
  }

  def accepts(str: String): Boolean = {
    var len: Int = str.length
    if(len == 0) {
      if(fin.contains(start)) {
        return true
      } else {
        return false
      }
    }

    //var crt: A = next(start, str(0))
    var crt: A = start

    for(i <- 0 until len) {
      var aux:A = crt
//      println("===== " + crt)
      aux = next(crt, str(i))
      if(aux == crt) {
        var ok:Boolean = false;
        transitions.foreach(tr => {
          if(tr._1 == tr._3 && tr._1 == crt && tr._2 == str(i)){
            ok = true
          }
        })
        if(!ok) {
//          println("i am at fault " + aux + crt)
          return false
        }
      }
      crt = aux
    }

//    println("judged crt " + crt)
    return isFinal(crt)
  }

  def getStates : Set[A] = {
    return fin ++ states + start
  }

  def getInit: A = {
    return start
  }

  def getTransition: Set[(A,Char,A)] = {
    return transitions
  }

  def getFin: Set[A] = {
    return fin
  }

  def getOther: Set[A] = {
    return states
  }

  def isFinal(state: A): Boolean = {
    return fin.contains(state)
  }
}

// This is a companion object to the Dfa class. This allows us to call the method fromPrenex without instantiating the Dfa class beforehand.
// You can think of the methods of this object like static methods of the Dfa class
object Dfa {
  def fromNfaToSetDfa(n: Nfa[Int]): Dfa[Set[Int]] = {
//        println("nfa init:" + n.getStart);
//        println("nfa other states:" + n.getStates);
//        println("nfa final states:" + n.getFin);
//        println("nfa trans:" + n.getTransitions);

    var g_init: Set[Int] = n.epsClosures(n.getStart)
    var g_other: Set[Set[Int]] = Set(g_init)
    var g_fin: Set[Set[Int]] = Set()
    var transitions: Set[(Set[Int], Char, Set[Int])] = Set()

    var a: Set[Char] = n.getAlphabet()
    var crt: Set[Int] = g_init
    var next_crt: Set[Int] = Set()
    var aux_next: Set[Int] = Set()

    var q: mutable.Queue[Set[Int]] = mutable.Queue()
    q.enqueue(crt)

    while (q.nonEmpty) {
      crt = q.dequeue()
      a.foreach(letter => {
        crt.foreach(st => {
          n.getTransitions.foreach { case (x, y, z) =>
            if (x == st && y == letter) {
              next_crt = next_crt + z
            }
          }
        })

        next_crt.foreach(st => {
          aux_next = aux_next ++ n.epsClosures(st)
        })

        next_crt = aux_next
        aux_next = Set.empty

        if (next_crt.nonEmpty) {
          var tr: (Set[Int], Char, Set[Int]) = (crt, letter, next_crt)
          transitions = transitions + tr

          if(!q.contains(next_crt) && !g_other.contains(next_crt)) {
            q.enqueue(next_crt)
            g_other = g_other + next_crt
          }
        }
        //println("nextcrt: " + next_crt);
        next_crt = Set.empty
      })
      //println(q.size)
    }

//    println("g_other: " + g_other)

    g_other.foreach(st => {
      if(st.contains(n.getFin)) {
        g_fin = g_fin + st
      }
    })

    g_other = g_other -- g_fin

    return new Dfa[Set[Int]](g_init, g_other, g_fin, transitions)
  }

  def fromSetDfaToDfa(d: Dfa[Set[Int]]): Dfa[Int] = {
    var aliases: Map[Set[Int], Int] = Map(d.getInit -> 0)
    //println("initial map: " + aliases)
    //println("initial state: " + d.getInit)
    //println("final states " + d.getFin)

    var i: Int = 1
    (d.getStates - d.getInit).foreach(x => {
      aliases = aliases + (x -> i)
      i += 1
    })

    //println("mapare: " + aliases)
    //println("stari: " + d.getStates)

    var other: Set[Int] = Set()
    for(j <- 1 to d.getOther.size) {
      other = other + j
    }

    var trans: Set[(Int, Char, Int)] = Set()
    d.getTransition.foreach{case (x, y, z) => {
      var aux: (Int, Char, Int) = (aliases(x), y, aliases(z))
      trans = trans + aux
    }}

    var fin: Set[Int] = Set()
    d.getFin.foreach(x => {
      fin = fin + aliases(x)
    })

    return new Dfa[Int](0, other, fin, trans)
  }

  def fromPrenex(str: String): Dfa[Int] = {
//    var DFA: Dfa[Set[Int]] = fromNfaToSetDfa(Nfa.fromPrenex(str))
    var DFA: Dfa[Set[Int]] = fromNfaToSetDfa(Nfa.fromPrenex(str))
    var dfa:Dfa[Int] = fromSetDfaToDfa(DFA)
//    println("init " + dfa.getInit)
//    println("fin: " + dfa.getFin)
//    println("other: " + dfa.getOther)
//    println("transition: " + dfa.getTransition)
//    println("-------------------");


    return dfa
  }

  // You can add more methods to this object
}
