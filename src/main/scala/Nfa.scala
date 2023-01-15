import scala.collection.mutable

class Nfa[A](start:A, states:Set[A], fin:A, transitions:Set[(A,Char,A)]) {

  // The following methods are only the methods directly called by the test suite. You can (and should) define more.

  def map[B](f: A => B) : Nfa[B] = {
    var start_B: B = f(start)
    var fin_B: B = f(fin)

    var states_B: Set[B] = Set()
    states.foreach(s => states_B = states_B + f(s))

    var transitions_B: Set[(B, Char, B)] = Set()
    transitions.foreach{case (crt, symbol, next) =>
                              var tr:(B, Char, B) = (f(crt), symbol, f(next))
                              transitions_B = transitions_B + tr}

    return new Nfa[B](start_B, states_B, fin_B, transitions_B)
  }

  def epsClosures(crt:A): Set[A] = {
    val q: mutable.Queue[A] = mutable.Queue()
    var res: Set[A] = Set()
    var tr: Set[(A, Char, A)] = Set()

    transitions.foreach{case (x, y, z) =>
      if(y == 'ε') {
        var aux: (A, Char, A) = (x, y, z)
        tr = tr + aux
      }
    }

    var s: A = crt
    res = res + crt
    q.enqueue(s)

    while (!q.isEmpty) {
      tr.foreach{case (x, y, z) =>
        if (x == s) {
          res = res + z
          q.enqueue(z)
        }
      }
      s = q.dequeue()
    }

    return res
  }

  def next(state:A, c: Char): Set[A] = {
    var possible: Set[A] = Set()
    transitions.foreach{ case (crt, symbol, next) =>
                        if (state == crt && c == symbol) {
                          possible = possible + next
                        }
    }
    return possible
  }

  def accepts(str: String): Boolean = {
    val len: Int = str.length - 1
    if (len == -1) {
      var pos_states: Set[A] = epsClosures(start)
      for(s <- pos_states) {
        if(s == fin) {
          return true
        }
      }
        return false
    }
    var pos_states: Set[A] = Set(start)
    var next_pos_states: Set[A] = Set()

    for (i <- 0 to len) {
      //add the e-closure for the current states
      for(crt <- pos_states) {
        next_pos_states = next_pos_states ++ epsClosures(crt)
      }
      pos_states = next_pos_states
      next_pos_states = Set.empty

      //consume a letter and go to possible next states
      for(crt <- pos_states) {
        next_pos_states = next_pos_states ++ next(crt, str(i))
      }

//      println("after consuming letter:" + pos_states)

      pos_states = next_pos_states
      next_pos_states = Set.empty

//      println("after adding e-closure: " + pos_states)
    }

    for(s <- pos_states) {
      next_pos_states = next_pos_states ++ epsClosures(s);
    }
    pos_states = next_pos_states;

    for (s <- pos_states) {
      if (isFinal(s)) {
        return true
      }
    }

    return false
  }

  def getStates : Set[A] =
  {
    return states + start + fin
  }

  def getOtherStates : Set[A] = {
    return states
  }

  def getStart: A = {
    return start
  }

  def getFin: A = {
    return fin
  }

  def getTransitions: Set[(A,Char,A)] = {
    return transitions
  }

  def getNrOfStates : Int = {
    var sf: Int = 1
    if(start != fin) {
      sf = 2
    }
    return states.size + sf
  }

  def isFinal(state: A): Boolean =
  {
    return state == fin
  }

  def getAlphabet(): Set[Char] = {
    var alpha: Set[Char] = Set()

    transitions.foreach{case (_, c, _) =>
      if(c != 'ε') {
        alpha = alpha + c
      }
    }

    return alpha
  }
}

// This is a companion object to the Nfa class. This allows us to call the method fromPrenex without instantiating the Nfa class beforehand.
// You can think of the methods of this object like static methods of the Nfa class
object Nfa {
  def splitString(str: String): Array[String] = {
    var arr: Array[String] = str.split(" ")

    for (i <- 0 to arr.length - 2) {
      if (arr(i) == "'" && arr(i + 1) == "'") {
        arr(i) = " "
        arr = arr.patch(i+1, Nil, 1)
      }
    }

    arr.foreach(token =>
      if(token == "eps") {
        arr(arr.indexOf(token)) = "ε"
      } else if(token == "void"){
        arr(arr.indexOf(token)) = "ω"
      } else if(token.endsWith("'") && token.startsWith("'")) {
        arr(arr.indexOf(token)) = token(1).toString
      }
    )
    return arr
  }

  def basic_NFA(s: Char): Nfa[Int] = {
    s match {
      case 'ω' => return new Nfa[Int](0,Set.empty,1,Set.empty)
      case 'ε' => return new Nfa[Int](0, Set.empty, 0, Set.empty)
      case _ => return new Nfa[Int](0, Set.empty, 1, Set((0, s, 1)))
    }
  }

  def rename(n:Nfa[Int], x:Int): Nfa[Int] = {
    return new Nfa[Int](n.getStart + x, n.getOtherStates.map(e => e + x), n.getFin + x,
      n.getTransitions.map{case (crt, symbol, next) => (crt + x, symbol, next + x)})
  }

  def CONCAT(e1: Nfa[Int], e2: Nfa[Int]): Nfa[Int] = {
    val len = e1.getNrOfStates
    var n2 = rename(e2, len)

    var i:Int = e1.getStart
    var f:Int = n2.getFin
    var o:Set[Int] = e1.getOtherStates ++ n2.getOtherStates
    o = o + e1.getFin + n2.getStart
    var t:Set[(Int, Char, Int)] = e1.getTransitions ++ n2.getTransitions
    var tr:(Int, Char, Int) = (e1.getFin, 'ε', n2.getStart)

//    println("init:" + i);
//    println("other states:" + o);
//    println("final states:" + f);
//    println("trans:" + t);
//    println("e-trans:" + tr);
    return new Nfa[Int](i, o, f, t + tr)
  }

  def UNION(e1: Nfa[Int], e2: Nfa[Int]): Nfa[Int] = {
    val len = e1.getNrOfStates + 1
    var n1 = rename(e1, 1)
    var n2 = rename(e2, len)

    var i:Int = 0
    var f:Int = len + e2.getNrOfStates

    var o:Set[Int] = n1.getStates ++ n2.getStates

    var t:Set[(Int, Char, Int)] = n1.getTransitions ++ n2.getTransitions
    var tr:(Int, Char, Int) = (i, 'ε', n1.getStart)
    t = t + tr
    tr = (i, 'ε', n2.getStart)
    t = t + tr
    tr = (n1.getFin, 'ε', f)
    t = t + tr
    tr = (n2.getFin, 'ε', f)
    t = t + tr

//    println("union init:" + i);
//    println("union other states:" + o);
//    println("union final states:" + f);
//    println("union trans:" + t);


    return new Nfa[Int](i, o, f, t)
  }

  def STAR(e1: Nfa[Int]): Nfa[Int] = {
    var n1 = rename(e1, 1)

    var i:Int = 0
    var f:Int = n1.getNrOfStates + 1

    var o:Set[Int] = n1.getStates

    var t:Set[(Int, Char, Int)] = n1.getTransitions
    var tr:(Int, Char, Int) =(i, 'ε', n1.getStart)
    t = t + tr
    tr = (n1.getFin, 'ε', f)
    t = t + tr
    tr = (i, 'ε', f)
    t = t + tr
    tr = (n1.getFin, 'ε', n1.getStart)
    t = t + tr

//    println("init:" + i);
//    println("other states:" + o);
//    println("final states:" + f);
//    println("trans:" + t);

    return new Nfa[Int](i, o, f, t)
  }

  def PLUS(e: Nfa[Int]): Nfa[Int] = {
    return CONCAT(e, STAR(e))
  }

  def MAYBE(e: Nfa[Int]): Nfa[Int] = {
    return UNION(e, basic_NFA('ε'))
  }

  def op_NFA(op: String, n1: Nfa[Int], n2: Nfa[Int]): Nfa[Int] = {
    op match {
      case "UNION"  => UNION(n1, n2)
      case "STAR"   => STAR(n1)
      case "CONCAT" => CONCAT(n1, n2)
      case "PLUS"   => PLUS(n1)
      case "MAYBE"   => MAYBE(n1)
    }
  }

  def isOp(s:String): Boolean = {
    s match {
      case "UNION"  => true
      case "STAR"   => true
      case "CONCAT" => true
      case "PLUS"   => true
      case "MAYBE"  => true
      case _        => false
    }
  }

  def fromPrenex(str: String): Nfa[Int] = {
    val new_str: Array[String] = splitString(str)

    if(new_str.length == 1) {
      return basic_NFA(new_str(0)(0))
    }

    var res_n:Nfa[Int] = null
    val reverse_str: Array[String] = new_str.reverse
    val stack: mutable.Stack[Nfa[Int]] = mutable.Stack()

    reverse_str.foreach(token =>
      if(!isOp(token)) {
        stack.push(basic_NFA(token(0)))
      } else {
        token match {
          case "UNION" | "CONCAT" => res_n = op_NFA(token, stack.pop(), stack.pop())
          case _                  => res_n = op_NFA(token, stack.pop(), null)
        }
        stack.push(res_n)
      }
    )

//    println(res_n.getStart)
//    println(res_n.getOtherStates)
//    println(res_n.getFin)
//    println(res_n.getTransitions)
    return res_n
  }
}