package Pour
class Pouring(capacity: Vector[Int]) {

  type State = Vector[Int]

  val initialState = capacity map (x => 0)

  //Moves
  trait Move{
    def change(state: State): State
  }
  case  class Empty(glass:Int) extends Move{
    def change(state: State): State = state updated(glass, 0)
  }
  case class Fill(glass:Int) extends  Move{
    def change(state: State): State = state updated(glass, capacity(glass))
  }
  case  class Pourto(from:Int, to:Int) extends Move{
    def change(state: State): State = {
      val amount = if (state(from) < capacity(to) - state(to)) state(from) else (capacity(to) - state(to))
      state updated(from, state(from) - amount) updated(to, state(to) + amount)
    }
  }

  val glasses = 0 until capacity.length
  val moves = (for(g <- glasses) yield Empty(g)) ++
              (for(g <- glasses) yield Fill(g)) ++
              (for(g1<-glasses; g2<-glasses if g1 != g2) yield Pourto(g1, g2))

  //Paths
  class Path(history:List[Move], val endState: State) {
    /*
    def endState: State = trackState(history)
    private def trackState(xs: List[Move]): State = {
      xs match {
        case Nil => initialState
        case move :: xs1 => move change (trackState(xs1))
      }
    }*/
    def extend(move: Move) = new Path(move :: history, move change(endState))

    override def toString: String = (history.reverse mkString " ") + "-->" + endState
  }
    val initialPath = new Path(Nil, initialState)

    def from(pathset:Set[Path], explored: Set[State]):LazyList[Set[Path]] = {
      if (pathset.isEmpty) LazyList.empty else
        {
          val add = for{
            path <- pathset
            next <- moves map path.extend
            if (!(explored contains(next.endState)))
          } yield next
          LazyList.cons(pathset, from(add, explored ++ (add map(_.endState))))
        }
    }
    val allPaths = from(Set(initialPath), Set(initialState))

    @scala.annotation.tailrec
    private def solution_helper2(value: Set[Pouring.this.Path], i: Int):LazyList[Path] = {
      if (value.isEmpty) LazyList.empty else
        {
          val curr_path = value.head
          //println(curr_path)
          val endstate = curr_path.endState
          if (endstate.contains(i)) LazyList(curr_path) else solution_helper2(value.tail, i)
        }
    }

    @scala.annotation.tailrec
    private def solutionhelper(value: => LazyList[Set[Pouring.this.Path]], i: Int):LazyList[Path] = {
      if (value.isEmpty) LazyList.empty else {
        val curr_set = value.head
        //println(curr_set)
        val sol = solution_helper2(curr_set, i)
        if (sol.isEmpty) solutionhelper(value.tail, i) else sol
      }
    }

    def solution(target:Int):LazyList[Path] = {
      solutionhelper(allPaths, target)
    }
}
