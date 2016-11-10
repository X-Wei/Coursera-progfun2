package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = Signal{
    b()*b() - 4*a()*c()
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = Signal{
    val delta = computeDelta(a,b,c)()
    if(delta<0) Set[Double]()
    else{
      val sqrtdelta = math.sqrt(delta)
      val (r1, r2) = ( (-b()+sqrtdelta)/2/a(), (-b()-sqrtdelta)/2/a() )
      Set(r1,r2)
    }
  }
}
