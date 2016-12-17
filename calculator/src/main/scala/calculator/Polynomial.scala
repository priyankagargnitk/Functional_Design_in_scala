package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    new Signal[Double](b() * b() - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    new Signal[Set[Double]]( delta() match  {
      case num if num < 0 => Set[Double]()
      case num if num == 0 => Set[Double](-b() / 2 * a())
      case _ => Set[Double]((-b() + Math.sqrt(delta()) )/ 2 * a() , (-b() - Math.sqrt(delta()) )/ 2 * a())
    })
  }
}
