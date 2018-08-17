package rainbow

import breeze.linalg._
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop._
import org.scalacheck._

object Rainbow extends App {
  import org.scalacheck.ops._
  //color by appending two lists from 1 to n together and then shuffling them
  def color(n:Int):List[Int] = {
    val li = (0 to n-1).toList
    val colored = scala.util.Random.shuffle(li ++ li)
//    println(colored)
    colored
  }

  def subsets(xs: List[Int]): List[List[Int]] = xs match {
    case Nil => List(List[Int]())
    case x::xs1 => subsets(xs1) ++ subsets(xs1).map{ys => x::ys}
  }

  def isRainbow(s:List[Int], colors: List[Int]): Boolean = {
    s.groupBy{i => colors(i)}.forall{
      x => x._2.size == 1
    }
  }

  def getRainbowSets(colors: List[Int], n:Int):List[List[Int]] = {
    val subs = subsets((0 until 2*n).toList).filter(s => s.size <= n/2 && s.size >= 1 && isRainbow(s,colors))
    subs
  }

  def matFromSet(set: Seq[List[Int]]): List[DenseVector[Int]] = {
    set.map{v => DenseVector(v.toArray)}.toList
  }

  def bla() = {
    val example = List(List(0,0,1,1,0),
      List(0,1,1,1,1), List(1,1,1,0,1), List(1,0,0,0,0), List(1,0,1,0,1),
      List(0,1,0,1,0), List(1,1,0,0,1), List(1,0,0,0,1), List(0,0,0,1,1),
      List(0,1,0,1,1), List(0,1,1,1,0), List(1,1,1,0,0))

    def add(li:List[Int], l2:List[Int], l3:List[Int]):List[Int] = {
      li.zip(l2).zip(l3).map{ case(p,x) => (p._1 + p._2 + x) % 2}
    }

    def ad(li:List[Int], l2:List[Int]):List[Int] = {
      li.zip(l2).map{ case(p,x) => (p + x) % 2}
    }

    val triples = for {
      x <-example
      y <- example
      z <- example
      if(!((x == y) || (x == z) || (y == z))) 
    } yield (x,y,z)

    val doubles = for {
      x <-example
      y <- example
      if(!(x == y))
    } yield (x,y)

    println(triples.map{case((a,b,c)) => add(a,b,c)})
    doubles.foreach{case((a,b)) =>
      if(ad(a,b) == List(0,0,0,0,0))
        println(List(a,b))}
    triples.forall{ case((a,b,c)) => add(a,b,c) != List(0,0,0,0,0)} &&
    doubles.forall{case((a,b)) => ad(a,b) != List(0,0,0,0,0)}
  }

  def test() = {
    val n = 6
    val genColors = Gen.containerOfN[List,Int](2*n, arbitrary[Int]).map{l => color(n)}
    val example = List(List(0,0,1,1,0),
      List(0,1,1,1,1), List(1,1,1,0,1), List(1,0,0,0,0), List(1,0,1,0,1),
      List(0,1,0,1,0), List(1,1,0,0,1), List(1,0,0,0,1), List(0,0,0,1,1),
            List(0,1,0,1,1), List(0,1,1,1,0), List(1,1,1,0,0))
    val mat = matFromSet(example)

    val prop = forAll(genColors) {
      colors:List[Int] =>
      val sets = getRainbowSets(colors,6)
      sets.forall{
        s =>
        val sum = s.foldLeft(DenseVector.zeros[Int]((n-1))){(acc,i) => (acc + mat(i)) % 2}
        sum != DenseVector.zeros[Int]((n-1))
      }
    }
    val param = org.scalacheck.Test.Parameters.defaultVerbose
    prop.check(param)
  }

  def rain(n:Int) = {

    val genList = Gen.containerOfN[List, Int]((n-1), Gen.oneOf(0,1))
    val genSet = Gen.setOfN(2*n, genList).map{si => si.toList}
    val genMat = genList.map(li => new DenseMatrix(n-1, 2*n, li.toArray))
//    val colors = color(n)

    val genColors = Gen.containerOfN[List,Int](2*n, arbitrary[Int]).map{l => color(n)}

    //def addBasisColors
    //def getStandardBasis

    def getRainbowSets(colors: List[Int]):List[List[Int]] = {
      val subs = subsets((0 until 2*n).toList).filter(s => s.size <= n/2 && s.size >= 1 && isRainbow(s,colors))
      subs
    }

    val prop =
      exists(genSet){
        si =>
        //mat:DenseMatrix[Boolean] =>
        val mat = matFromSet(si)
        exists(genColors) {
          colors:List[Int] =>
          val sets = getRainbowSets(colors)
          sets.forall{
            s =>
            val sum = s.foldLeft(DenseVector.zeros[Int]((n-1))){(acc,i) => (acc + mat(i)) % 2}
            sum != DenseVector.zeros[Int]((n-1))            
          }
        }
      }
    prop
  }

  val param = org.scalacheck.Test.Parameters.defaultVerbose.withMaxDiscardRatio(1000000.0f).withMinSuccessfulTests(1)
  //rain(8).check(param)
  test()
}

