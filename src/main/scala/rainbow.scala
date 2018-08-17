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
    val basis = (0 to n-2).toList
    val li = (n-1) :: (0 to n-1).toList
    val colored = scala.util.Random.shuffle(li)
//    println(colored)
    basis ++ colored
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

  def matFromSet(set: Seq[List[Int]]): List[DenseVector[Int]] = {
    set.map{v => DenseVector(v.toArray)}.toList
  }

  def rain(n:Int) = {

    //def addBasisColors
    def getStandardBasis() = {
      (0 until n-1).toList.map {
        i =>
        List.tabulate(n-1){
          k =>
          if(k == i) 1 else 0
        }
      }
    }
    val stb = getStandardBasis()

    val genList = Gen.containerOfN[List, Int]((n-1), Gen.oneOf(0,1)).suchThat(li => li.sum > 1)

    val genSet = Gen.setOfN(n + 1, genList).map{si => (si.toList ++ stb).sortBy(l => l.sum)} //sorted so first n-1 are basis
    val genMat = genList.map(li => new DenseMatrix(n-1, 2*n, li.toArray))
//    val colors = color(n)

    val genColors = Gen.containerOfN[List,Int](2*n, arbitrary[Int]).map{l => color(n)}


    def getRainbowSets(colors: List[Int]):List[List[Int]] = {
      val subs = subsets((n until 2*n).toList)

      val rain = subs.filter(s => s.size <= n/2 && s.size >= 1 && isRainbow(s,colors))
      rain.map(r => (0 until n).toList ++ r)
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
  rain(7).check(param)
}

