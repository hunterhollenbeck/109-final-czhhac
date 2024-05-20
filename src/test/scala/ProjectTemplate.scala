import spatial.dsl._

@spatial class GEMV extends SpatialTest {

  type T = FixPt[TRUE,_10,_22]

  val N = 1024  // A is NxN, and x is N wide
  val d = 128
  val M = 1024
  //val Bc = M/(4*d)
  //val Br = Math.min(Bc, d)
  val Bc = 1
  val Br = 1


  def main(args: Array[String]): Unit = {

    // These are on the HOST
    val Q_host = loadCSV2D[T](s"$DATA/Q.csv")
    val K_host = loadCSV2D[T](s"$DATA/K.csv")
    val V_host = loadCSV2D[T](s"$DATA/V.csv")
    val Q_dram = DRAM[T](N, d)
    val K_dram = DRAM[T](N, d)
    val V_dram = DRAM[T](N, d)
    setMem(Q_dram, Q_host)
    setMem(K_dram, K_host)
    setMem(V_dram, V_host)
    val l_dram = DRAM[T](N)
    val O_dram = DRAM[T](N)
    
    Accel {
      val Q_sram = SRAM[T](Br, d)
      val K_sram = SRAM[T](Bc, d)
      val V_sram = SRAM[T](Bc, d)
      val l_sram = SRAM[T](Br)
      val lij_sram = SRAM[T](Br)
      val O_sram = SRAM[T](Br, d)
      val Sij_sram = SRAM[T](Br, Bc)
      //A_sram load A_dram
      //x_sram load x_dram
      //val out_sram = SRAM[T](N)
      Foreach(Tc by 1) {j=>
	K_sram load K_dram(j*Bc::(j+1)*Bc, 0::d)
	V_sram load V_dram(j*Bc::(j+1)*Bc, 0::d)
	Foreach(Tr by 1) {i =>
	  Q_sram load Q_dram(i*Br::(i+1)*Br, 0::d)
	  O_sram load O_dram(i*Br::(i+1)*Br, 0::d)
	  l_sram load l_dram(i*Br::(i+1)*Br) 
	  
	  //calculate Sij
	  MemFold(Sij_sram)(d by 1){m=>
	    val tmp = SRAM[T](Br, Bc)
	    Foreach(Br by 1, Bc by 1){ (n,o)=>
	      tmp(n,o) = Q_sram(n, m) * K_sram(o, m)
	    }tmp
	  }{_+_}
	  //done with Sij
	  //calculate Pij (in Sij)
	  Foreach(Br by 1, Bc by 1){ (n,o)=>
	      Sij_sram(n,o) = exp(Sij_sram(n,o))
	  }
	  //done with Pij
	  //start lij
	  MemFold(li


	  } 

          val accum = Reg[T]
          Reduce(accum)(0 until N) {
            j => A_sram(i, j) * x_sram(j)
          } {_ + _}
          out_sram(i) = accum.value
      }
      out_host store out_sram
    }

    writeCSV1D(getMem(out_host), s"$DATA/output.csv")
    assert(Bit(true))
  }
}
