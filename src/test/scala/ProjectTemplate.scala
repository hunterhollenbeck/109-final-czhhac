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
    val O_dram = DRAM[T](N, d)
    
    Accel {
      val Q_sram = SRAM[T](Br, d)
      val K_sram = SRAM[T](Bc, d)
      val V_sram = SRAM[T](Bc, d)
      val l_sram = SRAM[T](Br)
      val lij_sram = SRAM[T](Br)
      val lnew_sram = SRAM[T](Br)
      val O_sram = SRAM[T](Br, d)
      val Sij_sram = SRAM[T](Br, Bc)
      val PijVj_sram = SRAM[T](Br, d)
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
	  MemFold(lij_sram)(Bc by 1){m =>
	    val tmp = SRAM[T](Br)
	    Foreach(Br by 1){n =>
	      tmp(n) = Sij_sram(m, n)
 	    }tmp
	  }{_+_} 
	  //end lij
	  //start lnew
	  Foreach(Br by 1){m =>
	    lnew_sram(m) = l_sram(m) + lij_sram(m)
	  }
	  //end lnew
	  //store l
	  l_dram(i*Br::(i+1)*Br) store lnew_sram
	  
	  //compute P_ijV_j
	  MemFold(PijVj_sram)(Bc by 1){m=>
	    val tmp = SRAM[T](Br, d)
	    Foreach(Br by 1, d by 1){ (n,o)=>
	      tmp(n,o) = Sij_sram(n, m) * V_j_sram(o, m)
	    }tmp
	  }{_+_}
	  //done compute P_ijV_j
	  //begin Oi
	  Foreach(Br by 1, d by 1){(n, o)=>
	    Oi_sram(n,o) = (l_sram(n) * Oi_sram(n,o) + PijVj_sram(n,o))/lnew_sram(n)
	  }
	  O_dram(i*Br::(i+1)*Br, 0::d) store Oi_sram
	//I THINK IT's DONE EXCEPT FOR THE WRITEBACK STEP
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
