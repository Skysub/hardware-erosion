import chisel3._
import chisel3.util._

class RegisterFile extends Module {
  val io = IO(new Bundle {
    //Define the module interface here (inputs/outputs)
    val dataIn = Input(UInt(16.W))
    val dataOut = Output(UInt(32.W))
    val dataOutAluOnly = Output(UInt(32.W))
    val jumpRegisterOut = Output(UInt(32.W))

    //Control
    val regWrite = Input(Bool())
    val registerControl = Input(UInt(16.W)) //The 15 lowest bits are the R1, R2, and R3 fields. Rest are control bits
  })


  val registerFile =  RegInit(VecInit(Seq.fill(32)(0.U(32.W)))) //The registerFile itself

  //Splitting up the registerControl input into the controls needed
  val R1 = io.registerControl(14, 10)
  val R2 = io.registerControl(9, 5)
  val R3 = io.registerControl(4, 0)
  val R3out = io.registerControl(15) //Do we output R3 or R1?


  io.jumpRegisterOut := registerFile(31) //jumpRegister
  io.dataOutAluOnly := registerFile(R2) //Connecting the value from R2 t0 the top wire in the diagram in registerFile

  when(io.regWrite) {
    registerFile(R1) := io.dataIn
  }

  when(R3out) {
    io.dataOut := registerFile(R3)
  } .otherwise {
    io.dataOut := registerFile(R1)
  }

  registerFile(0) := 0.U //reg 0 is always 0
}