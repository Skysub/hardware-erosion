import chisel3._
import chisel3.util._

class ProgramCounter extends Module {
  val io = IO(new Bundle {
    val halt = Input(Bool()) //Renamed to halt (also in the tester)
    val branch = Input(Bool()) //Renamed to branch (also in the tester)
    val run = Input(Bool())
    val reset = Input(Bool())
    val programCounterJump = Input(UInt(16.W))
    val programCounter = Output(UInt(16.W))
  })

  //Implement this module here (respect the provided interface, since it used by the tester)
  val PC = RegInit(0.U(16.W))
  io.programCounter := PC

  //Make sure not running the PC takes precedent, then branching, finally adding one to the PC
  when(io.halt || !io.run){
    PC := PC + 0.U
  } .elsewhen (io.branch) {
    PC := io.programCounterJump
  } .otherwise {
    PC := PC + 1.U
  }

  //Reset the PC
  when(io.reset){
    PC := 0.U
  }
}