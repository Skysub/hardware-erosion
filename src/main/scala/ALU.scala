import chisel3._
import chisel3.util._

class ALU extends Module {
  val io = IO(new Bundle {
    //Define the module interface here (inputs/outputs)
    val R2In = Input(UInt(32.W))
    val dataIn = Input(UInt(32.W))
    val output = Output(UInt(32.W))

    //Alu Controls

  })

  //Implement this module here

}