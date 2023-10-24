import chisel3._
import chisel3.util._

class ControlUnit extends Module {
  val io = IO(new Bundle {
    val instruction = Input(UInt(32.W))

    val immediate = Output(Bool())
    val fromAlu = Output(Bool())
    val immediateALU = Output(Bool())
    val returnC = Output(Bool())
    val regWrite = Output(Bool())
    val registerControl = Output(UInt(17.W))
    val aluControl = Output(UInt(10.W)) //Change this to the amount of control bits needed for ALU operation
    val branch = Output(Bool())
    val halt = Output(Bool())
    val reset = Output(Bool())
    val writeEnable = Output(Bool())
  })

  when(io.instruction === "hFFFFFFFF".U){
    io.halt := true.B
  }

}