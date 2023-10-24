import chisel3._
import chisel3.util._

class ControlUnit extends Module {
  val io = IO(new Bundle {
    val instruction = Input(UInt(32.W))
    val fromAluEqualsCheck = Input(Bool())

    val immediate = Output(Bool())
    val fromAlu = Output(Bool())
    val immediateALU = Output(Bool())
    val returnC = Output(Bool())
    val regWrite = Output(Bool())
    val registerControl = Output(UInt(18.W))
    val aluControl = Output(UInt(10.W)) //Change this to the amount of control bits needed for ALU operation
    val branch = Output(Bool())
    val halt = Output(Bool())
    val reset = Output(Bool())
    val writeEnable = Output(Bool())
  })

  //default values
  io.returnC := false.B;
  val R3out = 1.U(1.W) //Do we output R3 or R1?
  val topOfRegister = 0.U(1.W) //Do we put the input into the top of the register?
  val linkToPC = 0.U(1.W) //If we should save the PC to reg 31
  io.immediate := false.B //Data going in to the register file comes from instruction (else from alu/mem)
  io.regWrite := false.B
  io.immediateALU := false.B
  io.fromAlu := true.B
  io.writeEnable := false.B


  when(io.instruction(31) === 0.U){ //Register type operation
    io.regWrite := true.B
    //ALU stuff here and also a bit of control bit setting for the rest of the cpu

  } .elsewhen(io.instruction(30) === 0.U) { //Immediate type operation
    when(io.instruction(29) === 1.U){ //A memory operation
      when(io.instruction(28) === 1.U){ //moving an immediate value into a register
        io.regWrite := true.B
        io.immediate := true.B
        when(io.instruction(27) === 1.U) { //Moving into upper part
          topOfRegister := 1.U
        }
      } .otherwise {
        when(io.instruction(27) === 1.U){
          io.immediateALU := true.B
          when(io.instruction(26) === 1.U){ //Load from memory into a register
            io.regWrite := true.B
            io.fromAlu := false.B
            //THIS PART NEEDS ALU CONTROL (+)
          } .otherwise{ //Store to memory a registers value
            io.writeEnable := true.B
            R3out := false.B
            //THIS PART NEEDS ALU CONTROL (+)
          }
        } .otherwise {//Move data from register to register
          io.regWrite := true.B
          io.immediateALU := true.B //Requires the immediate part of the instruction to be all zero
          //If you can make the alu just pass R2 through, then we dont need the immediate part to be all zeros
          //THIS PART NEEDS ALU CONTROL (+?)
        }
      }
    } .otherwise { //the 8 non-memory immediate operations

    }
  } .otherwise { //Jump type operation
    when(io.instruction(29) === 1.U){
      R3out := false.B
      when(io.instruction(28) === 1.U) {
        io.branch := Mux(io.fromAluEqualsCheck, true.B, false.B) //Jump if equals
      }.otherwise {
        io.branch := Mux(io.fromAluEqualsCheck, false.B, true.B) //Jump if doesn't equal
      }
    } .otherwise {
      io.branch := true.B //unconditional jump

      when (io.instruction(28) === 1.U){
        when(io.instruction(27) === 1.U) {
          linkToPC := true.B //Jump and link
        } .otherwise{
          io.returnC := true.B //Return
        }
      }
    }
  }

  //Constructs the registerFile controls
  val t1 = Cat(linkToPC, topOfRegister)
  val t2 = Cat(t1, R3out)
  val adresses = io.instruction(25, 11)
  val regControlsDone = Cat(t2, adresses)
  io.registerControl := regControlsDone

  //Implements the halting the cpu
  when(io.instruction(31, 26) === "b111111".U){
    io.halt := true.B
  } .otherwise {
    io.halt := false.B
  }

  //Should have a button associated on actual hardware
  io.reset := false.B
}