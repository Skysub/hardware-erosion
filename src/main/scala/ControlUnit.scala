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
    val registerControl = Output(UInt(19.W))
    val aluControl = Output(UInt(5.W))
    val branch = Output(Bool())
    val halt = Output(Bool())
    val reset = Output(Bool())
    val writeEnable = Output(Bool())
  })
  val opcode = io.instruction(31,26)

  val R3out = Wire(UInt(1.W))
  val topOfRegister = Wire(UInt(1.W))
  val linkToPC = Wire(UInt(1.W))
  val regImmediate = Wire(UInt(1.W))
  regImmediate := io.immediate

  //default values

  //For the register file
  R3out := 1.U //Do we output R3 or R1?
  topOfRegister := 0.U //Do we put the input into the top of the register?
  linkToPC := 0.U //If we should save the PC to reg 31

  //For the CPUTop
  io.returnC := false.B; //Do we return to the address in the jump register?
  io.immediate := false.B //Data going in to the register file comes from instruction (else from alu/mem)
  io.regWrite := false.B //write to register
  io.immediateALU := false.B //data going into alu comes from instruction
  io.fromAlu := true.B //Data going into register file comes from alu (not mem)
  io.writeEnable := false.B //Write to memory
  io.aluControl := "b00000".U //Chooses the operation done by the ALU
  io.branch := false.B //Do we branch now?

  //00xxxx : register type operation
  //10xxxx : Immediate type operation
  //11xxxx : jump type operation

  when(io.instruction(31) === 0.U){
    when(io.instruction(30) === 1.U) { //Register type operation _01xxxx
      io.regWrite := true.B //Alle register type operations write to a register

      switch(opcode) {
        //Arithmetic operations
        //signed ops _0101xx
        is("b010100".U) { //ADD _010100
          io.aluControl := "b11000".U
        }
        is("b010101".U) { //MUL _010101
          io.aluControl := "b11010".U
        }
        is("b010110".U) { //DIV  _010110
          io.aluControl := "b11100".U
        }
        is("b010111".U) { //NEG _010111
          io.aluControl := "b11110".U
          R3out := false.B
        }

        //Unsigned ops _0100xx
        is("b010000".U) { //ADDU _010000
          io.aluControl := "b01001".U
        }
        is("b010001".U) { //MULU _010001
          io.aluControl := "b01011".U
        }
        is("b010010".U) { //DIVU _010010
          io.aluControl := "b01101".U
        }
        is("b010011".U) { //INC _010011
          io.aluControl := "b01111".U
          R3out := false.B
        }

        //Logical operations
        is("b011000".U) { //OR _011000
          io.aluControl := "b00100".U
        }
        is("b011001".U) { //AND _011001
          io.aluControl := "b00101".U
        }
        is("b011010".U) { //XOR  _011010
          io.aluControl := "b00110".U
        }
        is("b011011".U) { //NOT _011011
          io.aluControl := "b00111".U
          R3out := false.B
        }

        //Comparetive operations
        is("b011100".U) { //SLT _011100
          io.aluControl := "b10001".U
        }
        is("b011101".U) { //SLI _011101
          io.aluControl := "b10010".U
          io.immediateALU := true.B
        }
        is("b011110".U) { //SLU  _011110
          io.aluControl := "b00001".U
        }
        is("b011111".U) { //SLUI _011111
          io.aluControl := "b00010".U
          io.immediateALU := true.B
        }
      }
    }
  } .elsewhen(io.instruction(30) === 0.U) { //Immediate type operation _10xxxx
    when(io.instruction(29) === 1.U){ //A memory operation _101xxx
      when(io.instruction(28) === 1.U){ //moving an immediate value into a register _1011xx
        io.regWrite := true.B
        io.immediate := true.B
        when(io.instruction(27) === 1.U) { //Moving into upper part _10111x
          topOfRegister := true.B
        }
      } .otherwise { //_1010xx
        when(io.instruction(27) === 1.U){ //LOAD / STOR operation _10101x
          io.immediateALU := true.B
          io.aluControl := "b11000".U //Add signed operation needed for memory transfer
          when(io.instruction(26) === 1.U){ //Load from memory into a register _101011
            io.regWrite := true.B
            io.fromAlu := false.B
          } .otherwise{ //Store to memory a registers value _101010
            io.writeEnable := true.B
            R3out := false.B
          }
        } .otherwise {//Move data from register to register _101000
          io.regWrite := true.B
          io.immediateALU := true.B //Requires the immediate part of the instruction to be all zero
          //R2 is passed through the ALU by deafult
        }
      }
    } .otherwise { //the 6 non-memory immediate operations _100xxx
      io.regWrite := true.B
      io.immediateALU := true.B
      when(io.instruction(28) === 1.U){ //The 2 arithmetic operations _1001xx
        when(io.instruction(27) === 1.U){ //ADDI _10010x

          io.aluControl := "b11000".U
        }.otherwise { //ADDIU _10011x
          io.aluControl := "b01001".U
        }
      } .otherwise { //The 4 arithmetic operations _1000xx
        switch(opcode) {
          is("b100000".U) { //ORI _100000
            io.aluControl := "b00100".U
          }
          is("b100001".U) { //ANDI _100001
            io.aluControl := "b00101".U
          }
          is("b100010".U) { //BSL _100010
            io.aluControl := "b10100".U
          }
          is("b100011".U) { //BSR _100011
            io.aluControl := "b10101".U
          }
        }
      }
    }
  } .otherwise { //Jump type operation _11xxxx
    when(io.instruction(29) === 1.U){ // _111xxx
      R3out := false.B
      io.aluControl := "b00011".U //Compare operation needed
      when(io.instruction(28) === 1.U) {
        io.branch := Mux(io.fromAluEqualsCheck, true.B, false.B) //Jump if equals _1111xx
      }.otherwise {
        io.branch := Mux(io.fromAluEqualsCheck, false.B, true.B) //Jump if doesn't equal _1110xx
      }
    } .otherwise { //unconditional jump _110xxx
      io.branch := true.B

      when (io.instruction(28) === 1.U){ //Do other stuff as well? _1101xx
        when(io.instruction(27) === 1.U) {
          linkToPC := true.B //Jump and link _11011x
        } .otherwise{
          io.returnC := true.B //Return _11010x
        }
      }
    }
  }

  //Constructs the registerFile controls
  val t0 = Cat(regImmediate, linkToPC)
  val t1 = Cat(t0, topOfRegister)
  val t2 = Cat(t1, R3out)
  val adresses = io.instruction(25, 11)
  val regControlsDone = Cat(t2, adresses)
  io.registerControl := regControlsDone

  //Implements the halting of the cpu
  when(io.instruction(31, 26) === "b111111".U){
    io.halt := true.B
  } .otherwise {
    io.halt := false.B
  }

  //Should have a button associated on actual hardware, to reset the pc.
  io.reset := false.B
}