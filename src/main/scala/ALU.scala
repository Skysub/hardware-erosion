import chisel3._
import chisel3.util._

class ALU extends Module {
  val io = IO(new Bundle {
    //Define the module interface here (inputs/outputs)
    val opcode = Input(UInt(6.W))
    val R2In = Input(UInt(32.W))
    val dataIn = Input(UInt(32.W))
    val output = Output(UInt(32.W))
    val equalCheck = Output(Bool()) //Outputs true if the inputs are equal, always
    //Alu Controls
    val aluControl = Input(UInt(10.W)) //Placeholder, remember to change ControlUnit when implementing alu
  })

  //Splitting up dataIn into R1In and R3In for easier readability
  //In reality, R1 and R3 share the same bus and can't both be present in any ALU operation
  val R1In = io.dataIn
  val R3In = io.dataIn

  //Implement this module here

  switch(io.opcode){
    //Arithmetic
    //adds signed registers
    is("b000001".U){
      io.output := io.R2In + R3In
    }
    //Adds signed register and immediate value
    is("b000010".U){
      io.output := io.R2In + R1In
    }
    //Adds unsigned registers
    is("b000011".U){
      io.output := io.R2In + R3In
    }
    //Adds unsigned register and immediate value
    is("b000100".U){
      io.output := io.R2In + R1In
    }
    //Multiply signed registers
    is("b000101".U){
      io.output := io.R2In * R3In
    }
    //Multiply unsigned registers
    is("b000110".U){
      io.output := io.R2In * R3In
    }
    //Divide signed registers
    is("b000111".U){
      io.output := io.R2In / R3In
    }
    //Divide unsigned registers
    is("b001000".U){
      io.output := io.R2In / R3In
    }
    //Makes the number negative
    is("b001001".U){
      io.output := -R1In
    }
    //Increment
    is("b001010".U){
      io.output := R1In + 1.U
    }

    //Logical
    //OR two registers
    is("b001011".U){
      io.output := io.R2In | R3In
    }
    //AND two registers
    is("b001100".U){
      io.output := io.R2In & R3In
    }
    //OR a register and immediate value
    is("b001101".U){
      io.output := io.R2In | R1In
    }
    //AND a register and immediate value
    is("b001110".U){
      io.output := io.R2In & R1In
    }
    //XOR the bits
    is("b001111".U){
      io.output := io.R2In ^ R3In
    }
    //NOT the bits of a register
    is("b010000".U){
      io.output := ~R1In
    }
    //Bitshift left by immediate value
    is("b010001".U){
      io.output := ~io.R2In << R1In
    }
    //Bitshift right by immediate value
    is("b010010".U){
      io.output := ~io.R2In >> R1In
    }

    //compare
    //Set on less than
    is("b010011".U){
      when(io.R2In < R3In){io.output := 1.U} .otherwise(io.output := 0.U)
    }
    //Set on less than immediate
    is("b010100".U){
      when(io.R2In < R3In){io.output := R1In} .otherwise(io.output := 0.U)
    }
    //Set on less than unsigned
    is("b010101".U){
      when(io.R2In < R3In){io.output := 1.U} .otherwise(io.output := 0.U)
    }
    //set on less than immediate unsigned
    is("b010111".U){
      when(io.R2In < R3In){io.output := R1In} .otherwise(io.output := 0.U)
    }
  }

}
