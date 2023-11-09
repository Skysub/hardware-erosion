import chisel3._
import chisel3.util._

class ALU extends Module {
  val io = IO(new Bundle {
    //Define the module interface here (inputs/outputs)
    val R2In = Input(UInt(32.W))
    val dataIn = Input(UInt(32.W))
    val output = Output(UInt(32.W))
    val equalCheck = Output(Bool()) //Outputs true if the inputs are equal, always
    //Alu Controls
    val aluControl = Input(UInt(5.W)) //Placeholder, remember to change ControlUnit when implementing alu
  })

  //ALU Control bit explanation
  //First bit determines if its signed or not (or if the logical operation is a bitshift operation)
  //Second Bit determines whether its an arithmetic operation or not
      //If not, then the third bit determines whether its a logical operation or a comparative one
  //The rest of the bits correspond to a particular operations

  //Default value
  io.equalCheck := false.B
  io.output := io.R2In

  switch(io.aluControl){
    //Arithmetic
    //adds signed
    is("b11000".U){
      //Signed numbers exist only in the ALU, everywhere else numbers are unsigned.
      //Thats why we cast the inputs to signed values, do the operation, the cast the output to be unsigned
      io.output := (io.R2In.asSInt + io.dataIn.asSInt).asUInt
    }
    //Adds unsigned
    is("b01001".U){
      io.output := io.R2In + io.dataIn
    }
    //Multiply signed registers
    is("b11010".U){
      val temp = io.R2In.asSInt * io.dataIn.asSInt
      io.output := temp(31, 0).asUInt //The multiplication output is 64 bits, we discard the top part
    }
    //Multiply unsigned registers
    is("b01011".U){
      io.output := io.R2In * io.dataIn
    }
    //Divide signed registers
    is("b11100".U){
      io.output := (io.R2In.asSInt / io.dataIn.asSInt).asUInt
    }
    //Divide unsigned registers
    is("b01101".U){
      io.output := io.R2In / io.dataIn
    }
    //Makes the number negative
    is("b11110".U){
      io.output := (-(io.dataIn.asSInt)).asUInt
    }
    //Increment
    is("b01111".U){
      io.output := io.dataIn + 1.U
    }


    //Logical
    //OR two values
    is("b00100".U){
      io.output := io.R2In | io.dataIn
    }
    //AND two values
    is("b00101".U){
      io.output := io.R2In & io.dataIn
    }
    //XOR the bits
    is("b00110".U){
      io.output := io.R2In ^ io.dataIn
    }
    //NOT the bits
    is("b00111".U){
      io.output := ~io.dataIn
    }
    //Bitshift left by immediate value
    //Chisel doesn't allow bitshifting by more than 20 bits at a time. Which is why we have to do this.
    is("b10100".U) {
      when(io.dataIn > 31.U) {
        io.output := 0.U(32.W)
      }.otherwise {
        when(io.dataIn > 20.U) {
          val temp = io.R2In << 20.U
          val shift = Wire(UInt(4.W))
          shift := (io.dataIn - 20.U)
          val temp2 = temp << shift
          io.output := temp2(31, 0)
        }.otherwise {
          val shift = io.dataIn(4, 0)
          val temp2 = io.R2In << shift
          io.output := temp2(31, 0)
        }
      }
    }
    //Bitshift right by immediate value
    is("b10101".U){
      when(io.dataIn > 31.U){
        io.output := 0.U(32.W)
      } .otherwise {
        when(io.dataIn > 20.U) {
          val temp = io.R2In >> 20.U
          val shift = Wire(UInt(4.W))
          shift := (io.dataIn - 20.U)
          val temp2 = temp >> shift
          io.output := temp2(31, 0)
        } .otherwise {
          val shift = io.dataIn(4, 0)
          val temp2 = io.R2In >> shift
          io.output := temp2(31, 0)
        }
      }
    }

    //compare
    //Equality check (for the jump commands)
    is("b00011".U) {
       when(io.R2In.asSInt === io.dataIn.asSInt){io.equalCheck := true.B} .otherwise(io.equalCheck := false.B)
    }

    //Set on less than Signed
    is("b10001".U){
      when(io.R2In.asSInt < io.dataIn.asSInt){io.output := 1.U} .otherwise(io.output := 0.U)
    }
    //Set on less than immediate Signed
    is("b10010".U){
      when(io.R2In.asSInt < io.dataIn.asSInt){io.output := 1.U} .otherwise(io.output := 0.U)
    }
    //Set on less than unsigned
    is("b00001".U){
      when(io.R2In < io.dataIn){io.output := 1.U} .otherwise(io.output := 0.U)
    }
    //set on less than immediate unsigned
    is("b00010".U){
      when(io.R2In < io.dataIn){io.output := 1.U} .otherwise(io.output := 0.U)
    }

    //Special
    //Pass through R2, also the default value
    is("b00000".U) {
      io.output := io.R2In
    }

    //Future special operation
    is("b11111".U) {
    }

  }
}
