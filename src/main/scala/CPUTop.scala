import chisel3._
import chisel3.util._

class CPUTop extends Module {
  val io = IO(new Bundle {
    val done = Output(Bool ())
    val run = Input(Bool ())
    //This signals are used by the tester for loading and dumping the memory content, do not touch
    val testerDataMemEnable = Input(Bool ())
    val testerDataMemAddress = Input(UInt (16.W))
    val testerDataMemDataRead = Output(UInt (32.W))
    val testerDataMemWriteEnable = Input(Bool ())
    val testerDataMemDataWrite = Input(UInt (32.W))
    //This signals are used by the tester for loading and dumping the memory content, do not touch
    val testerProgMemEnable = Input(Bool ())
    val testerProgMemAddress = Input(UInt (16.W))
    val testerProgMemDataRead = Output(UInt (32.W))
    val testerProgMemWriteEnable = Input(Bool ())
    val testerProgMemDataWrite = Input(UInt (32.W))
  })

  //Creating components
  val programCounter = Module(new ProgramCounter())
  val dataMemory = Module(new DataMemory())
  val programMemory = Module(new ProgramMemory())
  val registerFile = Module(new RegisterFile())
  val controlUnit = Module(new ControlUnit())
  val alu = Module(new ALU())

  //Connecting the modules
  programCounter.io.run := io.run
  programMemory.io.address := programCounter.io.programCounter

  controlUnit.io.instruction := programMemory.io.instructionRead

  //Top part of diagram. Should the data going into the register file be from the instruction, the ALU, or the data memory
  registerFile.io.dataIn := Mux(controlUnit.io.immediate, programMemory.io.instructionRead, Mux(controlUnit.io.fromAlu, alu.io.output, dataMemory.io.dataRead))

  //The mux choosing what data flows into the alu, through the bottom most connection
  alu.io.dataIn := Mux(controlUnit.io.immediateALU, programMemory.io.instructionRead, registerFile.io.dataOut)

  //Finishing connection the register file, alu, and data memory together
  alu.io.R2In := registerFile.io.dataOutAluOnly
  dataMemory.io.address := alu.io.output
  dataMemory.io.dataWrite := registerFile.io.dataOut

  //Connecting the PC to get the correct address when branching
  programCounter.io.programCounterJump := Mux(controlUnit.io.returnC, registerFile.io.jumpRegisterOut(15, 0), programMemory.io.instructionRead(15, 0))
  registerFile.io.PC := programCounter.io.programCounter

  //Connecting the non-mux control logic
  registerFile.io.regWrite := controlUnit.io.regWrite
  registerFile.io.registerControl := controlUnit.io.registerControl
  alu.io.aluControl := controlUnit.io.aluControl
  programCounter.io.branch := controlUnit.io.branch
  programCounter.io.halt := controlUnit.io.halt
  programCounter.io.reset := controlUnit.io.reset
  dataMemory.io.writeEnable := controlUnit.io.writeEnable
  controlUnit.io.fromAluEqualsCheck := alu.io.equalCheck

  ////////////////////////////////////////////

  //This signals are used by the tester for loading the program to the program memory, do not touch
  programMemory.io.testerAddress := io.testerProgMemAddress
  io.testerProgMemDataRead := programMemory.io.testerDataRead
  programMemory.io.testerDataWrite := io.testerProgMemDataWrite
  programMemory.io.testerEnable := io.testerProgMemEnable
  programMemory.io.testerWriteEnable := io.testerProgMemWriteEnable
  //This signals are used by the tester for loading and dumping the data memory content, do not touch
  dataMemory.io.testerAddress := io.testerDataMemAddress
  io.testerDataMemDataRead := dataMemory.io.testerDataRead
  dataMemory.io.testerDataWrite := io.testerDataMemDataWrite
  dataMemory.io.testerEnable := io.testerDataMemEnable
  dataMemory.io.testerWriteEnable := io.testerDataMemWriteEnable
}