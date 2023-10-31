import chisel3._
import chisel3.util._
import chisel3.iotesters
import chisel3.iotesters.PeekPokeTester
import java.util

object Programs{

  //Test program that writes the address number to that specific address up to 800
  val program3 = Array(
    "b000000_00000_00000_0000000000000000".U(32.W), //NOP

    "b101100_00001_00000_0000000000000000".U(32.W), //MOVI R1 0
    "b101100_00010_00000_0000001100100000".U(32.W), //MOVI R2 800
    "b101010_00001_00001_0000000000000000".U(32.W), //STOR R1 R1 0
    "b010011_00001_00000_0000000000000000".U(32.W), //INC  R1
    "b111000_00001_00010_0000000000000011".U(32.W), //JNE  R1 R2 3

    "b111111_00000_00000_0000000000000000".U(32.W)  //HALT
  )

  //The erosion algorithm itself
  val program1 = Array(
    "b000000_00000_00000_0000000000000000".U(32.W), //NOP

    // Sæt konstant
    "b101000_00001_00000_0000000000000001".U(32.W),

    // Erode hjørner
    "b101010_00000_00000_0000000110010000".U(32.W),
    "b101010_00000_00000_0000000110100100".U(32.W),
    "b101010_00000_00000_0000011000111111".U(32.W),
    "b101010_00000_00000_0000011000011000".U(32.W),

    // Sæt konstanter til multiplilation
    "b101000_10100_00000_0000000000010100".U(32.W),
    "b101000_01010_00000_0000000000000001".U(32.W),

    // Sæt konstanter til loops
    "b101000_01001_00000_0000000000010100".U(32.W),
    "b101000_01000_00000_0000000000000010".U(32.W),

    //Under er den 0000 0000 0000 1010. operation. #1LOOP

    // Erode Øverste og nederste kant
    "b101010_00000_01000_0000000110010000".U(32.W), //#1LOOP (10)
    "b101010_00000_01000_0000001100001101".U(32.W),

    // Erode Venstre og højre kant
    "b010101_01010_10100_0100000000000000".U(32.W),
    "b101010_00000_01010_0000000101111101".U(32.W),
    "b101010_00000_01010_0000000110010000".U(32.W),

    // Sæt konstanter til indre loop
    "b101000_10000_00000_0000000000000001".U(32.W),
    "b101000_10001_00000_0000000000010011".U(32.W),
    "b101000_10011_00000_0000000000000001".U(32.W),
    "b101000_10100_00000_0000000000000001".U(32.W),

    //Under er den 0000 0000 0001 0011. operation. #2LOOP

    // Sæt pi0el der skal erodes
    "b010101_10010_10000_1010000000000000".U(32.W), //#2LOOP (19)
    "b010100_10010_10010_0100000000000000".U(32.W),

    //Spring hvis pi0el er sort
    "b111100_10010_00000_0000000000011111".U(32.W),

    // Tjek nedre pi0el
    "b100100_10011_10010_0000000000010100".U(32.W),
    "b111100_10011_00000_0000000000100100".U(32.W),

    // Tjek øvre pi0el
    "b100100_10011_10010_1111111111101100".U(32.W),
    "b111100_10011_00000_0000000000100100".U(32.W),

    // Tjek højre pi0el
    "b100100_10011_10010_0000000000000001".U(32.W),
    "b111100_10011_00000_0000000000100100".U(32.W),

    // Tjek venstre pi0el
    "b100100_10011_10010_1111111111111111".U(32.W),
    "b111100_10011_00000_0000000000100100".U(32.W),

    // Lad vær med at erode'e pi0el'en
    "b101010_00001_10010_0000000110010000".U(32.W),

    //Under er den 0000 0000 0001 1110. operation. #2LOOPINC

    // Inkrement indre loop
    "b010011_10000_00000_0000000000000000".U(32.W), //#2LOOPINC (31)
    "b111000_10000_10001_0000000000010011".U(32.W),

    // Inkrement ydre loop
    "b010011_01000_00000_0000000000000000".U(32.W),
    "b111000_01000_01001_0000000000001010".U(32.W),

    "b111111_00000_00000_0000000000000000".U(32.W), //HALT

    //Under er den 0000 0000 0010 0011. operation. #ERODE

    // Erode pi0el
    "b101010_00000_10010_0000000110010000".U(32.W), //#ERODE (36)
    "b110000_00000_00000_0000000000011111".U(32.W)
  )

  val program2 = Array(
    "h00000000".U(32.W),
    "h00000000".U(32.W),
    "h00000000".U(32.W),
    "h00000000".U(32.W),
    "h00000000".U(32.W),
    "h00000000".U(32.W),
    "h00000000".U(32.W),
    "h00000000".U(32.W),
    "h00000000".U(32.W),
    "h00000000".U(32.W),
    "h00000000".U(32.W),
    "h00000000".U(32.W)
  )
}