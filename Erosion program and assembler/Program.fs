module compiler =
    open System
    open System.IO
    open System.Diagnostics

    let rec padZeroes n =
        if n > 0 then "0" + padZeroes(n-1) else ""

    let rec toBinary (number, n) =
        if n = 0 then ""
        elif number = 0 then padZeroes(n)
        elif number % 2 = 0 then toBinary(number / 2, n-1) + "0"
        else toBinary((number-1) / 2, n-1) + "1"

    let chiselWrap (O, R1, R2, R3, C, E) =
        "\"b"+O+"_"+R1+"_"+R2+"_"+R3+C+"\".U(32.W), //"+E+""

    //Removes the R and turns the register number into a string of the right binary value
    let RTB (s : string) =
        toBinary (int <| s.[1..], 5)
        
    let stupid s =
        match s with
        | a::[] ->                  printfn "Unexpected instruction: %s" a
        | a::b::[] ->               printfn "Unexpected instruction: %s %s" a b
        | a::b::c::[] ->            printfn "Unexpected instruction: %s %s %s" a b c
        | a::b::c::d::[] ->         printfn "Unexpected instruction: %s %s %s %s" a b c d
        exit 1

    let parseInstruction (inst : string) = 
        printf "Now parsing: %s \n" inst
        match Array.toList <| inst.Split(' ') with
            //Arithmetic
            | "ADD"::R1::R2::R3::[] ->  chiselWrap("010100", RTB(R1), RTB(R2), RTB(R3), toBinary(0,11), "ADD")
            | "ADDI"::R1::R2::C::[] ->  chiselWrap("100100", RTB(R1), RTB(R2), "", toBinary(int C, 16), "ADDI")
            | "ADDU"::R1::R2::R3::[] -> chiselWrap("010000", RTB(R1), RTB(R2), RTB(R3), toBinary(0,11), "ADDU")
            | "ADDIU"::R1::R2::C::[] -> chiselWrap("100110", RTB(R1), RTB(R2), "", toBinary(int C, 16), "ADDIU")
            | "MUL"::R1::R2::R3::[] ->  chiselWrap("010101", RTB(R1), RTB(R2), RTB(R3), toBinary(0,11), "MUL")
            | "MULU"::R1::R2::R3::[] -> chiselWrap("010001", RTB(R1), RTB(R2), RTB(R3), toBinary(0,11), "MULU")
            | "DIV"::R1::R2::R3::[] ->  chiselWrap("010110", RTB(R1), RTB(R2), RTB(R3), toBinary(0,11), "DIV")
            | "DIVU"::R1::R2::R3::[] -> chiselWrap("010010", RTB(R1), RTB(R2), RTB(R3), toBinary(0,11), "DIVU")
            | "NEG"::R1::[] ->          chiselWrap("010111", RTB(R1), toBinary(0, 5), "", toBinary(0, 16), "NEG")
            | "INC"::R1::[] ->          chiselWrap("010011", RTB(R1), toBinary(0, 5), "", toBinary(0, 16), "INC")
            //Logical
            | "OR"::R1::R2::R3::[] ->   chiselWrap("011000", RTB(R1), RTB(R2), RTB(R3), toBinary(0,11), "OR")
            | "AND"::R1::R2::R3::[] ->  chiselWrap("011001", RTB(R1), RTB(R2), RTB(R3), toBinary(0,11), "AND")
            | "ORI"::R1::R2::C::[] ->   chiselWrap("100000", RTB(R1), RTB(R2), "", toBinary(int C, 16), "ORI")
            | "ANDI"::R1::R2::C::[] ->  chiselWrap("100001", RTB(R1), RTB(R2), "", toBinary(int C, 16), "ANDI")
            | "XOR"::R1::R2::R3::[] ->  chiselWrap("011010", RTB(R1), RTB(R2), RTB(R3), toBinary(0,11), "XOR")
            | "NOT"::R1::[] ->          chiselWrap("011011", RTB(R1), toBinary(0, 5), "", toBinary(0, 16), "NOT")
            | "BSL"::R1::R2::C::[] ->   chiselWrap("100010", RTB(R1), RTB(R2), "", toBinary(int C, 16), "BSL")
            | "BSR"::R1::R2::C::[] ->   chiselWrap("100011", RTB(R1), RTB(R2), "", toBinary(int C, 16), "BSR")
            //Memory management
            | "MOVR"::R1::R2::[] ->      chiselWrap("101000", RTB(R1), RTB(R2), "", toBinary(0, 16), "MOVR")
            | "LOAD"::R1::R2::C::[] ->  chiselWrap("101011", RTB(R1), RTB(R2), "", toBinary(int C, 16), "LOAD")
            | "STOR"::R1::R2::C::[] ->  chiselWrap("101010", RTB(R1), RTB(R2), "", toBinary(int C, 16), "STOR")
            | "MOV"::R1::C::[] ->      chiselWrap("101100", RTB(R1), toBinary(0, 5), "", toBinary(int C, 16), "MOV")
            | "MOVU"::R1::C::[] ->      chiselWrap("101110", RTB(R1), toBinary(0, 5), "", toBinary(int C, 16), "MOVU")
            //Branching
            | "JMP"::C::[] ->           chiselWrap("110000", toBinary(0, 5), toBinary(0, 5), "", toBinary(int C, 16), "JMP")
            | "JEQ"::R1::R2::C::[] ->   chiselWrap("111100", RTB(R1), RTB(R2), "", toBinary(int C, 16), "JEQ")
            | "JNE"::R1::R2::C::[] ->   chiselWrap("111000", RTB(R1), RTB(R2), "", toBinary(int C, 16), "JNE")
            | "JAL"::C::[] ->           chiselWrap("110110", toBinary(0, 5), toBinary(0, 5), "", toBinary(int C, 16), "JAL")
            | "RET"::[] ->              chiselWrap("110100", toBinary(0, 5), toBinary(0, 5), "", toBinary(0, 16), "RET")
            //Comparison
            | "SLT"::R1::R2::R3::[] ->  chiselWrap("011100", RTB(R1), RTB(R2), RTB(R3), toBinary(0,11), "SLT")
            | "SLI"::R1::R2::C::[] ->   chiselWrap("011101", RTB(R1), RTB(R2), "", toBinary(int C, 16), "SLI")
            | "SLU"::R1::R2::R3::[] ->  chiselWrap("011110", RTB(R1), RTB(R2), RTB(R3), toBinary(0,11), "SLU")
            | "SLUI"::R1::R2::C::[] ->  chiselWrap("011111", RTB(R1), RTB(R2), "", toBinary(int C, 16), "SLUI")
            //Special
            | "NOP"::[] ->              chiselWrap("000000", toBinary(0, 5), toBinary(0, 5), "", toBinary(0, 16), "NOP")
            | "NOOP"::[] ->             chiselWrap("000000", toBinary(0, 5), toBinary(0, 5), "", toBinary(0, 16), "NOP")
            | "HALT"::[] ->             chiselWrap("111111", toBinary(0, 5), toBinary(0, 5), "", toBinary(0, 16), "HALT")
            //Error handling
            | a ->                      stupid a            
                
    let rec assemble (theList : string list) =
        try
        match theList with
            | [] -> []
            | a::[] -> parseInstruction a ::[]
            | a::b -> parseInstruction a :: assemble b 
        with
            | :? System.Exception as ex -> printf "Could not assemble code. Error in file contents"; exit 1

    [<EntryPoint>]
    let main argv =
        if argv.Length < 1 then printf <| "First argument can't be empty" else printf "Assembling %s \n" <| argv[0]
        if argv.Length < 1 then 1 else
        
        printf("Reading file...")
        let file = File.ReadAllLines(argv[0])
        printf("Done \n")

        let temp = Array.toList <| argv[0].Split('\\')
        let temp2 =  Array.toList <| temp[temp.Length - 1].Split('.')
        let outName = temp2.Head + "_assembled.txt"

        printf("Writing to .\%s \n") outName
        File.WriteAllLines(@".\"+outName, assemble <| Array.toList file)

        printf("Done \n")

        0