structure Top =
struct
    fun readAndPrintLoop env gamma stream =
        let
            (*val stream = discardSemicolons stream*)
            val (dec, stream) = Parser.doParse stream
            val newGamma = Typeinf.typeinf gamma dec
            val newEnv = Eval.eval env dec
        in
            readAndPrintLoop newEnv newGamma stream
        end
    (*fun subTop inStream =
        let
            val lexer = Lexer.makeLexer inStream
        in
            (readAndPrintLoop lexer; TextIO.closeIn inStream)
        end
        handle Lexer.EOF => (TextIO.closeIn inStream)*)
    fun top file =
        let
            val inStream = case file of "" => TextIO.stdIn
                                      | _ => TextIO.openIn file
            val stream = Parser.makeStream inStream
        in
            readAndPrintLoop Value.emptyEnv TypeUtils.emptyTyEnv stream
            handle Parser.EOF => ()
                | Parser.ParseError => print "Syntax error\n"
                | Typeinf.TypeError => print "Type error\n"
                | Eval.RuntimeError => print "Runtime error";
            case file of "" => ()
                       | _ => TextIO.closeIn inStream
        end
end
