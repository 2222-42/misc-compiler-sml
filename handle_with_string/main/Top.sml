structure Top =
struct
    fun readAndPrintLoop lexer =
        let
            val token = lexer()
            val _ = print (Token.toString token ^ "\n")
        in
            readAndPrintLoop lexer
        end
    (*    fun subTop (lexer, inStream) =
        (readAndPrintLoop lexer; TextIO.closeIn inStream)
        handle Lexer.EOF => (TextIO.closeIn inStream)*)
    fun top file =
        let
            val inStream = TextIO.openIn file
            val lexer = Lexer.makeLexer inStream
        in
            readAndPrintLoop lexer;
            TextIO.closeIn inStream
                           (* readAndPrintLoopのところで例外が起きたら、closeが適切に走らないのでは、という疑問がある。*)
        end
        handle Lexer.EOF => ()
end
