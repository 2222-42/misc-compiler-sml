structure Top =
struct
    fun readAndPrintLoop inStream =
        let
            val _ = ReadString.skipSpaces inStream
            val s = ReadString.readString inStream
            val _ = print (s ^ "\n")
        in
            readAndPrintLoop inStream
        end
    fun subTop inStream =
        (readAndPrintLoop inStream; TextIO.closeIn inStream)
        handle ReadString.EOF => (TextIO.closeIn inStream)
    fun top file =
        let
            val inStream = TextIO.openIn file
        in
            subTop inStream
        end
        handle ReadString.EOF => ()
end
