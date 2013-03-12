General code design:

StateT IO {
    InputStream
        ----Parsec---> List of Actions
            ----> [Action -> Terminal -> Terminal]
}


Good tutorial:

http://www.vex.net/~trebla/haskell/parsec-generally.xhtml
