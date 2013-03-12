General code design:

StateT IO {
    InputStream
        ----Parsec---> List of Actions
            ----> [Action -> Terminal -> Terminal]
}
