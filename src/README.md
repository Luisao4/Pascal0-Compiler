**Lexer.x Parser.y AST.hs e Main.hs programas 100% funcionais.**
**Typecheck.hs com erros de tipo.**
_CMips.hs -> incompleto_


Compilar manualmente:
alex Lexer.x && happy Parser.y && ghc AST.hs && ghc Main.hs 
