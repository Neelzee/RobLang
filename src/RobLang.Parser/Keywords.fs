module RobLang.Parser.Keyword

open FParsec

type KwParser = Parser<unit, unit>

let letKw: KwParser = pstring "let" >>% ()

let assignmentKw: KwParser = pstring "=" >>% ()

let forKw: KwParser = pstring "for" >>% ()

let forStartKw: KwParser = pstring "do" >>% ()

let whileKw: KwParser = pstring "while" >>% ()

let ifKw: KwParser = pstring "if" >>% ()

let elseKw: KwParser = pstring "else" >>% ()

let returnKw: KwParser = pstring "return" >>% ()

let parens (p: Parser<'a, unit>) : Parser<'a, unit> =
  pchar '(' >>. p .>> pchar ')'

let list
  (s: Parser<'a, unit>)
  (e: Parser<'b, unit>)
  (elem: Parser<'c, unit>)
  : Parser<'c list, unit> =
  between s e (sepBy elem (spaces >>. pchar ',' .>> spaces)) 

let endStmtKw: KwParser = pchar ';' >>% ()

let consumeSpaces (p: Parser<'a, unit>): Parser<'a, unit> =
  between spaces1 spaces1 p