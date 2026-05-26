module RobLang.Parser.Keyword

open FParsec

type KwParser = Parser<unit, unit>

let letKw       : KwParser = pstring "let"    >>% ()
let assignKw    : KwParser = pstring "="      >>% ()
let inKw        : KwParser = pstring "in"     >>% ()
let forKw       : KwParser = pstring "for"    >>% ()
let forStartKw  : KwParser = pstring "do"     >>% ()
let whileKw     : KwParser = pstring "while"  >>% ()
let ifKw        : KwParser = pstring "if"     >>% ()
let elseKw      : KwParser = pstring "else"   >>% ()
let returnKw    : KwParser = pstring "return" >>% ()
let breakKw     : KwParser = pstring "break"  >>% ()
let robStmtKw   : KwParser = pstring "robot"  >>% ()
let robAccessKw : KwParser = pchar '.'        >>% ()
let endStmtKw   : KwParser = pchar ';'        >>% ()
let endBlockKw  : KwParser =
  pstring "end"  .>> notFollowedBy (letter <|> digit <|> pchar '_') >>% ()
let fnKw        : KwParser = pstring "fn"     >>% ()

let parens (p: Parser<'a, unit>) : Parser<'a, unit> =
  between (pchar '(') (pchar ')') p

let list
  (s   : Parser<'a, unit>)
  (e   : Parser<'b, unit>)
  (elem: Parser<'c, unit>)
  : Parser<'c list, unit> =
  between s e (sepBy elem (spaces >>. pchar ',' .>> spaces))
