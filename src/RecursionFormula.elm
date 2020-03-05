module RecursionFormula exposing (..)

import Parser exposing (..)
import Set exposing (empty)

type State = Applied | Editting

type ExprOp = Add | Sub

type TermOp = Mul | Div | Mod

type Expr
    = AppExpr ExprOp Term Expr
    | ETerm Term

type Term
    = AppTerm TermOp Factor Term
    | TFactor Factor

type Factor
    = FExpr Expr
    | Con Int
    | Var String


type alias RecursionFormula =
    { arg1 : String
    , arg2 : String
    , term : String
    , state : State
    }


init : () -> RecursionFormula
init _ =
    RecursionFormula "0" "0" "1" Editting


updateArg : Int -> String -> RecursionFormula -> RecursionFormula
updateArg n arg f =
    let
        ( arg1, arg2 ) = if n == 1 then ( arg, f.arg2 ) else ( f.arg1, arg )
    in
    { f | arg1 = arg1, arg2 = arg2 }


updateTerm : String -> RecursionFormula -> RecursionFormula
updateTerm term f =
    { f | term = term }


exprParser : Parser Expr
exprParser =
    oneOf
        [ succeed (\term op expr -> AppExpr op term expr)
            |. backtrackable spaces
            |= backtrackable termParser
            |. spaces
            |= backtrackable 
                ( oneOf
                    [ map (\_ -> Add) (symbol "+")
                    , map (\_ -> Sub) (symbol "-")
                    ]
                )
            |. spaces
            |= lazy (\_ -> exprParser)
            |. spaces
        , map ETerm termParser
        ]

termParser : Parser Term
termParser =
    oneOf
        [ succeed (\factor op term -> AppTerm op factor term)
            |. backtrackable spaces
            |= backtrackable factorParser
            |. backtrackable spaces
            |= backtrackable 
                ( oneOf
                    [ map (\_ -> Mul) (symbol "*")
                    , map (\_ -> Div) (symbol "/")
                    , map (\_ -> Mod) (symbol "%")
                    ]
                )
            |. backtrackable spaces
            |= (lazy (\_ -> termParser))
            |. spaces
        , map TFactor factorParser
        ]

factorParser : Parser Factor
factorParser =
    oneOf
        [ succeed FExpr
            |. backtrackable spaces
            |. (symbol "(")
            |. spaces
            |= (lazy (\_ -> exprParser))
            |. spaces
            |. (symbol ")")
            |. spaces
        , map Con int
        , map Var
            ( variable
                { start = Char.isAlpha
                , inner = Char.isAlpha
                , reserved = Set.empty
                }
            )
        ]


parseExpr : String ->  Result (List DeadEnd) Expr
parseExpr str =
    run exprParser str

apply : RecursionFormula -> RecursionFormula
apply f =
    case f.state of
        Applied -> f
        Editting -> { f | state = Applied }


stringOf : RecursionFormula -> String
stringOf rf =
    "dp[" ++ rf.arg1 ++ "][" ++ rf.arg2 ++ "] = " ++ rf.term