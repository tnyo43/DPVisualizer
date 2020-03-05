module RecursionFormula exposing (..)

import Parser exposing (..)
import Set exposing (empty)

type State = Applied | Editting


type Expr
    = Con Int
    | Var String
    | AddExpr Int Expr

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
        [ succeed (\e1 e2 -> AddExpr e1 e2)
            |. backtrackable spaces
            |= backtrackable int
            |. backtrackable spaces
            |. symbol "+"
            |. backtrackable spaces
            |= lazy (\_ -> exprParser)
            |. spaces
        , map Con int
        , map Var
            (variable
                { start = Char.isAlpha
                , inner = Char.isAlpha
                , reserved = Set.empty
                })
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