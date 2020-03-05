module RecursionFormula exposing (..)

import Parser exposing (..)
import Set exposing (empty)


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


type alias RFEditting =
    { arg1 : String
    , arg2 : String
    , term : String
    }

type alias RFApplied =
    { arg1 : Expr
    , arg2 : Expr
    , term : Expr
    }

type RecursionFormula
    = Editting RFEditting
    | Applied RFApplied

init : () -> RecursionFormula
init _ =
    RFEditting "0" "0" "1" |> Editting


updateArg : Int -> String -> RecursionFormula -> RecursionFormula
updateArg n arg rf =
    case rf of
        Editting f ->
            let
                ( arg1, arg2 ) = if n == 1 then ( arg, f.arg2 ) else ( f.arg1, arg )
            in
            Editting { f | arg1 = arg1, arg2 = arg2 }
        _ -> rf


updateTerm : String -> RecursionFormula -> RecursionFormula
updateTerm term rf =
    case rf of
        Editting f -> Editting { f | term = term }
        _ -> rf


exprParser : Parser Expr
exprParser =
    oneOf
        [ succeed (\term op expr -> AppExpr op term expr)
            |. backtrackable spaces
            |= backtrackable termParser
            |. spaces
            |= oneOf
                [ map (\_ -> Add) (symbol "+")
                , map (\_ -> Sub) (symbol "-")
                ]
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
            |= oneOf
                [ map (\_ -> Mul) (symbol "*")
                , map (\_ -> Div) (symbol "/")
                , map (\_ -> Mod) (symbol "%")
                ]
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
            |. symbol "("
            |. spaces
            |= (lazy (\_ -> exprParser))
            |. spaces
            |. symbol ")"
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
    case f of
        Applied _ -> f
        Editting rf ->
            let
                rf_ = parseExpr rf.arg1
                        |> Result.andThen (\arg1 -> parseExpr rf.arg2
                        |> Result.andThen (\arg2 -> parseExpr rf.term
                        |> Result.andThen (\term -> RFApplied arg1 arg2 term |> Ok)))
            in
            case rf_ of
                Ok rrf -> Applied rrf
                _ -> f


isEditting : RecursionFormula -> Bool
isEditting rf =
    case rf of
        Editting _ -> True
        _ -> False


stringOf : RecursionFormula -> String
stringOf _ = "dp[0][0] = 1"