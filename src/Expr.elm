module Expr exposing (..)

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


stringOf : Expr -> String
stringOf expr =
    case expr of
        AppExpr op t e ->
            let
                s1 = stringOfTerm t
                sop =
                    case op of
                        Add -> " + "
                        Sub -> " - "
                s2 = stringOf e
            in
            s1 ++ sop ++ s2
        ETerm t -> stringOfTerm t

stringOfTerm : Term -> String
stringOfTerm term =
    case term of
        AppTerm op f t ->
            let
                s1 = stringOfFactor f
                sop =
                    case op of
                        Mul -> " * "
                        Div -> " / "
                        Mod -> " % "
                s2 = stringOfTerm t
            in
            s1 ++ sop ++ s2
        TFactor t -> stringOfFactor t


stringOfFactor : Factor -> String
stringOfFactor factor =
    case factor of
        FExpr exp ->
            "(" ++ stringOf exp ++ ")"
        Con n ->
            String.fromInt n
        Var v ->
            v
