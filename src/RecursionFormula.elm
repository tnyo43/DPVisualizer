module RecursionFormula exposing (..)

import Expr as Expr


type alias RFEditting =
    { arg1 : String
    , arg2 : String
    , term : String
    }

type alias RFFixed =
    { arg1 : Expr.Term
    , arg2 : Expr.Term
    , term : Expr.Term
    }

type RecursionFormula
    = Editting RFEditting
    | Fixed RFFixed


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


fix : RecursionFormula -> RecursionFormula
fix f =
    case f of
        Fixed _ -> f
        Editting rf ->
            let
                rf_ = Expr.parse rf.arg1
                        |> Result.andThen (\arg1 -> Expr.parse rf.arg2
                        |> Result.andThen (\arg2 -> Expr.parse rf.term
                        |> Result.andThen (\term -> RFFixed arg1 arg2 term |> Ok)))
            in
            case rf_ of
                Ok rrf -> Fixed rrf
                _ -> f


isEditting : RecursionFormula -> Bool
isEditting rf =
    case rf of
        Editting _ -> True
        _ -> False


stringOf : RecursionFormula -> String
stringOf rf =
    let
        (s1, s2, st) =
            case rf of
                Fixed f ->
                    ( (Expr.stringOf f.arg1), (Expr.stringOf f.arg2), (Expr.stringOf f.term) )
                Editting f ->
                    ( f.arg1, f.arg2, f.term )
    in
    "dp[" ++ s1 ++ "][" ++ s2 ++ "] = " ++  st