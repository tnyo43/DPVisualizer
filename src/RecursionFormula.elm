module RecursionFormula exposing (..)

import Array exposing (Array)
import Expr as Expr


type alias FEditting =
    { arg1 : String
    , arg2 : String
    , term : String
    }

type alias FFixed =
    { arg1 : Expr.Term
    , arg2 : Expr.Term
    , term : Expr.Term
    }

type Formula
    = Editting FEditting
    | Fixed FFixed

type alias RecursionForumulas =
    { init : Array Formula
    , recursion : Array Formula
    }


initFEditting : () -> Formula
initFEditting _ =
 FEditting "0" "0" "1" |> Editting


init : () -> RecursionForumulas
init _ =
    RecursionForumulas ( Array.empty ) ( Array.empty )


updateArg : Int -> String -> Formula -> Formula
updateArg n arg rf =
    case rf of
        Editting f ->
            let
                ( arg1, arg2 ) = if n == 1 then ( arg, f.arg2 ) else ( f.arg1, arg )
            in
            Editting { f | arg1 = arg1, arg2 = arg2 }
        _ -> rf


updateTerm : String -> Formula -> Formula
updateTerm term rf =
    case rf of
        Editting f -> Editting { f | term = term }
        _ -> rf


isEditting : Formula -> Bool
isEditting rf =
    case rf of
        Editting _ -> True
        _ -> False


add : Array Formula -> Array Formula
add fs =
    if Array.length ( Array.filter isEditting fs ) > 0
    then fs
    else Array.push ( initFEditting () ) fs


addInit : RecursionForumulas -> RecursionForumulas
addInit rf =
    { rf | init = add rf.init }


addRecursion : RecursionForumulas -> RecursionForumulas
addRecursion rf =
    { rf | recursion = add rf.recursion }


remove : Int -> Array Formula -> Array Formula
remove n fs =
    Array.append ( Array.slice 0 n fs ) ( Array.slice (n+1) (Array.length fs) fs )


removeInit : Int -> RecursionForumulas -> RecursionForumulas
removeInit n rf =
    { rf | init = remove n rf.init }


removeRecursion : Int -> RecursionForumulas -> RecursionForumulas
removeRecursion n rf =
    { rf | recursion = remove n rf.recursion }


update : Int -> Int -> String -> Array Formula -> Array Formula
update row idx text fs =
    case Array.get row fs of
        Just ( Editting ef ) ->
            let
                ( a1, a2, t ) =
                    case idx of
                        0 -> ( text, ef.arg2, ef.term )
                        1 -> ( ef.arg1, text, ef.term )
                        _ -> ( ef.arg1, ef.arg2, text )
            in
            Array.set row ( Editting { ef | arg1 = a1, arg2 = a2, term = t } ) fs
        _ -> fs


updateInit : Int -> Int -> String -> RecursionForumulas -> RecursionForumulas
updateInit row idx text rf =
    { rf | init = update row idx text rf.init }


updateRecursion : Int -> Int -> String -> RecursionForumulas -> RecursionForumulas
updateRecursion row idx text rf =
    { rf | recursion = update row idx text rf.recursion }


fix : Formula -> Formula
fix frm =
    case frm of
        Editting ef ->
            let
                triedFrm =
                    Expr.parse ef.arg1
                    |> Result.andThen (\arg1 -> Expr.parse ef.arg2
                    |> Result.andThen (\arg2 -> Expr.parse ef.term
                    |> Result.andThen (\term -> FFixed arg1 arg2 term |> Ok)))
            in
            case triedFrm of
                Ok ff -> Fixed ff
                _ -> frm
        Fixed _ -> frm

fixFormulas : Int -> Array Formula -> Array Formula
fixFormulas row fs =
    case Array.get row fs of
        Just f ->
            Array.set row ( fix f ) fs
        _ -> fs
    

fixInit : Int -> RecursionForumulas -> RecursionForumulas
fixInit row rf =
    { rf | init = fixFormulas row rf.init }


fixRecursion : Int -> RecursionForumulas -> RecursionForumulas
fixRecursion row rf =
    { rf | recursion = fixFormulas row rf.recursion }


stringOfFormula : Formula -> String
stringOfFormula f =
    let
        (s1, s2, st) =
            case f of
                Fixed ff ->
                    ( (Expr.stringOf ff.arg1), (Expr.stringOf ff.arg2), (Expr.stringOf ff.term) )
                Editting ef ->
                    ( ef.arg1, ef.arg2, ef.term )
    in
    "dp[" ++ s1 ++ "][" ++ s2 ++ "] = " ++  st
