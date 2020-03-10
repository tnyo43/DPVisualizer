module RecursionFormula exposing (..)

import Array exposing (Array)
import Expr as Expr


type alias FEditting =
    { arg1 : String
    , arg2 : String
    , body : String
    , for : Array ( String, String, String )
    }

type alias FFixed =
    { arg1 : Expr.Term
    , arg2 : Expr.Term
    , body : Expr.Term
    , for : Array Expr.For
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
 FEditting "0" "0" "1" Array.empty |> Editting


init : () -> RecursionForumulas
init _ =
    RecursionForumulas ( Array.empty ) ( Array.empty )


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
                ( a1, a2, b ) =
                    case idx of
                        0 -> ( text, ef.arg2, ef.body )
                        1 -> ( ef.arg1, text, ef.body )
                        _ -> ( ef.arg1, ef.arg2, text )
            in
            Array.set row ( Editting { ef | arg1 = a1, arg2 = a2, body = b } ) fs
        _ -> fs


updateInit : Int -> Int -> String -> RecursionForumulas -> RecursionForumulas
updateInit row idx text rf =
    { rf | init = update row idx text rf.init }


updateRecursion : Int -> Int -> String -> RecursionForumulas -> RecursionForumulas
updateRecursion row idx text rf =
    { rf | recursion = update row idx text rf.recursion }


addInitFor : Int -> RecursionForumulas -> RecursionForumulas
addInitFor row rf =
    case Array.get row rf.init of
        Just ( Editting ef ) ->
            { rf | init = Array.set row ( Editting { ef | for = Array.push ( "", "", "" ) ef.for } ) rf.init }
        _ -> rf


addRecursionFor : Int -> RecursionForumulas -> RecursionForumulas
addRecursionFor row rf =
    case Array.get row rf.recursion of
        Just ( Editting ef ) ->
            { rf | recursion = Array.set row ( Editting { ef | for = Array.push ( "", "", "" ) ef.for } ) rf.recursion }
        _ -> rf


resetInitFor : Int -> RecursionForumulas -> RecursionForumulas
resetInitFor row rf =
    case Array.get row rf.init of
        Just ( Editting ef ) ->
            { rf | init = Array.set row ( Editting { ef | for = Array.empty } ) rf.init }
        _ -> rf


resetRecursionFor : Int -> RecursionForumulas -> RecursionForumulas
resetRecursionFor row rf =
    case Array.get row rf.recursion of
        Just ( Editting ef ) ->
            { rf | recursion = Array.set row ( Editting { ef | for = Array.empty } ) rf.recursion }
        _ -> rf


updateFor : Int -> Int -> Int -> String -> Array Formula -> Array Formula
updateFor row rowFor idx text fs =
    case Array.get row fs of
        Just ( Editting ef ) ->
            let
                forArray =
                    Array.get rowFor ef.for |> Maybe.andThen (\(var, begin, end) ->
                        Array.set
                            rowFor
                            ( case idx of
                                    0 -> ( text, begin, end )
                                    1 -> ( var, text, end )
                                    _ -> ( var, begin, text )
                            )
                            ef.for
                        |> Just
                    )
                    |> Maybe.withDefault ef.for
            in
            Array.set row ( Editting { ef | for = forArray } ) fs
        _ -> fs


updateInitFor : Int -> Int -> Int -> String -> RecursionForumulas -> RecursionForumulas
updateInitFor row rowFor idx text rf =
    { rf | init = updateFor row rowFor idx text rf.init }


updateRecursionFor : Int -> Int -> Int -> String -> RecursionForumulas -> RecursionForumulas
updateRecursionFor row rowFor idx text rf =
    { rf | recursion = updateFor row rowFor idx text rf.recursion }


fix : Formula -> Formula
fix frm =
    case frm of
        Editting ef ->
            let
                triedFrm =
                    Expr.parse ef.arg1 |> Result.andThen (\arg1 ->
                    Expr.parse ef.arg2 |> Result.andThen (\arg2 ->
                    Expr.parse ef.body |> Result.andThen (\body ->
                    Expr.parseForArray ef.for |> Result.andThen (\for ->
                        FFixed arg1 arg2 body for |> Ok
                    ))))
            in
            case triedFrm of
                Ok ff -> Fixed ff
                _ -> frm
        Fixed _ -> frm


fixInit : Int -> RecursionForumulas -> RecursionForumulas
fixInit row rf =
    let
        ini =
            Array.get row rf.init |> Maybe.andThen (\f ->
                Array.set row ( fix f ) rf.init |> Just
            )
            |> Maybe.withDefault rf.init
    in
    { rf | init = ini }


fixRecursion : Int -> RecursionForumulas -> RecursionForumulas
fixRecursion row rf =
    let
        recursion =
            Array.get row rf.recursion |> Maybe.andThen (\f ->
                case fix f of
                    Fixed ff ->
                        if Expr.isIncludingDP ff.body
                        then Array.set row (Fixed ff) rf.recursion |> Just
                        else Nothing
                    _ -> Nothing
            )
            |> Maybe.withDefault rf.recursion
    in
    { rf | recursion = recursion }


fixedFormulasOf : RecursionForumulas -> Array FFixed
fixedFormulasOf rf =
    Array.append rf.init rf.recursion
    |> Array.foldl
        (\f accArray -> case f of
            Fixed ff -> Array.push ff accArray
            _ -> accArray
        ) Array.empty


stringOfFormula : Formula -> String
stringOfFormula f =
    let
        (s1, s2, st) =
            case f of
                Fixed ff ->
                    ( (Expr.stringOf ff.arg1), (Expr.stringOf ff.arg2), (Expr.stringOf ff.body) )
                Editting ef ->
                    ( ef.arg1, ef.arg2, ef.body )
    in
    "dp[" ++ s1 ++ "][" ++ s2 ++ "] = " ++  st


stringOfFor : Formula -> String
stringOfFor f =
    let
        forTpls =
            case f of
                Fixed ff ->
                    Array.toList ff.for
                    |> List.map (\for -> ( for.var, Expr.stringOf for.begin, Expr.stringOf for.end ))
                Editting ef ->
                    Array.toList ef.for
    in
    forTpls
    |> List.map (\(var, begin, end) ->
                "for (" ++ var ++ " = " ++ begin ++ "; " ++ var ++ " < " ++ end ++ "; " ++ var ++ "++)"
        )
    |> List.foldr (\text acc -> text ++ " " ++ acc) ""