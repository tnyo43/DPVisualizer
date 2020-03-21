module RecursionFormula exposing (..)

import Array exposing (Array)
import Expr as Expr


type Formula
    = Editting
        { arg1 : String
        , arg2 : Maybe String
        , body : String
        , for : Array ( String, String, String )
        }
    | Fixed
        { arg1 : Expr.Term
        , arg2 : Maybe Expr.Term
        , body : Expr.Term
        , for : Array Expr.For
        }


type alias RecursionFormulas =
    { init : Array Formula
    , recursion : Array Formula
    }


initEditting : Int -> Formula
initEditting dim =
    if dim == 1
    then makeEditting "0" Nothing "1" Array.empty
    else makeEditting "0" (Just "0") "1" Array.empty


init : () -> RecursionFormulas
init _ =
    RecursionFormulas ( Array.empty ) ( Array.empty )


makeEditting : String -> Maybe String -> String -> Array (String, String, String) -> Formula
makeEditting arg1 arg2 body for =
    Editting
        { arg1 = arg1
        , arg2 = arg2
        , body = body
        , for = for
        }

makeFixed : Expr.Term -> Maybe Expr.Term -> Expr.Term -> Array Expr.For -> Formula
makeFixed arg1 arg2 body for =
    Fixed
        { arg1 = arg1
        , arg2 = arg2
        , body = body
        , for = for
        }


isEditting : Formula -> Bool
isEditting f =
    case f of
        Editting _ -> True
        _ -> False


isFixed : Formula -> Bool
isFixed f =
    case f of
        Fixed _ -> True
        _ -> False


add : Int -> Array Formula -> Array Formula
add dim fs =
    if Array.length ( Array.filter isEditting fs ) > 0
    then fs
    else Array.push ( initEditting dim ) fs


addInit : Int -> RecursionFormulas -> RecursionFormulas
addInit dim rf =
    { rf | init = add dim rf.init }


addRecursion : Int -> RecursionFormulas -> RecursionFormulas
addRecursion dim rf =
    { rf | recursion = add dim rf.recursion }


remove : Int -> Array Formula -> Array Formula
remove n fs =
    Array.append ( Array.slice 0 n fs ) ( Array.slice (n+1) (Array.length fs) fs )


removeInit : Int -> RecursionFormulas -> RecursionFormulas
removeInit n rf =
    { rf | init = remove n rf.init }


removeRecursion : Int -> RecursionFormulas -> RecursionFormulas
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
                        1 -> ( ef.arg1, Just text, ef.body )
                        _ -> ( ef.arg1, ef.arg2, text )
            in
            Array.set row ( Editting { ef | arg1 = a1, arg2 = a2, body = b } ) fs
        _ -> fs


updateInit : Int -> Int -> String -> RecursionFormulas -> RecursionFormulas
updateInit row idx text rf =
    { rf | init = update row idx text rf.init }


updateRecursion : Int -> Int -> String -> RecursionFormulas -> RecursionFormulas
updateRecursion row idx text rf =
    { rf | recursion = update row idx text rf.recursion }


addInitFor : Int -> RecursionFormulas -> RecursionFormulas
addInitFor row rf =
    case Array.get row rf.init of
        Just ( Editting ef ) ->
            { rf | init = Array.set row ( Editting { ef | for = Array.push ( "", "", "" ) ef.for } ) rf.init }
        _ -> rf


addRecursionFor : Int -> RecursionFormulas -> RecursionFormulas
addRecursionFor row rf =
    case Array.get row rf.recursion of
        Just ( Editting ef ) ->
            { rf | recursion = Array.set row ( Editting { ef | for = Array.push ( "", "", "" ) ef.for } ) rf.recursion }
        _ -> rf


resetInitFor : Int -> RecursionFormulas -> RecursionFormulas
resetInitFor row rf =
    case Array.get row rf.init of
        Just ( Editting ef ) ->
            { rf | init = Array.set row ( Editting { ef | for = Array.empty } ) rf.init }
        _ -> rf


resetRecursionFor : Int -> RecursionFormulas -> RecursionFormulas
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


updateInitFor : Int -> Int -> Int -> String -> RecursionFormulas -> RecursionFormulas
updateInitFor row rowFor idx text rf =
    { rf | init = updateFor row rowFor idx text rf.init }


updateRecursionFor : Int -> Int -> Int -> String -> RecursionFormulas -> RecursionFormulas
updateRecursionFor row rowFor idx text rf =
    { rf | recursion = updateFor row rowFor idx text rf.recursion }


fix : Formula -> Formula
fix frm =
    case frm of
        Editting ef ->
            Expr.parse ef.arg1 |> Result.andThen (\arg1 ->
            ( case ef.arg2 of
                Nothing ->
                    Ok Nothing
                Just a2 ->
                    Expr.parse a2 |> Result.andThen (\arg2 -> Just arg2 |> Ok) )
            |> Result.andThen (\arg2 ->
            Expr.parse ef.body |> Result.andThen (\body ->
            Expr.parseForArray ef.for |> Result.andThen (\for ->
                makeFixed arg1 arg2 body for |> Ok
            ))))
            |> Result.withDefault frm
        Fixed _ -> frm


fixFormulasOfIdx : Bool -> Int -> Array Formula -> Array Formula
fixFormulasOfIdx isInit row fs =
    Array.get row fs |> Maybe.andThen (\f ->
        case fix f of
            Fixed ff ->
                if isInit
                then
                    if Expr.isIncludingDP ff.body |> not
                    then Array.set row (Fixed ff) fs |> Just else Nothing
                else
                    if Expr.isIncludingDP ff.body && not (Array.isEmpty ff.for)
                    then Array.set row (Fixed ff) fs |> Just else Nothing
            _ -> Nothing
    )
    |> Maybe.withDefault fs


fixInit : Int -> RecursionFormulas -> RecursionFormulas
fixInit row rf =
    { rf | init = fixFormulasOfIdx True row rf.init }


fixRecursion : Int -> RecursionFormulas -> RecursionFormulas
fixRecursion row rf =
    { rf | recursion = fixFormulasOfIdx False row rf.recursion }


fixedFormulasOf : RecursionFormulas -> Array Formula
fixedFormulasOf rf =
    Array.append rf.init rf.recursion
    |> Array.filter isFixed


stringOfFormula : Formula -> String
stringOfFormula f =
    let
        (s1, s2, sb) =
            case f of
                Fixed ff ->
                    ( (Expr.stringOf ff.arg1)
                    , (ff.arg2 |> Maybe.andThen (Expr.stringOf >> Just))
                    , (Expr.stringOf ff.body)
                    )
                Editting ef ->
                    ( ef.arg1, ef.arg2, ef.body )
    in
    "dp[" ++ s1 ++ "]"
    ++ (s2 |> Maybe.andThen (\s -> "[" ++ s ++ "]" |> Just) |> Maybe.withDefault "")
    ++ " = "
    ++ sb


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