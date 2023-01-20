-- Implementation of the Johnson Trotter algorithm
-- https://www.geeksforgeeks.org/johnson-trotter-algorithm/
-- The haskell permutations function can't be expressed in Elm I think
-- https://stackoverflow.com/questions/24484348/what-does-this-list-permutations-implementation-in-haskell-exactly-do


module Optimisation.Permutations2 exposing (..)

import Array exposing (Array)
import Json.Decode exposing (array)


type Step a
    = Done
    | Next (State a) (List a)


type alias State a =
    { permutation : Array Element
    , initial : Array a
    }


right =
    -- maybe have a type for this, then can also represent an error type state
    True


left =
    False



-- initialState : Array a -> State a
-- initialState permutation =
--     { permutation = permutation
--     , direction = Dict.fromList (Array.toList (Array.indexedMap (\i _ -> ( i, right )) permutation))
--     }


type alias Element =
    -- parse don't validate this I think
    { initialPosition : Int
    , direction : Bool
    }


nextPermutation : State a -> Step a
nextPermutation state =
    let
        maybeMobile =
            calculateMobile state.permutation
    in
    case maybeMobile of
        Nothing ->
            Done

        Just mobile ->
            let
                permutation =
                    swapAndUpdateDirections mobile state.permutation
            in
            Next
                { state | permutation = permutation }
                (Array.map (\element -> Array.get (element.initialPosition - 1) state.initial) permutation
                    |> Array.toList
                    |> List.filterMap identity
                )


swapAndUpdateDirections : ( Int, Element ) -> Array Element -> Array Element
swapAndUpdateDirections ( mobileIndex, mobileElement ) array =
    let
        swapped =
            if mobileElement.direction == left then
                swap mobileIndex (mobileIndex - 1) array

            else
                swap mobileIndex (mobileIndex + 1) array
    in
    Array.map
        (\element ->
            if element.initialPosition > mobileElement.initialPosition then
                { element | direction = not element.direction }

            else
                element
        )
        swapped


swap : Int -> Int -> Array a -> Array a
swap index1 index2 array =
    let
        maybeValue1 =
            Array.get index1 array

        maybeValue2 =
            Array.get index2 array
    in
    case ( maybeValue1, maybeValue2 ) of
        ( Just value1, Just value2 ) ->
            Array.set index1 value2 array
                |> Array.set index2 value1

        ( _, _ ) ->
            array


calculateMobile : Array Element -> Maybe ( Int, Element )
calculateMobile array =
    calculateMobile_ array 0 Nothing


calculateMobile_ : Array Element -> Int -> Maybe ( Int, Element ) -> Maybe ( Int, Element )
calculateMobile_ array index maybeCurrentGreatest =
    let
        maybeElement =
            Array.get index array
    in
    -- maybeElement is Nothing when we have come to the end of the array
    -- maybeCurrentGreatest should be Nothing for the first iteration / recursion
    case ( maybeCurrentGreatest, maybeElement ) of
        ( Nothing, Nothing ) ->
            Nothing

        ( currentGreatest, Nothing ) ->
            currentGreatest

        ( currentGreatest, Just element ) ->
            calculateMobile_ array (index + 1) (updateCurrentGreatest element index currentGreatest array)


updateCurrentGreatest : Element -> Int -> Maybe ( Int, Element ) -> Array Element -> Maybe ( Int, Element )
updateCurrentGreatest element index currentGreatest array =
    -- Is Element at index a better candidate for the mobile than the current candidate
    if
        element.direction
            == left
            && index
            /= 0
            && greaterThan element (Array.get (index - 1) array)
            && greaterThan element (currentGreatest |> Maybe.map Tuple.second)
    then
        Just ( index, element )

    else if
        element.direction
            == right
            && index
            /= (Array.length array - 1)
            && greaterThan element (Array.get (index + 1) array)
            && greaterThan element (currentGreatest |> Maybe.map Tuple.second)
    then
        Just ( index, element )

    else
        currentGreatest


greaterThan : Element -> Maybe Element -> Bool
greaterThan element1 maybeElement2 =
    case maybeElement2 of
        Nothing ->
            True

        Just element2 ->
            element1.initialPosition > element2.initialPosition
