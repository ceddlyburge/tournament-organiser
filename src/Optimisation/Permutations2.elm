-- Implementation of the Johnson Trotter algorithm
-- https://www.geeksforgeeks.org/johnson-trotter-algorithm/
-- The haskell permutations function can't be expressed in Elm I think
-- https://stackoverflow.com/questions/24484348/what-does-this-list-permutations-implementation-in-haskell-exactly-do
-- A lot of things are exposed to make testing easier / better, but otherwise a parse don't validate approach would be better


module Optimisation.Permutations2 exposing (Element, State, Step(..), calculateMobile, calculateNextPermutation, first, getPermutation, getState, left, next, right)

import Array exposing (Array)


type alias Element =
    { initialPosition : Int
    , direction : Bool
    }


type Step a
    = Done
    | Next (State a) (List a)


type alias State a =
    { count : Int
    , permutation : Array Element
    , initial : Array a
    }


right : Bool
right =
    -- maybe have a type for this, then can also represent an error type state
    True


left : Bool
left =
    False


first : List a -> Step a
first initialList =
    let
        initialArray : Array a
        initialArray =
            Array.fromList initialList
    in
    Next
        { count = 0
        , permutation = Array.initialize (Array.length initialArray) (\index -> { initialPosition = index, direction = left })
        , initial = initialArray
        }
        initialList


getState : Step a -> Maybe (State a)
getState step =
    case step of
        Done ->
            Nothing

        Next state _ ->
            Just state


getPermutation : Step a -> Maybe (List a)
getPermutation step =
    case step of
        Done ->
            Nothing

        Next _ permutation ->
            Just permutation


next : State a -> Step a
next state =
    case calculateNextPermutation state.permutation of
        Nothing ->
            Done

        Just nextPermutation ->
            Next
                { state | permutation = nextPermutation, count = state.count + 1 }
                (Array.map (\element -> Array.get element.initialPosition state.initial) nextPermutation
                    |> Array.toList
                    |> List.filterMap identity
                )


calculateNextPermutation : Array Element -> Maybe (Array Element)
calculateNextPermutation previousPermutation =
    Maybe.map
        (swapAndUpdateDirections previousPermutation)
        (calculateMobile previousPermutation)


swapAndUpdateDirections : Array Element -> ( Int, Element ) -> Array Element
swapAndUpdateDirections previousPermutation ( mobileIndex, mobileElement ) =
    let
        swapped : Array Element
        swapped =
            if mobileElement.direction == left then
                swap mobileIndex (mobileIndex - 1) previousPermutation

            else
                swap mobileIndex (mobileIndex + 1) previousPermutation
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
        maybeValue1 : Maybe a
        maybeValue1 =
            Array.get index1 array

        maybeValue2 : Maybe a
        maybeValue2 =
            Array.get index2 array
    in
    case ( maybeValue1, maybeValue2 ) of
        ( Just value1, Just value2 ) ->
            Array.set index1 value2 array
                |> Array.set index2 value1

        _ ->
            array


calculateMobile : Array Element -> Maybe ( Int, Element )
calculateMobile array =
    calculateMobile_ array 0 Nothing


calculateMobile_ : Array Element -> Int -> Maybe ( Int, Element ) -> Maybe ( Int, Element )
calculateMobile_ array index maybeCurrentGreatest =
    let
        maybeElement : Maybe Element
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
