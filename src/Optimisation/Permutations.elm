module Optimisation.Permutations exposing (permutations, permutations2)

import List.Extra



{-
   [1, 2, 3]

   [(1, [2,3]), (2, [1, 3]), (3, [1,2])]
   []
   [[]]

   [(2, [3])]
   [1]
   [[]]

   [(3, [])]
   [1,2]
   [[]]

   []
   [1, 2, 3]
   [[]]

   []
   []
   [[1, 2, 3]]




   [(2, [1, 3]), (3, [1,2])]
   []
   [[1, 2, 3]]
-}
-- todo: limit the number of loops before returning, make it customisable
-- use it in the app, see if it is can analyse the games from se polo usefully.
-- also need to fix up the bugs in the game order metrics
-- todo, improve documentation somehow. Maybe using the run through of the comment above.


permutations2 : Int -> (a -> a -> Bool) -> List a -> List (List a)
permutations2 limit curtail xs =
    permutations2Internal limit curtail (List.Extra.select xs) [] ( 0, [] )
        |> Tuple.second


permutations2Internal : Int -> (a -> a -> Bool) -> List ( a, List a ) -> List a -> ( Int, List (List a) ) -> ( Int, List (List a) )
permutations2Internal limit curtail combinations currentPermutation ( iterationCount, finishedPermutations ) =
    if iterationCount >= limit then
        ( iterationCount, finishedPermutations )

    else
        case currentPermutation of
            -- If there is no permutation in progress
            [] ->
                case combinations of
                    -- and there are no combinations to analyse, then there is nothing more to do
                    [] ->
                        ( iterationCount + 1, finishedPermutations )

                    x :: xs ->
                        let
                            -- and there are more combinations to analyse, start a new permutation with the first combination (x)
                            ( depthFirstIterationCount, depthFirstFinishedPermutations ) =
                                permutations2Internal limit curtail (List.Extra.select (Tuple.second x)) [ Tuple.first x ] ( iterationCount + 1, finishedPermutations )
                        in
                        -- and then start a new permutation with the remaining permuation (xs)
                        permutations2Internal limit curtail xs [] ( depthFirstIterationCount + 1, depthFirstFinishedPermutations )

            -- If there is a permutation in progress
            xs ->
                if maybeCurtail2 curtail (List.Extra.getAt 0 xs) (List.Extra.getAt 1 xs) then
                    ( iterationCount + 1, finishedPermutations )

                else
                    case combinations of
                        -- and there are no combinations left, then we are finished creating this permutation, so we add it
                        [] ->
                            ( iterationCount + 1, currentPermutation :: finishedPermutations )

                        -- and there is one combination left, then analyse the last combination
                        y :: [] ->
                            permutations2Internal limit curtail (List.Extra.select (Tuple.second y)) (Tuple.first y :: xs) ( iterationCount + 1, finishedPermutations )

                        -- and there are multiple combinations left, analyse the first combination (recursively), then the rest.
                        y :: ys ->
                            let
                                ( depthFirstIterationCount, depthFirstFinishedPermutations ) =
                                    permutations2Internal limit curtail (List.Extra.select (Tuple.second y)) (Tuple.first y :: xs) ( iterationCount + 1, finishedPermutations )
                            in
                            permutations2Internal limit curtail ys xs ( depthFirstIterationCount + 1, depthFirstFinishedPermutations )


permutations : (a -> a -> Bool) -> List a -> List (List a)
permutations curtail xs =
    permutationsInternal curtail Nothing xs


permutationsInternal : (a -> a -> Bool) -> Maybe a -> List a -> List (List a)
permutationsInternal curtail previousValue xs_ =
    case xs_ of
        [] ->
            [ [] ]

        xs ->
            let
                f : ( a, List a ) -> List (List a)
                f ( y, ys ) =
                    if maybeCurtail curtail previousValue y then
                        [ [] ]

                    else
                        List.map
                            (\permuation ->
                                case ( ys, permuation ) of
                                    ( _ :: _, [] ) ->
                                        []

                                    ( _, zs ) ->
                                        y :: zs
                            )
                            (permutationsInternal curtail (Just y) ys)
            in
            List.concatMap f (List.Extra.select xs)
                |> List.filter (\list -> List.length list > 0)


maybeCurtail2 : (a -> a -> Bool) -> Maybe a -> Maybe a -> Bool
maybeCurtail2 curtail maybePreviousValue currentValue =
    Maybe.map2 curtail maybePreviousValue currentValue
        |> Maybe.withDefault False


maybeCurtail : (a -> a -> Bool) -> Maybe a -> a -> Bool
maybeCurtail curtail maybePreviousValue currentValue =
    Maybe.map2 curtail maybePreviousValue (Just currentValue)
        |> Maybe.withDefault False



-- attempt at converting the haskell permutation function, but I don't think it
-- is representable in Elm
-- permutations : List a -> List (List a)
-- permutations xs =
--     let
--         perms ts is =
--             let
--                 interleave xs_ r =
--                     let
--                         ( _, zs ) =
--                             interleave_ is xs_ r
--                     in
--                     zs
--                 interleave_ f_ ys_ r_ =
--                     case ( f_, ys_, r_ ) of
--                         ( _, [], r ) ->
--                             ( ts, r )
--                         ( f, y :: ys, r ) ->
--                             let
--                                 ( us, zs ) =
--                                     interleave_ (f >> (::) y) ys r
--                             in
--                             ( y :: us, f {- t :: -} (y :: us) :: zs )
--             in
--             case ( ts, is ) of
--                 ( [], _ ) ->
--                     []
--                 ( t__ :: ts__, is__ ) ->
--                     List.foldr interleave (perms ts__ (is__ :: t__)) (permutations is__)
--     in
--     case xs of
--         [ a ] ->
--             [ [ a ] ]
--         xs0 ->
--             xs0 :: perms xs0 []
-- permutations            :: [a] -> [[a]]
-- permutations xs0        =  xs0 : perms xs0 []
--   where
--     perms []     _  = []
--     perms (t:ts) is = foldr interleave (perms ts (t:is)) (permutations is)
--       where interleave    xs     r = let (_,zs) = interleave' id xs r in zs
--             interleave' _ []     r = (ts, r)
--             interleave' f (y:ys) r = let (us,zs) = interleave' (f . (y:)) ys r
--                                      in  (y:us, f (t:y:us) : zs)
