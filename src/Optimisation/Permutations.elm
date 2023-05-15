module Optimisation.Permutations exposing (permutations, permutationsInternal)

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

   etc
-}


permutations : Int -> (a -> a -> Bool) -> List a -> List (List a)
permutations limit curtail xs =
    permutationsInternal limit curtail (List.Extra.select xs) [] ( 0, [] )
        |> Tuple.second


permutationsInternal : Int -> (a -> a -> Bool) -> List ( a, List a ) -> List a -> ( Int, List (List a) ) -> ( Int, List (List a) )
permutationsInternal limit curtail combinations currentPermutation ( iterationCount, finishedPermutations ) =
    -- let
    --     _ =
    --         Debug.log "permutationsInternal" ( combinations, currentPermutation, finishedPermutations )
    -- in
    if iterationCount > limit then
        -- Debug.log "iterationCount > limit"
        ( iterationCount, finishedPermutations )

    else
        case currentPermutation of
            -- If there is no permutation in progress
            [] ->
                case combinations of
                    -- and there are no combinations to analyse, then there is nothing more to do
                    [] ->
                        -- let
                        --     _ =
                        --         Debug.log "[]" ( iterationCount, finishedPermutations )
                        -- in
                        ( iterationCount, finishedPermutations )

                    x :: xs ->
                        let
                            -- _ =
                            --     Debug.log "x :: xs 1" "hello"
                            -- and there are more combinations to analyse, start a new permutation with the first combination (x)
                            ( depthFirstIterationCount, depthFirstFinishedPermutations ) =
                                permutationsInternal limit curtail (List.Extra.select (Tuple.second x)) [ Tuple.first x ] ( iterationCount + 1, finishedPermutations )

                            -- _ =
                            --     Debug.log "x :: xs 2" "hello"
                        in
                        -- and then start a new permutation with the remaining permuation (xs)
                        permutationsInternal limit curtail xs [] ( depthFirstIterationCount + 1, depthFirstFinishedPermutations )

            -- If there is a permutation in progress
            xs ->
                if maybeCurtail curtail (List.Extra.getAt 0 xs) (List.Extra.getAt 1 xs) then
                    -- Debug.log "curtail"
                    ( iterationCount, finishedPermutations )

                else
                    case combinations of
                        -- and there are no combinations left, then we are finished creating this permutation, so we add it
                        [] ->
                            -- Debug.log "currentPermutation :: finishedPermutations"
                            ( iterationCount, currentPermutation :: finishedPermutations )

                        -- and there is one combination left, then analyse the last combination
                        y :: [] ->
                            -- let
                            --     _ =
                            --         Debug.log "y :: []" "hello"
                            -- in
                            permutationsInternal limit curtail (List.Extra.select (Tuple.second y)) (Tuple.first y :: xs) ( iterationCount + 1, finishedPermutations )

                        -- and there are multiple combinations left, analyse the first combination (recursively), then the rest.
                        y :: ys ->
                            let
                                -- _ =
                                --     Debug.log "y :: ys 1" "hello"
                                ( depthFirstIterationCount, depthFirstFinishedPermutations ) =
                                    permutationsInternal limit curtail (List.Extra.select (Tuple.second y)) (Tuple.first y :: xs) ( iterationCount + 1, finishedPermutations )

                                -- _ =
                                --     Debug.log "y :: ys 2" "hello"
                            in
                            permutationsInternal limit curtail ys xs ( depthFirstIterationCount + 1, depthFirstFinishedPermutations )



-- permutations : (a -> a -> Bool) -> List a -> List (List a)
-- permutations curtail xs =
--     permutationsInternal curtail Nothing xs
-- permutationsInternal : (a -> a -> Bool) -> Maybe a -> List a -> List (List a)
-- permutationsInternal curtail previousValue xs_ =
--     case xs_ of
--         [] ->
--             [ [] ]
--         xs ->
--             let
--                 f : ( a, List a ) -> List (List a)
--                 f ( y, ys ) =
--                     if maybeCurtail curtail previousValue y then
--                         [ [] ]
--                     else
--                         List.map
--                             (\permuation ->
--                                 case ( ys, permuation ) of
--                                     ( _ :: _, [] ) ->
--                                         []
--                                     ( _, zs ) ->
--                                         y :: zs
--                             )
--                             (permutationsInternal curtail (Just y) ys)
--             in
--             List.concatMap f (List.Extra.select xs)
--                 |> List.filter (\list -> List.length list > 0)


maybeCurtail : (a -> a -> Bool) -> Maybe a -> Maybe a -> Bool
maybeCurtail curtail maybePreviousValue currentValue =
    Maybe.map2 curtail maybePreviousValue currentValue
        |> Maybe.withDefault False



-- maybeCurtail : (a -> a -> Bool) -> Maybe a -> a -> Bool
-- maybeCurtail curtail maybePreviousValue currentValue =
--     Maybe.map2 curtail maybePreviousValue (Just currentValue)
--         |> Maybe.withDefault False
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
