module Lagrangian (solve, toAcceleration) where

{-|
We can compute the behavior of a system if we start with special equation called
a Lagrangian.

    Lagrangian = KineticEnergy - PotentialEnergy

@docs toAcceleration, solve
-}

import Expression exposing (..)
import Mechanics
import Types


{-| Given a Lagrangian, try to compute the acceleration of the system. The result
is a list of expressions describing the acceleration of each coordinate.

This function will return `Nothing` if the expression it receives does not obey
the following rules:

* Every coordinate index must have a velocity-squared term.

    -- Bad: The expression has (coordinate 1) but no (square (velocity 1))
    (square (velocity 0)) `plus` (coordinate 0) `plus` (coordinate 1)

    -- Good: Not every velocity needs a corresponding coordinate
    sum [ square (velocity 0), square (velocity 1), square (velocity 2) ]

    -- Bad: (square (velocity 3)) appears, but 1 and 2 are missing
    sum [ square (velocity 0), square (velocity 3) ]


* Velocities with different indexes can be added but not combined in other ways.

    -- Bad: (velocity 0) and (velocity 1) are multiplied, not added
    (square (velocity 0)) `times` (square (velocity 1))

    -- Good: It is OK to multiply coordinates and velocities
    (square (velocity 0)) `plus` ((coordinate 0) `times` (square (velocity 1)))
-}
solve : Expression -> Maybe (List Expression)
solve lagr =
    let
        indexes = [0..(dimension lagr - 1)]

        spaceTerm =
            List.map (\i -> partial (coordinate i) lagr) indexes

        speedPartial =
            List.map (\i -> partial (velocity i) lagr) indexes

        hessian =
            List.map2 (\i d2L -> partial (velocity i) d2L) indexes speedPartial

        timeTerm =
            List.map (partial time) speedPartial

        spaceSpeedPartial =
            List.map (\i -> List.map (partial (coordinate i)) speedPartial) indexes

        speedVector =
            List.map velocity indexes

        speedTerm =
            speedVector `dotProduct` spaceSpeedPartial

        accel =
            List.map4
                (\d1L d0d2L vd1d2L d2d2L ->
                    if (d2d2L == (num 0)) then
                        Nothing
                    else
                        Just ((d1L `minus` d0d2L `minus` vd1d2L) `over` d2d2L)
                )
                spaceTerm
                timeTerm
                speedTerm
                hessian
    in
        if hasSeparableVelocities hessian then
            List.foldr (Maybe.map2 (::)) (Just []) accel
        else
            Nothing


hasSeparableVelocities : List Expression -> Bool
hasSeparableVelocities expressions =
    let
        containsJust i expr =
            case expr of
                Types.Const _ ->
                    True

                Types.Time ->
                    True

                Types.Coord _ ->
                    True

                Types.Vel j ->
                    i == j

                Types.Sum terms ->
                    List.all (containsJust i) terms

                Types.Prod _ factors ->
                    List.all (containsJust i) factors

                Types.Pow base power ->
                    containsJust i base && containsJust i power

                Types.Log x ->
                    containsJust i x

                Types.Sin x ->
                    containsJust i x

                Types.Cos x ->
                    containsJust i x
    in
        List.indexedMap containsJust expressions
            |> List.all identity


dotProduct : List Expression -> List (List Expression) -> List Expression
dotProduct vector matrix =
    List.map2 (times >> List.map) vector matrix
        |> List.foldl
            (\row result ->
                if (List.isEmpty result) then
                    row
                else
                    List.map2 plus row result
            )
            []



{-| The same as `solve`, but wraps up the solution in a
`Mechanics.Acceleration` value. The result can be plugged directly into
`Mechanics.evolve`.
-}
toAcceleration : Expression -> Maybe Mechanics.Acceleration
toAcceleration lagrangian =
    let
        accelerate expressions =
            Mechanics.acceleration
                (\state ->
                    List.map (flip eval state) expressions
                )
    in
        Maybe.map accelerate (solve lagrangian)
