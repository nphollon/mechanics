module Lagrangian (solve, toAcceleration) where

import Expression exposing (..)
import Mechanics
import Types


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
