module DoublePendulum (State, Params, simulate) where

import Mechanics

import Array
import Maybe
import Signal exposing (Signal)
import Time exposing (Time)

type alias State = { q1 : Float, q2 : Float, p1 : Float, p2 : Float }
type alias Params = { m1 : Float, m2 : Float, r1 : Float, r2 : Float, g : Float, calm : Bool }


simulate : State -> Signal Params -> Signal Time -> Signal State
simulate init extern time =
  Mechanics.evolve converter force init (Mechanics.external (0.25 * Time.second) time extern)
           

converter : Mechanics.Operators State
converter = 
  let
    add s t =
      { q1 = s.q1 + t.q1
      , q2 = s.q2 + t.q2
      , p1 = s.p1 + t.p1
      , p2 = s.p2 + t.p2
      }

    scale a state =
      { q1 = a * state.q1
      , q2 = a * state.q2
      , p1 = a * state.p1
      , p2 = a * state.p2
      }
  in
    Mechanics.operators add scale

             
force : Params -> State -> State
force c s =
  let
    dq1 = dT_dp1 c s
    dq2 = dT_dp2 c s
    dp1 = if c.calm then negate s.p1 else negate (dU_dq1 c s + dT_dq1 c s)
    dp2 = if c.calm then negate s.p2 else negate (dU_dq2 c s - dT_dq1 c s)
  in { q1 = dq1, q2 = dq2, p1 = dp1, p2 = dp2 }

dU_dq1 c s = negate (c.m1 + c.m2) * c.g * c.r1 * sin s.q1
dU_dq2 c s = negate c.m2 * c.g * c.r2 * sin s.q2

dT_dp1 c s =
  let
    a' = a c s
    b' = b c s
    dadp' = da_dp1 c s
    dbdp' = db_dp1 c s
    cosd' = cosd s
  in amp c s * (2*a'*dadp' + b'*dbdp' + cosd'*a'*dbdp' + cosd'*b'*dadp')

dT_dp2 c s =
  let
    a' = a c s
    b' = b c s
    dadp' = da_dp2 c s
    dbdp' = db_dp2 c s
    cosd' = cosd s
  in amp c s * (2*a'*dadp' + b'*dbdp' + cosd'*a'*dbdp' + cosd'*b'*dadp')

dT_dq1 c s = 
  let
    a' = a c s
    b' = b c s
    dadq' = da_dq1 c s
    dbdq' = db_dq1 c s
    cosd' = cosd s
    sind' = sind s
  in amp c s * (2*a'*dadq' + b'*dbdq' - a'*b'*sind' + a'*dbdq'*cosd' + b'*dadq'*cosd') + 
    damp_dq1 c s * (a'^2 + 0.5*b'^2 + a'*b'*cosd')

cosd s = cos (s.q1 - s.q2)
sind s = sin (s.q1 - s.q2)
sin2d s = 2 * cosd s * sind s
discr c s = c.m1 + c.m2 * (sind s)^2
mrat c = 1 + c.m1 / c.m2

a c s = c.r2 * s.p1 - c.r1 * s.p2 * cosd s
da_dp1 c _ = c.r2
da_dp2 c s = negate c.r1 * cosd s
da_dq1 c s = c.r1 * s.p2 * sind s

b c s = mrat c * c.r1 * s.p2 - c.r2 * s.p1 * cosd s
db_dp1 c s = negate c.r2 * cosd s
db_dp2 c _ = mrat c * c.r1
db_dq1 c s = c.r2 * s.p1 * sind s

amp c s = c.m2 / c.r1 / c.r2 / (discr c s)^2
damp_dq1 c s = negate c.m2 * amp c s * sin2d s / discr c s
