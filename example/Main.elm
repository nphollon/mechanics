module Main where

import Color
import Graphics.Collage as Collage
import Graphics.Element as Layout
import Graphics.Input as Input
import Text
import Time
import Signal exposing ((<~), (~))

import TimeEvolution


main : Signal.Signal Layout.Element
main =
  view <~ TimeEvolution.evolve laws startState extern ~ geometry


-- Initial Conditions --

type alias State =
  { q1 : Float
  , q2 : Float
  , p1 : Float
  , p2 : Float
  }
                 
                 
type alias Params =
  { m1 : Float
  , m2 : Float
  , r1 : Float
  , r2 : Float
  , g : Float
  , calm : Bool
  }

                  
startState =
  { q1 = degrees 120
  , q2 = degrees 190
  , p1 = 1
  , p2 = 2
  }


extern : TimeEvolution.Externals Params
extern =
  TimeEvolution.limit (0.25 * Time.second)
                 { deltas = Time.fps 60
                 , env = geometry
                 }

geometry = 
  let
    sub = .signal
  in
    (\m1 m2 r1 r2 g calm -> { m1=m1, m2=m2, r1=r1, r2=r2, g=g, calm=calm }) <~
    sub m1Ch ~ sub m2Ch ~ sub r1Ch ~ sub r2Ch ~ sub gravityCh ~ sub calmCh

        
m1Ch = Signal.mailbox 1.0
m2Ch = Signal.mailbox 1.0
r1Ch = Signal.mailbox 1.0
r2Ch = Signal.mailbox 1.0
gravityCh = Signal.mailbox -9.81
calmCh = Signal.mailbox False


-- Physics --

laws : TimeEvolution.Laws State Params
laws =
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
    { add = add
    , scale = scale
    , force = force
    }


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
               

-- UI --

view : State -> Params -> Layout.Element
view s c =
  Layout.flow Layout.right [ draw s c , controls ]

           
draw : State -> Params -> Layout.Element
draw state c =
  doublePendulum (c.m1, c.r1, state.q1) (c.m2, c.r2, state.q2)
    |> Collage.collage 500 500

       
controls = Layout.flow Layout.down [ 
    control "Blue Bob" m1Ch bobOptions,
    control "Green Bob" m2Ch bobOptions,
    control "Inner Rod" r1Ch rodOptions,
    control "Outer Rod" r2Ch rodOptions,
    control "Gravity" gravityCh gravityOptions,
    control "Too Hyper?" calmCh calmOptions,
    Layout.link "https://github.com/nphollon/nphollon.github.io/" <| print "View the source code"
     ]

control : String -> Signal.Mailbox a -> List (String, a) -> Layout.Element
control name mailbox options =
  let
    dropDown = Input.dropDown (Signal.message mailbox.address) options
    label = print name
  in Layout.flow Layout.right [ label, dropDown ]

print : String -> Layout.Element
print = Layout.leftAligned << Text.fromString

bobOptions = [ ("Default", 1.0), ("Big", 2.0), ("Huge", 4.0) ]
rodOptions = [ ("Default", 1.0), ("Short", 0.5), ("Long", 1.5) ]
gravityOptions = [ ("Default", -9.81), ("Light", -3), ("Heavy", -30) ]
calmOptions = [("OK, carry on", False), ("Please calm down", True)]

-- Pendulum Graphics --

type alias Point = (Float, Float)

doublePendulum : (Float,Float,Float) -> (Float,Float,Float) -> List Collage.Form
doublePendulum (m1, r1, a1) (m2, r2, a2) =
  let
    pos1 = polar r1 a1
    pos2 = polar r2 a2 |> offset pos1
  in [ thread (0,0) pos1, thread pos1 pos2, bob Color.blue m1 pos1, bob Color.darkGreen m2 pos2 ] 

polar : Float -> Float -> Point
polar r a = fromPolar (100 * r, (a - degrees 90))

offset : Point -> Point -> Point
offset (x,y) (x',y') = (x + x', y + y')

thread : Point -> Point -> Collage.Form
thread pos1 pos2 = Collage.traced (Collage.solid Color.black) (Collage.segment pos1 pos2)

bob : Color.Color -> Float -> Point -> Collage.Form
bob color m pos =
  let radius = sqrt m * 10
  in Collage.filled color (Collage.circle radius) |> Collage.move pos
