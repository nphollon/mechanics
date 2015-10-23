module Main where

import DoublePendulum

import Color
import Graphics.Collage as Collage
import Graphics.Element as Layout
import Graphics.Input as Input
import Text
import Time

import Signal exposing ((<~), (~))


main =
  view <~ DoublePendulum.simulate startState geometry (Time.fps 60) ~ geometry


-- Initial Conditions --

startState =
  { q1 = degrees 120
  , q2 = degrees 190
  , p1 = 1
  , p2 = 2
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

-- UI --

view : DoublePendulum.State -> DoublePendulum.Params -> Layout.Element
view s c =
  Layout.flow Layout.right [ draw s c , controls ]

           
draw : DoublePendulum.State -> DoublePendulum.Params -> Layout.Element
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
