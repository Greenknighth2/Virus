globals
[
  current-shoppers
  total-time
  one-run-time
  initial-infected

  infected-a-day
  extra-infected
  day-time

  amount-infected
  amount-uninfected
  amount-cured
  amount-sick-or-dead
]


turtles-own
[
  new-heading
  time-infected
  infected?            ;; If true, the person is infected.
  cured?               ;; If true, the person has lived through an infection. They cannot be re-infected.

  nb-infected          ;; Number of secondary infections caused by an infected person at the end of the tick
  nb-recovered         ;; Number of recovered people at the end of the tick
  mask-efficiency
  sick-or-dead?
  distance-infected
]

;;;
;;; SETUP PROCEDURES
;;;

to setup
  clear-all
  setup-globals
  setup-people
  reset-ticks
end

to setup-globals
  set total-time 0

  set amount-infected 0
  set amount-uninfected 0
  set amount-cured 0
  set amount-sick-or-dead 0

  ;; Paints the layout of the store, red being walls or shelves, black being the floor and yellow being the cashregisters

  ask patches with [(pxcor = -12) or (pxcor > 11) or (pycor = max-pycor)or (pycor = min-pycor)]
    [ set pcolor red  ]
  ask patches with [(pxcor =  -4 and pycor >= 2 and pycor < 10)] ;;abs pycor
  [set pcolor red ]
  ask patches with [(pxcor =  -4 and pycor >= -9 and pycor < 0)]
  [set pcolor red ]
  ask patches with [(pxcor =  -8 and pycor >= 2 and pycor < 10)] ;;abs pycor
  [set pcolor red ]
  ask patches with [(pxcor =  -8 and pycor >= -9 and pycor < 0)]
  [set pcolor red ]
  ask patches with [(pxcor =  -4 and pycor >= -12 and pycor < 0)]
  [set pcolor red ]
  ask patches with [(pxcor =  0 and pycor >= 2 and pycor < 10)] ;;abs pycor
  [set pcolor red ]
  ask patches with [(pxcor =  0 and pycor >= -9 and pycor < 0)]
  [set pcolor red ]
  ask patches with [((pxcor =  4 or pxcor = 5) and pycor >= 2 and pycor < 9)]
  [set pcolor red ]
  ask patches with [((pxcor =  8 or pxcor = 9) and pycor >= 2 and pycor < 9)]
  [set pcolor red ]
  ask patches with [((pxcor =  4 or pxcor = 5) and pycor >= -2 and pycor < 0)]
  [set pcolor red ]
  ask patches with [((pxcor =  8 or pxcor = 9) and pycor >= -2 and pycor < 0)]
  [set pcolor red ]
  ask patches with [(pxcor =  4 and pycor >= -8 and pycor < -3)] ;;abs pycor
  [set pcolor red ]
  ask patches with [(pxcor =  8 and pycor >= -8 and pycor < -3)]
  [set pcolor red ]
  ask patches with [(pxcor >= 3 and pxcor < 10 and pycor = -11)]
  [set pcolor yellow ]



end

;; Create initial-people number of people.
to setup-people
  let initial-infected-store (initial-percentage-infected / 100 ) * initial-people
  let count-costumers 0
  create-turtles initial-people[

      set size 0.8

      set cured? false
      set time-infected 0
      set infected? false

      set hidden? true
      set one-run-time 0
      set current-shoppers 0
      set sick-or-dead? false
      set time-infected 0

      set mask-efficiency 0.8 - ((random-float 60) / 100) ;; mask efficiency range between 20 to 80 % with cloth masks



      ;; infection chance
      ifelse (count-costumers < initial-infected-store)
      [ set infected? true
        set amount-infected amount-infected + 1
        set time-infected random ((average-recovery-time) * 60 * 14 * 30)
      set count-costumers count-costumers + 1


    ][set amount-uninfected amount-uninfected + 1]
      assign-color
      ]
  set initial-infected count turtles with[infected? = true]
  set day-time 0
  set infected-a-day initial-infected
  set extra-infected 0

end



;; Different people are displayed in 3 different colors depending on health
;; green is a survivor of the infection
;; red is an infected person
;; white is neither infected nor cured

to assign-color ;; turtle procedure

  ifelse cured?
    [ set color green ]
      [ ifelse infected?
        [set color red ]
        [set color white]]

end


to make-network
  ask turtles
  [
    create-links-with turtles-on neighbors
  ]
end


;;;
;;; GO PROCEDURES
;;;


to go


    let open-patches nobody
    let open-patch nobody
    set open-patches patches with [not any? turtles-here and pcolor != yellow and pcolor != red]
    set open-patch one-of open-patches

  ask turtles with [infected? = true][
  set time-infected time-infected + 2

    if time-infected = (average-recovery-time * 60 * 14 * 30)[
      ifelse sick-or-dead-chance < (random 100) [set infected? false
        set cured? true set amount-cured amount-cured + 1 set amount-infected amount-infected - 1]
      [set sick-or-dead? true set infected? false set cured? false set amount-infected amount-infected - 1 set amount-sick-or-dead amount-sick-or-dead + 1]
  ] ]


  if current-shoppers < people-in-store[
    ask one-of turtles with [sick-or-dead? = false][
      set heading random-float 360
      move-to open-patch
      set hidden? false
  ] set current-shoppers current-shoppers + 1]

  set one-run-time one-run-time + 2
  set day-time day-time + 2

  if day-time = (14 * 60 * 60)[
    set extra-infected amount-infected - infected-a-day
    set infected-a-day amount-infected
    set day-time 0
  ]

  ifelse one-run-time < (average-shopping-time * 60)[
    ask turtles with [hidden? = false]
        [ let contagious? false
          if time-infected >= (incubation-time * 60 * 14 * 30)[set contagious? true]
          if infected? = true and contagious? = true [infect]
          ifelse SocialDistancing? [move2][move]
    assign-color]
  ][set current-shoppers 0
    set one-run-time 0
    ask turtles [move-to patch 0 0
    set hidden? true]
  ]


  set total-time total-time + 2
  if total-time = (days-testing * 14 * 60 * 60)[      ;;14 uur per dag open, keer 60 minuten
  stop]

  tick
end


to move  ;; turtle procedure, if able to walk forward (no obstruction) walk forward, or else stand still and new heading
  let old-heading heading
    ;;set heading new-heading
    if [pcolor] of patch-ahead 1 != black
  [ ifelse (random-float 100 < 50)[fd 0][set heading random-float 360 ]]

    ifelse [pcolor] of patch-ahead 1 = black and not any? turtles-on patch-ahead 1
  [ fd 1 ][ifelse (random-float 100 < 50)[fd 0][set heading random-float 360 ]]
end

;; Like 'move' but with the restriction of 1.5 patch distance between turtles
to move2
  let old-heading heading
  let moveD false

    ;;set heading new-heading
    if [pcolor] of patch-ahead 1 != black
  [ ifelse (random-float 100 < 50)[fd 0][set heading random-float 360 ]]

  ask patch-ahead 1[ if count turtles-on neighbors = 1 [set moveD true]]
  ifelse moveD [ifelse [pcolor] of patch-ahead 1 = black and not any? turtles-on patch-ahead 1;; (count turtles-on patch-ahead 1 of neighbors  = 0)
    [ fd 1 ][ifelse (random-float 100 < 50)[fd 0][set heading random-float 360 ]]][set heading random-float 360]


end



to infect  ;; turtle procedure

    let caller self

    let nearby-uninfected (turtles in-radius (8 / 1.5))
    with [ not infected? and not cured? and not hidden?]
  let masks? 1




    if nearby-uninfected != nobody
    [

       ask nearby-uninfected
       [
      if Masks-on?[set masks? mask-efficiency]

      let no-walls? true
      let prev-heading heading
      set distance-infected distance caller
      face caller
      let n 1
      let d distance-infected
      While [d > 0]
      [
        ask patch-ahead d [if pcolor = red [set no-walls? false]]
        set d d - 1
      ]
      if no-walls? [

        ;;Using the Wells Riley model here

       let infection-chance2 ((1 - exp( (-1 * (-18.19 * ln(distance-infected * 1.5) + 43.276 ) / 100 ) * (0.238 * 0.3 / 0.3 * 0.8 ))) * 100 ) * masks?


             if random 1000 < infection-chance2
             [
               set infected? true
               set amount-infected amount-infected + 1
               set amount-uninfected amount-uninfected - 1
               set nb-infected (nb-infected + 1)
             ]

    ]set heading prev-heading]


    ]

end


; Copyright 2021 Hugo van Meeteren.
@#$#@#$#@
GRAPHICS-WINDOW
646
27
1129
511
-1
-1
19.0
1
10
1
1
1
0
0
0
1
-12
12
-12
12
1
1
1
seconds
30.0

BUTTON
480
205
545
250
setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
550
205
615
250
go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

SLIDER
20
20
289
53
people-in-store
people-in-store
2
50
35.0
1
1
NIL
HORIZONTAL

PLOT
325
485
588
628
Population
hours
# people
0.0
10.0
0.0
350.0
true
true
"" ""
PENS
"Infected" 1.0 0 -2674135 true "" "plot amount-infected"
"Not Infected" 1.0 0 -7500403 true "" "plot amount-uninfected"
"Dead(ly sick)" 1.0 0 -16777216 true "" "plot amount-sick-or-dead"
"Cured" 1.0 0 -13840069 true "" "plot amount-cured"

SLIDER
300
100
570
133
recovery-chance
recovery-chance
10
100
100.0
5
1
NIL
HORIZONTAL

SWITCH
20
230
167
263
SocialDistancing?
SocialDistancing?
0
1
-1000

SLIDER
300
60
568
93
average-recovery-time
average-recovery-time
0
30
15.0
1
1
NIL
HORIZONTAL

PLOT
15
495
295
615
Cumulative Infected and Recovered
hours
% total pop.
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"% infected" 1.0 0 -2674135 true "" "plot (((amount-cured + amount-infected) / initial-people) * 100)"
"% recovered" 1.0 0 -10899396 true "" "plot ((amount-cured / initial-people) * 100)"

SLIDER
20
60
290
93
initial-people
initial-people
0
10000
5000.0
1000
1
NIL
HORIZONTAL

SLIDER
20
180
290
213
average-shopping-time
average-shopping-time
0
30
10.0
1
1
NIL
HORIZONTAL

SLIDER
20
140
290
173
days-testing
days-testing
0
180
30.0
2
1
NIL
HORIZONTAL

MONITOR
395
205
470
250
Minutes
total-time / 60
0
1
11

MONITOR
325
205
395
250
Days
total-time / 60 / 14 / 60
0
1
11

PLOT
240
340
440
460
Additional infected start
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"set-plot-x-range 0 (days-testing * 60 * 14 * 30)\n" ""
PENS
"default" 1.0 0 -10141563 true "" "plot amount-infected - initial-infected "

MONITOR
400
260
470
305
Infected
amount-infected
1
1
11

PLOT
20
340
220
460
Additional infected a day
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"set-plot-x-range 0 (days-testing * 60 * 14 * 30)" ""
PENS
"default" 1.0 0 -16777216 true "" "plot extra-infected"

MONITOR
470
260
545
305
Cured
amount-cured
0
1
11

MONITOR
545
260
620
305
Dead(ly sick)
amount-sick-or-dead
1
1
11

SLIDER
300
20
570
53
sick-or-dead-chance
sick-or-dead-chance
0
20
19.0
1
1
NIL
HORIZONTAL

SLIDER
20
100
290
133
initial-percentage-infected
initial-percentage-infected
0
50
5.0
1
1
NIL
HORIZONTAL

MONITOR
325
260
400
305
Unaffected
amount-uninfected
17
1
11

SWITCH
20
270
132
303
Masks-on?
Masks-on?
0
1
-1000

SLIDER
300
140
570
173
incubation-time
incubation-time
0
10
5.0
1
1
NIL
HORIZONTAL

@#$#@#$#@
For information on the model and a review of the inner workings see:

van Meeteren, H.G.J. (2021). Modelling and reviewing the effectiveness of 
COVID-19 related restrictions in supermarkets. Utrecht University 
(Utrecht, The Netherlands).


<!-- 2021 Cite: van Meeteren, H. -->
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

person lefty
false
0
Circle -7500403 true true 170 5 80
Polygon -7500403 true true 165 90 180 195 150 285 165 300 195 300 210 225 225 300 255 300 270 285 240 195 255 90
Rectangle -7500403 true true 187 79 232 94
Polygon -7500403 true true 255 90 300 150 285 180 225 105
Polygon -7500403 true true 165 90 120 150 135 180 195 105

person righty
false
0
Circle -7500403 true true 50 5 80
Polygon -7500403 true true 45 90 60 195 30 285 45 300 75 300 90 225 105 300 135 300 150 285 120 195 135 90
Rectangle -7500403 true true 67 79 112 94
Polygon -7500403 true true 135 90 180 150 165 180 105 105
Polygon -7500403 true true 45 90 0 150 15 180 75 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.2.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>amount-uninfected = 0</exitCondition>
    <metric>amount-infected</metric>
    <metric>amount-uninfected</metric>
    <metric>amount-cured</metric>
    <metric>amount-sick-or-dead</metric>
    <enumeratedValueSet variable="people-in-store">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="days-testing">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-recovery-time">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="links?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="infection-chance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-people">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sick-or-dead-chance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-shopping-time">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="recovery-chance">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-percentage-infected">
      <value value="5"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
1
@#$#@#$#@
