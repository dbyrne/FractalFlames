import FractalComponents._ 

/*
Copyright (C) 2010 David Byrne
david.r.byrne@gmail.com

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

object SampleFlames {

  val bubbleSample = Flame((-1.75,0.75,-1.5,1.0), 3.25, Rainbow(),
                           scala.List[Function] (new Function(0.5, 0.0, (-0.559531, -0.253265, 0.253265, -0.559531, -0.187406, -0.073585)) {
                                                              override val variations = scala.List(linear(.2),bubble(.8))},
                                                 new Function(1.0, 1.0, (0.85214, -0.139685, -0.139686, -0.85214, -0.487627, -0.358286)) {
                                                              override val variations =  scala.List(linear(.2),spherical(.8))}))

  val julianSample = Flame((-2,2,-2,2), 4.00, Rainbow(),
                           scala.List[Function] (new Function(0.5, 0.0, (0.259446, 0.0972, 0.156567, -0.260863, 0.1, 0.0)) {
                                                              override val variations = scala.List(rings(1.0))},
                                                 new Function(1.0, 1.0, (0.397296, -0.334164, 0.006008, 0.489671, 0.006868, -0.351789)) {
                                                              override val variations =  scala.List(julian(1.0,4.0,-1.16))}))

  val curlSample = Flame((-4,4,-4.1,3.9), 2.00, Rainbow(),
                         scala.List[Function] (new Function(0.5, 0.0, (0.875336, 0.199837, -0.31253, 1.260907, -0.35957, -0.703643)) {
                                                            override val variations = scala.List(linear(0.75),curl(0.3,0.831897747766335,0.0))},
                                               new Function(1.0, 1.0, (-0.565955, -0.119407, 0.148909, -0.513352, 0.570999, -0.684702)) {
                                                            override val variations = scala.List(linear(0.8),spherical(.05),curl(1.0,0.831897747766335,0.0))}))

  val sample2 = Flame((-1,4,-3.5,1.5), 4.0, Rainbow(),
                      scala.List[Function] (new Function(0.333, 0.0, (-0.007287, 0.141009, -0.261044, 0.047784, 0.015346, -0.001513)) {
                                                         override val variations = scala.List(horseshoe(1.0))},
                                            new Function(0.666, 0.5, (0.648674, -0.556483, -0.58627, -0.238837, -0.215097, -0.075511)) {
                                                         override val variations = scala.List(horseshoe(1.0))},
                                            new Function(1.0, 1.0, (0.04417, -0.261728, -0.405825, 0.096763, 0.341195, -0.232007)) {
                                                         override val variations = scala.List(spherical(1.0))}))

  val trout = Flame((-.75,.5,-.85,.40), 4.0, Rainbow(),
                    scala.List[Function] (new Function(0.5, 0.0,(-0.611536, -0.198789, 0.308349, -0.517899, -0.28474, -0.250627)) {
                                                       override val variations = scala.List(popcorn(1.0),rings(1.0))},
                                          new Function(1.0, 1.0, (0.102329, -0.67052, 0.732627, 0.189097, 0.057882, -0.08797)) {
                                                       override val variations = scala.List(swirl(1.0))}))

  val brain = Flame((-3,5,-4,4), 3.0, Rainbow(),
                    scala.List[Function] (new Function(0.5, 0.0, (1.015245, -1.466244, 2.361193, 1.471095, -0.128157, 1.185009)) {
                                                       override val variations = scala.List(linear(.5),spiral(.5))},
                                          new Function(1.0, 1.0, (-0.567588, -0.536166, -0.325195, 0.801476, -0.90069, 1.08108)) {
                                                       override val variations = scala.List(linear(.5),spiral(.5))}))

/*
  val sample1 = Flame((-7,5,-6,6),
                      1.0,
                      Rainbow(),
                      scala.List[Function] (
                                            Function(0.5,
                                                     0.0,
                                                     affineTransform(-0.013021, -0.335641, 0.250371, 0.071523, -0.066408, -0.003406),
                                                     scala.List(linear(.5),spherical(.5))),
                                            Function(1.0,
                                                     1.0,
                                                     affineTransform(0.215821, 0.715917, -0.335224, -0.054829, -0.155678, -0.205658),
                                                     scala.List(linear(0.75))))) 

  
  val pinwheel = Flame((-.25,1.25,-.75,.75),
                        4.0,
                        Rainbow(),
                        scala.List[Function] ( //Function list must be sorted by weights right now
                                              Function(0.5,
                                                       0.0,
                                                       affineTransform(0.7158, -0.311476, -0.250535, 0.728467, 6.63E-4, 0.075547),
                                                       scala.List(linear(0.33), handkerchief(1.0))),
                                              Function(1.0,
                                                       1.0,
                                                       affineTransform(0.343091, -0.799811, 0.687143, 0.524347, 0.593742, 0.249853),
                                                       scala.List(sinusoidal(.8),horseshoe(0.2)))))

*/

}