{-|
Module      : MyAnimation
Description : Coursework SVG animation using a custom library.
Copyright   : (c) Wiktor Kornatowski, 2015
-}

module MyAnimation where
import Animation

width :: Double
width = 800
height :: Double
height = 600

-- Creates a black background and uses drawOrbs and particleAxis to create the animation.
picture :: Animation
picture = (withPaint (always black) (rect (always width)(always height)))
		`plus`
		drawOrbs
		`plus`
		translate(always (width/2, height/2))
		(combine [particleAxis 0 6 0 i | i <- [5..49]])	

-- Uses drawCircle to produce a single cirular axis with 4 rotating orbs made up of drawParticles.	
drawOrbs :: Animation
drawOrbs = translate (always (width/2, height/2))
		   (combine
			[drawCircle (cycleSteps 0.1 [red, lime]) 0.4 230
			`plus`
			(combine [particleAxis (-4) (-(k*460-230)) 0 i | k <- [0..1]]) 
			`plus`
			(combine [particleAxis 4 (-(k*460-230)) 0 i | k <- [0..1]]) 
			| i <- [1..10]])

-- Uses drawCircle and drawParticles to produce multiple layers of circles and particles. Spins with given value.
particleAxis :: Time -> Double -> Double -> Length -> Animation
particleAxis rs w h i = (rotate (spinner rs) 
			(translate (always (w, h))						
				(drawCircle (cycleSteps 0.1 [lime, magenta]) 0.4 (6*i)
			`plus`
				(combine [drawParticles (-(k*0.4-0.2)*i) 6 i | k <- [0..1]])
			`plus`
				(combine [drawParticles (-(k*0.4-0.2)*i) (-6) i | k <- [0..1]]))))

-- Draws animation particles, circles of fixed size 1.5 with paint. Spins with given value.
drawParticles :: Time -> Double -> Length -> Animation
drawParticles rs w c = (rotate (spinner rs)
			(translate (always (w*c,0))
			(withPaint (cycleSteps 0.1 [white, red])(circle (always 1.5)))))

-- Draws singular paintless cirles, with coloured border.
drawCircle :: Varying Colour -> Double -> Double -> Animation 	
drawCircle c s r = (withBorder (c) (always s) (withoutPaint (circle (always r))))	
