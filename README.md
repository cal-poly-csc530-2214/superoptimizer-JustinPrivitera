# asgn1 superoptimizer

## Overview

The idea was to make a simple language, and then generate all possible expressions in that language that would be equivalent to specified values, given arguments

My language has these binary operators: + * and or < <= > >= ==

## Constraints:
I decided to only allow my synthesizer to make programs that had numbers -1, 0, and 1, and binary operations that did not have any children, i.e. (+ (* 2 3) x) is not allowed. Also, only specific binary operators were allowed: {+ * and or < >} Finally, I allowed it to generate if statements that used binary operations as described in the previous sentence, and nest either the else or then clauses, but not both. As soon as I allowed the synthesizer to create if statements, the number of valid programs blew up. At first I generated some 22 million programs, and so I decided to whittle down the number by slapping on more and more constraints. Eventually I restricted operations and numbers further than I had originally intended, bringing the total number of generated programs down to about 93970.

## Results

With these constraints, I was able to generate some pretty cool results. For example, I asked the synthesizer to generate all programs that, given inputs 0, 1, and 2, would generate outputs 1, 2, and 3, respectively. It was able to generate a sizable number of expressions that would lead to those outputs. I also asked the synthesizer to make programs equivalent to the signum function specified in the Massalin text. This is what will happen if you run 'racket check.rkt'.

While there are few truly amazing results, unlike the example in the Massalin text, I was able to generate all of these programs that accomplished the same goal (as signum), in 3 minutes and 19 seconds:

'((if (< x -1) -1 (if (< x 1) (+ x 0) 1)) (if (< x -1) -1 (if (< x 1) (* x 1) 1)) (if (< x -1) -1 (if (> x 0) 1 (+ x 0))) (if (< x -1) -1 (if (> x 0) 1 (* x 1))) (if (< x -1) -1 (if (> x 1) 1 (+ x 0))) (if (< x -1) -1 (if (> x 1) 1 (* x 1))) (if (< x 0) -1 (if (< x 1) 0 1)) (if (< x 0) -1 (if (< x 1) (+ x 0) 1)) (if (< x 0) -1 (if (< x 1) (* x -1) 1)) (if (< x 0) -1 (if (< x 1) (* x 0) 1)) (if (< x 0) -1 (if (< x 1) (* x 1) 1)) (if (< x 0) -1 (if (> x 0) 1 0)) (if (< x 0) -1 (if (> x 0) 1 (+ x 0))) (if (< x 0) -1 (if (> x 0) 1 (* x -1))) (if (< x 0) -1 (if (> x 0) 1 (* x 0))) (if (< x 0) -1 (if (> x 0) 1 (* x 1))) (if (< x 0) -1 (if (> x 1) 1 (+ x 0))) (if (< x 0) -1 (if (> x 1) 1 (* x 1))) (if (> x 0) 1 (if (< x -1) -1 (+ x 0))) (if (> x 0) 1 (if (< x -1) -1 (* x 1))) (if (> x 0) 1 (if (< x 0) -1 0)) (if (> x 0) 1 (if (< x 0) -1 (+ x 0))) (if (> x 0) 1 (if (< x 0) -1 (* x -1))) (if (> x 0) 1 (if (< x 0) -1 (* x 0))) (if (> x 0) 1 (if (< x 0) -1 (* x 1))) (if (> x 0) 1 (if (> x -1) 0 -1)) (if (> x 0) 1 (if (> x -1) (+ x 0) -1)) (if (> x 0) 1 (if (> x -1) (* x -1) -1)) (if (> x 0) 1 (if (> x -1) (* x 0) -1)) (if (> x 0) 1 (if (> x -1) (* x 1) -1)) (if (> x 1) 1 (if (< x -1) -1 (+ x 0))) (if (> x 1) 1 (if (< x -1) -1 (* x 1))) (if (> x 1) 1 (if (< x 0) -1 (+ x 0))) (if (> x 1) 1 (if (< x 0) -1 (* x 1))) (if (> x 1) 1 (if (> x -1) (+ x 0) -1)) (if (> x 1) 1 (if (> x -1) (* x 1) -1)) (if (< x 1) (if (< x -1) -1 (+ x 0)) 1) (if (< x 1) (if (< x -1) -1 (* x 1)) 1) (if (< x 1) (if (< x 0) -1 0) 1) (if (< x 1) (if (< x 0) -1 (+ x 0)) 1) (if (< x 1) (if (< x 0) -1 (* x -1)) 1) (if (< x 1) (if (< x 0) -1 (* x 0)) 1) (if (< x 1) (if (< x 0) -1 (* x 1)) 1) (if (< x 1) (if (> x -1) 0 -1) 1) (if (< x 1) (if (> x -1) (+ x 0) -1) 1) (if (< x 1) (if (> x -1) (* x -1) -1) 1) (if (< x 1) (if (> x -1) (* x 0) -1) 1) (if (< x 1) (if (> x -1) (* x 1) -1) 1) (if (> x -1) (if (< x 1) 0 1) -1) (if (> x -1) (if (< x 1) (+ x 0) 1) -1) (if (> x -1) (if (< x 1) (* x -1) 1) -1) (if (> x -1) (if (< x 1) (* x 0) 1) -1) (if (> x -1) (if (< x 1) (* x 1) 1) -1) (if (> x -1) (if (> x 0) 1 0) -1) (if (> x -1) (if (> x 0) 1 (+ x 0)) -1) (if (> x -1) (if (> x 0) 1 (* x -1)) -1) (if (> x -1) (if (> x 0) 1 (* x 0)) -1) (if (> x -1) (if (> x 0) 1 (* x 1)) -1) (if (> x -1) (if (> x 1) 1 (+ x 0)) -1) (if (> x -1) (if (> x 1) 1 (* x 1)) -1))

So that's what I did this week.
