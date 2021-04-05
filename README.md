# asgn1 superoptimizer

## was this a good idea? hmmmm

the idea was to make a simple language, and then generate all possible expressions in that language that would be equivalent to a specified value

you can see a definition for a language in asgn1.rkt. I wrote a python script to generate all possible expressions that were arithmetic binary operators and only going 2 levels deep. Then in check.rkt, I ran through all of the generated expressions to see which ones were equal to a specified value.
