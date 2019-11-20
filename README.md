# Tableau External Services API: Adding Haskell Expressions as Calculations

> adding the Haskell programming language as a calculation engine to Tableau, by using External Services API.


## Tableau External Services API?

External Services API is the most underrated, underdocumented API in the whole Tableau Ecosystem. It’s so neglected, that it doesn’t even belong to Tableau’s extensibility team.  Which is fine, I hate popular things, and frankly, being a trailblazer these days are getting highly appreciated.

If you search the term “Tableau External Services“, you will find some documentation on Tableau’s help site regarding what it is (“Tableau supports a set of functions that you can use to pass expressions to external services for integration with R, MATLAB and Python.”), but none of these pages mentioning its APIs. The only single document I found is here, on TabPy’s github wiki. But it also forgets to mention what this REST API is actually doing.

So here is the deal: with the External Services API basically you can:

 1. Add new supported programming/script languages to the Tableau Calculation engine ( `script_` commands)
 2. Call web services directly as calculations without any TabPy, RServer or other middleware
 
Pretty cool, ha? #2 works well when you start building your own company-specific calculations or data processing functions in pure AWS Lambdas then invoke it directly from Tableau Desktop or Server. I have honestly no clue why Tableau keeps it as a secret, but it’s definitely a good material for a hackathon. However, as I am about to add a new language to Tableau let’s focus on #1.

## Why Haskell?

Being a special snowflake is my number one mission during these hackathons. As a programming language, Haskell seems to have a similar mission, it is really different (in a good way) than the other programming languages. To understand why Haskell is different, let me share this quote from this “How to Learn Haskell” guide:

First of all, you should probably pretend like you’ve never programmed before. If you run into a word like return or class don’t assume that you know what that means; chances are that you don’t. Here’s a general idea of where you stand, depending on what languages you already know well:

C, C++, Java: Haskell will probably blow your mind. The notion of classes is almost completely different, return doesn’t mean what you think it does, the algorithmic patterns are distinct, you don’t get loops, and code doesn’t run in the order it’s typed on the screen. What!? Yeah. It’s pretty awesome.

Trust me, it is *really* awesome, especially when it goes to analytical expressions. Look at this Fibonacci calculation code snippet:

`fix (scanl (+) 0 . (1:))`

Every single character has its meaning. Fix function is typed recursive lambda generator, scanl  is an accumulator function, “.” is a function composition operator and “:” a list constructor. Makes sense.

Another example for calculating prime numbers:

```haskell
primesT = sieve [2..]
          where
          sieve (p:xs) = p : sieve [x | x <- xs, rem x p > 0]
```

Looks exactly as the mathematical definition of prime numbers. This is one Haskell’s value: its expressions are close to mathematical equations.

More here: https://databoss.starschema.net/tableau-external-services-api-adding-haskell-calculations/
