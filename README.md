ti83.hs
=========

Make TI-83 BASIC programs with (some) help from Haskell.

## Premise

The idea is to convert the popular calculator TI-83's language into a Haskell DSL of sorts that enables you to build TI-83 programs from within Haskell. There are two premises to this idea:

1. Evaluating data programs where you use Haskell data types to build your programs
2. Building a program using a series of Haskell functions via Monadic connectors

Hence there will be two different approaches to building out these programs. One will be a list of data constructors while the other is just a bunch of chained function calls.

## Data Types vs. Monadic Chain

As I was developing this program, I started noticing patterns that made me wonder what ways I could go about simplifying this. I counted two ways in my head that made sense, but I feel both have drawbacks. So I'm just going to go ahead and write about both approaches.

1. Data-type program construction allows very rigid development of programs. All the TI-83 BASIC functions and syntax are modeled and proves to be very simple to edit. Haskell can then be used almost as a macro-level language to create complex programs without much user-overhead. When evaluating these data-type lists, errors will be verbose and should help reduce program errors (lack of GOTO symbols, out-of-loop-scope assignments, etc)

2. The Monadic chain approach using the classic `>>=` function interested me because I wanted to see how we could enable the use of monads to develop a program like this. However, what I ended up writing with write states simply looked exactly like the `Either a b` monad, so we will be directly inheriting all the properties of the `Either` monad for this. Our programs in both cases will always have a type that looks like `Either [TIError] TIState` to represent our evaluation, but the monadic approach will be less intuitive because once it reaches an error, it will stop all evalulation. But since this is a series of function calls, this step is done at compile-time instead of run-time, making it faster than the data-type evaluation approach.

### More about Monad Chains

Between the two, there will be some major differences. One being that for data evaluation, it relies only on a few functions to be evaluated entirely. This is because of Haskell's sum-type matching rules you can write on top of functions. When using the bind (`>>=`) function to create a sprawling monadic function, there are some key differences to note with some functions versus the data constructor approach.

Namely it has all to do with sub-block scopes and evaluation. In our rule-based approach, we copy our state and run our evaluation function over a list of instructions (`[Instruction]`). This idea changes when we switch to monad functions. Do we have a list of `Instruction` now? Nope. We have one big function that looks like `TIState -> Either [TIError] TIState`. How is this different? Well, this one doesn't get evaluated in the same sense as the data type model. It instead is just called like a regular function.

Where our `FOR` keyword may have looked like this with sum-types:
```haskell
for :: String -> (Int, Int) -> [Instruction] -> TIEither
```

It now becomes this to handle the outcome of the sub-block:
```haskell
for :: String -> (Int, Int) -> (TIState -> TIEither) -> TIState -> TIEither
```

So really, all our conditional monadic functions will have signatures looking like:
```haskell
for :: String -> (Int, Int) -> (TIState -> TIEither) -> TIState -> TIEither
forI :: String -> (Int, Int, Int) -> (TIState -> TIEither) -> TIState -> TIEither
while :: ArithE -> (TIState -> TIEither) -> TIState -> TIEither
if :: ArithE -> (TIState -> TIEither) -> TIState -> TIEither
ifelse :: ArithE -> (TIState -> TIEither) -> (TIState -> TIEither) -> TIState -> TIEither
```

Okay so if-else is pretty massive but it makes sense to have two functions callable for both paths.


### Things to Think About

1. Scope and lexical scope. This one is a tough approach because when we run through sub-blocks like `FOR` or `IF`, should we have access to exterior variables from the scope level above it? Can we somehow differentiate scope level so we can read some variables but be unable to write to them to prevent bugs in code? I may later on transition to keeping track of level of scope just to determine if we have access in certain blocks, but so far, all programs will have lexical scope without guaranteed safety of no mutations.

2. Goto jumps outside of blocks. Whenever Goto is implemented in languages, jumping out of scopes like loops is fairly common. But should we keep it safe to help prevent bugs? Should gotos in loops only be allowed to access labels in that scope level, or should we allow gotos to go anywhere they so please?

These two issues I may later on try to create parameters to allow certain conditions so it is easy to read from the state (would just be a few boolean values attached to the record).

I'm going to do my best to maintain both approaches as I find both are pretty interesting approaches to a single problem.
