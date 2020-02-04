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

I'm going to do my best to maintain both approaches as I find both are pretty interesting approaches to a single problem.
