\title{\textbf{Distributed Modal Logic}}

\author{Matt Parsons}

\maketitle

\abstract{
Logic, type theory, and category theory give three equivalent ways of expressing
computation. Logical theorems and axioms can be implemented as type systems for
programming languages using the Curry-Howard correspondence, providing a novel
means for safer code. This paper explores this connection and uses a category
theory interpretation of modal logic to present an implementation of modal logic
applied to distributed systems.
}

# Introduction

Conal Elliot's functional reactive programming [@Elliott99:Spritify] provides a
declarative and denotative basis for expressing programs that vary over time.
The naive implementation of FRP is prone to space- and time-leaks, which can be
fixed in two ways. The first is through category theory: by using an abstraction
called an `Arrow`, Hudak et al [@hudak2002] solve many of these issues. The
second is through logic: Jeffery [@Jeffrey2012] demonstrates that Linear-time
Temporal Logic types FRP well, and the additional rules of LTL provide
safeguards against these performance issues.

Foner's "Getting a Quick Fix on Comonads" [@foner2015] examined an
interpretation of Löb's theorem in Haskell and determined that it did not
properly satisfy the axioms of $S4$ modal logic. By finding an appropriate
Haskell type class that fit the logic more closely, he was able to derive a
rather natural and efficient implementation of $n$-dimensional spreadsheets with
relative references. The type class in question is `ComonadApply`, a concept
imported from category theory.

Tom Murphy VII's "Modal types for Mobile Code" presented a novel interpretation
of $S5$ modal logic applicable to distributed systems. [@murphy2008modal] Murphy
interprets $\Box A$ to be a continuation yielding an $A$ that may be run
anywhere on the network, and $\Diamond A$ to be an address to a remote value of
type $A$.

Logic and categories both offer compelling resources for software developers to
improve their work. Murphy does not mention category theory in his work. Is it
possible that a categorical interpretation of his distributed system might
provide additional insights or implementation tricks?

# The Logic

Before diving into the logic presented in Murphy's paper on Lambda 5
[@murphy2004symmetric], let's review the basic concepts of modal logic.

## Modal Logic

Modal logic is an augmentation of propositional logic that provides a pair of
new operators:

- $\Box$, signifying 'always' or 'necessarily'
- $\Diamond$, signifying 'at some point in the future' or 'possibly'

The precise semantics of modal logic are determined by which axioms you take.
The common axioms are described here:

- N: Necessity: if $A$ is a theorem, then so is $\Box A$.
- K: Distribution $\Box(A \implies B) \implies (\Box A \implies \Box B)$
- T: Reflexivity $\Box A \implies A$
- 4: $\Box A \implies \Box \Box A$
- B: $A \implies \Box \Diamond A$
- 5: $\Diamond A \implies \Box \Diamond A$

The core of modal logic is a tower of these axioms, each added one at a time: K
is the combination of N and K axioms, T adds reflexivity, $S4$ adds the 4 axiom,
and $S5$ adds either the 5 axiom or the B axiom.

|   Transitivity   |    Reflexivity    |     Symmetry     |
|:-------------------:|:-------------------:|:-------------------:|
| $\Box A \implies \Box \Box A$ |  $\Box A \implies A$  | $\Diamond A \implies \Box \Diamond A$ |
| $$
\begin{tikzcd}
A \arrow[r, bend right] \arrow[rr, bend left, dotted] & B \arrow[r, bend right] & C
\end{tikzcd}
$$ | $$
 \begin{tikzpicture}
   \node (a) {$A$};
   \path[->] (a) edge [loop above] node {} ();
  \end{tikzpicture}
$$ | $$
\begin{tikzcd}
A \arrow[r, bend right] & B \arrow[l, bend right]
\end{tikzcd} $$ |

These axioms speak to the specific characteristics of the accessibility
relationship. It's easiest to discuss these in terms of the 'possible worlds'
model of modal logic, where $A$ means that $A$ is true in this world, $\Box A$
means that $A$ is necessarily true in all worlds, and $\Diamond A$ means that
$A$ is true in some possible world. The three major traits of the accessibility
relationship are transitivity (if world A can access world B, and world B can
access world A, then A can access C), reflexivity (world A can access world A),
and symmetry (if world A can access world B, then world B can access world A).
As an aside, the reflexivity and transitivity properties are precisely what is
necessary to form a category.

## Propositions as Types

The Curry-Howard correspondence tells us that a logical proposition is
equivalent to a type signature in a programming language, a logical proof is
equivalent to an expression fitting the type of the sentence, and the evaluation
of programs is equivalent to the simplification of proofs.
[@wadler2015propositions] Therefore, the type systems for our programming
languages serve as systems to make logical statements about our programs. The
more interesting things we can say in our types, the more useful they are in
verifying the correctness of our programs. By having these interesting things
correspond to a well-behaved formal logic, then we constrain ourselves to making
sense. Compile-time verification is particularly appealing: we can't even run
the code if it doesn't make logical sense!

## Lambda 5

Let's evaluate the logical system Lambda 5 as defined by Murphy.
[@murphy2004symmetric] It's designed to be used as a type theory for distributed
computation. The "many worlds" in this logic are the distinct computers running
in a distributed network. Murphy's logic interprets $\Box A$ to mean "Mobile
code of type $A$ that can be executed on any computer," and $\Diamond A$ to mean
"an address of a remote value of type $A$."

Because the relationship between computers on a network is reflexive,
transitive, and symmetric, the $S5$ logic is the basis for this system.
Furthermore, because constructive proofs are more useful for type theories, the
intuitionistic variant $IS5$ is used. Intuitionistic logic dispenses with double
negation and the law of excluded middle. In this system, the two operators can
no longer be expressed in terms of each other. This makes the $B$ and $5$ axioms
tricky. Since it's generally not accepted to do $A \implies \Box A$, and the two
axioms are able to generate some boxiness given some diamondness, then they
*must* be related somehow.

The specific axioms for $IS5$ are given below: [@Galmiche2010]

1. $\Box (A \implies B) \implies (\Box A \implies \Box B)$
2. $\Box (A \implies B) \implies (\Diamond A \implies \Diamond B)$
3. $\Diamond \bot \implies \bot$
4. $\Diamond (A \land B) \implies (\Diamond A \land \Diamond B)$
5. $(\Diamond A \implies \Box B) \implies \Box (A \implies B)$
6. $(\Box A \implies A) \land (A \implies \Diamond A)$
7. $(\Diamond \Box A \implies \Box A) \land (\Diamond A \implies \Box \Diamond A)$

The operational semantics for the Lambda 5 language involve deterministic
sequential machines operating on various worlds, termed $w_i$. A value can be
accessed remotely given a world name $w_i$ and the label for the value.

One of the key examples given is the symmetry axiom, $\Diamond \Box A \implies
\Box A$. We can read this as "Given a remote address to a value of mobile code
that can run anywhere and produce an $A$, we can retrieve that code and run it
here." The proof term given in the paper is:

$$\lambda x. letd \ w.y = x \ in \ \text{fetch[w]} y$$

where $letd$ is a primitive of the $\Diamond$ elimination rule, which binds a
pair of variables $w$ (representing the world that the value originated in) and
$y$ (the value at that world) in the expression after $in$. `fetch` is a
primitive that takes a value $M : \Box A @ w'$ and runs it in the current world
$w$. The signature $M : \Box A @ w'$ can be read as: $M$ is a continuation that
can be executed at any world currently located at $w'$.

# Category Theory

Category theory is a branch of abstract mathematics that seeks to provide a
unifying meta-language to talk about mathematics. The following sections
introduce ideas in category theory that we need in order to provide the link to
the modal logic above. This theory will inform our implementation in Haskell.

## Category

A category is an algebraic structure consisting of a collection of objects and
morphisms (also known as arrows) between objects. For a first intuition,
categories generalize sets, and arrows generalize functions where the domain and
range are the same. For $\cat{C}$ to be a category, the objects and morphisms
must satisfy the following properties:

1. Each object has an identity arrow:
$$\forall a \in \cat{C}, \exists f \in \cat{C}_{\to}. f \ a = a$$
 $$ \begin{tikzpicture}
   \node (a) {$a$};
   \path[->] (a) edge [loop above] node {$id$} ();
  \end{tikzpicture} $$

2. Arrows compose associatively
$$\forall a, b, c \in \cat{C}. (a \to b) \to c = a \to (b \to c)$$
$$ \begin{tikzcd}
a \arrow[dr,"f"] \arrow[rr, "f \circ g"] & & c \\
& b \arrow[ur, "g"] &
\end{tikzcd} $$

Arrows in category theory correspond with implication in logic and function
types in type theory. The notion of objects and arrows permits the drawing of
illustrative diagrams. The associativity property means that any diagram is
*commutative*: any path you follow from one object to another using the directed
arrows yields the same final result. If we have two paths through a diagram,
then we can select the shortest path and be confident that our result is the
same.

## Functor

Arrows in a category don't quite generalize functions in the same way that
categories generalize sets. For a category $\cat{C}$, the arrows in the category
correspond to endomorphisms: functions with the type $f : \forall a \in \cat{C}
.\ a \to a$. Functions like $\lambda x. x^2 : \bR \to \bR$ or $Succ : \bN \to
\bN$ are examples.

Functions that map elements from one set to another, like $\lambda x y.
\frac{x}{y} : \bN \to \bN \to \bQ$, require a more powerful concept. The
categorical equivalent is called a functor. A functor $F$ is a mapping between
categories $\cat{C}$ and $\cat{D}$. $F$ maps every object and arrow in $\cat{C}$
to $\cat{D}$ with two laws:

$$\begin{tikzcd}
F(a) \in \cat{D} \arrow[r, "F(f)"] & F(b)
\\
a \in \cat{C} \arrow[u, "F"] \arrow[r, "f"] & b \in \cat{C} \arrow[u, "F"]
\end{tikzcd} $$

### Identity

$$\forall a \in \cat{C}. \ id (F \ a) = F (id \ a)$$

$$\begin{tikzcd}
a \arrow[d, "id"{left}] \arrow[r, "F"] & F(a) \arrow[d, "id"]
\\
a \arrow[u, ""{name=ID,right}] & F(a) \arrow[u, ""{name=FID,left}]
\arrow[from=ID, to=FID, "F(id)", dotted]
\end{tikzcd}$$

If a functor maps an object $a$ in $\cat{C}$ to $b$ in $\cat{D}$, then the
functor must map the identity arrow for $a$ to the identity arrow to $b$.

### Composition

$$\forall f, g : a \to b \in \cat{C}. \ F (f \circ g) = F(f) \circ F(g)$$

$$\begin{tikzcd}
& b \arrow[dr, "g"{name=G}] \arrow[ddd, dotted, crossing over, bend left] &
\\
a \arrow[ur, "f"{name=F}] \arrow[d, bend right, dotted, "F", swap] \arrow[rr, "f \circ g"{name=fog}] &  & c \arrow[d, bend left, dotted]
\\
x \arrow[dr, "h"{name=H}] \arrow[rr, "h \circ k"{name=hok}] & & z
\\
& y \arrow[ur, "k"{name=K}] &
\arrow[from=fog,to=hok, dotted, bend right]
\arrow[from=F,to=H, dotted, bend right]
\arrow[from=G,to=K, dotted, bend left]
\end{tikzcd}$$

In the above diagram, the functor $F$ maps each object and arrow from the top
category $\cat{C}$ to the bottom category $\cat{D}$ with the dotted lines
representing the mappings. The mapping of composition of arrows must be equal to
the composition of the mapping of arrows.

Functors give rise to the category of categories $Cat$, where objects are
categories and morphisms are functors between categories.

### Correspondence in Type Theory

Functors correspond to type constructors or type functions in type theory, with
the added laws of identity and composition. We can gain an intuition on functors
by considering examples of generic or parametric types.

In Haskell, functors (specifically, endofunctors on the category $Hask$ of
Haskell values) are defined as:

```haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b
```

Containers form easy functors to get an intuition about. A container can be a
functor if you can provide an implementation of `fmap` which transforms each `a`
in the container to an element of type `b`. Many non-container types are
functors, too, like functions with a fixed input value:

```haskell
instance Functor ((->) input) where
    fmap :: (a -> b) -> (input -> a) -> (input -> b)
    fmap f g = \x -> f (g x)
```

as well as continuations:

```haskell
newtype Cont r a = Cont { runCont :: (a -> r) -> r }

instance Functor (Cont r) where
    fmap f c = Cont (\k -> runCont c (k . f))
```

and stateful computation:

```haskell
newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
    fmap f sa = State (\s -> let (a, s') = runState sa s
                              in (f a, s'))
```

The `fmap` operation is about generalizing a function that works on ordinary
values to work on a whole `Functor`ful of ordinary values.

## An Entirely Natural Transformation

A natural transformation is a mapping $\eta$ between two functors $F$ and $G$ on
two categories $\cat{C}$ and $\cat{D}$ that satisfy the following commutative
diagram:

$$\begin{tikzcd}
F(X) \arrow[d, "\eta_X"] \arrow[r, "F(f)"] & F(Y) \arrow[d, "\eta_Y"]
\\
G(X) \arrow[r, "G(f)"] & G(Y)
\end{tikzcd}$$

A natural transformation is a way to convert one functor into another. Similar
to how categories+functors form a category, natural transformations and functors
between two categories also form a category. The objects in the category are the
functors, and the morphisms are the natural transformations.

### Code Examples

The essence of a natural transformation is that you shouldn't use any of the
information from the objects in the underlying category when mapping the
functors. In Haskell, this can be expressed in terms of a rank 2 type:

```haskell
type Nat f g = forall x. f x -> g x
```

The use of the rank 2 type constrains the function from being able to view or
inspect the values inside the functors.

A function to convert a `[a]` into a `Vector a` is an easy natural
transformation, as is the inverse There's also a natural transformation `[a]` to
`Map Int a`, where the integer keys are the indexes in the list.

A natural transformation is not required to preserve all information. `Maybe`
has a natural transformation to `List`:

```haskell
maybeToList :: Nat Maybe []
maybeToList (Just x) = [x]
maybeToList Nothing  = []
```

Note that the type signature does not mention the type of the value in the
container. We can expand the type signature to read as:

```haskell
maybeToList :: forall a. [a] -> Maybe a
```

We're asserting that this function must work for all types `a`, and that we
can't use any information about the `a`s inside in order to implement the
function. The reversal of this natural transformation is necessarily lossy:

```haskell
listToMaybe :: Nat [] Maybe
listToMaybe (x:_) = Just x
listToMaybe []    = Nothing
```

## Monad

A monad is an endofunctor (a mapping from $\cat{C}$ to $\cat{C}$) that is
equipped with two natural transformations:

1. $\eta$, or `return`, taking any object from the identity functor $I(\cat{C})$ to $F(\cat{C})$.
2. $\mu$, or `join`, taking objects from $F(F(\cat{C}))$ to $F(\cat{C})$.

Monads follow the monoid laws of associativity and identity where the unit is
$\eta$ and $\oplus$ is $\mu$. The identity functor is an easy monad. The natural
transformation $\eta$ is the identity natural transformation. For $\mu$, we can
show (by commutativity of the diagram) that taking identity twice is equivalent
to taking identity once:

$$\begin{tikzcd}
\cat{C} \arrow[r, "Id", bend right] \arrow[rr, "Id", bend left, dotted] & \cat{C} \arrow[r, "Id", bend right] & \cat{C}
\end{tikzcd}$$

### More Code Examples

Monads can be represented as a type class in Haskell, with the following
definition:

```haskell
class Applicative m => Monad m where
    return :: a -> m a
    (>>=)  :: m a -> (a -> m b) -> m b
    join   :: m (m a) -> ma

    ma >>= f = join (fmap f ma)
    join mma = mma >>= \ma -> ma
```

We can provide a default implementation of bind (written here with the infix
operator `>>=`) and join written in terms of each other. Lists and optionals
form monads.

```haskell
instance Monad [] where
    return a = [a]
    join (xs:xss) = xs ++ join xss
```

The list monad is used for nondeterministic programming and is the basis for
list comprehensions in Haskell.

```haskell
instance Monad Maybe where
    return a = Just a
    Just a  >>= f = f a
    Nothing >>= _ = Nothing
```

The `Maybe` monad is useful when sequencing actions that might fail, like
looking up items in a dictionary.

A monad is a powerful abstraction for sequencing computational effects. Given a
value in a monadic structure `m`, we can provide a function that operates on
that value and yields more monadic structure. `bind` handles the boilerplate of
flattening it back out. Monads are a convenient way to express state,
asynchronous computation, and computation that may error.

The use of monads in computer science was initially discovered by Eugenio Moggi
[@moggi89] and then popularized by Wadler [@wadler95] and others for use in
functional programming.

## Comonad

A comonad is the categorical dual of a monad. In category theory, all ideas have
a dual. The dual of a concept is given by taking the object's diagram reversing
all of the arrows. We can arrive at something similar by reversing the arrows of
types in type signatures. Flipping the `Monad` function's arrows gives us:

```haskell
return    :: a -> m a
coreturn  :: a <- m a
extract   :: w e -> e

join      :: m (m a) -> m a
cojoin    :: m (m a) <- m a
duplicate :: w e -> w (w e)

bind      :: m a -> (a -> m b) -> m b
cobind    :: m a <- (a <- m b) <- m b
extend    :: w q -> (w q -> e) -> w e
```

Where a monad gives us the ability to take a single value in a structure and
generate new structure, a comonad gives us the ability to take a structure of
values and yield a single new value. The canonical comonad is the infinite
stream data type:

```haskell
data Stream a = a :< Stream a

instance Functor Stream where
    fmap f (a :< sa) = f a :< fmap f sa

instance Comonad Stream where
    extract (a :< _) = a
    duplicate s@(a :< as) = s :< duplicate as
    extend f = fmap f . duplicate
```

To `duplicate` a stream, we set the value of index $i_n$ to be the entire
stream, and the position at $i_{n + 1}$ to be the stream starting from $i_{n + 1}$.

## Adjunctions

An adjunction is a pair of functors which generalize the notion of a Galois
connection. From the category $\cat{C}$, the functor $R$ *lifts* an object to
the category $\cat{D}$ by providing some free structure. The functor $L$ then
*loses* some information about an object in $D$ to map the object back to $C$.
This isn't necessarily an isomorphism, as forgetting composed with lifting $R
\circ L$ loses some information and can't map the object back. However, lifting
followed by forgetting $L \circ R$ is equivalent to the $Id$ functor, and $R
\circ L \circ R$ is equivalent to $R$.

In this case, we write $L \dashv R$ to say that $L$ is left adjoint to $R$ and
that $R$ is right adjoint to $L$. An interesting property of an adjunction is
that every adjunction gives rise to both a monad through $R \circ L$ and a
comonad through $\L \circ R$.

### A Common Adjunction: Pair and Function

The Haskell tuple `(r, a)` (also known as $Prod_r \ a$) and function `r -> a`
(also known as $Exp_r \ a$) are both functors, and it is the case that $Prod
\dashv Exp$.

```haskell
instance Functor ((,) r) where
    fmap f (r, a) = (r, f a)

instance Functor ((->) r) where
    fmap f k = f . k

class (Functor g, Functor f) => Adjunction f g | f -> g, g -> f where
    unit   :: a -> g (f a)
    counit :: f (g a) -> a
    left   :: (f a -> b) -> a -> g b
    right  :: (a -> g b) -> f a -> b

instance Adjunction ((,) r) ((->) r) where
    unit a        = \r -> (r, a)
    counit (r, f) = f r
```

We can specialize the type of `unit` to this specific adjunction, which
illustrates the monad/comonad:

```haskell
unit   :: a -> r -> (r, a)
counit :: (r, r -> a) -> a
```

Indeed, we can write a generic instance of the `Monad` type class in Haskell
that works with any composition of two functors which form an adjunction. If we
flip the composition, then we can write a comonad instance as well. This is
witnessed below:

```haskell
newtype Compose f g a = Compose { decompose :: f (g a) }

instance (Adjunction f g, Applicative f, Applicative g) => Monad (Compose g f) where
    return = Compose . unit
    m >>= f = Compose . fmap (right (decompose . f)) . decompose $ m

instance (Adjunction f g) => Comonad (Compose f g) where
    extract = counit . decompose
    extend f = Compose . fmap (left (f . Compose)) . decompose
```

If this is the case, then `Compose ((,) r) ((->) r)` should be a comonad, and
`Compose ((->) r) ((,) r)` should be a monad. If we expand the types, we'll see
that:

```haskell
Compose ((->) r) ((,) r)
  = (->) r ((,) r a)
  = r -> (r, a)

Compose ((,) r) ((->) r)
  = (,) r ((->) r a)
  = (r, r -> a)
```

Indeed, the monad formed by the adjunction of the two functors is the `State`
monad! The comonad is known as the `Store` comonad, and is less common in
practice.

# Link to Modal Logic

Now that we've gathered all of our toys from logic and category theory, it's
time to figure out how they're related. The type signatures of the functions in
the `Monad` and `Comonad` classes look awfully familiar to the axioms for the
$\Diamond$ and $\Box$ constructs. Let's see if there's more to that.

## Diamonad

The $\Diamond$ operator in modal logic looks like it corresponds to the monad in
category theory. Let's consider the natural transformations $\eta : I(A) \to
M(A)$ and $\mu : M(M(A)) \to M(A)$. Can we prove these?

$\eta$ is simple enough. It corresponds with the $\Diamond$ introduction rule $A
\vdash \Diamond A$: "If A is true, then A is possibly true." $\mu$, likewise, is
the $\Diamond 4$ axiom.

We can additionally prove the type of `bind :: Monad m => m a -> (a -> m b) -> m
b` by taking advantage of the distribution of $\Diamond$ over implication.

\begin{prooftree}
\AxiomC{$\Diamond A$}

\AxiomC{$A \implies \Diamond B$}

\RightLabel{$\Diamond_I$}
\UnaryInfC{$\Diamond(A \implies \Diamond B)$}

\RightLabel{Distribution}
\UnaryInfC{$\Diamond A \implies \Diamond \Diamond B$}

\BinaryInfC{$\Diamond \Diamond B$}

\RightLabel{$\mu$}
\UnaryInfC{$\Diamond B$}

\end{prooftree}

## Comonad

The fundamental operations for a comonad are duplicate ($\Box A \implies \Box
\Box A$), which is precisely the axiom that gives rise to $S4$ modal logic, and
extract ($\Box A \implies A$), which is the $\Box$ elimination rule. Comonads,
as functors, also implement `fmap`, and the interaction of `fmap`, `duplicate`,
and `extract` must follow the following laws:

\begin{align}
extract \circ duplicate &= id \\
fmap \ extract \circ duplicate &= id \\
duplicate \circ duplicate &= fmap \ duplicate \circ duplicate
\end{align}

In $S4$ modal logic, a plain comonad is close to the $\Box$ operator, but we
don't have $\Box (a \implies b) \implies (\Box a \implies \Box b)$. The Haskell
type class `ComonadApply` equips a `Comonad` with exactly that function,
allowing it to satisfy the $S4$ axioms.

As Foner observed [@foner2015], all comonads in Haskell have a property known as
*functorial strength*. Given some `f a` and a `b`, you can `strong b = fmap (\a
-> (a, b))` to lift the `b` value into the `Functor`. This corresponds to a
proposition of the form $B \implies \Box A \implies \Box (A \land B)$, which is
impermissible in the logic. We'll need to take special care when writing our
implementation to avoid this.

## To IS5 and Beyond

The modal logic used in the Lambda 5 system uses $IS5$ as a basis, not $S4$.
Extending $S4$ to $S5$ adds the following axiom:

$$\Diamond A \implies \Box \Diamond A$$

This additional axiom expresses the idea that, if $A$ is possible, then the
possibility of $A$ is necessary. This makes a category theory interpretation
somewhat tricky. One of the necessary features of both $\Box$ and a comonad is
that we don't have $A \implies \Box A$ or $f : \forall w. Comonad \ w \implies a
\to w \ a$. In this case, we're relying on some structure of the monad that
permits us to push comonadic structure beneath.

In order to extend this interpretation to $S5$, we can't take just any old monad
and comonad for our interpretation. We'll need to delve deeper.

## Double the Adjunction, Double the Functor

Suppose we have two categories $\cat{C}$ and $\cat{D}$ with a triple of functors
$L$, $U$, $R$, where $L \dashv U$ and $U \dashv R$, and (as a natural
consequence of the composition of adjunctions) $L \dashv U \dashv R$.

$$\begin{tikzcd}
\cat{C} \arrow[r, "L", shift right=2ex, bend right] \arrow[r, "R", shift left=2ex, bend left] & \cat{D} \arrow[l, "U", swap]
\end{tikzcd}$$

This gives rise to a monad $M = U \circ L$ and a comonad $W = U \circ R$ on
$\cat{C}$. Since adjunctions compose, the combination $M \dashv W$ is an
adjunction. By assigning $M$ to $\Diamond$ and $W$ to $\Box$, we've arrived at
the axiom we require: $\Diamond \dashv \Box$. [@awodey2006] Asserting that in
Haskell gives us `Adjunction dia box` with a function `unit :: a -> box (dia
a)`, which satisfies our requirement.

Our task is to now find the appropriate functors and categories to provide a
suitable implementation of Lambda 5.

# Implementing in Haskell

The theory has presented us with a neat implementation plan. We can arrive at an
S5 modal logic categorically. For the complete implementation, see the
`Logic.Modal` Haskell module.

## Defining the Interface

First, we'll define a type class that represents the axioms of $S4$ modal logic.

```haskell
class CModalS4 box where
    axiom4 :: box p -> box (box p)
    axiomT :: box p -> p
    axiomK :: box (p -> q) -> box p -> box q
```

We can demonstrate that this is exactly equivalent to the `ComonadApply` type
class by providing an implementation solely in terms of that type class. To
avoid requiring an `UndecidableInstance`, a `newtype` wrapper will be used to
disambiguate the context. The only point of the newtype wrapper is to serve as a
witness that the type `w` is an instance of `ComonadApply`.

```haskell
newtype S4Witness w a = Valhalla { witnessMe :: w a }

instance ComonadApply w => CModalS4 (S4Witness w) where
    axiom4 = Valhalla . fmap Valhalla . duplicate . witnessMe
    axiomT = extract . witnessMe
    axiomK (Valhalla f) (Valhalla a) = Valhalla (f <@> a)
```

Since this establishes that they're equivalent, we can relegate `ModalS4` as a
type alias. Equipped with this, the definition for `ModalS5` is the addition of
the $B$ axiom in type signature form:

```haskell
type ModalS4 = ComonadApply

class (Monad dia, ModalS4 box) => ModalS5 box dia where
    axiomB :: a -> box (dia a)
```

As is customary when doing fancy things with Haskell, we'll provide some infix
type operator aliases to pretend that we're real mathematicians. These will help
clarify the instance contexts when defining the generic `Monad` and `Comonad`
instance

```haskell
type f :.: g = Compose f g
type f -| g = Adjunction f g

instance (f -| g, Applicative f, Applicative g) => Monad (g :.: f) where
    return = Compose . unit
    m >>= f = Compose . fmap (rightAdjunct (getCompose . f)) . getCompose $ m

instance (f -| g) => Comonad (f :.: g) where
    extract = counit . getCompose
    extend f = Compose . fmap (leftAdjunct (f . Compose)) . getCompose
```

The `Applicative` instances are required, as all `Monad`s must be instances of
the `Applicative` type class in recent versions of Haskell (and morally in all
versions of Haskell). And now, for the final bit of fun:

```haskell
instance (g -| u, u -| f, ModalS4 (u :.: f), Applicative u, Applicative g)
        => ModalS5 (u :.: f) (u :.: g) where
    axiomB = unit
```

Provided that `g` is left adjoint to `u`, `u` is left adjoint to `f`, `u` and
`g` are both `Applicative` functors, and finally that the composition of `u` and
`f` satisfy the `ModalS4` axioms, then the compositions of `u`, `f`, and `g`
satisfy the `ModalS5` axioms automatically. Now that we've defined the logical
type class, all that's left is to provide the functors `u`, `f`, and `g` that
satisfy the required constraints.

## Mechanical Satisfaction

In keeping with the spirit of programming in a lazy language, I'll lazily define
the necessary functors as empty type constructors, and have the compiler assert
the various requirements we need. We'll fill in only the bare minimum to get it
to compile, punting any real design decisions until later.

```haskell
data U a
data F a
data G a

type Box = U :.: F
type Dia = U :.: G

pls :: ModalS5 Box Dia => ()
pls = ()
```

By repeatedly requesting the `:type` of `pls` in GHCi, this causes the solver to
attempt to find instances and resolve them. When it fails, it reports the
failure, and we can fill in a pretend implementation. The list it provides is:

* `Applicative G`
* `Applicative U`
* `ComonadApply (U :.: F)`
* `Adjunction U F`
* `Functor F`
* `Representable F`
* `Distributive F`
* `Adjunction G U`
* `Representable U`
* `Distributive U`

which we can laughingly provide a dummy implementation as:

```haskell
data U a
    deriving (Functor)

instance Applicative U where
    pure = undefined
    (<*>) = undefined

instance ComonadApply (U :.: F) where
    (<@>) = undefined

instance Adjunction U F where
    unit = undefined
    counit = undefined

instance Adjunction G U where
    unit = undefined
    counit = undefined

instance Representable U where
    type Rep U = U Int
    tabulate = undefined
    index = undefined

instance Distributive U where

data F a
    deriving (Functor)

instance Representable F where
    type Rep F = F Int
    tabulate = undefined
    index = undefined

instance Distributive F where

data G a
    deriving (Functor)

instance Applicative G where
    pure = undefined
    (<*>) = undefined
```

Now that we've mechanically arrived at our constraints, it is time to consider
the actual semantics of these types. The semantics of the types will provide the
implementation of the data constructors, which will permit implementations of
the type classes, which will provide an implementation of the modal logic we
require.

## G, F, U and Meaning

### Remote Diamond

Lambda 5's $\Diamond A$ represents a remote address of a value of type $A$. The
$\Diamond$ is comparatively conceptually simple to implement. Let's inspect a
snippet of code to see how it might work:

```haskell
remoteValue :: Dia Int
remoteValue = ...

someFoo :: Dia Bool
someFoo = do
    value <- remoteValue
    return (10 < value)
```

Supposing we have some `remoteValue` representing an `Int` available at some
location, we can bind the `Int` out of that, and do operations on it in the
`Dia` monad. Alternatively, we should also be able to attempt to `fetch` the
remote value. Fetching will naturally have some concept of failure, and will
need to take place in `IO` in order to access network resources. The natural
type of `fetch`, then, is:

```haskell
fetch :: Dia a -> ExceptT NetworkError IO a
```

Which, in turn, provides some information as to what the functors for `Dia`
might be. `ExceptT NetworkError IO a` is a monad transformer stack representing
an `IO (Either NetworkError a)`. We might be tempted, then, to declare `U` as
`IO` and `G` as `Either NetworkError`. There's an issue with that: The `U`
functor is shared with the `Comonad`, and `IO` is certainly not a comonad of any
sort. This implementation then fails to satisfy our logic. `U` remains elusive,
though `G` could very well be `ExceptT NetworkError IO a`.

### Boxing it up

Lambda 5's $\Box A$ represents a continuation yielding a value of type $A$ that
can be run anywhere on the network. We might be tempted to note that the
definition of $\Box$ sounds like a pure computation with no dependencies,
equivalent to a lambda expression with no free variables. If we take that road,
then $\Box$ is simply the type of pure computations, or the `Identity` comonad.
We could dispense with the `Identity`, leaving us an interpretation of $\Box$
that was simply ordinary Haskell values. Wherever we have a comonad on
$\cat{C}$, we also have a monad with the reversed morphisms, so we can
categorically *shift* the monads up a stack. We'd require a pair of monads $M_1$
and $M_2$, then, one to represent ordinary expressions in modal logic, and
another to represent $\Diamond$.

Unfortunately, this fails to work in Haskell for one reason: free variables!
Consider the following snippet:

```haskell
foo :: Int -> Int -> Int
foo x y = x + y * bar

bar :: Int
bar = 6
```

If we tried to `box` up `foo` and send it across the network, it'd blow up at
runtime, requesting access to the `bar` term. This demonstrates that the $\Box$
must be a sealed, complete $\Box$: no free variables. The "effect" of plain
Haskell values then is access to free variables in the local machine state. In
order to provide computations that may be run remotely, we must eliminate these
free variables. The composition $U \circ F$ must provide some means of packaging
values and closures and making them available.

For inspiration on our next step, let's consider the introduction rule for
$\Box$:

$$A \vdash \Box A$$

which we may read in plain English as "If it is provable that, with no
assumptions, $A$ is true, then $\Box A$ is true." By substituting provability
for runnability and assumptions with dynamic values, we arrive at "If $A$ is
runnable with no input at run-time, then $\Box A$ is runnable." The model that
we're seeking is akin to the `constexpr` or `const` from C++ template
metaprogramming. Our $\Box$ will be built at compile-time for our run-time
programs to be able to use, unpack, and send about the network.

This insight fueled the development of Cloud Haskell. [@Epstein11] Cloud Haskell
features a Template Haskell directive to package a closure up at compile time as
long as all of the values are instances of a `Serializable` type class. The
`distributed-static` library [@distributed-static-0.3.4.0] makes these functions
available.

## Head in the Clouds

The Cloud Haskell paper describes a `Static` type that corresponds with values
known at compile time or top-level definitions, along with a `Closure` type that
represents serialized function closures. The paper describes a primitive,
`static e`, which (at compile time) takes an `e :: a` and makes a `Static a` out
of it, and another primitive `unstatic :: Static a -> a`. All terms described in
the `e` to be `static`ed must be top level definitions or constants, permitting
the compiler to evaluate them at compile time, serialize them, and apply the
`Static` constructor. If we can define either `duplicate :: Static a -> Static
(Static a)` or `extend :: Static a -> (Static a -> b) -> Static b`, then we've
got a `Comonad` instance. These primitives require an extension to the Haskell
compiler and runtime to implement, and that work has yet to be completed.

This instance satisfies the requirements to form the comonad we require in
order to implement $\Box$ for our logic.

```haskell
instance Comonad Static where
    extract = unstatic
    duplicate s = static s

instance ComonadApply Static where
    (<@>) = staticApply
```

We're safe to use `static` on the `s :: Static a` as the type `Static a` is a
proof that `s` is entirely serialized or a top-level definition.

The library that implements these concepts without compiler extensions
[@distributed-static-0.3.4.0] has a different model of execution. Since the
compiler isn't able to enforce the correctness of the static values and lookup
table, the type of `unstatic` is:

```hs
unstatic :: Typeable a => RemoteTable -> Static a -> Either String a
```

The `RemoteTable` is a runtime dictionary that maps serialized static labels to
their actual values. This function accepts a `RemoteTable` as input and looks up
the `Static a` value in the table. The value is stored as a `Dynamic` value, and
if the type safe `fromDynamic :: Typeable a => Dynamic -> Maybe a` cast
succeeds, then the function returns the `Right` result. If the lookup or cast
failed, then the function returns the `Left` error message.

`Either` cannot be made into a `Comonad`. What are we to do?

# Coercing the Cloud

On the one hand, we have Murphy's Lambda 5 distributed lambda calculus. On the
other, we have an implementation of Cloud Haskell. These two are tantalizingly
close to working together, if we can figure out some way to make an instance of
`ComonadApply` for the provided `Static` type in `distributed-static`

# References
