{-# OPTIONS_GHC -fglasgow-exts #-}
-- for the exported rank-2 type of runPeg,
-- as well as the implementation using GADTs, generalised newtype deriving,
-- also a phantom datatype used with unsafeCoerce

-- |
--
-- Linear time composable parser for PEG grammars.
--
-- frisby is a parser library that can parse arbitrary PEG grammars in linear
-- time. Unlike other parsers of PEG grammars, frisby need not be supplied with
-- all possible rules up front, allowing composition of smaller parsers.
--
-- PEG parsers are never ambiguous and allow infinite lookahead with no
-- backtracking penalty. Since PEG parsers can look ahead arbitrarily, they can
-- easily express rules such as the maximal munch rule used in lexers, meaning
-- no separate lexer is needed.
--
-- In addition to many standard combinators, frisby provides routines to
-- translate standard regex syntax into frisby parsers.
--
-- PEG based parsers have a number of advantages over other parsing strategies:
--
-- * PEG parsers are never ambiguous
--
-- * PEG is a generalization of regexes, they can be though of as extended regexes with recursion, predicates, and ordered choice
--
-- * you never need a separate lexing pass with PEG parsers, since you have arbitrary lookahead there is no need to break the stream into tokens to allow the limited LALR or LL lookahead to work.
--
-- * things like the maximal munch and minimal munch rules are trivial to specify with PEGs, yet tricky with other parsers
--
-- * since you have ordered choice, things like the if then else ambiguity are nonexistent.
--
-- * parsers are very very fast, guaranteeing time linear in the size of the input, at the cost of greater memory consumption
--
-- * the ability to make local choices about whether to accept something lets you write parsers that deal gracefully with errors very easy to write, no more uninformative "parse error" messages
--
-- * PEG parsers can be fully lazy, only as much of the input is read as is needed to satisfy the demand on the output, and once the output has been processed, the memory is immediately reclaimed since a PEG parser never 'backtracks'
--
-- * PEG parsers can deal with infinite input, acting in a streaming manner
--
-- * PEG parsers support predicates, letting you decide what rules to follow based on whether other rules apply, so you can have rules that match only if another rule does not match, or a rule that matches only if two other rules both match the same input.
--
-- Traditionally, PEG parsers have suffered from two major flaws:
--
-- * A global table of all productions must be generated or written by hand, disallowing composable parsers implemented as libraries and in general requiring the use of a parser generator tool like 'pappy'
--
-- * Although memory consumption is linear in the size of the input, the constant factor is very large.
--
-- frisby attempts to address both these concerns.
--
-- frisby parsers achieve composability by having a 'compilation' pass,
-- recursive parsers are specified using the recursive do notation 'mdo' which
-- builds up a description of your parser where the recursive calls for which
-- memoized entries must be made are explicit. then 'runPeg' takes this
-- description and compiles it into a form that can be applied, during this
-- compilation step it examines your composed parser, and collects the global
-- table of rules needed for a packrat parser to work.
--
-- Memory consumption is much less of an issue on modern machines; tests show
-- it is not a major concern, however frisby uses a couple of techniques
-- for reducing the impact. First it attempts to create parsers that are as
-- lazy as possible -- this means that no more of the file is read into memory
-- than is needed, and more importantly, memory used by the parser can be
-- reclaimed as you process its output.
--
-- frisby also attempts to 'optimize' your parser, using specialized strategies
-- when allowed to reduce the number of entries in your memoization tables.
--
-- frisby attempts to be lazy in reading the results of parsers, parsers tend
-- to work via sending out \'feeler\' predicates to get an idea of what the
-- rest of the file looks like before deciding what pass to take, frisby
-- attempts to optimize these feeler predicates via extra lazyness such that
-- they do not cause the actual computation of the results, but rather just
-- compute enough to determine whether a predicate would have succeeded or not.
--
-- (It is interesting to note that the memory efficiency of frisby depends
-- vitally on being as lazy as possible, in contrast to traditional thoughts
-- when it comes to memory consumption)
--
-- frisby is a work in progress, it has a darcs repo at
-- <http://repetae.net/repos/frisby> which may be browsed at
-- <http://repetae.net/dw/darcsweb.cgi?r=frisby;a=summary>
--
-- And its homepage is at <http://repetae.net/computer/frisby>
--
--
-- to learn more about PEG parsers, see this paper
-- <http://pdos.csail.mit.edu/~baford/packrat/popl04> and Bryan Ford's packrat
-- parsing page <http://pdos.csail.mit.edu/~baford/packrat/>
--


module Text.Parsers.Frisby(
-- * The basic types
-- ** The type of primitive parsers
    P(),
-- ** The monad used to create recursive parsers via rules
    PM(),
    newRule,
    runPeg,

    module Control.Applicative,

-- * basic operators
    (//),
    (<>),
    (<++>),

-- ** derived operators
    (->>),
    (<<-),
    (//>),
-- ** modification operators
    (##),
    (##>),

-- * basic combinators
    anyChar,
    bof,
    eof,
    getPos,
    char,
    noneOf,
    oneOf,
    text,
    unit,
    rest,
    discard,
    parseFailure,
-- ** speculative combinators

-- | These are how a frisby parser decides what path to take, whereas a
-- backtracking parser might try a path, then backtrack if it got it wrong, a
-- frisby parser can look at all possible paths before deciding which one to
-- take via these predicates. this is what allows much of the power of packrat
-- parsing, a parser is free to evaluate every alternative fully before
-- committing to a particular path.
--
-- packrat parsers have no past, but can \'see\' arbitrarily far into all of
-- its potential futures, traditional monadic parsers can accumulate a history, but
-- cannot see more than a token or two into the future, and evaluating multiple
-- futures to any degree imposes a significant run-time penalty due to backtracking.
--
    peek,
    doesNotMatch,
    isMatch,
    onlyIf,
    matches,
-- ** looping combinators
    many,
    many1,
    manyUntil,

-- ** various utility combinators
    between,
    choice,
    option,
    optional,
-- * regular expression syntax
-- | take a string in extended regex format and return a frisby parser that has the same behavior.
--
-- the behavior is slightly different than POSIX regular expressions,
--
-- frisby regular expressions always follow the 'true' maximal or minimal munch
-- rules, rather than the (unuseful and inefficient) greedy rule of POSIX
-- regexs.
--
-- what this means is something like
-- @x*x@ will never match, because the first @x*@ will much every x available so the second won't match.
--
-- minimal munching can be expressed like in perl
-- @.*?y@ will grab everything up to the next y, in posix it would grab everything up til the last y in the file.
--
-- these are much more natural semantics and much more efficient to implement.
--
    newRegex,
    regex,
    showRegex
   )where

import Control.Applicative
   hiding(many,optional) --though same meaning 'many', and superior 'optional'
import qualified Data.IntSet as IntSet
import Control.Monad.Fix
import Control.Monad.Identity
import Char(ord,chr)
import Control.Monad.State
import Data.Array hiding((//))
import Data.Monoid hiding(Any)
import qualified Data.Map as Map

--inline usable part of Unsafe.Coerce until that module is commonly available
import GHC.Base (unsafeCoerce#)
unsafeCoerce :: a -> b
unsafeCoerce = unsafeCoerce#

-- Essentially we are manipulating a polytypic cyclic graph (the P structure).
-- This is difficult to do in Haskell.
-- Graphs have always been difficult in Haskell. They're not inductive.
-- GADTs suffice well for polytypic trees.
-- The moment it becomes non-inductive...
-- _Implicit_ (directed-)acyclic-graph sharing can be introduced
--                with non-recursive lets/lambdas.
-- _Implicit_ (directed or not)-cyclic-graph-sharing can be introduced
--                with recursive lets / the least fixed point operator.
-- But when sharing is implicit we can't detect it while performing induction
-- over the graph, so we get infinite loops or simply failing to detect
-- intended sharing.  After all Haskell is referentially transparent!
-- But we want to be able to optimize this graph to achieve the potential
-- of PEG parsers.  This is possible to do explicitly, though a bit ugly
-- and seems to be beyond Haskell's static type system when not everything
-- in the graph has the same type.
--
-- To specify input in a way that we can understand the finite structure,
-- the best we have, to recommend, is mdo.  With this, it's the user's
-- responsibility not to create cycles that don't go through the monad.
-- (e.g. let q p = (conid <++> text "." <++> q p) // p
-- needs to involve mdo/mfix such as
--  let q p = mfix $ \qp -> (conid <++> text "." <++> qp) // p
-- , which makes its usage a little different)
-- With rank-2 types, it is *not* the user's responsibility to avoid mixing
-- up P's from different PM's, which is a good thing because that's not
-- typesafe or at least not abstraction-safe.  (e.g. this can't be used
-- in a parser monad that is itself runPeg'd:
--     runPeg (mdo
--               good <- newRule $ text "good"
--               evil <- newRule $ unit good
--               return evil
--            )
-- ).
--
-- Now we have the polytypic cyclic graph, can we manipulate it with Haskell's
-- type-system?  Unfortunately, the Named graph-vertices pose a difficulty.
-- The PM monad could perhaps be based on, instead of numerical indexes,
-- ST(Refs) (since the ST interface cannot be implemented in any known Haskell
-- typesystem without unsafeCoerce, it constitutes an extension to the
-- type system, in a sense -- is this correct?).  But then we have to
-- duplicate the memoization for each character in the input stream, keeping
-- each reference to a top-level parser referring to it --
-- not to the original, but to the saturated, memoized (lazy thunk) version!
-- Not sure this is possible here without unsafeCoerce.
--
-- Of course, with unsafeCoerce we could theoretically eliminate a bunch of
-- other type system extensions we internally use, such as GADTs. That would
-- be a terrible hack.

newtype Token = Token Int
    deriving(Eq,Ord,Num,Show,Ix)

-- the monad used for creating recursive values
newtype PM s a = PM (PMImp a)
    deriving(Monad,MonadFix,Functor)

type PMImp a = State Token a


-- 's' added for safe state, just as the ST monad's interface uses
newtype P s a = P { fromP :: PE a }
    deriving(Functor,Applicative,Alternative,Monoid)

data PE a where
    Char :: IntSet.IntSet -> PE Char
    Any  ::  PE Char
    Failure :: PE a
    Named :: Token -> PE a -> PE a
    Not :: PE a -> PE ()
    PMap :: (a -> b) -> PE a -> PE b
    Slash :: PE a -> PE a -> PE a
    ThenCat :: PE [a] -> PE [a] -> PE [a]
    Star :: PE a -> PE [a]
    StarUntil :: PE a -> PE b -> PE [a]
    StarMax :: Int -> PE a -> PE [a]

    Then :: PE a -> PE b -> PE (a,b)
    GetPos :: PE Int
    Unit :: a -> PE a
    When :: PE a -> (a -> Bool) -> PE a
    Rest :: PE [Char]
    Peek :: PE a -> PE a



instance Functor PE where
    fmap = PMap

instance Applicative PE where
--should another constructor be added, rather?
--perhaps Then and ThenCat combined and parameterized by
--the function (++), (,) ... but, 'text', etc, does this too
    mf <*> ma = PMap (\(f,a) -> f a) (Then mf ma)
    pure = Unit
instance Alternative PE where
    (<|>) = Slash
    empty = Failure

instance Monoid (PE a) where
    mappend = Slash
    mempty = Failure


-- | return a value, always succeeds
unit :: a ->  P s a
unit a = P $ Unit a

-- | match a specified character
char :: Char -> P s Char
char c = P $ Char (IntSet.singleton (ord c))

-- | match some text
text :: String -> P s String
text (x:xs) = fmap ( \ (c,cs) -> c:cs) $ char x <> text xs
text [] = unit []

-- | immediately consume and return the rest of the input
-- equivalent to (many anyChar), but more efficient.
rest :: P s String
rest = P Rest

-- | match any character, fails on EOF
anyChar :: P s Char
anyChar = P Any

infixl 1 //, //>
infix  2 ##, ##>
infixl 3 <>, <++>
infixl 4 ->>, <<-

-- | match first argument, then match the second, returning both in a tuple
(<>) :: P s a -> P s b -> P s (a,b)
P x <> P y = P $ x `Then` y

-- | match a pair of lists and concatenate them
(<++>) :: P s [a] -> P s [a] -> P s [a]
P x <++> P y = P $ x `ThenCat` y

-- | match first argument, then match the second, returning only the value on the left
--
-- > x <<- y = x <> y ## fst
--
(<<-) :: P s a -> P s b -> P s a
x <<- y = x <> y ## fst

-- | match first argument, then match the second, returning only the value on the right
--
-- > x ->> y = x <> y ## snd
(->>) :: P s a -> P s b -> P s b
x ->> y = x <> y ## snd

-- | ordered choice, try left argument, if it fails try the right one
-- this does not introduce any backtracking or penalty.
(//) :: P s a -> P s a -> P s a
P x // P y = P $ x `Slash` y

-- | ordered choice, try left argument, if it fails then return right argument
(//>) :: P s a -> a -> P s a
x //> y = x // unit y

-- | map a parser through a function. a fancy version of 'fmap'
(##) :: P s a -> (a -> b) -> P s b
x ## y = fmap y x

-- | Parse left argument and return the right argument
(##>) :: P s a -> b -> P s b
x ##> y = discard x ->> unit y


-- | succeeds when the argument does not.
doesNotMatch :: P s a -> P s ()
doesNotMatch (P x) = P $ Not x

-- | succeeds when the argument does, but consumes no input
-- equivalant to \p -> discard (peek p)
matches :: P s a -> P s ()
matches =  peek . discard

-- | parse something and return it,  but do not advance the input stream.
peek :: P s a -> P s a
peek (P p) = P $ Peek p

-- | succeed only if thing parsed passes a predicate
onlyIf :: P s a -> (a -> Bool) -> P s a
onlyIf (P x) y = P $ When x y

-- | parse many of something. behaves like * in regexes.
-- this eats as much as it possibly can, if you want a minimal much rule, then use 'manyUntil' which stops when a
--

many :: P s a -> P s [a]
many (P p) = P $ Star p

-- | parse many of something via the minimal munch rule. behaves like *? in
-- perl regexes.  the final item is not consumed.

manyUntil :: P s b -> P s a -> PM s (P s [a])
manyUntil final p = mdo
    rule <- newRule $ matches final ##> []
                    // p <> rule ## uncurry (:)
    return rule

-- | first matching parse wins, a simple iteration of (\/\/)
choice :: [P s a] -> P s a
choice = mconcat

-- | get current position in file as number of characters since the beginning.
getPos :: P s Int
getPos = P GetPos

-- | equivalent to
--
-- > between open close thing = open ->> thing <<- close
between :: P s a -> P s b -> P s c -> P s c
between open close thing = open ->> thing <<- close

-- | parse something if you can, else return first value
--
-- > option a p = p // unit a
option :: a -> P s a -> P s a
option a p = p // unit a

-- | parse something if you can, discarding it.
--
-- > option a p = discard p // unit ()
optional :: P s a -> P s ()
optional p = discard p // unit ()

-- | throw away the result of something
--
-- > discard p = p ->> unit ()
discard :: P s a -> P s ()
discard p = p ->> unit ()

-- | am at the end of string
eof :: P s ()
eof = doesNotMatch anyChar

-- | am at the beginning of the string
bof :: P s ()
bof = discard (getPos `onlyIf` (== 0))


-- | match one or more of something via maximal munch rule
many1 :: P s a -> P s [a]
many1 x  = (\ (c,cs) -> c:cs)  `fmap` (x <> many x)

-- | match one of the set of characters
oneOf :: [Char] -> P s Char
oneOf [] = parseFailure
oneOf xs = P $ Char (IntSet.fromList $ map ord xs) -- foldl (//) parseFailure (map char xs)

-- | match any character other than the ones in the list
noneOf :: [Char] -> P s Char
noneOf [] = anyChar
noneOf xs = doesNotMatch (oneOf xs) ->> anyChar  -- foldl (//) parseFailure (map char xs)

-- | fails, is identity of (\/\/) and unit of (\<\>)
parseFailure :: P s a
parseFailure = P Failure




-- just used to coerce values to so they can be stashed away in the array.
data Unknown

type DerivMapTo a = Array Token a

type NM a = State (Token,Map.Map Token Token,[(Token,PE Unknown)]) a

normalizePElem :: PE a -> (PE a, DerivMapTo (PE Unknown))
normalizePElem pe = (rootNormPE, normPEs)
  where
    (rootNormPE, state) = runState (normalizePElemNM pe) (0,mempty,mempty)
    normPEs = array (0, nTokens - 1) assocNormPEs
                where (nTokens, _, assocNormPEs) = state

normalizePElemNM :: PE a -> NM (PE a)
normalizePElemNM pe = f pe where
    f :: forall a . PE a -> NM (PE a)
    f (Then x y) = do
        x <- f x
        y <- f y
        case (x,y) of
            (Failure,_) -> return Failure
            (_,Failure) -> return Failure
            (Unit a,Unit b) -> return (Unit (a,b))
            (x,y) -> return (Then x y)
    f (ThenCat x y) = do
        x <- f x
        y <- f y
        case (x,y) of
            (Failure,_) -> return Failure
            (_,Failure) -> return Failure
            (Unit a,Unit b) -> return (Unit (a ++ b))
            (x,y) -> return (ThenCat x y)
    f (Slash x y) = do
        x <- f x
        y <- f y
        return $ slash x y

    f (Char x) | IntSet.null x = return Failure
    f c@Char {} = return c
    f p@Failure = return p
    f p@Unit {} = return p
    f p@Any = return p
    f Rest = return Rest
    f (When p fn) = f p >>= \p' -> return (When p' fn)
    f (PMap fn x) = liftM (PMap fn) (f x)
    f (Star p) = f p >>= \x -> case x of
        Failure -> return $ Unit []
--        Unit x -> return $ repeat x
        x -> return (Star x)
    f (Not p) = do
        x <- f p
        case x of
            Rest -> return Failure
            Unit {} -> return Failure
            Failure -> return (Unit ())
            x -> return (Not x)
    f (Peek p) = f p >>= \x -> case x of
        -- No need to backtrack-Peek if we're not consuming anything anyway
        x | mayConsumeInput x == False -> return x
        x -> return (Peek x)
    f (Named n p) = do
        (i,m,cm) <- get
        case Map.lookup n m of
            Just v -> return (Named v (error "no need"))
            Nothing -> do
                put (i + 1,Map.insert n i m,cm)
                p' <- f p
                (ni,m,cm) <- get
                put (ni,m,(i,unsafeCoerce p' :: PE Unknown):cm)
                return (Named i (error "no need"))
    slash :: forall a . PE a -> PE a -> PE a
    slash a Failure  = a
    slash Failure b  = b
    slash (Unit a) _ = (Unit a)
    slash (Rest) _   = Rest
    slash (Char x) (Char y) = (Char (x `mappend` y))
    slash Any Char {} = Any
    slash Char {} Any = Any
    slash x y = Slash x y
    -- It's okay, just suboptimal, to return True when input can't be consumed;
    -- it's incorrect to return False when it might in fact consume input.
    mayConsumeInput :: PE a -> Bool
    mayConsumeInput Failure = False
    mayConsumeInput Unit {} = False
    mayConsumeInput (Then x y) = mayConsumeInput x || mayConsumeInput y
    mayConsumeInput (ThenCat x y) = mayConsumeInput x || mayConsumeInput y
    mayConsumeInput (Slash x y) = mayConsumeInput x || mayConsumeInput y
    mayConsumeInput Not {} = False
    mayConsumeInput _ = True

-- these fields must not be strict!
-- although, derivIndex is explicitly seq'd in one place before
-- being put into a Derivs, which is fine (in fact, important,
-- so that an unevaluated chain of thunks from the past doesn't
-- build up when the character index isn't needed for a while)
data Derivs = Derivs {
    derivChar :: (Results Char),
    derivIndex :: Int,
    derivArray :: DerivMapTo (Results Unknown),
    derivRest :: String
    }

data Results a = Parsed a Derivs | NoParse

--this instance really should be derived
--(once deriving Functor is available) :
instance Functor Results where
    fmap f (Parsed a arr) = Parsed (f a) arr
    fmap _ NoParse = NoParse



-- | run a PEG grammar. takes the rank-2 argument in order to ensure a rule
-- created in one PM session isn't returned and used in another PEG parser.
--
-- there is no need for special error handling, as it can be trivially implemented via
--
-- >  -- parse complete file, returning 'Nothing' if parse fails
-- >  fmap Just (myParser <<- eof) // unit Nothing
--
-- there is also no need for the parser to return its unused input, as that can be retrieved via 'rest'
--
-- > -- now this returns (a,String) where String is the unconsumed input.
-- > myParser <> rest
--
--

runPeg :: (forall s . PM s (P s a)) -> String -> a
runPeg peg =
   --there is a nontrivial amount of work that only depends
   --on peg, so let's suggest that to be shared by using an
   --explicit lambda here that the where clause is not "inside"
    (\input -> pout input)
  where
    pout input = case rootParser (f 0 input) of
        Parsed a _ -> a
        NoParse -> error "runPeg: no parse"
    emptyDAt n = emptyD { derivIndex = n }
      where emptyD = f 0 [] --is the sharing here (particularly the array)
              -- worth much? (does it necessarily even exist if we stuff
              -- it into a where clause like this?)
    --Optimize the parser once initially
    --The two separate uses of state:
    -- -first (evalState) we number all parsers (newRules) to make
    --   loops detectable. The numbering scheme is entirely arbitrary here;
    --   it doesn't really matter what number to start with in the state.
    -- -then (runState) we optimize the parsers to be more similar to
    --   each other where possible(?), in the process renumbering the
    --   parsers such that unused ones are not included and the order
    --   is somewhat arbitrary.  The new set of numbers (called "Token"s)
    --   start counting from zero, to be compactly used in an
    --   array of length equal to the number of referenced (potentially-used)
    --   named parsers.  This state's Map is just to look up the meaning
    --   of the old token-numbering in terms of the new numbers.
    --
    --rootPElemBeforeNormalization actually contains all parsers it references,
    --recursively, just labelled by PM so the infinite recursion can be
    --detected and stopped.
    rootPElemBeforeNormalization = fromP $ evalState (case peg of PM x -> x) 1
    --rootPElemAfterNormalization need not be among the array if it is just
    --the parser used to get started at the beginning of input, such as:
    --       mdo p <- newRule $ ...; return (p <> rest)
    (rootPElemAfterNormalization, arrayNormalizedPElems)
                      = normalizePElem rootPElemBeforeNormalization
    --arrayParsers, rootParser are out here for increased sharing of g's work
    arrayParsers = fmap g arrayNormalizedPElems
    rootParser = g rootPElemAfterNormalization
    f n s = n' `seq` d where
        --At each position in the file, we memoize (lazily) the results of all
        --our finite number of parsers.  Since lookahead is similarly
        --memoized... When(onlyIf) (some asymptotically complex function)
        --risks a more difficult than O(n) parse however.
        d = Derivs chr n (fmap ($ d) arrayParsers) s
        --chr is the secret recursion over the input characters that
        --grabs all of their positions and generates the lazy shared
        --sequence of arrays.
        chr = case s of (x:xs) -> Parsed x (f n' xs) ; [] -> NoParse
        n' = n + 1
    --the lets are explicitly floated outside the deriv-lambdas so that
    --their results will be shared given the partial application in defs
    --(essentially this avoids repeating the process of turning the PE tree
    --into functions, nothing huge)
    g :: PE a -> Derivs -> Results a
    g (Named n _) = \ (Derivs _ _ d _) -> unsafeCoerce (d ! n)
    g Any = \ (Derivs p _ _ _) -> p
    g (Char cs) = \ (Derivs p _ _ _) -> case p of
        Parsed c d | ord c `IntSet.member` cs -> Parsed c d
        _ -> NoParse
    g GetPos = \d -> Parsed (derivIndex d) d
    g Failure = \_ -> NoParse
    g (Not p) = let m = g p in \d -> case m d of
        Parsed {} -> NoParse
        NoParse {} -> Parsed () d
    g (PMap fn p) = let p' = g p in \ d -> fmap fn (p' d)
    g (Slash x y) = let x' = g x; y' = g y in \d -> case x' d of
        p@Parsed {} -> p
        NoParse -> y' d
    g (Then x y) = let x' = g x; y' = g y in \d -> case x' d of
        NoParse -> NoParse
        Parsed a d' -> case y' d' of
            Parsed b d'' -> Parsed (a,b) d''
            NoParse -> NoParse
    g (ThenCat x y) = let x' = g x; y' = g y in \d -> case x' d of
        NoParse -> NoParse
        Parsed a d' -> case y' d' of
            Parsed b d'' -> Parsed (a ++ b) d''
            NoParse -> NoParse
    g Rest = \d -> Parsed (derivRest d) (emptyDAt (derivIndex d + length (derivRest d)))
    g (Unit x) = \d -> Parsed x d
    g (Peek p) = let p' = g p in \d -> case p' d of
        Parsed r _ -> Parsed r d
        NoParse -> NoParse
    g (When x fn) = let x' = g x in \d -> case x' d of
        NoParse -> NoParse
        Parsed x d -> if fn x then Parsed x d else NoParse
    g (Star p) = let p' = g p in \d -> let
        r d = case p' d of
            Parsed x d' -> let (a,b) = r d' in (x:a,b)
            NoParse -> ([],d)
        (fv,fd) = r d
        in Parsed fv fd




-- | create a new rule, which may be used recursively and caches its results.
--
-- This is intended to be use in an 'mdo' block. such as the following.
--
-- > additive = mdo
-- >     additive <- newRule $ multitive <> char '+' ->> additive ## uncurry (+) // multitive
-- >     multitive <- newRule $ primary <> char '*' ->> multitive ## uncurry (*) // primary
-- >     primary <- newRule $ char '(' ->> additive <<- char ')' // decimal
-- >     decimal <- newRule $ many1 (oneOf ['0' .. '9']) ## read
-- >     return additive
--
-- all recursive calls must be bound via a rule. Left recursion should be avoided.
--

newRule :: P s a -> PM s (P s a)
newRule pe@(P Any {}) = return pe
newRule pe@(P Char {}) = return pe
newRule pe@(P x) = f x where
    f Named {} = return pe
    f Unit {} = return pe
    --f Any {} = return pe
    --f Char {} = return pe
    f Failure {} = return pe
    f pe = PM $ do
        x <- get
        put $! (x + 1)
        return (P $ Named x pe)


data Regex =
    RegexChars Bool IntSet.IntSet
    | RegexAny
    | RegexMany {
        regexWhat :: Regex,
        regexMin  :: Int,
        regexMax  :: Maybe Int,
        regexMunch:: Bool
        }
    | RegexCat [Regex]
    deriving(Show,Eq,Ord)

normalizeRegex :: Regex -> Regex
normalizeRegex r = f r where
    f RegexAny = RegexAny
    f (RegexCat xs) = regexCat $ g (map f xs)
    f rm@RegexMany { regexWhat = r }
        | RegexCat [] <- r' = RegexCat []
        | otherwise = regexCat (replicate (regexMin rm) r' ++ [rm { regexWhat = r', regexMin = 0, regexMax = fmap (subtract $ regexMin rm) (regexMax rm) }])
       where r' = f r
    f r@RegexChars {} = r
    g (RegexCat x:xs) = x ++ g xs
    g (x:xs) = x:g xs
    g [] = []

    regexCat [x] = x
    regexCat xs = RegexCat xs

regexToParser :: Regex -> P s String
regexToParser r = f r where
    f RegexAny = anyChar ## (:[])
    f (RegexChars True m)  = oneOf  (map chr $ IntSet.toList m) ## (:[])
    f (RegexChars False m) = noneOf (map chr $ IntSet.toList m) ## (:[])
    f (RegexCat []) = unit ""
    f (RegexCat (x:xs)) = f x <++> f (RegexCat xs)
    f RegexMany { regexWhat = r, regexMin = 0, regexMax = Nothing } = many (f r) ## concat
    f rm@RegexMany { regexWhat = r, regexMin = n, regexMax = Nothing } = f r <++> f rm { regexMin = n - 1 }
    f RegexMany { regexWhat = r, regexMin = 0, regexMax = Just 1 } = f r // unit ""



parseRegex :: forall s . PM s (P s (Maybe Regex))
parseRegex = mdo
    regex <- newRule $ primary <<- char '*' <> isMatch (char '?')   ## (\ (r,m) -> RegexMany { regexWhat = r, regexMin = 0, regexMax = Nothing, regexMunch = m })
             // primary <<- char '+' <> isMatch (char '?')         ## (\ (r,m) -> RegexMany { regexWhat = r, regexMin = 1, regexMax = Nothing, regexMunch = m })
             // primary <<- char '?' <> isMatch (char '?')         ## (\ (r,m) -> RegexMany { regexWhat = r, regexMin = 0, regexMax = Just 1, regexMunch = m })
             // primary
    primary <- newRule $ char '(' ->> fregex <<- char ')'
            // char '.' ##> RegexAny
            // text "[^" ->> char_class <<- char ']' ## RegexChars False . IntSet.fromList . map ord
            // char '['  ->> char_class <<- char ']' ## RegexChars True  . IntSet.fromList . map ord
            // rchar ## RegexChars True . IntSet.singleton . ord
    rchar <-   newRule $ text "\\n" ##> '\n'
            // text "\\t" ##> '\t'
            // text "\\f" ##> '\f'
            // text "\\a" ##> '\a'
            // text "\\e" ##> '\033'
            // text "\\r" ##> '\r'
            // text "\\0" ##> '\0'
            // char '\\' ->> anyChar
            // noneOf ".[*+()\\"
    char_class1 <- newRule $
            anyChar <<- char '-' <> anyChar ## uncurry enumFromTo
            // anyChar ## (:[])
    char_class <- fmap (fmap concat) $ manyUntil (char ']') char_class1
    fregex <- newRule $  many regex ## RegexCat
    return $ fmap (Just . normalizeRegex) (fregex <<- eof) // unit Nothing


-- | always succeeds, returning true if it consumed something
isMatch :: P s a -> P s Bool
isMatch p = p ->> unit True // unit False

parse_regex :: String -> Maybe Regex
parse_regex = runPeg parseRegex




-- | create a new regular expression matching parser. it returns something in a
-- possibly failing monad to indicate an error in the regular expression iself.

newRegex :: Monad m => String -> m (PM s (P s String))
newRegex s = case parse_regex s of
    Just r -> return (return $ regexToParser r)
    Nothing -> err
   where err = fail $ "invalid regular expression: " ++ show s


-- | show a representation of the parsed regex, mainly for debugging
showRegex :: String -> IO ()
showRegex s = do
    putStrLn $ "Parsing: " ++ show s
    print (parse_regex s)

-- | make a new regex but abort on an error in the regex string itself
regex :: String -> PM s (P s String)
regex s = runIdentity (newRegex s)




