{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <&>" #-}

-- | Visualization of constraint-based type inference.

module Main where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
-- import Control.Monad.TransMaybe

import Data.Function ( on )
import Data.Functor  ( (<&>) )
import Data.IntMap   ( IntMap )
import Data.IntMap   qualified as IntMap
import Data.Map      ( Map )
import Data.Map      qualified as Map
import Data.Maybe
import Data.Set      ( Set )
import Data.Set      qualified as Set

import Prettyprinter
import Prettyprinter.Util ( putDocW )
import Prettyprinter.Internal ( Doc(Empty) )

import System.Console.ANSI ( clearScreen, setCursorPosition )
import System.Exit   ( exitFailure)
import System.IO     ( hFlush, readFile, stdout )

import Lam.Abs       qualified as A
import Lam.Abs       ( Ident(..), Exp(..) )
import Lam.Par       ( pExp, myLexer )
import Lam.Print     ( Print, printTree )

import Options

-- | State of the type inference problem.

data St = St
  { stDerivation          :: Derivation
      -- ^ The problem tree.
  , stConstraints         :: Constraints
      -- ^ The collected unsolved constraints.
  , stSolution            :: Maybe Solution
      -- ^ A solve meta to be added to the substitution.
  , stSubstitution        :: Substitution
      -- ^ The solved constraints.
  , stMetas               :: MetaVariables
      -- ^ The collection of allocated metas
  , stMetaSupply          :: MetaSupply
      -- ^ A supply of identifiers for meta-variables.
  , stAction              :: Maybe Action
      -- ^ Last action taken on derivation.
  }

-- | Collection of meta variables indexed by their suggested name.
--   For each name suggestion, we collect the take name suffixes.

type MetaVariables = Map Ident (IntMap Suffix)
type Suffix        = Int

-- | Supply of available UIDs for metas.

type MetaSupply = [MetaID]
type MetaID     = Int

-- | A type meta variable.

data Meta = Meta
  { metaID     :: MetaID
      -- ^ UID of the meta
  , metaName   :: Ident
      -- ^ Suggestion for the name.
  , metaSuffix :: Suffix
      -- ^ Suffix for disambiguation of name.
  }

instance Eq Meta where (==) = (==) `on` metaID
instance Ord Meta where compare = compare `on` metaID

data Ty
  = TyMeta Meta
  | TyFun  Ty Ty
  deriving (Eq, Ord)

data Judgement = Judgement Exp Ty

-- | Derivations are fully-annotated lambda-terms with holes.

data Derivation
  = DLeaf Exp Ty (Maybe TypeError)
      -- ^ Unsolved ('Nothing') or error ('Just') leaf in the derivation tree.
  | DVar Ident Ty
      -- ^ Solved leaf in the derivation tree.
  | DAbs Ident Ty Exp Ty Derivation
      -- ^ Lambda abstraction with type of domain and range.
  | DApp Exp Exp Ty Derivation Derivation
      -- ^ Application.
  | DRoot Exp Ty Derivation
      -- ^ The original problem.

data Action
  = Check String
  | Simplify Equation -- ^ Equation between function types
  | Trivial Meta      -- ^ Reflexive equation.
  | Solve Meta Ty
  | Substitute Meta Ty
  | Fail

data WithAction a = WithAction Action a
type TypeError = String

data Equation = Equation Ty Ty
  deriving (Eq, Ord)

data Solution = Solution Meta Ty

newtype Constraints = Constraints { theConstraints :: [Equation] }

type Substitution = Map Meta Ty

type Context = Map Ident Ty

type M = State St

main :: IO ()
main = do

  -- Parse options.
  opts@Options{..} <- options
  let
    strategy = if optJ then strategyJ else strategyC
    batch = optBatch || isNothing optFile
    clear = unless optNoColors do
      clearScreen
      setCursorPosition 0 0


  -- Read expression from file or stdin.
  input <- case optFile of
    Nothing -> getContents
    Just f  -> readFile f

  -- Parse expression.
  e <- case pExp (myLexer input) of
    Left err -> do
      putStrLn err
      exitFailure
    Right e -> pure e

  let
    st = initSt e
    tr = evalState (trampolin strategy) st
    ss = st : map (\ (WithAction _ s) -> s) tr

  clear
  if batch then do
    showState st
    putStrLn ""
  else putStrLn "(Press ENTER to start)"

  forM_ (zip ss tr) \ (s0, WithAction a s) -> do
    unless batch do
      getLine
      clear
      showState s0
      hFlush stdout
      getLine
      pure ()
    showStateA a s


showState :: St -> IO ()
showState st = putDocW 80 $ pretty st

showStateA :: Action -> St -> IO ()
showStateA a st = putDocW 80 $ pretty $ WithAction a st -- $ stDerivation st

-- | Initial type inference problem.

initSt :: Exp -> St
initSt e = St
  { stDerivation   = DRoot e t $ DLeaf e t Nothing
  , stConstraints  = Constraints []
  , stSolution     = Nothing
  , stSubstitution = Map.empty
  , stMetas        = addMeta m Map.empty
  , stMetaSupply   = [1..]
  , stAction       = Nothing
  }
  where
    x   = Ident "?"
    mid = 0
    suf = 0
    m   = Meta{ metaID = mid, metaName = x, metaSuffix = suf }
    t   = TyMeta m

strategyC :: M (Maybe Action)
strategyC = foldl1 orElse $
  [ stepDerivation
  , stepSolution
  , stepConstraints
  ]

strategyJ :: M (Maybe Action)
strategyJ = foldl1 orElse
  [ stepSolution
  , stepConstraints
  , stepDerivation
  ]

orElse :: Monad m => m (Maybe Action) -> m (Maybe Action) -> m (Maybe Action)
orElse c1 c2 = do
  c1 >>= \case
    Nothing -> c2
    Just Fail -> return $ Nothing
    Just a  -> return $ Just a

-- | Try to make progress in the derivation.

stepDerivation :: M (Maybe Action)
stepDerivation = do
  putAction Nothing
  d <- gets stDerivation
  putDerivation =<< loop Map.empty d
  gets stAction
  where
    loop :: Context -> Derivation -> M Derivation
    loop cxt d = gets stAction >>= \case
      Just{}  -> return d
      Nothing -> case d of
        DRoot e t d        -> DRoot e t <$> loop cxt d
        d@DVar{}           -> pure d
        DAbs x t1 e t2 d   -> DAbs x t1 e t2 <$> loop (Map.insert x t1 cxt) d
        DApp e1 e2 t d1 d2 -> DApp e1 e2 t <$> loop cxt d1 <*> loop cxt d2
        DLeaf e t Just{}   -> pure d
        DLeaf e t Nothing  -> do
          case e of
            EId x -> do
              checkAction "variable"
              case Map.lookup x cxt of
                Nothing -> pure $ DLeaf e t $ Just $ "unbound variable"
                Just t1 -> DVar x t <$ subType t1 t
            EAbs _lam x _arr body -> do
              checkAction "abstraction"
              (t1, t2) <- splitFunType (Just x) t
              pure $ DAbs x t1 body t2 $ DLeaf body t2 Nothing
            EApp e1 e2 -> do
              checkAction "application"
              dom <- TyMeta <$> newMeta Nothing
              pure $ DApp e1 e2 t (DLeaf e1 (TyFun dom t) Nothing) (DLeaf e2 dom Nothing)
    checkAction = putAction . Just . Check

splitFunType :: Maybe Ident -> Ty -> M (Ty, Ty)
splitFunType x = \case
  TyFun t1 t2 -> return (t1, t2)
  t@TyMeta{}  -> do
    t1 <- TyMeta <$> newMeta x
    t2 <- TyMeta <$> newMeta Nothing
    (t1, t2) <$ equate t (TyFun t1 t2) -- subType (TyFun t1 t2) t

-- | Try to apply the last solution

stepSolution :: M (Maybe Action)
stepSolution = do
  gets stSolution >>= \case
    Nothing -> return Nothing
    Just (Solution x t)
      | occurs x t -> return $ Just Fail
      | otherwise -> Just (Substitute x t) <$
          modify \ st -> st
            { stSolution     = Nothing
            , stDerivation   = substitute x t (stDerivation st)
            , stConstraints  = substitute x t (stConstraints st)
            , stSubstitution = substitute x t (stSubstitution st)
            }

class Substitute a where
  substitute :: Meta -> Ty -> a -> a

instance Substitute Ty where
  substitute x t0 = \case
    TyMeta y -> if x == y then t0 else TyMeta y
    TyFun t1 t2 -> TyFun (substitute x t0 t1) (substitute x t0 t2)

instance Substitute Equation where
  substitute x t0 (Equation t1 t2) = Equation (substitute x t0 t1) (substitute x t0 t2)

instance Substitute a => Substitute [a] where
  substitute x t0 = fmap $ substitute x t0

instance Substitute a => Substitute (Map k a) where
  substitute x t0 = fmap $ substitute x t0

instance Substitute Constraints where
  substitute x t0 = Constraints . substitute x t0 . theConstraints

instance Substitute Derivation where
  substitute x0 t0 = \case
    DRoot e t d        -> DRoot e (substitute x0 t0 t) (substitute x0 t0 d)
    DLeaf e t err      -> DLeaf e (substitute x0 t0 t) err
    DAbs x t1 e t2 d   -> DAbs x  (substitute x0 t0 t1) e (substitute x0 t0 t2) (substitute x0 t0 d)
    DApp e1 e2 t d1 d2 -> DApp e1 e2 (substitute x0 t0 t) (substitute x0 t0 d1) (substitute x0 t0 d2)
    DVar x t           -> DVar x     (substitute x0 t0 t)


-- | Try to make progress with the constraints

stepConstraints :: M (Maybe Action)
stepConstraints = do
  gets (theConstraints . stConstraints) >>= \case
    [] -> return Nothing
    Equation t1 t2 : cs -> do
      modify \ st -> st{ stConstraints = Constraints cs }
      unify t1 t2

unify :: Ty -> Ty -> M (Maybe Action)
unify t1 t2 = case (t1, t2) of
  (TyFun dom1 rng1, TyFun dom2 rng2) -> do
    equate rng1 rng2
    equate dom1 dom2
    return $ Just $ Simplify $ Equation t1 t2
  (TyFun{}, TyMeta m) -> solve m t1
  (TyMeta m, TyFun{}) -> solve m t2
  (TyMeta m1, TyMeta m2) ->
    -- Heuristics: keep older metas
    case (compare `on` metaID) m1 m2 of
      LT -> solve m2 t1
      EQ -> return $ Just $ Trivial m1
      GT -> solve m1 t2

solve :: Meta -> Ty -> M (Maybe Action)
solve m t = do
  modify \ st -> st{ stSolution = Just $ Solution m t }
  return $ Just $ Solve m t

occurs :: Meta -> Ty -> Bool
occurs x = \case
  TyMeta y -> x == y
  TyFun t1 t2 -> occurs x t1 || occurs x t2

-- | From the current state, unfold with the given modifier until no more action is produced.
--   (Classic trampolin.)

trampolin :: M (Maybe Action) -> M [WithAction St]
trampolin step = loop
  where
    loop = step >>= \case
      Nothing -> return []
      Just a  -> do
        st <- get
        (WithAction a st :) <$> loop

-- * State manipulation

equate :: Ty -> Ty -> M ()
equate t1 t2 = modify \ st -> st{ stConstraints = addConstraint (Equation t1 t2) (stConstraints st) }

subType :: Ty -> Ty -> M ()
subType t1 t2 = modify \ st -> st{ stConstraints = addConstraint (Equation t1 t2) (stConstraints st) }

putAction :: Maybe Action -> M ()
putAction a = modify \ st -> st{ stAction = a }

putDerivation :: Derivation -> M ()
putDerivation a = modify \ st -> st{ stDerivation = a }

newMeta :: Maybe Ident -> M Meta
newMeta mx = do
  let x = fromMaybe (Ident "?") mx
  st <- get
  case stMetaSupply st of
    i:is -> do
      let
        metas = stMetas st
        -- Get next available suffix for x
        suf = case Map.lookup x metas of
          Nothing -> 0
          Just ss -> maximum ss + 1
        m = Meta i x suf
      m <$ put st{ stMetaSupply = is, stMetas = addMeta m metas }

-- * Auxiliary functions

addMeta :: Meta -> MetaVariables -> MetaVariables
addMeta (Meta i x suf) = Map.insertWith IntMap.union x (IntMap.singleton i suf)

addConstraint :: Equation -> Constraints -> Constraints
addConstraint e (Constraints cs) = Constraints $ e : cs

-- * Pretty printing

-- class Pretty where
--   pretty :: a -> Doc ann

instance Pretty St where
  pretty St{ stDerivation = d, stSolution = ms, stConstraints = cs } =
    ruleSep [ pretty d, pretty ms, pretty cs ]

ruleSep :: [Doc ann] -> Doc ann
ruleSep = vsep . punctuate (line <> pretty (replicate 12 '─')) . filter (not . isEmpty)
  where
    isEmpty = \case
      Empty -> True
      _ -> False

instance Pretty Derivation where
  pretty = \case
    DRoot e t d        -> ("¿" <+> judgement e t) $$ pretty d
    DVar x t           -> checkMark (judgement x t)
    DAbs x t1 e t2 d   -> checkMark (brackets (judgement x t1) <+> judgement e t2) $$ indent 2 (pretty d)
    DApp e1 e2 t d1 d2 -> checkMark (judgement (EApp e1 e2) t) $$ indent 2 (pretty d1 $$ pretty d2)
    DLeaf e t Nothing  -> "•" <+> judgement e t
    DLeaf e t (Just err) -> "✗" <+> judgement e t <+> brackets (pretty err)

instance Pretty Constraints where
  pretty = vsep . map pretty . theConstraints

instance Pretty Equation where
  pretty (Equation t1 t2) = pretty t1 <+> "≟" <+> pretty t2

instance Pretty Solution where
  pretty (Solution x t) = pretty x <+> "=" <+> pretty t <+>
    (if occurs x t then "✗ recursive" else "✓")

instance Pretty Action where
  pretty = \case
    Check s        -> "checking" <+> pretty s
    Simplify eq    -> "simplify" <+> pretty eq
    Solve x t      -> "occurs check" <+> pretty x <+> "=" <+> pretty t
    Substitute x t -> "substitute" <+> pretty x <+> ":=" <+> pretty t
    Trivial x      -> "discard trivial equation" <+> pretty x <+> "=" <+> pretty x

instance Pretty a => Pretty (WithAction a) where
  pretty (WithAction a d) = line <> "==>" <+> pretty a <> line <> line <> pretty d <> line

instance Pretty Ident where
  pretty = pretty . printTree

instance Pretty Exp where
  pretty = pretty . printTree

instance Pretty Ty where
  pretty = pretty . printTree . tyA

instance Pretty Meta where
  pretty = pretty . metaString

($$) :: Doc ann -> Doc ann -> Doc ann
d1 $$ d2 = d1 <> line <> d2

checkMark :: Doc ann -> Doc ann
checkMark = ("✓" <+>)

judgement :: (Pretty a, Pretty b) => a -> b -> Doc ann
judgement x t = pretty x <+> colon <+> pretty t

tyA :: Ty -> A.Ty
tyA = \case
  TyFun t1 t2 -> A.TArr (tyA t1) (A.Arrow "→") (tyA t2)
  TyMeta m    -> A.TId $ Ident $ metaString m

metaString :: Meta -> String
metaString (Meta _ (Ident x) suf) = x ++ show suf
