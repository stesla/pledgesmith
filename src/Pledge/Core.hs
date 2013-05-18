module Pledge.Core where

import Control.Monad

data Pledge = Pledge
     { pledgeType :: Type
     , duration :: Duration
     , participants :: [Participant]
     } deriving (Show, Eq)

data Participant = Participant
     { name :: String
     , tasks :: [Aspect Task]
     , boons :: [Aspect Boon]
     , sanctions :: [Aspect Sanction]
     } deriving (Show, Eq)

data Aspect a = Aspect a [Level] deriving (Ord, Eq, Show)

class IsAspect a where
  checkAspect :: Aspect a -> Either String ()

checkAspects :: IsAspect a => [Aspect a] -> Either String ()
checkAspects = mapM_ checkAspect

data Level = Lesser | Medial | Greater
           deriving (Bounded, Enum, Ord, Eq, Show)

checkLevel :: Show a => Aspect a -> Either String ()
checkLevel (Aspect a l)
  | length l == 0 = Left $ "must provide level for " ++ (show a)
  | otherwise = Right()

cannotLevel :: Show a => Aspect a -> Either String ()
cannotLevel (Aspect _ []) = Right ()
cannotLevel (Aspect a _) = Left $ "cannot provide level for " ++ (show a)

data Task = Alliance
          | Dreaming
          | Endeavor
          | EnsorcellmentTask
          | Fealty
          | Forbiddance
          deriving (Bounded, Enum, Eq, Ord, Show)

instance IsAspect Task where
    checkAspect (Aspect a@Alliance l)
        | length l == 1 = Right ()
        | otherwise = Left $ "must provide only one level for " ++ (show a)
    checkAspect a@(Aspect Endeavor _) = checkLevel a
    checkAspect a@(Aspect Forbiddance _) = checkLevel a
    checkAspect a = cannotLevel a

data Boon = Adroitness
          | Blessing
          | EnsorcellmentBoon
          | Favor
          | Glamour
          | Vassalage
          deriving (Bounded, Enum, Eq, Ord, Show)

instance IsAspect Boon where
    checkAspect a@(Aspect Blessing _) = checkLevel a
    checkAspect a@(Aspect Favor _) = checkLevel a
    checkAspect a = cannotLevel a

data Sanction = Banishment
              | Curse
              | Death
              | Flaw
              | Pishogue
              | Poisoning
              | VulnerableGlamour
              | VulnerableViolence
              | VulnerableBoth
              deriving (Bounded, Enum, Eq, Ord, Show)

instance IsAspect Sanction where
    checkAspect a@(Aspect Curse _) = checkLevel a
    checkAspect a@(Aspect Pishogue _) = checkLevel a
    checkAspect a@(Aspect Poisoning _) = checkLevel a
    checkAspect a = cannotLevel a

data Type = Vow
          | NameObscured
          | NameUnsullied
          | NameOfKeeper
          | NameOfHigherPower
          | MortalEmblem
          | CourtEmblem
          | SeemingEmblem
          | TitleEmblem
          | NemesisEmblem
          deriving (Bounded, Enum, Eq, Ord, Show)

data Duration = Day
              | Week
              | Moon
              | Season
              | YearAndDay
              | Decade
              | Lifelong
              | Generational
              deriving (Bounded, Enum, Eq, Ord, Show)
