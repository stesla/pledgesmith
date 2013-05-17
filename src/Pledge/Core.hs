module Pledge.Core where

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

data Aspect a = Basic a
              | Level a Spec
              | Combined a [Spec]
              deriving (Ord, Eq, Show)

data Spec = Spec Level String
          deriving (Ord, Eq, Show)

data Level = Lesser | Medial | Greater
           deriving (Bounded, Enum, Ord, Eq, Show)

data Task = Alliance
          | Dreaming
          | Endeavor
          | EnsorcellmentTask
          | Fealty
          | Forbiddance
          deriving (Bounded, Enum, Eq, Ord, Show)

data Boon = Adroitness
          | Blessing
          | EnsorcellmentBoon
          | Favor
          | Glamour
          | Vassalage
          deriving (Bounded, Enum, Eq, Ord, Show)

data Sanction = Banishment
              | Curse
              | Death
              | Flaw
              | Pishogue
              | Poisoning
              | VulnerableGlamour
              | VulnerableViolence
              deriving (Bounded, Enum, Eq, Ord, Show)

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
