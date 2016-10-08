{-# OPTIONS --no-eta-equality #-}

module AncientLanguage.PrimarySource where

open import Agda.Builtin.Nat
open import Agda.Builtin.List public
open import Agda.Builtin.String

append : {A : Set} → (xs ys : List A) → List A
append [] ys = ys
append (x ∷ xs) ys = x ∷ append xs ys

join : {A : Set} → List (List A) → List A
join [] = []
join (x ∷ xs) = append x (join xs)

record Verse : Set where
  constructor verse
  field
    getChapter : Nat
    getVerse : Nat

data Milestone : Set where
  verse : Verse → Milestone
  paragraph : Milestone

record Word : Set where
  constructor word
  field
    getPrefix : String
    getText : String
    getSuffix : String

data Content : Set where
  milestone : Milestone → Content
  word : Word → Content

pattern v cn vn = milestone (verse (verse cn vn))
pattern w t = word (word "" t "")
pattern ws t s = word (word "" t s)
pattern wp p t s = word (word p t s)
pattern p = milestone paragraph

record Source : Set where
  constructor source
  field
    getId : String
    getTitle : String
    getLicense : List String
    getContents : List Content

data Language : Set where
  Greek Hebrew Latin : Language

record Group : Set where
  constructor group
  field
    getId : String
    getLanguage : Language
    getTitle : String
    getDescription : List String
    getSources : List Source
