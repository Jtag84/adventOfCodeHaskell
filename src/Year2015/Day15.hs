module Year2015.Day15 (runDay) where

import Algorithm.Search
import Control.Applicative.Combinators
import Control.Monad (forM_, join, zipWithM_)
import Control.Monad.ST (runST)
import Control.Monad.State (State, evalState, runState)
import Data.Attoparsec.Text as P (Parser, char, choice, endOfInput, endOfLine, many', many1', parseOnly, scientific, takeText, letter, scientific)
import Data.Foldable
import Data.Function (on)
import Data.Functor
import Data.List hiding (groupBy)
import Data.List.GroupBy (groupBy)
import Data.Map.Strict qualified as Map
import Data.Matrix qualified as M
import Data.Maybe
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Scientific (Scientific, toBoundedInteger)
import Data.Sequence (mapWithIndex)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Vector qualified as Vec
import Options.Applicative (Alternative (empty), value, (<|>))
import Program.RunDay qualified as R (Day, runDay)
import Util.Cache (Cache, caching)
import Util.Coordinate
import Util.LinkedList
import Util.Range
import Util.Util qualified as U

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ TYPES ------------

type Capacity = Int
type Durability = Int
type Flavor = Int
type Texture = Int
type Calories = Int

type Ingredient = (Capacity, Durability, Flavor, Texture, Calories)

type Frosting = Ingredient
type Candy = Ingredient
type Butterscotch = Ingredient
type Sugar = Ingredient

type FrostingAmount = Int
type CandyAmount = Int
type ButterscotchAmount = Int
type SugarAmount = Int

type IngredientsAmount = (FrostingAmount, CandyAmount, ButterscotchAmount, SugarAmount)

frosting :: Ingredient
frosting = (4,-2,0,0,5)

candy :: Ingredient
candy = (0,5,-1,0,8)

butterscotch :: Ingredient
butterscotch = (-1,0,5,0,6)

sugar :: Ingredient
sugar = (0,0,-2,2,1)

ingredients :: (Frosting, Candy, Butterscotch, Sugar)
ingredients = (frosting, candy, butterscotch, sugar)

------------ PARSER ------------

inputParser :: Parser Text
inputParser = takeText

------------ PART A ------------

multiplyIngredient :: Int -> Ingredient -> Ingredient
multiplyIngredient k (capacity, durability, flavor, texture, calories) = (k * capacity, k * durability, k * flavor, k * texture, k * calories)

addIngredients :: Ingredient -> Ingredient -> Ingredient
addIngredients (leftCapacity, leftDurability, leftFlavor, leftTexture, leftCalories) (rightCapacity, rightDurability, rightFlavor, rightTexture, rightCalories) = (leftCapacity + rightCapacity, leftDurability + rightDurability, leftFlavor + rightFlavor, leftTexture + rightTexture, leftCalories + rightCalories)

multiplyByAmounts :: IngredientsAmount -> (Frosting, Candy, Butterscotch, Sugar) -> (Frosting, Candy, Butterscotch, Sugar) 
multiplyByAmounts (frostingAmount, candyAmount, butterscotchAmount, sugarAmount) (frosting, candy, butterscotch, sugar) = (frostingAmount `multiplyIngredient` frosting, candyAmount `multiplyIngredient` candy, butterscotchAmount `multiplyIngredient` butterscotch, sugarAmount `multiplyIngredient` sugar)

sumIngredients :: (Frosting, Candy, Butterscotch, Sugar) -> Ingredient
sumIngredients (frosting, candy, butterscotch, sugar) = frosting `addIngredients` candy `addIngredients` butterscotch `addIngredients` sugar

areAllPropertiesButCaloriesPositive :: (Capacity, Durability, Flavor, Texture, Calories) -> Bool
areAllPropertiesButCaloriesPositive (capacity, durability, flavor, texture, _) = capacity > 0 && durability > 0 && flavor > 0 && texture > 0 

multiplyAllPropertiesButCalories ::  (Capacity, Durability, Flavor, Texture, Calories) -> Int
multiplyAllPropertiesButCalories (capacity, durability, flavor, texture, _) = capacity * durability * flavor * texture

-- Part A:
-- 18965440
-- (0.059344s)
partA _ = maximum 
            . map multiplyAllPropertiesButCalories 
            . filter areAllPropertiesButCaloriesPositive 
            $ map (sumIngredients . (`multiplyByAmounts` ingredients)) getAllIngredientPermutations

getAllIngredientPermutations :: [IngredientsAmount]
getAllIngredientPermutations = [(a,b,c,d) | a <- [0..100], b <- take (101 - a) [0..100], c <- take (101 - a - b) [0..100], d <- [[100,99 ..0] !! (a+b+c)]]

------------ PART B ------------

hasExactly500Calories :: (Capacity, Durability, Flavor, Texture, Calories) -> Bool
hasExactly500Calories (_, _, _, _, calories) = calories == 500

-- Part B:
-- 15862900
-- (0.001226s)
partB _ = maximum 
            . map multiplyAllPropertiesButCalories 
            . filter hasExactly500Calories
            . filter areAllPropertiesButCaloriesPositive 
            $ map (sumIngredients . (`multiplyByAmounts` ingredients)) getAllIngredientPermutations
