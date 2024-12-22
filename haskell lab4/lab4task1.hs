import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Maybe (isJust)

-- Определяем тип AttributeName
data AttributeName
  = Str        -- Сила
  | Dex        -- Ловкость
  | Wis        -- Мудрость
  | Acrobatics -- Акробатика
  | Berserk    -- Берсерк
  | SpellCraft -- Искусство заклинаний
  deriving (Show, Eq)

-- Определяем тип Hero
data Hero = Hero
  { name  :: String                 -- Имя героя
  , xp    :: Int                    -- Очки опыта
  , traits :: [(AttributeName, Int)] -- Список характеристик и их значений
  } deriving (Show, Eq)

--а Функция для поиска героя в списке по имени
findHero :: String -> [Hero] -> Maybe Hero
findHero heroName heroes =
  case filter (\h -> name h == heroName) heroes of
    (h:_) -> Just h  -- Если нашли героя, возвращаем его
    []    -> Nothing -- Если не нашли, возвращаем Nothing


hero1 :: Hero
hero1 = Hero "Thor" 100 [(Str, 30), (Dex, 10), (Wis, 21), (Berserk, 5)]

hero2 :: Hero
hero2 = Hero "Lanselot" 120 [(Str, 10), (Dex, 20), (Wis, 33), (Acrobatics, 20)]

hero3 :: Hero
hero3 = Hero "Arthur" 120 [(Str, 10), (Dex, 20), (Wis, 33), (Acrobatics, 10),(SpellCraft, 2)]

heroes :: [Hero]
heroes = [hero1, hero2,hero3]

--б Функция для добавления очков опыта герою
addXP :: Int -> Hero -> Hero
addXP amount hero = hero { xp = xp hero + amount }

--в Функция увеличения уровня характеристики или добавления её на первом уровне
incAttrLevel :: AttributeName -> Hero -> Hero
incAttrLevel attr hero =
  hero { traits = updateOrAdd (traits hero) }
  where
    updateOrAdd [] = [(attr, 1)] -- Если список пустой, добавляем новую характеристику
    updateOrAdd ((a, l) : xs)
      | a == attr = (a, l + 1) : xs -- Если нашли характеристику, увеличиваем её уровень
      | otherwise = (a, l) : updateOrAdd xs -- Иначе продолжаем поиск


--г Функция mightyTeam
mightyTeam :: Int -> [Hero] -> Maybe [Hero]
mightyTeam requiredXP heroes =
  let -- Сортируем героев по возрастанию их опыта
      sortedHeroes = sortBy (comparing xp) heroes
      -- Рекурсивно ищем минимальный набор героев
      findTeam currentXP currentTeam [] = 
        if currentXP >= requiredXP then Just currentTeam else Nothing
      findTeam currentXP currentTeam (h:hs)
        | currentXP >= requiredXP = Just currentTeam
        | otherwise = findTeam (currentXP + xp h) (h : currentTeam) hs
  in findTeam 0 [] sortedHeroes

--д

-- Проверяет, может ли герой удовлетворить конкретное требование
canSatisfy :: (AttributeName, Int) -> Hero -> Bool
canSatisfy (attr, reqLevel) hero = 
  case lookup attr (traits hero) of
    Just level -> level >= reqLevel
    Nothing    -> False

-- Проверяет, удовлетворяет ли команда всем требованиям
teamSatisfies :: [(AttributeName, Int)] -> [Hero] -> Bool
teamSatisfies [] _ = True
teamSatisfies ((attr, reqLevel):reqs) heroes =
  any (canSatisfy (attr, reqLevel)) heroes && teamSatisfies reqs heroes

-- Функция gatherTeam
gatherTeam :: [(AttributeName, Int)] -> [Hero] -> Maybe [Hero]
gatherTeam requirements heroes = 
  let subsets [] = [[]]
      subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)
      allSubsets = subsets heroes
      validTeams = filter (\team -> teamSatisfies requirements team) allSubsets
  in if null validTeams then Nothing else Just (head validTeams)






