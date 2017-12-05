{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE RecordWildCards       #-}
module Moba.Client
  ( -- * Fight!
    fight
  ) where

import           Data.Aeson
import           Data.ByteString.Lazy (ByteString)
import           Data.List            (find, sortBy)
import           Data.Maybe
import           Moba.Internal

-- | 加权指令
type Q = (Float, ToServer)
type Nearest = Bool

fight :: FromServer -> Maybe [ToServer]
fight (Update c g@(GameState{..})) = do
  let sortedTowers = sortBy sort_tower towers
      enemy_towers = filter (\(TowerState{..}) -> tower_camp /= c) sortedTowers
      aura_towers = filter (\(TowerState{..}) -> tower_aura /= NoAura) towers
      our_towers   = filter (\(TowerState{..}) -> tower_camp == c) sortedTowers
      enemy_heros  = filter (\(HeroState{..})  -> hero_camp /= c) heros
      our_heros    = filter (\(HeroState{..})  -> hero_camp == c) heros
      enemy_alive_heros = filter (\(HeroState{..})  -> hero_camp /= c && hero_status /= Dead) heros
      our_alive_heros   = filter (\(HeroState{..})  -> hero_camp == c && hero_status /= Dead) heros

  -- 判定条件:
  --  good: 比分是否落后
  --  t: 时间是否不多了
  let good = checkScores c camps
      n    = time >= 165 * 1000

  -- 抢塔
  let ops_tower = catMaybes $ map (\self -> reactTower n self enemy_alive_heros enemy_towers) our_alive_heros
  -- 杀敌
  let ops_hero  = catMaybes $ map (\self -> self `reactHero` enemy_alive_heros) our_alive_heros
  -- 释放技能
  let ops_skill  = catMaybes $ map (\self -> self `reactSkill` enemy_alive_heros) our_alive_heros

  -- 专注抢塔
  let forcus_on_towers = map (\(h, t) -> attackTower h t enemy_towers) $ zip our_heros sortedTowers
  -- 最后阶段
  let last_towers = map (\(h, t) -> attackTower h t enemy_towers) $ zip our_alive_heros enemy_towers

  -- 综合命令
  let ops = sortBy rank $
        if time <= 165 * 1000
          then
            if good
              then concat [ops_tower, ops_hero]
              else
                if length forcus_on_towers == 0
                  then ops_hero
                  else concat [forcus_on_towers, ops_skill]
          else
            if length forcus_on_towers == 0
              then ops_hero
              else concat [forcus_on_towers, ops_skill]

  -- 指令集
  Just $ map snd ops

-- | ignore
fight _ = Nothing

-- | 对英雄采取行动
reactHero :: HeroState -> [HeroState] -> Maybe Q
hero `reactHero` enemy =
  case takeNearestHero hero enemy of
    Nothing -> Nothing
    Just en -> Just $ hero <|> en

-- | 对英雄采取行动，只使用技能
reactSkill :: HeroState -> [HeroState] -> Maybe Q
hero `reactSkill` enemy =
  case takeNearestHero hero enemy of
    Nothing -> Nothing
    Just en -> hero `skill` en

-- | 对塔采取行动
reactTower :: Nearest -> HeroState -> [HeroState] -> [TowerState] -> Maybe Q
reactTower n hero enemy towers =
  let en = takeNearestHero hero enemy
      tower = takeNearestTower n hero towers
  in
  f hero en tower
  where
    f :: HeroState -> Maybe HeroState -> Maybe TowerState -> Maybe Q
    f hero Nothing Nothing = Nothing
    f hero (Just en) Nothing = Just $ hero <|> en
    f hero Nothing (Just tower) = Just $ attackTower hero tower []
    f hero (Just en) (Just tower) =
      if hero <-> en <= hero <.> tower
        then Just $ hero <|> en
        else Just $ attackTower hero tower []

-- | 策略函数
--   1. 优先释放技能
--   2. 补刀空血英雄
--   3. 其次攻击建筑
--   4. 最后攻击英雄
rank :: Q -> Q -> Ordering
rank (a, _) (b, _) = if a <= b then LT else GT

-- | 是否领先
checkScores :: Camp -> Camps -> Bool
checkScores c Camps{..} =
  if c == Red
    then red >= blue || (towerCount red == 3)
    else blue >= red || (towerCount red == 3)

-- | 应对英雄
(<|>) :: HeroState  -- 我方英雄
      -> HeroState  -- 敌方英雄
      -> Q
hero <|> enemy    =
  let h_id     = hero_id hero
      enemy_id    = hero_id enemy
      h_name   = hero_name hero
      enemy_name  = hero_name enemy
      firecd      = hero_fireCD hero
      distance    = hero <-> enemy
      not_daz     = hero_status enemy /= Dazing
      dazingW     = if not_daz then 0 else 100
  in
    case h_name of
      Shooter ->
        -- 和敌方战士保持 150 距离
        if (distance <= 150 && enemy_name == Warrior)
          then  (600, Move h_id (goBack hero enemy))
          else if firecd == 0 && distance < 470
                 then (1000, Fire h_id enemy_id)
                 else (125 - 100 * (checkHealth enemy) + dazingW, Attack h_id HeroTarget enemy_id)
      Warrior -> if firecd == 0 && distance < 320 && not_daz
                   then (1000, Fire h_id enemy_id)
                   else (125 - 100 * (checkHealth enemy) + dazingW, Attack h_id HeroTarget enemy_id)

-- | 释放技能
skill :: HeroState  -- 我方英雄
      -> HeroState  -- 敌方英雄
      -> Maybe Q
hero `skill` enemy    =
  let h_id     = hero_id hero
      enemy_id    = hero_id enemy
      h_name   = hero_name hero
      firecd      = hero_fireCD hero
      distance    = hero <-> enemy
      not_daz     = hero_status enemy /= Dazing
  in
    case h_name of
      Shooter ->
        if firecd == 0 && distance < 470
            then Just (1000, Fire h_id enemy_id)
            else Nothing
      Warrior ->
        if firecd == 0 && distance < 320 && not_daz
          then Just (1000, Fire h_id enemy_id)
          else Nothing

-- | 塔排序
sort_tower :: TowerState -> TowerState -> Ordering
sort_tower t1 t2 = if (tower_aura t1 == NoAura && tower_aura t2 /= NoAura) then GT else LT

-- | 攻击建筑
attackTower :: HeroState    -- 行动的英雄
            -> TowerState   -- 攻击的塔
            -> [TowerState] -- 倘若攻击的是己方的塔，那么从敌方塔中挑最近的攻击; 如果保证对象是敌方塔，可以设置为空数组
            -> Q
attackTower HeroState{..} TowerState{..} [] = (100, Attack hero_id TowerTarget tower_id)
attackTower hero@(HeroState{..}) tower@(TowerState{..}) en =
  if hero_camp /= tower_camp then (100, Attack hero_id TowerTarget tower_id)
                      else case takeNearestTower False hero en of
                             Nothing -> (0, Attack hero_id TowerTarget tower_id)
                             Just t  -> attackTower hero t []

-- | take nearest tower
takeNearestTower :: Nearest -> HeroState -> [TowerState] -> Maybe TowerState
takeNearestTower _ _ [] = Nothing
takeNearestTower True hero ts =
  let (t:_) = sortBy (\x y -> if hero <.> x <= hero <.> y then LT else GT) ts
  in Just t
takeNearestTower False hero ts =
  let (t:_) = sortBy (\x y -> if hero <.> x <= hero <.> y then LT else GT) ts
  in Just t
  where
    -- 对属性塔加权重
    (<..>) :: HeroState -> TowerState -> Float
    hero <..> tower =
      case (tower_aura tower) of
        NoAura  -> hero <.> tower + 500
        Magic   -> hero <.> tower
        Speed   -> hero <.> tower + 100
        Physics -> hero <.> tower + 100

-- | take nearest hero
takeNearestHero :: HeroState -> [HeroState] -> Maybe HeroState
takeNearestHero _ [] = Nothing
takeNearestHero hero hs =
  let (h:_) = sortBy (\x y -> if hero <-> x <= hero <-> y then LT else GT) hs
  in Just h

-- | 检查英雄健康度
checkHealth :: HeroState -> Float
checkHealth HeroState{..} = hero_healthPoint / hero_initHealthPoint

-- | 获取周围英雄数
aroundHero :: HeroState -> [HeroState] -> Int
aroundHero h = length . filter (\x -> h <-> x <= 150)

-- | 获取退避点
goBack :: HeroState -> HeroState -> Position
goBack hero enemy =
  let pos_hero_x = x $ hero_position hero
      pos_hero_y = y $ hero_position hero
      pos_enem_x = x $ hero_position enemy
      pos_enem_y = y $ hero_position enemy
      pos_x = if pos_hero_x - pos_enem_x == 0
                then pos_hero_x
                else pos_hero_x + (220 / sqrt 2) * (pos_hero_x - pos_enem_x) / abs (pos_hero_x - pos_enem_x)
      pos_y = if pos_hero_y - pos_enem_y == 0
                then pos_hero_y
                else pos_hero_y + (220 / sqrt 2) * (pos_hero_y - pos_enem_y) / abs (pos_hero_y - pos_enem_y)
  in Position pos_x pos_y
