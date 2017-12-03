{-# LANGUAGE TemplateHaskell #-}
module Moba.Internal.Hero where

import           Data.Aeson
import           Data.Aeson.TH
import           Moba.Internal.Types

-- | 英雄数据
data HeroState = HeroState
               { hero_id                 :: HeroId
               , hero_name               :: HeroType
               , hero_camp               :: Camp
               , hero_moveSpeed          :: Int
               , hero_attackDamageMin    :: Int
               , hero_attackDamageMax    :: Int
               , hero_attackRange        :: Int
               , hero_attackAnimation    :: Int
               , hero_attackGap          :: Int
               , hero_attackMissileSpeed :: Int
               , hero_fireName           :: String
               , hero_fireDamage         :: Int
               , hero_fireDuration       :: Int
               , hero_fireRange          :: Int
               , hero_fireAnimation      :: Int
               , hero_fireGap            :: Int
               , hero_fireMissileSpeed   :: Int
               -- 位置
               , hero_position           :: Position
               -- 朝向
               , hero_orientation        :: Orientation
               -- 状态
               , hero_status             :: HeroStatus
               , hero_attackCD           :: Int
               , hero_fireCD             :: Int
               , hero_dazingRemaining    :: Int
               , hero_rebornRemaining    :: Int
               , hero_healthPoint        :: Float
               , hero_initHealthPoint    :: Float
               }
deriveJSON defaultOptions { fieldLabelModifier = \("hero":_:xs) -> xs } ''HeroState
