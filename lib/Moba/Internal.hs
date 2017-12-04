{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
module Moba.Internal where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Char           (toLower)
import           Data.Monoid
import           Moba.Internal.Types

-- | Status 游戏状态
data Status = GamePending | GamePicking | GameLoading | GameStart | GameEnd deriving (Show)

-- | 阵营
data Camp = Red | Blue | White deriving (Show, Eq)
instance FromJSON Camp where
    parseJSON "red"  = pure Red
    parseJSON "blue" = pure Blue
    parseJSON ""     = pure White
    parseJSON s      = fail $ "错误的阵营: " <> (show s)
instance ToJSON Camp where
    toJSON Red   = "red"
    toJSON Blue  = "blue"
    toJSON White = ""

data Position = Position
              { x :: Float
              , y :: Float
              } deriving (Show, Eq)
deriveJSON defaultOptions ''Position

-- | 光环
data Aura = Physics | Speed | Magic | NoAura deriving (Show, Eq)
instance FromJSON Aura where
    parseJSON "physics" = pure Physics
    parseJSON "speed"   = pure Speed
    parseJSON "magic"   = pure Magic
    parseJSON ""        = pure NoAura
    parseJSON s         = fail $ "光环类型不能识别: " <> (show s)
instance ToJSON Aura where
    toJSON Physics = "physics"
    toJSON Speed   = "speed"
    toJSON Magic   = "magic"
    toJSON NoAura  = ""

-- | 塔状态
data TowerState = TowerState
                { t_id              :: TargetId
                , t_camp            :: Camp
                , t_position        :: Position
                , t_goldPerSecond   :: Int
                , t_aura            :: Aura
                , t_initHealthPoint :: Float
                , t_healthPoint     :: Float
                }
deriveJSON defaultOptions { fieldLabelModifier = \(_:_:xs) -> xs } ''TowerState

-- | 英雄: Shooter(远程)／Warrior(近战)
data HeroType = Shooter | Warrior deriving (Show, Eq)
instance FromJSON HeroType where
    parseJSON "shooter" = pure Shooter
    parseJSON "warrior" = pure Warrior
    parseJSON s         = fail $ "英雄不能识别: " <> (show s)
instance ToJSON HeroType where
    toJSON Shooter = "shooter"
    toJSON Warrior = "warrior"

-- | 英雄状态
data HeroStatus = Moving | Attacking | WaitingAttack | Firing | WaitingFire | Stop | Dazing | Dead deriving (Show, Eq)
deriveJSON defaultOptions { constructorTagModifier = \(x:xs) -> (toLower x):xs } ''HeroStatus

-- | 英雄朝向
data Orientation = HTop | HLeftTop | HLeft | HLeftBottom | HBottom | HRightBottom | HRight | HRightTop deriving (Show, Eq)
deriveJSON defaultOptions { constructorTagModifier = \(_:x:xs) -> (toLower x):xs } ''Orientation

-- | 英雄数据
data HeroState = HeroState
               { h_id                 :: HeroId
               , h_name               :: HeroType
               , h_camp               :: Camp
               , h_moveSpeed          :: Int
               , h_attackDamageMin    :: Int
               , h_attackDamageMax    :: Int
               , h_attackRange        :: Int
               , h_attackAnimation    :: Int
               , h_attackGap          :: Int
               , h_attackMissileSpeed :: Int
               , h_fireName           :: String
               , h_fireDamage         :: Int
               , h_fireDuration       :: Int
               , h_fireRange          :: Int
               , h_fireAnimation      :: Int
               , h_fireGap            :: Int
               , h_fireMissileSpeed   :: Int
               -- 位置
               , h_position           :: Position
               -- 朝向
               , h_orientation        :: Orientation
               -- 状态
               , h_status             :: HeroStatus
               , h_attackCD           :: Int
               , h_fireCD             :: Int
               , h_dazingRemaining    :: Int
               , h_rebornRemaining    :: Int
               , h_healthPoint        :: Float
               , h_initHealthPoint    :: Float
               }
deriveJSON defaultOptions { fieldLabelModifier = \(_:_:xs) -> xs } ''HeroState

-- | 双方阵营得分
data Camps = Camps
           { blue :: Scores
           , red  :: Scores
           }
-- | 得分情况
data Scores = Scores
            { towerCount :: Int
            , killCount  :: Int
            , goldCount  :: Int
            } deriving (Show, Eq)
instance Ord Scores where
    -- 领先塔视为领先
    score1 <= score2 = towerCount score1 <= towerCount score2

-- | GameState 游戏状态
data GameState = GameState
               { time   :: Int
               , camps  :: Camps
               , towers :: [TowerState]
               , heros  :: [HeroState]
               }
deriveJSON defaultOptions ''GameState

-- | 攻击目标
data TargetType = HeroTarget | TowerTarget deriving (Show)
instance FromJSON TargetType where
    parseJSON "hero"  = pure HeroTarget
    parseJSON "tower" = pure TowerTarget
    parseJSON s       = fail $ "攻击类型不能识别: " <> (show s)
instance ToJSON TargetType where
    toJSON HeroTarget  = "hero"
    toJSON TowerTarget = "tower"

-- | 服务器回传命令
data FromServer = Ping -- 服务器每 **60*1000ms** 发送心跳包, 忽略它
                | Picking
                | Loading Camp GameState
                | Start
                | Update Camp GameState -- {"type": "updatePlayer" | "update"}
                | End
                | CmdError String
instance ToJSON FromServer where
    toJSON Ping           = "ping"
    toJSON Picking        = "picking"
    toJSON (Loading _ _)  = "loading"
    toJSON Start          = "start"
    toJSON (Update _ g)   = toJSON g
    toJSON End            = "end"
    toJSON (CmdError err) = toJSON err

instance FromJSON FromServer where
    parseJSON = withObject "server" $ \server -> do
      kind :: String
        <- server .: "type"
      case kind of
        "ping"         -> pure Ping
        "picking"      -> pure Picking
        "loading"      -> do g <- GameState <$> server .: "time"
                                            <*> server .: "camps"
                                            <*> server .: "towers"
                                            <*> server .: "heros"
                             Loading <$> server .: "myCamp" <*> pure g
        "start"        -> pure Start
        "update"       -> Update <$> server .: "myCamp" <*> server .: "state"
        "updatePlayer" -> Update <$> server .: "myCamp" <*> server .: "state"
        "end"          -> pure End -- <$> server .: "state"
        "cmdError"     -> CmdError <$> server .: "message"
        _              -> fail $ "服务器命令不识别: " ++ kind

-- | 传向服务器指令
data ToServer = Join GameId Token
              | PickHero [HeroType]
              | Attack HeroId TargetType TargetId
              | Fire HeroId HeroId
              | Move HeroId Position
              | StopHero HeroId
instance ToJSON ToServer where
    -- | join the game
    toJSON (Join gameId token) = do
      let join = "join" :: String
      object [ "type"   .= join
              , "gameId" .= gameId
              , "token"  .= token
              ]
    -- | pickHero: pick five heros
    toJSON (PickHero hs@(_:_:_:_:_:[])) = do
      let pickHero = "pickHero" :: String
      object [ "type"  .= pickHero
             , "heros" .= hs
             ]
    toJSON (PickHero _) = error "请选择 5 个英雄"

    -- | attack
    -- | 游戏平台将根据指令自动寻路到最佳可攻击地点进行攻击，参赛队伍不需要实现相关的算法
    toJSON (Attack hid ttype tid) = do
      let attack = "attack" :: String
      object [ "type"       .= attack
             , "heroId"     .= hid
             , "targetType" .= ttype
             , "targetId"   .= tid
             ]

    -- | fire, 仅能向英雄施法
    -- | 游戏平台将根据指令自动寻路到最佳可施放技能的地点施放技能，参赛队伍不需要实现相关的算法
    toJSON (Fire from to) = do
      let fire = "fire" :: String
      object [ "type"     .= fire
             , "heroId"   .= from
             , "targetId" .= to
             ]

    -- | move, 移动
    -- | 游戏平台将根据指令自动寻路、自动躲避障碍，参赛队伍不需要实现相关的算法
    toJSON (Move hid (Position x y)) = do
      let move = "move" :: String
      object [ "type"   .= move
             , "heroId" .= hid
             , "x"      .= x
             , "y"      .= y
             ]

    -- | stop
    toJSON (StopHero hid) = do
      let stop = "stop" :: String
      object [ "type" .= stop
             , "heroId" .= hid
             ]
deriveFromJSON defaultOptions ''ToServer

instance ToJSON Status where
    toJSON GamePending = "pending"
    toJSON GamePicking = "picking"
    toJSON GameLoading = "loading"
    toJSON GameStart   = "start"
    toJSON GameEnd     = "end"

instance FromJSON Status where
    parseJSON "pending" = pure GamePending
    parseJSON "picking" = pure GamePicking
    parseJSON "loading" = pure GameLoading
    parseJSON "start"   = pure GameStart
    parseJSON "end"     = pure GameEnd
    parseJSON s         = fail $ "游戏状态不识别: " <> (show s)

deriveJSON defaultOptions ''Scores
deriveJSON defaultOptions ''Camps

-- | 获取距离
class Pos a where
    (<->) :: a -> a -> Float

instance Pos Position where
    {-# INLINE (<->) #-}
    p1 <-> p2 = sqrt $ (x p1 - x p2) ** 2 + (y p1 - y p2) ** 2

instance Pos HeroState where
    h1 <-> h2 = (h_position h1) <-> (h_position h2)

instance Pos TowerState where
    t1 <-> t2 = (t_position t1) <-> (t_position t2)

{-# INLINE (<.>) #-}
(<.>) :: HeroState -> TowerState -> Float
HeroState{..} <.> TowerState{..} = h_position <-> t_position
