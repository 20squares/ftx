{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

module FTX where

import Engine.Engine
import Preprocessor.Preprocessor



-- You cannot withdraw from FTX

-- You can withdraw TRX

-- TRX price is higher inside FTX than outside FTX

-- Justin Suns is providing liquitity in TRX to FTX

-- They can print TRX

-- Sun gives TRX to FTX and in return receives ETH

-- Sun now has a boatload of ETH bought at discount price

-- TRX inside is higher than outside

-- YOu as a user cannot add TRX to FTX but you can take it out.

-- Stay as you are OR convert everything in TRX and take it out.

-- Withdraw at 80%

-- Give up your credit towards FTX in exchange for having less risk

-- If enough people do so, FTX's debt reduces

-- If you don't but enough people do, you get your money back


-- Sun gives TRX at a high discount to FTX in terms of ETH. 
-- Users can exchange their internal assets in FTX to TRX at a relatively high price - higher than the outside price anyways
--

----------
-- 0 Types
----------

type Price = Double

type TRX = Double

type Eth = Double

-- Define generic alternative asset locked in ftx
type AltAssetFTX = Double

-- Grid parameter for action space
type GridParameter = TRX

-- Define action space given grid parameter and max balance for coin
actionSpace :: GridParameter -> (x,Price) -> [x]
actionSpace par (balance,_) = [0,par..balance]


----------------------------
-- 1 Auxiliary functionality
----------------------------


--------------
-- 1 Decisions
--------------
-- | Withdraw TRX
withdrawTRX name gridParameterTRX = [opengame|

    inputs    : balanceTRX,outsidePrice;
    feedback  : ;

    :-----:
    inputs    : balanceTRX,outsidePrice ;
    feedback  : ;
    operation : dependentDecision name $ actionSpace gridParameterTRX ;
    outputs   : withdrawTRX ;
    returns   : returnsFromWithdrawlTRX ;

    :-----:

    outputs   : withdrawTRX ;
    returns   : returnsFromWithdrawlTRX;
|]

-- | Exchange alternative asset into TRX at a given price
exchangeToTRX name balanceAlternativeCoin gridParameterAlternativeCoin = [opengame|

    inputs    : balanceAlternativeCoin, exchangePrice;
    feedback  : ;

    :-----:
    inputs    : balanceAlternativeCoin, exchangePrice ;
    feedback  : ;
    operation : dependentDecision name $ actionSpace gridParameterAlternativeCoin ;
    outputs   : trx ;
    returns   : 0 ;
    \\ Assume for now that this decision does not generate immediate value per se

    :-----:

    outputs   : trx ;
    returns   : ;
|]

