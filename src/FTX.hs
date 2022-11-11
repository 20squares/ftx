{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveAnyClass #-}

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

type ExchangeRatio = Double

type TRX = Double

-- Generic asset held inside ftx platform
type CoinXInside = Double

-- Generic asset as exchange target for trx outside ftx platform
type CoinYOutside = Double

-- Define generic alternative asset locked in ftx

-- Grid parameter for action space
type GridParameter = TRX

-- Define action space given grid parameter and max balance for coin
actionSpace :: (Num x,Enum x) => x -> (x,ExchangeRatio) -> [x]
actionSpace par (balance,_) = [0,par..balance]

-- Define eCoinchange function
exchangeFunction :: Num x => x -> x -> x
exchangeFunction ratio x = x * ratio

-- Define helper subtract and addition functions
addToBalance balance x = balance + x
subtractFromBalance balance x = balance - x
----------------------------
-- 1 Auxiliary functionality
----------------------------
-- | Swap coin x against y given current prices
swapCoinXforY = [opengame|

    inputs    : amountCoinX,exchangeRatio;
    feedback  : ;

    :-----:
    inputs    : amountCoinX, exchangeRatio ;
    feedback  : ;
    operation : forwardFunction $ uncurry $ exchangeFunction   ;
    outputs   : amountCoinY;
    returns   : ;

    :-----:

    outputs   : amountCoinY ;
    returns   : ;
|]

--------------
-- 2 Decisions
--------------
-- | Exchange alternative asset into TRX at a exchange given price
-- NOTE: Decision inside ftx platform
exchangeToTRX name gridParameterCoinX = [opengame|

    inputs    : balanceCoinX, exchangePrice;
    feedback  : ;

    :-----:
    inputs    : balanceCoinX, exchangePrice ;
    feedback  : ;
    operation : dependentDecision name $ actionSpace gridParameterCoinX ;
    outputs   : coinX ;
    returns   : 0 ;
    // Assume for now that this decision does not generate immediate value per se

    :-----:

    outputs   : coinX ;
    returns   : ;
    |]


-- | Withdraw TRX
withdrawTRX name gridParameterTRX = [opengame|

    inputs    : balanceTRX,exchangePrice;
    feedback  : ;

    :-----:
    inputs    : balanceTRX,exchangePrice ;
    feedback  : ;
    operation : dependentDecision name $ actionSpace gridParameterTRX ;
    outputs   : withdrawTRX ;
    returns   :  0;
    // We settle values later

    :-----:

    outputs   : withdrawTRX ;
    returns   : ;
|]

-- | Exchange TRX asset into alternative asset at a given exchange price
-- NOTE Decision outside ftx platform
exchangeFromTRX name gridParameterTRX = [opengame|

    inputs    : balanceTRX, exchangePrice;
    feedback  : ;

    :-----:
    inputs    : balanceTRX, exchangePrice ;
    feedback  : ;
    operation : dependentDecision name $ actionSpace gridParameterTRX ;
    outputs   : coinY ;
    returns   : 0 ;
    // Assume for now that this decision does not generate immediate value per se

    :-----:

    outputs   : coinY ;
    returns   : ;
|]


----------------------
-- 3 Composed Decision
----------------------


decision name gridParameterCoinX gridParameterTRX = [opengame|

    inputs    : balanceCoinX, balanceCoinY, balanceTRX, exchangePriceXtoTRX, exchangePriceTRXtoY;
    feedback  : ;

    :-----:
    inputs    : balanceCoinX, exchangePriceXtoTRX ;
    feedback  : ;
    operation : exchangeToTRX name gridParameterCoinX ;
    outputs   : coinX ;
    returns   : ;

    inputs    : balanceCoinX, coinX ;
    feedback  : ;
    operation : forwardFunction $ uncurry subtractFromBalance ;
    outputs   : balanceCoinXNew ;
    returns   : ;

    inputs    : coinX, exchangePriceXtoTRX ;
    feedback  : ;
    operation : swapCoinXforY ;
    outputs   : trx ;
    returns   : ;

    inputs    : balanceTRX, trx ;
    feedback  : ;
    operation : forwardFunction $ uncurry addToBalance ;
    outputs   : balanceTRXIntermediate ;
    returns   : ;

    inputs    : balanceTRXIntermediate, exchangePriceTRXtoY ;
    feedback  : ;
    operation : withdrawTRX name gridParameterTRX ;
    outputs   : trxWithdrawn ;
    returns   : ;

    inputs    : trxWithdrawn, exchangePriceTRXtoY ;
    feedback  : ;
    operation : exchangeFromTRX name gridParameterTRX ;
    outputs   : trxExchanged ;
    returns   : ;

    inputs    : balanceTRXIntermediate, trxWithdrawn ;
    feedback  : ;
    operation : forwardFunction $ uncurry subtractFromBalance ;
    outputs   : balanceTRXNew ;
    returns   : ;

    inputs    : trxExchanged, exchangePriceTRXtoY ;
    feedback  : ;
    operation : exchangeToTRX name gridParameterTRX ;
    outputs   : coinY ;
    returns   : ;

    inputs    : balanceCoinY, coinY ;
    feedback  : ;
    operation : forwardFunction $ uncurry addToBalance ;
    outputs   : balanceCoinYNew ;
    returns   : ;


    :-----:

    outputs   : (balanceCoinXNew,balanceCoinYNew,balanceTRXNew) ;
    returns   : ;
|]

------------------------------------
-- 3 Create probbility distributions
------------------------------------

  

----------------------
-- 4 Payoffs
----------------------

