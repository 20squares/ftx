{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}

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

-- Haskell type issue
deriving instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l, Show m, Show n, Show o, Show p) => Show (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)
deriving instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l, Show m, Show n, Show o, Show p, Show q) => Show (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q)



-- Define action space given grid parameter and max balance for coin
actionSpace :: (Num x,Enum x) => x -> (x,ExchangeRatio) -> [x]
actionSpace par (balance,_) = [0,par..balance]

-- Define eCoinchange function
exchangeFunction :: Num x => x -> x -> x
exchangeFunction ratio x = x * ratio

-- Define helper subtract and addition functions
addToBalance,subtractFromBalance :: Double -> Double -> Double
addToBalance balance x = balance + x
subtractFromBalance balance x = balance - x

-- Compute profits 
computePayoffs (diffX,diffY,diffTRXInside,diffTRXOutside,pX,pY,pTRXInsideFTX,pTRXOutside) =
  (diffX* pX)  +  (diffY*pY) +  (diffTRXInside * pTRXInsideFTX) +  (diffTRXOutside * pTRXOutside)

-- Compute the differences of accounts
computeDiffBalances
  :: (Num a, Num b, Num c, Num d) =>
     (a, b, c, d, a, b, c, d) -> (a, b, c, d)
computeDiffBalances (balanceCoinX, balanceCoinY, balanceTRXInside,balanceTRXOutside,balanceCoinXNew,balanceCoinYNew,balanceTRXInsideNew,balanceTRXOutsideNew) =
  (balanceCoinXNew - balanceCoinX, balanceCoinYNew - balanceCoinY, balanceTRXInsideNew - balanceTRXInside, balanceTRXOutsideNew - balanceTRXOutside)

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


decisions name gridParameterCoinX gridParameterTRX = [opengame|

    inputs    : balanceCoinX, balanceCoinY, balanceTRXInside,balanceTRXOutside, exchangePriceXtoTRX, exchangePriceTRXtoY;
    feedback  : ;

    :-----:
    inputs    : balanceCoinX, exchangePriceXtoTRX ;
    feedback  : ;
    operation : exchangeToTRX name gridParameterCoinX ;
    outputs   : coinX ;
    returns   : ;
    // exchange x for trx inside

    inputs    : coinX, exchangePriceXtoTRX ;
    feedback  : ;
    operation : swapCoinXforY ;
    outputs   : trx ;
    returns   : ;
    // compute exchange

    inputs    : balanceCoinX, coinX ;
    feedback  : ;
    operation : forwardFunction $ uncurry subtractFromBalance ;
    outputs   : balanceCoinXNew ;
    returns   : ;
    // update balance of x coin inside

    inputs    : balanceTRXInside, trx ;
    feedback  : ;
    operation : forwardFunction $ uncurry addToBalance ;
    outputs   : balanceTRXIntermediate ;
    returns   : ;
    // update balance of trx inside

    inputs    : balanceTRXIntermediate, exchangePriceTRXtoY ;
    feedback  : ;
    operation : withdrawTRX name gridParameterTRX ;
    outputs   : trxWithdrawn ;
    returns   : ;
    // withdraw trx from ftx

    inputs    : balanceTRXIntermediate, trxWithdrawn ;
    feedback  : ;
    operation : forwardFunction $ uncurry subtractFromBalance ;
    outputs   : balanceTRXNewInside ;
    returns   : ;
    // update balance of trx inside ftx

    inputs    : balanceTRXOutside, trxWithdrawn ;
    feedback  : ;
    operation : forwardFunction $ uncurry addToBalance ;
    outputs   : balanceTRXOutsideIntermediate ;
    returns   : ;
    // update balance of trx outside ftx

    inputs    : balanceTRXOutsideIntermediate, exchangePriceTRXtoY ;
    feedback  : ;
    operation : exchangeFromTRX name gridParameterTRX ;
    outputs   : trxExchanged ;
    returns   : ;
    // exchange trx outside for y

    inputs    : balanceTRXOutsideIntermediate, trxExchanged ;
    feedback  : ;
    operation : forwardFunction $ uncurry subtractFromBalance ;
    outputs   : balanceTRXNewOutside ;
    returns   : ;
    // update balance of trx outside

    inputs    : trxExchanged, exchangePriceTRXtoY ;
    feedback  : ;
    operation : exchangeToTRX name gridParameterTRX ;
    outputs   : coinY ;
    returns   : ;
    // compute actual exchange of y

    inputs    : balanceCoinY, coinY ;
    feedback  : ;
    operation : forwardFunction $ uncurry addToBalance ;
    outputs   : balanceCoinYNew ;
    returns   : ;
    // update balance of y outside

    :-----:

    outputs   : balanceCoinXNew,balanceCoinYNew,balanceTRXNewInside,balanceTRXNewOutside ;
    returns   : ;
|]

------------------------------------
-- 3 Create probbility distributions
------------------------------------
-- Create probability distributions for the value of coin x and trx _inside_ ftx
-- We assume that the other parameters are taken as exogenous

-- with prob p value of coin x inside ftx 0; with prob (1-p) value
distributionPriceX p value = distFromList [(0,p),(value,(1-p))]

-- with prob p value of trx inside ftx 0; with prob (1-p) value
distributionPriceTRX p value = distFromList [(0,p),(value,(1-p))]


priceDistributions pX valueX pTRX valueTRX = [opengame|

    inputs    : ;
    feedback  : ;

    :-----:
    inputs    : ;
    feedback  : ;
    operation : nature $ distributionPriceX pX valueX;
    outputs   : priceX;
    returns   : ;

    inputs    : ;
    feedback  : ;
    operation : nature $ distributionPriceTRX pTRX valueTRX;
    outputs   : priceTRX;
    returns   : ;

    :-----:

    outputs   : priceX,priceTRX;
    returns   : ;
|]



------------
-- 4 Payoffs
------------

payoffs name  = [opengame|

    inputs    : balanceCoinX, balanceCoinY, balanceTRXInside,balanceTRXOutside,balanceCoinXNew,balanceCoinYNew,balanceTRXInsideNew,balanceTRXOutsideNew,pX,pY,pTRXInsideFTX,pTRXOutside ;
    feedback  : ;

    :-----:

    inputs    : balanceCoinX, balanceCoinY, balanceTRXInside,balanceTRXOutside, balanceCoinXNew,balanceCoinYNew,balanceTRXInsideNew,balanceTRXOutsideNew ;
    feedback  : ;
    operation : forwardFunction $ computeDiffBalances ;
    outputs   : diffX,diffY,diffTRXInside,diffTRXOutside ;
    returns   : ;

    inputs    : diffX,diffY,diffTRXInside,diffTRXOutside,pX,pY,pTRXInsideFTX,pTRXOutside;
    feedback  : ;
    operation : forwardFunction $ computePayoffs ;
    outputs   : profit ;
    returns   : ;

    inputs    : profit ;
    feedback  : ;
    operation : addPayoffs name ;
    outputs   : ;
    returns   : ;

    :-----:

    outputs   : ;
    returns   : ;
|]

---------------------------
-- 5 Assemble complete game
---------------------------

completeGame name  pX valueX pTRX valueTRX gridParameterCoinX gridParameterTRX exchangePriceXtoTRX  exchangePriceTRXtoY = [opengame|

    inputs    : balanceCoinX, balanceCoinY, balanceTRXInside,balanceTRXOutside,pTRXInsideFTX,pTRXOutside ;
    feedback  : ;

    :-----:

    inputs    : ;
    feedback  : ;
    operation : priceDistributions pX valueX pTRX valueTRX ;
    outputs   : priceX,priceTRX ;
    returns   : ;

    inputs    : balanceCoinX, balanceCoinY, balanceTRXInside,balanceTRXOutside, exchangePriceXtoTRX, exchangePriceTRXtoY;
    feedback  : ;
    operation : decisions name gridParameterCoinX gridParameterTRX;
    outputs   : balanceCoinXNew,balanceCoinYNew,balanceTRXInsideNew,balanceTRXOutsideNew ;
    returns   : ;

    inputs    : balanceCoinX, balanceCoinY, balanceTRXInside,balanceTRXOutside,balanceCoinXNew,balanceCoinYNew,balanceTRXInsideNew,balanceTRXOutsideNew,priceX,priceTRX,pTRXInsideFTX,pTRXOutside;
    feedback  : ;
    operation : payoffs name ;
    outputs   : ;
    returns   : ;


    :-----:

    outputs   : ;
    returns   : ;
|]

-----------
-- Analysis
-----------

analysis
  :: Double
     -> Payoff
     -> Double
     -> Payoff
     -> ExchangeRatio
     -> Double
     -> ExchangeRatio
     -> ExchangeRatio
     -> List
          '[Kleisli Stochastic (Double, ExchangeRatio) Double,
            Kleisli Stochastic (Double, ExchangeRatio) Double,
            Kleisli Stochastic (Double, ExchangeRatio) Double,
            Kleisli Stochastic (Double, ExchangeRatio) Double]
     -> StochasticStatefulContext
          (ExchangeRatio, Double, Double, Double, Payoff, Payoff) () () ()
     -> IO ()
analysis pX valueX pTRX valueTRX gridParameterCoinX gridParameterTRX exchangePriceXtoTRX  exchangePriceTRXtoY strat context = generateIsEq $ evaluate (completeGame "agent" pX valueX pTRX valueTRX gridParameterCoinX gridParameterTRX exchangePriceXtoTRX  exchangePriceTRXtoY) strat context

-------------
-- Strategies
-------------

-- Always exchange, withdraw etc. max
maxStrategy :: Kleisli Stochastic (Double, ExchangeRatio) Double
maxStrategy = Kleisli (\(balance,_) -> playDeterministically balance)

-- For all decisions 
overallMaxStrategy = maxStrategy ::- maxStrategy ::- maxStrategy ::- maxStrategy ::- Nil

-- Always exchange, withdraw etc. nothing
zeroStrategy :: Kleisli Stochastic (Double, ExchangeRatio) Double
zeroStrategy = pureAction 0

 -- For all decisions  
overallZeroStrategy = zeroStrategy ::- zeroStrategy ::- zeroStrategy ::- zeroStrategy ::- Nil


------------------
-- Initial Context
------------------

initialContextLinear balanceCoinX balanceCoinY balanceTRXInside balanceTRXOutside pTRXInsideFTX pTRXOutside = StochasticStatefulContext (pure ((),(balanceCoinX, balanceCoinY, balanceTRXInside,balanceTRXOutside,pTRXInsideFTX,pTRXOutside))) (\_ _ -> pure ())

scenario strat = analysis 10 10 0.1 2 1 1 1  1 strat (initialContextLinear 10 0 0 0 1 0.5 )

checkStrategiesMax = scenario overallMaxStrategy
