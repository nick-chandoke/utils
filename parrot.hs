#!/usr/bin/env stack
-- stack exec ghc --package rio --package wai --package warp --package wai-extra --package lucid --package case-insensitive -- -threaded -O2 -Wall

{-# language TypeApplications, BlockArguments, OverloadedStrings, NoImplicitPrelude, ViewPatterns #-}

module Main where

import RIO
import Prelude (read) -- safety be damned
import System.IO (hPutStrLn, stderr)

-- the server essentials
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Char8 as BS

import Lucid
import Data.CaseInsensitive (original)

import System.Environment (lookupEnv)

main :: IO ()
main = do
  port <- maybe 1111 (read @Int) <$!> lookupEnv "PORT"
  hPutStrLn stderr "parrot: a server whose response is determined exclusively by its request"
  hPutStrLn stderr $! "starting to run on port " <> show port <> "..."
  runEnv port \req resp ->
      let hs = requestHeaders req
          headerTable = table_ $ do
              thead_ $ tr_ (th_ "Name" <> th_ "Value") -- TODO: add stylesheet
              tbody_ $ foldMap
                  (\(n,v) -> tr_ $ (th_ (toHtml $ original n) <> th_ (toHtml v)))
                  hs
          headerTable2 = foldMap -- bytestring version
              (\(n,v) -> original n <> ": " <> v <> "\n")
              hs
      in do
          resp . responseLBS ok200 [] <=< renderBST . doctypehtml_ $! do
              head_ (title_ "Parrot")
              body_ $! do
                  let str1 = (BS.pack . show $! httpVersion req) <> " " <> requestMethod req <> " " <> rawPathInfo req <> rawQueryString req <> " from " <> BS.pack (show $ remoteHost req)
                  liftIO (BS.putStrLn str1) *> p_ (toHtml str1)
                  body <- liftIO $ strictRequestBody req
                  if LBS.null body then
                      liftIO (BS.putStrLn "[no request body]") *> p_ "[no request body]"
                  else do
                      liftIO . LBS.putStrLn $ "request body:\n" <> body
                      p_ "request body:" *> p_ (toHtml body)
                  liftIO . BS.putStrLn $ "Headers:\n" <> headerTable2
                  h2_ "headers" *> headerTable
