{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module StaticFiles where

import Language.Haskell.TH
import System.Directory (doesFileExist)

{- | 编译期检查静态文件是否存在，并返回其 Web 路径（Text 类型）。
  如果文件不存在，则会触发编译错误。
-}
staticFile :: FilePath -> Q Exp
staticFile filePath = do
    let fullPath = "static/" ++ filePath -- 假设所有静态文件都在 'static/' 目录下
    exists <- runIO $ doesFileExist fullPath
    if exists
        then -- 如果文件存在，生成一个 Text 表达式，包含 Web 路径
        -- 注意：这里假设 filePath 是相对于 'static/' 目录的路径
        -- 所以在生成的 Text 中需要加上 "/static/" 前缀
            [|toText $ "/static/" ++ filePath|]
        else -- 如果文件不存在，则抛出编译错误
            fail $ "静态文件不存在: " ++ fullPath