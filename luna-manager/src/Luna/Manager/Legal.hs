module Luna.Manager.Legal where

import Prologue

companyName, copyright, productName, productDescription
    :: (IsString a, Semigroup a) => a
companyName        = "New Byte Order Sp. z o. o."
copyright          = "Copyright (c) 2018 " <> companyName
-- [TODO]: We should try to extract the `productName` and `productDescription` from application name
productName        = "Luna Studio"
productDescription = "Luna Studio"
