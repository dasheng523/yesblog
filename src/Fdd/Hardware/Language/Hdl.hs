module Fdd.Hardware.Language.Hdl where

import Fdd.Hardware.Common

type ComponentIndex = String

data ComponentDef
    = ComponentDef ComponentIndex ComponentPassport

type Hdl = [ComponentDef]