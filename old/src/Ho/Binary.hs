module Ho.Binary() where


import Control.Monad
import Data.Binary

import Ho.Type


instance Binary HoHeader where
    put (HoHeader aa ab ac ad) = do
	    put aa
	    put ab
	    put ac
	    put ad
    get = do
        aa <- get
        ab <- get
        ac <- get
        ad <- get
        return (HoHeader aa ab ac ad)

instance Binary HoBuild where
    put (HoBuild ae af ag ah ai ak al am) = do
	    put ae
	    put af
	    put ag
	    put ah
	    put ai
	    put ak
	    put al
	    put am
    get = do
    ae <- get
    af <- get
    ag <- get
    ah <- get
    ai <- get
    ak <- get
    al <- get
    am <- get
    return (HoBuild ae af ag ah ai ak al am)


instance Binary HoExp where
    put (HoExp ac ad) = do
	    put ac
	    put ad
    get = do
    ac <- get
    ad <- get
    return (HoExp ac ad)
