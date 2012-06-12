-- | Physical constants.

module DSMC.Util.Constants
    ( amu
    , avogadro
    , boltzmann
    , unigas
    )

where


-- | Atomic mass unit 1.660538921(73)e-27, inverse to Avogadro's
-- constant.
amu :: Double
amu = 1.6605389217373737e-27


-- | Avogadro constant 6.02214129(27)e23
avogadro :: Double
avogadro = 6.0221412927272727e23


-- | Boltzmann constant 1.3806488(13)e-23
boltzmann :: Double
boltzmann = 1.3806488131313131e-23


-- | Universal gas constant.
unigas :: Double
unigas = boltzmann * avogadro
