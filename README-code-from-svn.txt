                             l i b S B M L
                    
==========================================================================
Stable
==========================================================================
The libSBML code for SBML Level 3 accepted packages has been integrated
with the main libSBML trunk of the SVN repository.

https://svn.code.sf.net/p/sbml/code/trunk/libsbml

The code in the above directory contains the full code for core libSBML 
and any accepted L3 packages.

==========================================================================
Experimental
==========================================================================

The libSBML code for SBML Level 3 packages that are currently under 
development is considered to be 'experimental' code; as it is subject 
to change. This code has been integrated with the accepted code into a 
branch of the SVN repository.

https://svn.code.sf.net/p/sbml/code/branches/libsbml-experimental

The code in this branch contains the full code for core libSBML, 
any accepted L3 packages and any packages which are under development.

==========================================================================
Using libSBML with/without packages
==========================================================================

In order to use either the stable or experimental code:

1. Checkout the appropriate code (one of the paths given above).

2. Run CMake and enable those packages that you wish to use.
   (Note all packages are disabled by default.)

3. Build.

==========================================================================
The LibSBML Team.


     .-://///:`  .:/+++++/-`      .--.             `---`  `--
  -/++//:---:.`://+syyyssoo+`    ohhy`            /hhh.  -hy`
`/++/-`       ::/ohhyyssssoss-   ohhh+           .yhhh.  .hy`          
:++/.        `:::sysoo+++++oss.  ohoyh-         `ohoyh.  .hy`          
++//`        `--:/oo+///://+os:  oh//hs`        :hs.yh.  .hy`          
/+//.       `..--:////:--:/oos.  oh/`sh/       `yh-`yh.  .hy`          
`////:-.......---::://///++oo-   oh/ -hy.      +h+ `yh.  .hy`          
  .:///:::::--::::://///++oo:    oh/  +hs     -hy` `yh.  .hy`          
`::-``..--::::::://osyyysoooo.   oh/  `yh:   `sh:  `yh.  .hy`          
:o+/`      .:////oyhyyyyyyssss`  oh/   :hy`  /ho`  `yh.  .hy`          
/oo/        .///oyysoo+++oosyy-  oh/    oho .hh.   `yh.  .hy`          
.sso:       `+++oso+//////syyy`  oh/    .hh-oh/    `yh.  .hy`          
 :sss+-`   ./oooooo//:::+syyy.   oh/     /hhhs`    `yh.  -hy`          
  `/syssooossssssssssssyyyy/`    oh/      shh-     `yh.  -hhooooooooooo
    `-/+oso+/-.-:/osyyso/-`      -:.      .:-      `--`  `:::::::::::::
