#! /usr/bin/env nix-shell
#! nix-shell -i bash -p nix bash cacert rsync -I nixpkgs=channel:nixos-19.03
set -eu

app=$(basename *.cabal .cabal)

nix-shell -I nixpkgs=channel:nixos-19.03 --run "cabal v1-configure --ghcjs; cabal v1-build"

outpath=dist/build/$app/$app.jsexe

sed -i 's/var h$currentThread = null;/originalProcess=process; process=undefined; var h$currentThread=null;/' $outpath/all.js
sed -i 's/h$main(h$mainZCZCMainzimain);/h$main(h$mainZCZCMainzimain) ; process = originalProcess;/' $outpath/all.js

sed -i 's/function h$ap_1_0(h$RTS_577)/function h$ap_1_0_deleted(h$RTS_577)/' $outpath/all.js
sed -i '0,/function h$ghcjsbn_toDouble_b/s/function h$ghcjsbn_toDouble_b/function h$ghcjsbn_toDouble_b_deleted/' $outpath/all.js
sed -i '0,/function h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e/s/function h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e/function h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e_deleted/' $outpath/all.js
sed -i '0,/function h$integerzmgmpZCGHCziIntegerziTypeziJnzh_con_e/s/function h$integerzmgmpZCGHCziIntegerziTypeziJnzh_con_e/function h$integerzmgmpZCGHCziIntegerziTypeziJnzh_con_e_deleted/' $outpath/all.js
sed -i '0,/function h$integerzmgmpZCGHCziIntegerziTypeziJpzh_con_e/s/function h$integerzmgmpZCGHCziIntegerziTypeziJpzh_con_e/function h$integerzmgmpZCGHCziIntegerziTypeziJpzh_con_e_deleted/' $outpath/all.js
sed -i '0,/function h$integerzmgmpZCGHCziIntegerziTypeziSzh_con_e/s/function h$integerzmgmpZCGHCziIntegerziTypeziSzh_con_e/function h$integerzmgmpZCGHCziIntegerziTypeziSzh_con_e_deleted/' $outpath/all.js

sed -i 's/use strict//g' $outpath/all.js

#./node_modules/.bin/google-closure-compiler $outpath/all.js --compilation_level=ADVANCED_OPTIMIZATIONS --jscomp_off=checkVars --externs=$outpath/all.js.externs > $outpath/all.min.js

rsync --checksum $outpath/all.js ./rnproject/src/all.js
