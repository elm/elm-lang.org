cd server
ghc --make -O2 -hidir ghc_output -odir ghc_output Server.hs
mv Server ..
cd ..