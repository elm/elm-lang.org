cd server
ghc --make -O2 -threaded -hidir ghc_output -odir ghc_output Server.hs
mv Server ..
cd ..