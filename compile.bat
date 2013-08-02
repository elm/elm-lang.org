taskkill /F /IM ElmServer.exe
cd server
ghc --make -O2 -threaded -hidir ghc_output -odir ghc_output Server.hs -o ElmServer
move ElmServer.exe ..
cd ..
start "Elm Server" ElmServer.exe
