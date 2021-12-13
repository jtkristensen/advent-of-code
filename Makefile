
%:
	echo "module Main where"                          > ./app/Main.hs ; \
	echo "import $@"                                 >> ./app/Main.hs ; \
	echo "main :: IO ()"                             >> ./app/Main.hs ; \
	echo "main = readFile \"$(input)\" >>= solution" >> ./app/Main.hs ; \
	path=$$(echo $@ | sed -r 's/\./\//g')                             ; \
	cd "./src/$$(dirname $$path)"                                     ; \
	stack run
