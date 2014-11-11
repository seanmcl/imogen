
default : dist

configure :
	runhaskell Setup.hs configure --user

build : 
	runhaskell Setup.hs build

install : 
	runhaskell Setup.hs install

dist : configure build install 

hlint :
	hlint src -h .hlint

.PHONY : doc

doc : 
	runhaskell Setup.hs haddock --executables \
                                    --hyperlink-source \
	                            --hscolour-css=util/hscolour.css \
                                    --hoogle \
	 		            --haddock-option="-h" \
	                            --haddock-option="--use-unicode" \
#                                   --haddock-option="--read-interface=/Users/sean/save/projects/atp/dist/doc/html/ATP/ATP.haddock" \

clean :
	runhaskell Setup.hs clean 
	rm -rf dist imogen.log imogen.prof
	find . \( -name "*~" -or -name "*.o" -or -name "*.hi" \) -exec rm -f {} \;

