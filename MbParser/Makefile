all:
	happy -gca ParMbCore.y
	alex -g LexMbCore.x
	ghc --make TestMbCore.hs -o TestMbCore
	ghc --make Interpreter.hs
	
clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocMbCore.* LexMbCore.* ParMbCore.* LayoutMbCore.* SkelMbCore.* PrintMbCore.* TestMbCore.* AbsMbCore.* TestMbCore ErrM.* SharedString.* ComposOp.* MbCore.dtd XMLMbCore.* Makefile*
