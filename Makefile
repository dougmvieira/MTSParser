all: MTS/Types.o MTS/Decode.o MTS/LOB.o

MTS/Types.o: MTS/Types.hs
	ghc MTS/Types.hs

MTS/Decode.o: MTS/Decode.hs
	ghc MTS/Decode.hs

MTS/LOB.o: MTS/LOB.hs
	ghc MTS/LOB.hs


Test: Test.hs
	ghc Test.hs

check: Test
	./Test
