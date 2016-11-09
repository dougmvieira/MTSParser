all: MTS/Types.o MTS/Query.o MTS/Decode.o MTS/LOB.o MTS/Encode.o

MTS/Types.o: MTS/Types.hs
	ghc MTS/Types.hs

MTS/Query.o: MTS/Query.hs MTS/Types.o
	ghc MTS/Query.hs

MTS/Decode.o: MTS/Decode.hs MTS/Types.o
	ghc MTS/Decode.hs

MTS/LOB.o: MTS/LOB.hs MTS/Types.o MTS/Decode.o
	ghc MTS/LOB.hs

MTS/Encode.o: MTS/Encode.hs MTS/LOB.o
	ghc MTS/Encode.hs


Test: Test.hs
	ghc Test.hs

check: Test
	./Test
